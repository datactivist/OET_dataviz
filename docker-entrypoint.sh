#!/usr/bin/env bash
set -euo pipefail

echo "=== [Entrypoint] Initialisation du container ==="

# Usage: docker run ... <image> [mode]
# Modes:
#   import  - run import_data_dashboard.R then exit (useful for data-only runs)
#   serve   - start quarto serve WITHOUT importing (assumes data exists)
#   serve-import - attempt import then start quarto serve (safe: keeps old data on failure)
#   help    - show this message

MODE="${1:-serve-import}"
shift || true

# Fichier témoin pour indiquer que des données valides existent
DATA_MARKER="/data/.data_imported"
# Fichier pour tracer la dernière tentative d'import
LAST_IMPORT="/data/.last_import_attempt"

run_import() {
  if [ -f "import_data_dashboard.R" ]; then
    echo "=== Exécution du script R d'importation des données ==="
    
    # Créer une sauvegarde temporaire si des données existent déjà
    if [ -f "$DATA_MARKER" ]; then
      echo "=== Sauvegarde des données existantes ==="
      BACKUP_DIR="/data/.backup_$(date +%Y%m%d_%H%M%S)"
      mkdir -p "$BACKUP_DIR"
      # Copier tous les fichiers sauf les backups et fichiers cachés système
      find /data -maxdepth 1 -type f ! -name ".*" -exec cp {} "$BACKUP_DIR/" \; 2>/dev/null || true
      echo "Sauvegarde créée dans : $BACKUP_DIR"
    fi
    
    if R -e "source('import_data_dashboard.R')" ; then
      echo "=== Importation des données : OK ==="
      # Marquer l'import comme réussi
      touch "$DATA_MARKER"
      date +%s > "$LAST_IMPORT"
      # Nettoyer les anciennes sauvegardes (garder les 3 plus récentes)
      if [ -d "/data" ]; then
        ls -dt /data/.backup_* 2>/dev/null | tail -n +4 | xargs rm -rf 2>/dev/null || true
      fi
      return 0
    else
      echo "!!! ATTENTION : échec de l'import des données !!!"
      
      # Restaurer la sauvegarde si elle existe
      if [ -n "${BACKUP_DIR:-}" ] && [ -d "$BACKUP_DIR" ]; then
        echo "=== Restauration des données depuis la sauvegarde ==="
        cp "$BACKUP_DIR"/* /data/ 2>/dev/null || true
        echo "Données restaurées. Les anciennes données sont préservées."
      fi
      
      # Vérifier si des données utilisables existent
      if [ -f "$DATA_MARKER" ]; then
        echo "=== Des données valides préexistantes sont disponibles ==="
        echo "L'application continuera avec les données existantes."
        return 0
      else
        echo "=== Aucune donnée valide disponible ==="
        return 1
      fi
    fi
  else
    echo "ATTENTION : Le script import_data_dashboard.R est introuvable."
    return 2
  fi
}

serve_qmd() {
  QMD_FILE="application_OET.qmd"
  if [ ! -f "$QMD_FILE" ]; then
    echo "ERREUR : Le fichier $QMD_FILE n'existe pas dans le répertoire."
    exit 1
  fi
  
  # Vérifier que des données existent avant de lancer le serveur
  if [ ! -f "$DATA_MARKER" ]; then
    echo "ERREUR : Aucune donnée importée détectée."
    echo "Veuillez d'abord exécuter le mode 'import' ou 'serve-import'."
    exit 1
  fi
  
  echo "=== Lancement du serveur Quarto/Shiny ==="
  echo "Fichier : $QMD_FILE"
  if [ -f "$LAST_IMPORT" ]; then
    LAST_DATE=$(date -d "@$(cat $LAST_IMPORT)" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || echo "inconnue")
    echo "Dernière importation réussie : $LAST_DATE"
  fi
  exec quarto serve "$QMD_FILE" --port 3838 --host 0.0.0.0
}

case "$MODE" in
  import)
    run_import
    EXIT_CODE=$?
    if [ "$EXIT_CODE" -eq 0 ]; then
      echo "Import terminé avec succès. Sortie du container."
      exit 0
    else
      echo "Import terminé avec code $EXIT_CODE. Sortie du container."
      exit $EXIT_CODE
    fi
    ;;
    
  serve)
    # Mode serve pur : ne fait PAS d'import, démarre directement
    echo "=== Mode SERVE : démarrage sans import ==="
    serve_qmd
    ;;
    
  serve-import)
    # Mode serve avec import : tente l'import puis démarre le serveur
    echo "=== Mode SERVE-IMPORT : import puis démarrage ==="
    run_import || {
      EXIT_CODE=$?
      if [ "$EXIT_CODE" -eq 1 ]; then
        echo "ERREUR : Import échoué et aucune donnée valide disponible."
        echo "Impossible de démarrer l'application sans données."
        exit 1
      fi
      # Si EXIT_CODE=0, c'est que des données préexistantes sont OK
    }
    serve_qmd
    ;;
    
  help|-h|--help)
    echo "Usage: docker run ... <image> [mode]"
    echo ""
    echo "Modes disponibles:"
    echo "  import        - Exécute uniquement l'import puis quitte"
    echo "                  (avec sauvegarde/restauration automatique)"
    echo ""
    echo "  serve         - Lance UNIQUEMENT le serveur Quarto/Shiny"
    echo "                  (sans import, suppose que les données existent)"
    echo ""
    echo "  serve-import  - Import PUIS lance le serveur (défaut)"
    echo "                  (sécurisé : garde les anciennes données si échec)"
    echo ""
    echo "  help          - Affiche ce message"
    echo ""
    echo "Workflow recommandé:"
    echo "  1. docker run --rm <image> import     # Import initial"
    echo "  2. docker run <image> serve           # Démarre l'app (pas de ré-import)"
    echo ""
    echo "  OU en une seule commande:"
    echo "  docker run <image> serve-import       # Import + serve en une fois"
    exit 0
    ;;
    
  *)
    echo "Mode inconnu: $MODE"
    echo "Utilisez 'help' pour voir les modes disponibles."
    exit 2
    ;;
esac
