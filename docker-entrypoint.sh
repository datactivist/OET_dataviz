#!/usr/bin/env bash
set -euo pipefail

echo "=== [Entrypoint] Initialisation du container ==="

# Usage: docker run ... <image> [mode]
# Modes:
#   import  - run import_data_dashboard.R then exit (useful for data-only runs)
#   serve   - attempt non-blocking import then start `quarto serve` (default)
#   help    - show this message

MODE="${1:-serve}"
shift || true

# Fichier témoin pour indiquer que des données valides existent
DATA_MARKER="/data/.data_imported"

run_import() {
  if [ -f "import_data_dashboard.R" ]; then
    echo "=== Exécution du script R d'importation des données ==="
    
    # Créer une sauvegarde temporaire si des données existent déjà
    if [ -f "$DATA_MARKER" ]; then
      echo "=== Sauvegarde des données existantes ==="
      BACKUP_DIR="/data/.backup_$(date +%Y%m%d_%H%M%S)"
      mkdir -p "$BACKUP_DIR"
      # Copier tous les fichiers sauf les backups
      find /data -maxdepth 1 -type f ! -path "*/.*" -exec cp {} "$BACKUP_DIR/" \; 2>/dev/null || true
      echo "Sauvegarde créée dans : $BACKUP_DIR"
    fi
    
    if R -e "source('import_data_dashboard.R')" ; then
      echo "=== Importation des données : OK ==="
      # Marquer l'import comme réussi
      touch "$DATA_MARKER"
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
  
  echo "=== Lancement du serveur Quarto/Shiny ==="
  echo "Fichier : $QMD_FILE"
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
    # En mode serve, on tente l'import mais on continue même en cas d'échec
    # si des données valides existent déjà
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
    sed -n '1,200p' "$0" | sed -n '1,80p'
    echo
    echo "Usage: docker run ... <image> [import|serve]"
    echo ""
    echo "Modes:"
    echo "  import  - Exécute uniquement l'import (avec sauvegarde automatique)"
    echo "  serve   - Lance l'application (tente l'import, utilise les données existantes si échec)"
    echo "  help    - Affiche ce message"
    exit 0
    ;;
    
  *)
    echo "Mode inconnu: $MODE"
    echo "Usage: docker run ... <image> [import|serve]"
    exit 2
    ;;
esac
