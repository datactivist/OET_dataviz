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

run_import() {
  if [ -f "import_data_dashboard.R" ]; then
    echo "=== Exécution du script R d'importation des données ==="
    if R -e "source('import_data_dashboard.R')" ; then
      echo "=== Importation des données : OK ==="
      return 0
    else
      echo "!!! ATTENTION : échec de l'import des données !!!"
      return 1
      #modif pour pas écraser les anciennes données
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
    serve_qmd
    ;;
  help|-h|--help)
    sed -n '1,200p' "$0" | sed -n '1,80p'
    echo
    echo "Usage: docker run ... <image> [import|serve]"
    exit 0
    ;;
  *)
    echo "Mode inconnu: $MODE"
    echo "Usage: docker run ... <image> [import|serve]"
    exit 2
    ;;
esac

