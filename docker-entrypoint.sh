#!/usr/bin/env bash
set -e

echo "=== [Entrypoint] Initialisation du container ==="

# 1. Import des données avant de lancer l'app
if [ -f "import_data_dashboard.R" ]; then
  echo "=== Exécution du script R d'importation des données ==="
  R -e "source('import_data_dashboard.R')"
else
  echo "ATTENTION : Le script import_data_dashboard.R est introuvable."
fi

# 2. Rendre / lancer le dashboard Quarto + Shiny
QMD_FILE="application_OET.qmd"

if [ ! -f "$QMD_FILE" ]; then
  echo "ERREUR : Le fichier $QMD_FILE n'existe pas dans le répertoire."
  exit 1
fi

echo "=== Lancement du serveur Quarto/Shiny ==="
echo "Fichier : $QMD_FILE"

# Serve le .qmd avec Shiny sur 0.0.0.0:3838 
quarto serve "$QMD_FILE" --port 3838 --host 0.0.0.0

echo "=== Quarto/Shiny démarré ==="
