#!/usr/bin/env bash
set -e

echo "=== [Entrypoint] Initialisation du container ==="

###############################################
# 1. TENTATIVE D'IMPORT DES DONNÉES (non bloquant)
###############################################

if [ -f "import_data_dashboard.R" ]; then
  echo "=== Tentative d'exécution du script R d'importation des données ==="

  # On capture les erreurs mais elles ne bloquent pas le démarrage de l'app
  if R -e "source('import_data_dashboard.R')" ; then
      echo "=== Importation des données : OK ==="
  else
      echo "!!! ATTENTION : échec de l'import des données !!!"
      echo "L'application va démarrer avec les données précédentes (si existantes)."
  fi

else
  echo "ATTENTION : Le script import_data_dashboard.R est introuvable."
  echo "L'application démarre sans tentative d'import."
fi


###############################################
# 2. DÉMARRAGE DU DASHBOARD QUARTO + SHINY
###############################################

QMD_FILE="application_OET.qmd"

if [ ! -f "$QMD_FILE" ]; then
  echo "ERREUR : Le fichier $QMD_FILE n'existe pas dans le répertoire."
  echo "Impossible de démarrer l'application."
  exit 1
fi

echo "=== Lancement du serveur Quarto/Shiny ==="
echo "Fichier : $QMD_FILE"

# Commande compatible toutes versions de Quarto
quarto serve "$QMD_FILE" --port 3838 --host 0.0.0.0

