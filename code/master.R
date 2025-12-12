############################################################
# Master-Skript zur Replikation
# Autor: Paul Ernst
# Datum: 25.08.2025
#
# Zweck:
# Dieses Skript führt alle R-Skripte aus, die zur Replikation
# der Ergebnisse der Bachelorarbeit notwendig sind.
#
# Nutzung:
# 1. Öffnen Sie das R-Projekt (.Rproj) in RStudio
# 2. Führen Sie dieses Skript aus 
# 3. Alle Ergebnisse (Tabellen und Abbildungen) werden im Ordner /output/ gespeichert
############################################################

# --- Setup ---
source(here::here("code", "0-setup.R"))

# --- Hauptanalyse ---
source(here::here("code", "1-main.R"))

# Ende des Master-Skripts