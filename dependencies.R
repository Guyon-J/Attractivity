# ==========================================
# Dépendances
# ==========================================

required_packages <- c("bslib",
                       "dplyr",
                       "leaflet",
                       "shiny")

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(bslib)
library(dplyr)
library(leaflet)
library(shiny)