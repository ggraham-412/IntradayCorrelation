library(shiny)
source("intraday.R")

################
#  UI Defaults
################

intervalChoices <- list("5 min" = 300, "15 min" = 900, "30 min" = 1800)
minDays <- 1
maxDays <- 28

defaultSymbol <- "GOOG"
defaultIntervalMin <- 30  # Minutes
defaultIntervalSec <- defaultIntervalMin * 60  # Minut
defaultDays <- 8

defaultOutputChoices <- list(outputChoice_raw, outputChoice_pctChange)
defaultOutputChoice <- outputChoice_pctChange
