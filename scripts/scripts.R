################################################################################
# Integrated species distribution model for Esox lucius
#
# Data sources:
#   1. GBIF  -> presence-only
#   2. eDNA  -> presence-absence
#
# Spatial unit:
#   Lakes represented by point-on-surface locations
#
# Covariates:
#   predictor_stack.tif / object `predictors`
#
# Model:
#   Shared latent spatial occurrence process
#   Dataset-specific observation intercepts
#   GBIF sampling-bias field
################################################################################


# ==============================================================================
# 0. Packages
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(PointedSDMs)
library(fmesher)
library(INLA)
library(inlabru)
library(tidyterra)
library(ggplot2)

terraOptions(
  memfrac  = 0.25,       # Fraction of available RAM
  memmax   = 4,          # Maximum memory allocation in GB
  threads  = 2,          # Number of processing threads
  parallel = TRUE,
  todisk   = TRUE,       # Prefer disk-based processing
  tempdir  = "C:/terra_tmp",
  datatype = "FLT4S",
  progress = 3
)

# Path for the dataFolder
#dataFolder <- file.path(here::here(), "00_data")
dataFolder <- file.path("C:/ninaProjects/invasiveFish/invasiveFishModeling", "00_data")

# ==============================================================================
# Define one common CRS
# ==============================================================================
target_crs <- sf::st_crs(25833)

allSpecies  <- c("Esox lucius",
  "Perca fluviatilis",          
"Phoxinus phoxinus" ,
"Scardinius erythrophthalmus")

for(sppOfInt in allSpecies){

message("Fitting model for: ", sppOfInt)
  
source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "01_prepareData.R"))

source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "02_prepareCovariates.R"))

source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "04_getStudyRegion.R"))

# pathToResults <- file.path(here::here(),
#                           "02_results",
#                           sppOfInt)
  
pathToResults <- file.path("C:/ninaProjects/invasiveFish/invasiveFishModeling",
                           "02_results",
                           sppOfInt)

if(!dir.exists(pathToResults)) dir.create(pathToResults, recursive = TRUE)

source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "03_modelFitSensitivity.R"))

source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "05_makePredictions.R"))

}
