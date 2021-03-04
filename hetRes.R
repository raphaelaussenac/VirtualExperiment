###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
# require(devtools)
#devtools::install_gitlab('arnaud.guyennon/forestdiversity', build_vignettes=TRUE)
library(forestdiversity)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment")

# load simulation data
baugesSalem <- read.csv('./data/bauges/virtualExperiment_SALEM_02_2021.csv')
