###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment")

# load source (all R files in R folder)
source('./R/cws.R')
source('./R/wsDamages.R')
source('./R/disturbe.R')

###############################################################
# select model (salem, ...)
###############################################################

model <- 'salem'

###############################################################
# disturbe stands
###############################################################

# running time
start_time <- Sys.time()

# define folder structure
tempPath <- paste0('./data/temp/', model)
modPath <- paste0('./', model, 'Disturbed')
evalPath <- paste0(modPath, '/evaluation')

# create temp, landscape and evaluation folders
if (!(dir.exists(tempPath))) {dir.create(tempPath, recursive = TRUE)}
if (!(dir.exists(modPath))) {dir.create(modPath, recursive = TRUE)}
if (!(dir.exists(evalPath))) {dir.create(evalPath, recursive = TRUE)}

# calculate [c]ritical [w]ind [s]peed
cws(model)

# calculate BA damages depending on wind speed
wsDamages()

# set [w]ind [s]peed
ws <- c(75, 80, 85)

# create disturbed stands
disturbe(ws)

# running time
end_time <- Sys.time()
end_time - start_time
