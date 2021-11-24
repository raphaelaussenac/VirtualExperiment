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
source('./R/hetRes.R')
source('./R/evalHetRes.R')

###############################################################
# select model (salem, ...)
###############################################################

model <- 'salem'

###############################################################
# create file architecture
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

###############################################################
# calculate tree [c]ritical [w]ind [s]peed
###############################################################

cws(model)

###############################################################
# disturb stands
###############################################################

# define wind speed modalities or percentage of damaged BA
# set [w]ind [s]peed
ws <- c(75, 80, 85)
# set percentage of [d]amaged [BA]
dBA <- c(25, 50, 75)

# plot damaged BA = f(wind speed)
wsDamages(method = 'dBA', thresholds = dBA)

# create disturbed stands
disturbe()

###############################################################
# calculate resilience metrics
###############################################################

# calculate complexity and resilience metrics
hetRes(model)

# plot complexity and resilience relationship
evalHetRes()

# running time
end_time <- Sys.time()
end_time - start_time
