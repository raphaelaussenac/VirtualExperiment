###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# set work directory
setwd('C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment')

# load source (all R files in R folder)
source('./R/cws.R')
source('./R/wsDamages.R')
source('./R/disturbe.R')
source('./R/hetRes.R')
source('./R/evalHetRes.R')
source('./R/ftp.R')

###############################################################
# select model (salem, ...)
###############################################################

model <- 'salem'

###############################################################
# create file architecture
###############################################################

# define folder structure
tempPath <- paste0('./temp/', model)
modPath <- paste0('./', model)
evalPath <- paste0(modPath, '/evaluation')
resultPath <- paste0(modPath, '/results')

# create temp, landscape and evaluation folders
if (!(dir.exists(tempPath))) {dir.create(tempPath, recursive = TRUE)}
if (!(dir.exists(modPath))) {dir.create(modPath, recursive = TRUE)}
if (!(dir.exists(evalPath))) {dir.create(evalPath, recursive = TRUE)}
if (!(dir.exists(resultPath))) {dir.create(resultPath, recursive = TRUE)}

###############################################################
# calculate tree [c]ritical [w]ind [s]peed
###############################################################

cws(model)

###############################################################
# disturb stands
###############################################################

# prescribe wind speed modalities or percentage of damaged BA
# set [w]ind [s]peed
ws <- c(75, 80, 85)
# or set percentage of [d]amaged [BA]
dBA <- c(25, 50, 75)

# plot damaged BA = f(wind speed)
wsDamages(method = 'dBA', thresholds = dBA)

# create disturbed stands
disturbe()

###############################################################
# calculate resilience metrics
###############################################################

# define variable of interest
# e.g. BA, V_m3
var <- 'BA'
# define resilience metrics
# e.g. c('DegreeResilience', 'AbsRecovery', 'AbsResistance')
metric = c('DegreeResilience', 'AbsRecovery', 'AbsResistance')

# calculate complexity and resilience metrics
hetRes(model, var)

# plot complexity - resilience relationship
evalHetRes(metric, var, model)
