###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load library
library(tidyverse)
library(data.table)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment/data")

###############################################################
# load initial stands
###############################################################

# salem initial stands
initPath <- paste0('./init/salem/')
csvFile <- list.files(path = initPath, pattern = '\\.csv$')
salemInit <- read.csv(paste0(initPath, csvFile), sep = ';')
salemInit$mod <- 'salem'

# other models initial stands
initPath <- paste0('./init/otherModels/')
csvFile <- list.files(path = initPath, pattern = '\\.csv$')
otherInit <- read.csv(paste0(initPath, csvFile), sep = ';')
nbrows <- nrow(otherInit)
otherInit <- rbind(otherInit, otherInit, otherInit)
otherInit$mod <- c(rep('landclim', nbrows), rep('4c', nbrows), rep('samsara', nbrows))

# bind all init stands
init <- rbind(salemInit, otherInit)


###############################################################
# load pre-dist simulations and stack on init stands
###############################################################

# model list
mod <- c('salem', 'samsara', 'landclim')

# create file list
simList <- function(mod){
  fl <- list.files(path = paste0('./initSim/', mod), pattern = '\\.csv$')
  fl <- paste0('./initSim/', mod, '/', fl)
  return(fl)
}
fileList <- unlist(lapply(mod, simList))
#

# read and bind
readBind <- function(file, mod){
  model <- mod[str_detect(file, mod)]
  df <- read.csv(file, sep = ';')
  df$mod <- model
  return(df)
}
df <- lapply(fileList, readBind, mod)
df <- rbindlist(df)

###############################################################
# test whether all initial stands have their associated sim
###############################################################

# count nb of [i]nitial stands and [s]imulations
si <- length(unique(init[init$mod == 'salem', 'simID']))
ss <- nrow(unique(df[df$mod == 'salem', 'simID']))
li <- length(unique(init[init$mod == 'landclim', 'simID']))
ls <- nrow(unique(df[df$mod == 'landclim', 'simID']))
sami <- length(unique(init[init$mod == 'samsara', 'simID']))
sams <- nrow(unique(df[df$mod == 'samsara', 'simID']))
ci <- length(unique(init[init$mod == '4c', 'simID']))
cs <- nrow(unique(df[df$mod == '4c', 'simID']))

if(si != ss){
  warning('missing stands from salem:', si, ' initial stands - ', ss, ' simulations')
}
if(li != ls){
  warning('missing stands from landclim:', li, ' initial stands - ', ls, ' simulations')
}
if(sami != sams){
  warning('missing stands from samsara:', sami, ' initial stands - ', sams, ' simulations')
}
if(ci != cs){
  warning('missing stands from 4c: ', ci, ' initial stands - ', cs, ' simulations')
}


###############################################################
# calculate BA trajectories
###############################################################

# stack on init stands
df <- rbind(init, df)
df$year <- as.integer(df$year)

# calculate BAtot
batot <- df %>% group_by(mod, simID, year) %>% summarise(BAtot = sum((pi * (D_cm/200)^2) * weight))

# add specific columns with modalities
modalities <- data.frame(str_split(batot$simID, '-', simplify = TRUE))
colnames(modalities) <- c('cl', 'cd', 'gi', 'dg')
batot <- cbind(batot, modalities)

# specify climate
batot <- batot %>% mutate(cl = case_when(cl == 'CL1' ~ 'peak',
                                         cl == 'CL2' ~ 'hotter',
                                         cl == 'CL3' ~ 'wetter',
                                         cl == 'CL4' ~ 'hotter-wetter'))
#
# plot all simID
ggplot(batot) +
geom_line(aes(x = year, y = BAtot, col = simID)) +
facet_wrap(.~mod, nrow = 1) +
theme_bw() +
theme(legend.position = 'none')

# plot all simID but assign color depending on CD factor
ggplot(batot) +
geom_line(aes(x = year, y = BAtot, col = cl, group = simID)) +
facet_wrap(.~mod, nrow = 1) +
theme_bw() +
theme(legend.position = 'bottom')

# plot only climate 'hotter'
ggplot(batot %>% filter(cl == 'hotter')) +
geom_line(aes(x = year, y = BAtot, col = cl, group = simID)) +
facet_wrap(.~mod, nrow = 1) +
theme_bw() +
theme(legend.position = 'bottom')
