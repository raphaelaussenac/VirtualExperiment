###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load library
library(tidyverse)
library(data.table)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment/data/initSim")

###############################################################
# load and stack simulations
###############################################################

# TODO: add initial state

# model list
mod <- c('salem', 'samsara', 'landclim')

# create file list
simList <- function(mod){
  fl <- list.files(path = paste0('./', mod), pattern = '\\.csv$')
  fl <- paste0('./', mod, '/', fl)
  return(fl)
}
fileList <- unlist(lapply(mod, simList))
#

# read and bind
readBind <- function(file, mod){
  model <- mod[str_detect(file, mod)]
  df <- read.csv(file, sep = ';')
  if(model == 'landclim'){
    df$comment <- NA
  }
  df$mod <- model
  return(df)
}
df <- lapply(fileList, readBind, mod)
df <- rbindlist(df)


###############################################################
# calculate BA trajectories
###############################################################

# calculate BA
ba <- df %>% group_by(mod, simID, year) %>% summarise(BAtot = sum((pi * (D_cm/200)^2) * weight))

ggplot(ba) +
geom_line(aes(x = year, y = BAtot, col = simID)) +
facet_wrap(.~mod) +
theme_bw() +
theme(legend.position = 'none')



# TODO: check if number of simulation == for all models
# --> sur le fichier final: ba?


test <- ba %>% filter(year == 2001, mod == 'salem') %>% arrange(BAtot)


###############################################################
# plot
###############################################################
