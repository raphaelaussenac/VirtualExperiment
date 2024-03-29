###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load library
library(tidyverse)
library(data.table)

# set work directory
setwd('./Documents/github/VirtualExperiment/data')

# load source
source('../R/ftp.R')

# create folders
if (!(dir.exists('../standModCompatibility'))) {dir.create('../standModCompatibility', recursive = TRUE)}

###############################################################
# load initial stands
###############################################################

# salem initial stands
initPath <- paste0('./init/salem/')
csvFile <- list.files(path = initPath, pattern = '.csv')
salemInit <- read.csv(paste0(initPath, csvFile), sep = ';')
salemInit$mod <- 'salem'

# other models initial stands
initPath <- paste0('./init/otherModels/')
csvFile <- list.files(path = initPath, pattern = '.csv')
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
mod <- c('salem', 'samsara', 'landclim', '4c')

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

test <- function(model, init, df){
  ini <- length(unique(init[init$mod == model, 'simID']))
  sim <- nrow(unique(df[df$mod == model, 'simID']))
  if(ini != sim){
    stop('missing stands from ', model, ': ', ini, ' initial stands - ', sim, ' simulations')
  }
  if(sum(is.na(init$D_cm)) > 0){
    stop(sum(is.na(init$D_cm)), ' missing diameter in ', model, ' initial stands')
  }
  if(sum(is.na(df$D_cm)) > 0){
    stop(sum(is.na(df$D_cm)), ' missing diameter in ', model, ' simulations')
  }
}
invisible(lapply(mod, test, init, df))

###############################################################
# calculate BA trajectories
###############################################################

# stack on init stands
df <- rbind(init, df)
df$year <- as.integer(df$year)
# remove years > 2050
df <- df %>% filter(year<=2010)

# calculate BAtot
batot <- df %>% group_by(mod, simID, year) %>% summarise(BAtot = sum((pi * (D_cm/200)^2) * weight))

# add specific columns with modalities
modalities <- data.frame(str_split(batot$simID, '-', simplify = TRUE))
colnames(modalities) <- c('cl', 'cd', 'gi', 'dg')
batot <- cbind(batot, modalities)

# specify climate and composition
batot <- batot %>% mutate(cl = case_when(cl == 'CL1' ~ 'peak',
                                         cl == 'CL2' ~ 'hotter',
                                         cl == 'CL3' ~ 'wetter',
                                         cl == 'CL4' ~ 'hotter-wetter'),
                          cd = case_when(cd == 'CD1' ~ 'fasy',
                                         cd == 'CD2' ~ 'qupe',
                                         cd == 'CD3' ~ 'pisy',
                                         cd == 'CD4' ~ 'piab',
                                         cd == 'CD5' ~ 'fasy qupe',
                                         cd == 'CD6' ~ 'fasy pisy',
                                         cd == 'CD7' ~ 'fasy piab',
                                         cd == 'CD8' ~ 'qupe pisy',
                                         cd == 'CD9' ~ 'qupe piab',
                                         cd == 'CD10' ~ 'pisy piab',
                                         cd == 'CD11' ~ 'fasy qupe pisy',
                                         cd == 'CD12' ~ 'fasy qupe piab',
                                         cd == 'CD13' ~ 'fasy pisy piab',
                                         cd == 'CD14' ~ 'qupe pisy piab',
                                         cd == 'CD15' ~ 'fasy qupe pisy piab'))
#
# plot all simID
pl1 <- ggplot(batot) +
geom_line(aes(x = year, y = BAtot, group = simID), alpha = 0.1) +
facet_wrap(.~mod, nrow = 1) +
theme_bw() +
theme(legend.position = 'none', strip.background = element_rect(colour = 'black', fill = 'white')) +
scale_x_continuous(minor_breaks = seq(2000 , 2010, 1), breaks = seq(2000, 2010, 1))

# plot all simID but assign color depending on climate factor
pl2 <- ggplot(batot) +
geom_line(aes(x = year, y = BAtot, col = cl, group = simID), alpha = 0.2) +
facet_wrap(. ~ mod, nrow = 1) +
theme_bw() +
theme(legend.position = 'bottom', strip.background = element_rect(colour = 'black', fill = 'white')) +
scale_x_continuous(minor_breaks = seq(2000 , 2010, 1), breaks = seq(2000, 2010, 1))

# plot climate separately
climates <- c('peak', 'hotter', 'wetter', 'hotter-wetter')
y <- c(0, 45)
myTheme <- theme_bw() +
theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', strip.background = element_rect(colour = 'black', fill = 'white'))

# plot climate and composition separately
myTheme2 <- theme_bw() +
theme(plot.title = element_text(hjust = 0.5),
      legend.position = 'none',
      strip.background = element_rect(colour = 'black', fill = 'white'),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text.x = element_text(size = 6.5))
#
# plot functions
clim <- function(clim, batot){
  pl3 <- ggplot(batot %>% filter(cl == clim)) +
  geom_line(aes(x = year, y = BAtot, group = simID), alpha = 0.3) +
  facet_wrap(.~mod, nrow = 1) +
  ggtitle(paste(clim, 'climate')) +
  scale_x_continuous(minor_breaks = seq(2000 , 2010, 1), breaks = seq(2000, 2010, 1)) +
  ylim(y) + myTheme
}

climCompo <- function(clim, batot){
  pl4 <- ggplot(batot %>% filter(cl == clim)) +
  geom_line(aes(x = year, y = BAtot, group = simID, col = cd)) +
  facet_grid(mod~cd) +
  ggtitle(paste(clim, 'climate')) +
  scale_x_continuous(minor_breaks = seq(2000 , 2010, 2), breaks = seq(2000, 2010, 2)) +
  ylim(y) + myTheme2
}

pdf('../standModCompatibility/BAtrajectories.pdf', width = 15, height = 10)
print(pl1)
print(pl2)
lapply(climates, clim, batot)
lapply(climates, climCompo, batot)
dev.off()

# upload to ftp
up(file = '../standModCompatibility/BAtrajectories.pdf',
   serverPath = '/01_DATA/virtual_experiment/stands/3_initial_stands_sim/BAtrajectories.pdf')
#
