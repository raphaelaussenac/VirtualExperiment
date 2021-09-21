###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
# library(devtools)
# devtools::install_github('raphaelaussenac/forestdiversity', build_vignettes=TRUE)
library(forestdiversity)
library(dplyr)
library(ggplot2)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment")

# load simulation data
df <- read.csv('./data/bauges/virtualExperiment_SALEM_02_2021.csv')

###############################################################
# compute diversity and resilience indices
###############################################################

# format  output
df2 <- format_salem(df, Out = 'HillNb')

# plot salem output
# plot(df2) # it works !!
ggplot(df2, aes(x=year, y=V_m3)) + geom_point(aes(col=preDisturbance)) +
    geom_line() + facet_wrap(~site)

# resilience indices
RecMet <- EventResilience(df2, Nvar='V_m3', RecTime=20)

###############################################################
# complexity - resilience relationship
###############################################################

# descretise initial Dg
hist(RecMet$Dgini)
RecMet$Dgini <- cut(RecMet$Dgini, breaks = 2, labels = c('20', '30'))

# effet compo + diviser les facteurs en classe
mod <- lm(ThetaRecovery ~ H1Sizeini + H1Spini + Dgini, data = RecMet)
