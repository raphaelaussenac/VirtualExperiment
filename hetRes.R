###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
# require(devtools)
#devtools::install_gitlab('arnaud.guyennon/forestdiversity', build_vignettes=TRUE)
library(forestdiversity)
library(dplyr)
library(ggplot2)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment")

# load simulation data
DF <- read.csv('./data/bauges/virtualExperiment_SALEM_02_2021.csv')

###############################################################
# compute diversity indices
###############################################################

# size diversity index
DivIndexSize <- CalcDivIndex(DF, Nvar='D_cm', type='BA', Inter=10)

# species diversity index
DivIndexSpecies <- CalcDivIndex(DF, Nvar='species', type='BA')

# retrieve diversity indices at first year
initYear <- DivIndexSize[, minyear:=min(year), by=list(site, src)]
initSizeDiv <- dplyr::filter(initYear, year==minyear)
initYearSp <- DivIndexSpecies[, minyear:=min(year), by=list(site, src)]
initSpDiv <- dplyr::filter(initYearSp, year==minyear)
initSpDiv <- rename(initSpDiv, GiniSp = Gini, ShSp = Sh, GSSp = GS, SimpSp = Simp, NclassSp = Nclass)

# plot size diversity index
ggplot(initSizeDiv,aes(x=Gini, y=Sh)) + geom_point(aes(size=as.factor(Nclass)), alpha=0.5) +
    geom_text(aes(x=Gini+0.02, y=Sh+0.04, label=site))

###############################################################
# compute resilience indices
###############################################################

# format salem output
DF2 <- format_Salem(DF)

# plot salem output
ggplot(DF2, aes(x=year, y=V_m3)) + geom_point(aes(col=PreDisturb)) +
    geom_line() + facet_wrap(~site)

# resilience indices
RecMet <- EventResilience(DF2, Nvar='V_m3', RecTime=20)

###############################################################
# complexity - resilience relationship
###############################################################

# merge data
all <- merge(initSizeDiv, initSpDiv[, c('site', 'GiniSp', 'ShSp', 'GSSp', 'SimpSp', 'NclassSp')], by='site', all.x = TRUE)
all <- merge(all, RecMet, by='site', all.x = TRUE)
# saveRDS(All, file='ExampIndexSalem.Rds')

# plot
ggplot(all[!is.na(Theta) & Theta > 0,]) +
geom_point(aes(x = Nclass, y = DegRec))

ggplot(all[!is.na(Theta) ,]) +
geom_point(aes(x = GSSp, y = DegRec))
# geom_boxplot(aes(x = GiniSp, y = DegRec))


# effet compo + diviser les facteurs en classe
