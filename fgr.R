###############################################################
# initialisation
###############################################################

# clean up environment
rm(list = ls())

# load packages
# library("devtools")
# devtools::install_github("tom-locatelli/fgr", build_vignettes = FALSE)
library(fgr)
library(dplyr)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment")

# load pre-disturbance stands
df <- read.csv('./data/salem/virtualExperiment_SALEM_02_2021.csv')

# load sp correspondance table
spCorr <- read.csv('./data/spCodeCorrespond.csv')

# create correspondance between sp latin names and fgr sp codes
# fgr::species_parameters
fgrSpCode <- data.frame(latinName = c('Fagus sylvatica', 'Quercus petraea', 'Quercus robur', 'Abies alba', 'Picea abies', 'Pinus sylvestris'),
                        fgrSpCode = c('BE', 'OK', 'OK', 'NF', 'NS', 'SP'))
#

###############################################################
# convert sp code and calculate stand level variables
###############################################################

# keep only first site
# df <- df %>% filter(site == 1) <------------------------------------------------

# add tree id
df$tree <- c(1:nrow(df))
# keep only pre-disturbance year
df <- df %>% filter(year == min(df$year), postDisturbance == 'false')
# remove trees < 7.5 DBH
df <- df %>% filter(D_cm>=7.5)

# convert species codes into species latin name
df <- merge(df, spCorr[!is.na(spCorr$franceCode), c('latinName', 'franceCode')], by.x = 'species', by.y = 'franceCode')

# add a column with fgr species codes
df <- merge(df, fgrSpCode, by = 'latinName')

# calculate stand level variables
df <- df %>% group_by(site) %>%
            mutate(N = sum(weight),
            meanDBH = sum(D_cm * weight) / sum(weight),
            # Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),
            meanH = sum(H_m * weight) /sum(weight),
            topH = max(H_m),
            spacing = sqrt(10000/N)) %>%
            ungroup()
#

###############################################################
# calculate critical wind speed cws for all trees
###############################################################

# single tree calculation
# i <- 1
damage <- function(i, df){
  output <- fg_tmc(stand_id = as.character(df$site[i]),
         tree_id = as.character(df$tree[i]),
         species = df$fgrSpCode[i],
         tree_ht = df$H_m[i],
         dbh = df$D_cm[i],
         spacing_current = df$spacing[i],
         stand_mean_ht = df$meanH[i],
         stand_top_ht = df$topH[i],
         stand_mean_dbh = df$meanDBH[i],
         stem_vol = df$V_m3[i],
         full_output = 0)
  return(c(output$tree_id, output$u10_damage))
}
# damage(1, df)

# vectorise for multiple stands and trees
cws <- lapply(c(1:nrow(df)), damage, df)
cws <- data.frame(matrix(unlist(cws), nrow=length(cws), byrow=TRUE))
colnames(cws) <- c('tree', 'cws_ms')

# add to df
df <- merge(df, cws, by = 'tree', all.x = TRUE)
df$cws_ms <- as.numeric(df$cws_ms)
df$cws_kmh <- df$cws * 3.6
# hist(df$cws_kmh)

###############################################################
# create disturbed stands
###############################################################

# function to create disturbed stands
# i <- 100
disturb <- function(i, df){
  stand <- df %>% filter(cws_kmh > i) %>% select(site, year, postThinning,
                                         postDisturbance, species, D_cm,
                                         H_m, V_m3, X, Y, weight)
  return(stand)
}
# test <- disturb(100, df)

# define storm wind speed
ws <- c(75, 90, 100)
disturbedStands <- lapply(ws, disturb, df)

# function to assign name to disturbed stands (with the associated ws)
disturbedStandName <- function(i, disturbedStands, ws){
  assign(x = paste0('stands', ws[i]), value = as.data.frame(disturbedStands[i]), envir = .GlobalEnv)
  return()
}
# run the function
lapply(c(1:length(ws)), disturbedStandName, disturbedStands, ws)

###############################################################
# save disturbed stands
###############################################################

# create directory to save disturbed stands
if (!(dir.exists('disturbedStands'))){dir.create('disturbedStands', recursive = TRUE)}

# function to save disturbed stands in csv files
standList <- ls(pattern = "stands")
saveStands <- function(i, standList){
  write.csv(get(standList[i]), paste0('./disturbedStands/', standList[i], '.csv'), row.names = FALSE)
}

# save
lapply(c(1:length(standList)), saveStands, standList)


# TODO
# --> verifier toute la procedure avec Barry (calcul de top_ht, spacing... et tous les autres arguments, utilisation de u10_damage)
#
