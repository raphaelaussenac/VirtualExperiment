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
library(ggplot2)
library(tidyr)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/VirtualExperiment")

# load pre-disturbance stands
# define columns classes (because species = 2 or 02 causes problems)
dfClass <- c('integer', 'integer', rep('character',3), rep('numeric',6))
df <- read.csv('./data/salem/virtualExperiment_SALEM_02_2021.csv', colClasses = dfClass)

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
# first calculate dominant height = mean height of 100 trees with largest dbh
df <- df %>% group_by(site) %>%
             arrange(-D_cm) %>%
             mutate(Ncumul = cumsum(weight),
                    XX = Ncumul-100,
                    YY = ifelse(XX >= 0,1,0),
                    ZZ = cumsum(YY),
                    weightTopH100 = ifelse(ZZ < 1, weight, ifelse(ZZ == 1, weight - XX, 0)),
                    WW = round(cumsum(weightTopH100),2),
                    domH = sum(H_m * weightTopH100) /sum(weightTopH100)) %>%
             select(-Ncumul, -XX, -YY, -ZZ, -weightTopH100, -WW) %>%
             ungroup()
#
# then calculate other stand variables
df <- df %>% group_by(site) %>%
             mutate(N = sum(weight),
                    # meanDBH = sum(D_cm * weight) / sum(weight),  # arithmetic mean
                    Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),   # quadratic mean
                    meanH = sum(H_m * weight) / sum(weight),
                    spacing = sqrt(10000/N)) %>%
             ungroup()
#
# # plot heights
# ggplot(data = df)+
# geom_boxplot(aes(x = as.factor(site), y = H_m)) +
# geom_point(aes(x = as.factor(site), y = meanH), col = 'green', size = 5) +
# geom_point(aes(x = as.factor(site), y = domH), col = 'orange', size = 5)

###############################################################
# calculate normalised BAl competition index
###############################################################

# calculate tree and stand BA
df <- df %>% mutate(BAtree = (pi * (df$D_cm/200)^2) * df$weight) %>%
             group_by(site) %>% mutate(BAtot = sum((pi * (D_cm/200)^2) * weight)) %>%
             ungroup()
#
# assign to each tree the ratio of "BA of larger trees" / BAtot
df <- df %>% arrange(-D_cm) %>% mutate(Drank = 1:nrow(df)) %>% group_by(site) %>%
             mutate(BAl = cumsum(BAtree))
# shift column BAl down by one row
siteList <- sort(unique(df$site))
for(i in siteList){
  a <- as.data.frame(df[df$site == i, 'BAl'][1:(nrow(df[df$site == i, 'BAl'])-1),])
  df[df$site == i, 'BAl'] <- c(0, a$BAl)
}
# competition index
df$CI <- df$BAl / df$BAtot

# remove useless columns
df <- df %>% select(-BAtot, -Drank, -BAl)

# ---> envoyer sample à Barry ---------------------------------------------------------------
# df <- df %>% select(site, tree, fgrSpCode, H_m, D_cm, weight, spacing, meanH, domH, Dg, V_m3, CI)
# write.csv(df, 'C:/Users/raphael.aussenac/Desktop/sample.csv', row.names = F)

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
                   stand_top_ht = df$domH[i],
                   stand_mean_dbh = df$Dg[i],
                   stem_vol = df$V_m3[i],
                   ci = "bal",
                   ci_value = df$CI[i],
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

# plot cws values
ggplot() +
  geom_point(data = df, aes(x = D_cm, y = cws_kmh, col = latinName)) +
  geom_hline(yintercept = 75, col = 'red') +
  geom_hline(yintercept = 90, col = 'red') +
  geom_hline(yintercept = 100, col = 'red') +
  facet_wrap(. ~ site) +
  theme_bw()

# define all cws values for trees < 15cm to the max cws (within site and sp)
df <- df %>% group_by(site, species) %>% mutate(cws_ms = ifelse(D_cm < 15, max(cws_ms[D_cm > 15]), cws_ms) ,
                                                cws_kmh = ifelse(D_cm < 15, max(cws_kmh[D_cm > 15]), cws_kmh))
#

# plot cws values
ggplot() +
  geom_point(data = df, aes(x = D_cm, y = cws_kmh, col = latinName)) +
  geom_hline(yintercept = 75, col = 'red') +
  geom_hline(yintercept = 90, col = 'red') +
  geom_hline(yintercept = 100, col = 'red') +
  facet_wrap(. ~ site) +
  theme_bw()

###############################################################
# choose wind speed modalities depending on wind damages
###############################################################

# calculate proportion of BA impacted by wind damages
windDamages <- function(i, df){
   damdf <- df %>% group_by(site) %>% summarise(BApre = sum(BAtree),
                                              BApost = sum(BAtree[cws_kmh >= i])) %>%
                                        mutate(damagedBA = 100 - (BApost * 100 / BApre)) %>%
                                        select(damagedBA)
  return(damdf)
}
# define storm wind speeds
ws <- c(seq(70, 120, by = 2))
damdf <- lapply(ws, windDamages, df)
damSite <- data.frame(site = (1:nrow(damdf[[1]])))
damdf <- as.data.frame(do.call(cbind, damdf))
damdf <- cbind(damSite, damdf)
colnames(damdf) <- c('site', paste0('ws', ws))
damdf <- damdf %>% pivot_longer(cols = starts_with('ws'), names_to = 'ws',
                                values_to = 'damagedBA', names_prefix = 'ws')
damdf$ws <- as.numeric(damdf$ws)
#

# plot damaged BA = f(ws)
ggplot(data = damdf) +
geom_boxplot(aes(x = ws, y = damagedBA, group = ws), size = 1) +
geom_point(aes(x = ws, y = damagedBA, col = as.factor(site)), size = 2) +
geom_line(aes(x = ws, y = damagedBA, col = as.factor(site))) +
theme_bw()

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

###############################################################
# check
###############################################################

ggplot() +
  geom_bar(data = df[df$site == 9,], aes(x = D_cm, y = weight, fill = as.factor(species)), stat = 'identity') +
  geom_bar(data = stands90[stands90$site == 9,], aes(x = D_cm, y = weight), col = 'grey', fill = 'grey', stat = 'identity', width = 0.5) +
  geom_bar(data = stands100[stands100$site == 9,], aes(x = D_cm, y = weight), col = 'black', fill = 'black', stat = 'identity', width = 0.2) +
  facet_grid(as.factor(species) ~ site) +
  theme_light() +
  theme(legend.position = "bottom")
