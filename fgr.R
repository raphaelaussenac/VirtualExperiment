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
df <- read.csv('./data/salem/VirtualExperiment_InitialStands_Salem_20_09_2021.csv', sep = ';')
df$simID <- as.character(df$simID)
df$species <- as.factor(df$species)

# create correspondance between sp latin names and fgr sp codes
# fgr::species_parameters
fgrSpCode <- data.frame(latinName = c('Fagus sylvatica', 'Quercus petraea', 'Picea abies', 'Pinus sylvestris'),
                        fgrSpCode = c('BE', 'OK', 'NS', 'SP'),
                        Imaestro = c('fasy', 'qupe', 'piab', 'pisy'))
#

###############################################################
# convert sp code and calculate stand level variables
###############################################################

# add tree id
df$tree <- c(1:nrow(df))

# add a column with fgr species codes
df <- merge(df, fgrSpCode, by.x = 'species', by.y = 'Imaestro', all.x = TRUE)

# calculate stand level variables
# first calculate dominant height = mean height of 100 trees with largest dbh
df <- df %>% group_by(simID) %>%
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
df <- df %>% group_by(simID) %>%
             mutate(N = sum(weight),
                    Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),   # quadratic mean
                    meanH = sum(H_m * weight) / sum(weight),
                    spacing = sqrt(10000/N)) %>%
             ungroup()
#

###############################################################
# calculate normalised BAl competition index
###############################################################

# calculate tree and stand BA
df <- df %>% mutate(BAtree = (pi * (df$D_cm/200)^2) * df$weight) %>%
             group_by(simID) %>% mutate(BAtot = sum((pi * (D_cm/200)^2) * weight)) %>%
             ungroup()
#
# assign to each tree the ratio of "BA of larger trees" / BAtot
df <- df %>% arrange(-D_cm) %>% mutate(Drank = 1:nrow(df)) %>% group_by(simID) %>%
             mutate(BAl = cumsum(BAtree))
# shift column BAl down by one row
siteList <- sort(unique(df$simID))
for(i in siteList){
  a <- as.data.frame(df[df$simID == i, 'BAl'][1:(nrow(df[df$simID == i, 'BAl'])-1),])
  df[df$simID == i, 'BAl'] <- c(0, a$BAl)
}
# competition index
df$CI <- df$BAl / df$BAtot

# remove useless columns
df <- df %>% select(-BAtot, -Drank, -BAl)

# add soil group
df$soil <- 1 # 1 = soil class A
df$roots <- 2 # 2 = roots can penetrate to >=80 cm


###############################################################
# calculate critical wind speed cws for all trees
###############################################################

# single tree calculation
damage <- function(i, df){
  output <- fg_tmc(stand_id = as.character(df$simID[i]),
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
                   soil_group = df$soil[i],
                   rooting = df$roots[i],
                   full_output = 0)
  return(c(output$tree_id, output$u10_damage))
}

# temporarily remove trees < 7.5 cm (forestGales does not accept them)
df75 <- df %>% filter(D_cm >= 7.5)

# vectorise for multiple stands and trees
start_time <- Sys.time()
cws <- lapply(c(1:nrow(df75)), damage, df75)
cws <- data.frame(matrix(unlist(cws), nrow=length(cws), byrow=TRUE))
colnames(cws) <- c('tree', 'cws_ms')
end_time <- Sys.time()
end_time - start_time

# add to df
df <- merge(df, cws, by = 'tree', all.x = TRUE)
df$cws_ms <- as.numeric(df$cws_ms)
df$cws_kmh <- df$cws_ms * 3.6

# set cws of small trees to maximum cws of their sp
# first assign max cws to trees < 7.5cm
df <- df %>% group_by(simID, species) %>% mutate(cws_ms = ifelse(is.na(cws_ms), max(cws_ms, na.rm = TRUE), cws_ms),
                                                 cws_kmh = ifelse(is.na(cws_kmh), max(cws_kmh, na.rm = TRUE), cws_kmh))
# then for the remaining 'small trees'
df <- df %>% group_by(simID, species) %>% mutate(D_cwsMax = max(ifelse(cws_ms == max(cws_ms), D_cm, NA), na.rm = TRUE),
                                                cws_ms = ifelse(D_cm <= D_cwsMax, max(cws_ms), cws_ms),
                                                cws_kmh = ifelse(D_cm <= D_cwsMax, max(cws_kmh), cws_kmh))
#

# TODO: save after forestGales (in temp file?)

# plot cws values
ggplot() +
  geom_point(data = df[df$simID %in% unique(df$simID)[1:12],], aes(x = D_cm, y = cws_kmh, col = latinName)) +
  geom_hline(yintercept = 75, col = 'red') +
  geom_hline(yintercept = 80, col = 'red') +
  geom_hline(yintercept = 85, col = 'red') +
  facet_wrap(. ~ simID) +
  theme_bw()

###############################################################
# choose wind speed modalities depending on wind damages
###############################################################

# calculate proportion of BA impacted by wind damages
windDamages <- function(i, df){
   damdf <- df %>% group_by(simID) %>% summarise(BApre = sum(BAtree),
                                              BApost = sum(BAtree[cws_kmh >= i])) %>%
                                        mutate(damagedBA = 100 - (BApost * 100 / BApre)) %>%
                                        select(damagedBA)
  return(damdf)
}
# define storm wind speeds
ws <- c(seq(50, 120, by = 5))
damdf <- lapply(ws, windDamages, df)
damSite <- data.frame(simID = (1:nrow(damdf[[1]])))
damdf <- as.data.frame(do.call(cbind, damdf))
damdf <- cbind(damSite, damdf)
colnames(damdf) <- c('simID', paste0('ws', ws))
damdf <- damdf %>% pivot_longer(cols = starts_with('ws'), names_to = 'ws',
                                values_to = 'damagedBA', names_prefix = 'ws')
damdf$ws <- as.numeric(damdf$ws)
#

# plot damaged BA = f(ws)
pl1 <- ggplot(data = damdf) +
geom_boxplot(aes(x = ws, y = damagedBA, group = ws), size = 2) +
# geom_point(aes(x = ws, y = damagedBA, group = as.factor(simID)), size = 2, alpha = 0.2) +
geom_line(aes(x = ws, y = damagedBA, col = as.factor(simID)), alpha = 0.1) +
theme(panel.grid.major = element_line(colour = "black"),
  panel.grid.minor = element_line(colour = "black", size = 0.25),
  legend.position = "none") +
scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10)) +
scale_x_continuous(minor_breaks = seq(50 , 120, 5), breaks = seq(50, 120, 5))
pl1
# save plot
ggsave(file = './disturbedStands/wsDamages.pdf', plot = pl1, width = 10, height = 10)

###############################################################
# create disturbed stands
###############################################################

# function to create disturbed stands
disturb <- function(i, df){
  # deduce ws modality index (used in simulation codes)
  index <- match(i, ws)
  # remove trees depending in their critical ws
  stand <- df %>% filter(cws_kmh > i) %>%
                  select(simID, year, postThinning,
                         postDisturbance, species, D_cm,
                         H_m, V_m3, weight, comment) %>%
                  mutate(simID = paste0('W', index, '-', simID))
  return(stand)
}

# define storm wind speed
ws <- c(75, 80, 85)
disturbedStands <- lapply(ws, disturb, df)
disturbedStands <- bind_rows(disturbedStands)

# save
disturbedStands$postDisturbance <- 'true'
write.csv(disturbedStands, paste0('./disturbedStands/salem.csv'), row.names = FALSE)

###############################################################
# evaluation
###############################################################

# calculate stand BA
ba <- disturbedStands %>% group_by(simID) %>%
                          summarise(BA = sum((pi * (D_cm/200)^2) * weight)) %>%
                          mutate(ws = ifelse(substr(simID, 1, 2) =='W1', ws[1], ifelse(substr(simID, 1, 2) =='W2', ws[2], ws[3])))
#
# plot BA of disturbed stands depending on their associated ws
pl2 <- ggplot() +
geom_boxplot(data = ba, aes(x = ws, y = BA, group = ws)) +
theme_bw() +
scale_x_continuous(minor_breaks = seq(70 , 90, 5), breaks = seq(70, 90, 5))
pl2
# save plot
ggsave(file = './disturbedStands/BA.pdf', plot = pl2, width = 10, height = 10)


#
# --------------> dbh avant/apres
#
# disturbedStands <- disturbedStands %>% group_by(simID) %>%
#                             mutate(ws = ifelse(substr(simID, 1, 2) =='W1', ws[1], ifelse(substr(simID, 1, 2) =='W2', ws[2], ws[3])))
# #
#
# ggplot(disturbedStands[disturbedStands$D_cm <= 15, ]) +
#   geom_histogram(aes(x = D_cm), binwidth = 1) +
#   facet_wrap(. ~ ws)



ggplot() +
  geom_histogram(data = df, aes(x = D_cm)) +
  geom_histogram(data = disturbedStands, aes(x = D_cm), fill = 'orange', alpha = 0.2) +
  facet_wrap(. ~ ws)
