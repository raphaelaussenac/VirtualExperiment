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

# load simulation data
df <- read.csv('./data/salem/virtualExperiment_SALEM_02_2021.csv')

# keep only pre-disturbance date
df <- df %>% filter(year == 2000, postDisturbance == 'false')

# load sp correspondance table
spCorr <- read.csv('./data/spCodeCorrespond.csv')

###############################################################
# convert sp code and calculate stand level variables
###############################################################

# fg_tmc()
# uh_overturning_tmc()

# keep only first site
df <- df %>% filter(site == 1)

# convert species codes into species latin name
df <- merge(df, spCorr[!is.na(spCorr$franceCode), c('latinName', 'franceCode')], by.x = 'species', by.y = 'franceCode')

# add a column with the species codes used in fgr
# fgr::species_parameters
fgrSpCode <- data.frame(latinName = 'Fagus sylvatica', fgrCode = 'BE')
df <- merge(df, fgrSpCode, by = 'latinName')

# add a column with tree id
df$tree <- c(1:nrow(df))

# calculate stand level variables
df <- df %>% group_by(site) %>%
            mutate(N = sum(weight),
            meanDBH = sum(D_cm * weight) / sum(weight),
            meanH = sum(H_m * weight) /sum(weight),
            topH = max(H_m),
            spacing = sqrt(10000/N)) %>%
            ungroup()
#

###############################################################
# calculate critical wind speed for overturning and breakage
# for all trees
###############################################################

i <- 1


df <- df %>% filter(D_cm>=7.5)

# single tree calculation
damage <- function(i, df){
  output <- fg_tmc(stand_id = as.character(df$site[i]),
         tree_id = as.character(df$tree[i]),
         species = df$fgrCode[i],
         tree_ht = df$H_m[i],
         dbh = df$D_cm[i],
         spacing_current = df$spacing[i],
         stand_mean_ht = df$meanH[i],
         stand_top_ht = df$topH[i],
         stand_mean_dbh = df$meanDBH[i],
         stem_vol = df$V_m3[i],
         full_output = 0)
  output <- output$u10_damage
  return(output)
}
# damage(1, df)

# vectorise for multiple stands and trees
lapply(c(1:nrow(df)), damage, df)






# TODO
# -->  ne semble pas fonctionner sur les petits diamètres <7.5
# --> il faut un tree id venant des modelisateurs
# --> vectoriser sur tous les sites (group_by)
# --> verifier toute la procedure avec Barry
# --> sortir les peuplements perturbés
#














# single tree calculation
damage <- function(df, i){
  output <- fg_tmc(stand_id = as.character(df$site[i]),
         tree_id = as.character(df$tree[i]),
         species = df$fgrCode[i],
         tree_ht = df$H_m[i],
         dbh = df$D_cm[i],
         spacing_current = df$spacing[i],
         stand_mean_ht = df$meanH[i],
         stand_top_ht = df$topH[i],
         stand_mean_dbh = df$meanDBH[i],
         stem_vol = df$V_m3[i],
         full_output = 0)
  output <- output$u10_damage
  return(output)
}
damage(df, i)






# single tree calculation
fg_tmc(stand_id = '1',
       tree_id = '1',
       species = 'BE',
       tree_ht = 17.95,
       dbh = 21.5,
       spacing_current = 3.2360, # convert density into spacing (poisson distribution?)
       stand_mean_ht = 16.29,
       stand_top_ht = 26.03,
       stand_mean_dbh = 19.04,
       stem_vol = 0.31,
       full_output = 1)
#


# vectorise for multiple stands and trees

args.to.vectorize <- formalArgs(fg_tmc)
args.to.vectorize <- args.to.vectorize[!args.to.vectorize %in% c("species_parameters", "fgr_constants")]
fg_tmc_v <- Vectorize(fg_tmc, vectorize.args = args.to.vectorize)
ex.stand_id <- as.character(df_tmc$stand_id) ex.tree_id <- as.character(df_tmc$tree_id) ex.date <- df_tmc$date
ex.species <- as.character(df_tmc$species) ex.tree_ht <- df_tmc$tree_ht
ex.dbh <- df_tmc$dbh
ex.spacing_current <- df_tmc$spacing_current
ex.stand_mean_ht <- df_tmc$stand_mean_ht
ex.stand_top_ht <- df_tmc$stand_top_ht
ex.stand_mean_dbh <- df_tmc$stand_mean_dbh
ex.predominant_species <- as.character(df_tmc$predominant_species) ex.full_output <- 1
ex.weib_a <- df_tmc$weib_a
ex.weib_k <- df_tmc$weib_k
ex.cr_width <- df_tmc$cr_width
ex.cr_depth <- df_tmc$cr_depth
ex.stand_cr_width <- df_tmc$stand_cr_width
ex.stand_cr_depth <- df_tmc$stand_cr_depth
ex.soil_group <- df_tmc$soil_group
ex.rooting <- df_tmc$rooting
ex.dist_edge <- df_tmc$dist_edge
ex.gap_size <- df_tmc$gap_size
ex.spacing_before <- df_tmc$spacing_before
ex.years_since_thin <- df_tmc$years_since_thin
ex.moe <- df_tmc$moe
ex.mor <- df_tmc$mor
ex.fknot <- df_tmc$fknot
ex.stem_vol <- df_tmc$stem_vol
ex.crown_vol <- df_tmc$crown_vol ex.stem_density <- df_tmc$stem_density ex.crown_density <- df_tmc$crown_density ex.c_reg <- df_tmc$c_reg
ex.c_drag <- df_tmc$c_drag
ex.n_drag <- df_tmc$n_drag
ex.drag_upper_limit <- df_tmc$drag_upper_limit ex.snow_depth <- df_tmc$snow_depth ex.snow_density <- df_tmc$snow_density
ex.ci <- as.character(df_tmc$ci)
ex.ci_value <- df_tmc$ci_value
ex.ro <- df_tmc$ro
ex.x <- df_tmc$x
ex.y <- df_tmc$y
ex.z <- df_tmc$z
ex.dams <- df_tmc$dams
out_tmc <- as.data.frame(t(fg_tmc_v(stand_id = ex.stand.id, tree_id = ex.tree_id, date = ex.date, species = ex.species, tree_ht = ex.tree_ht, dbh = ex.dbh, spacing_current = ex.spacing_current, predominant_species = ex.predominant_species, stand_mean_ht = ex.stand_mean_ht, stand_mean_dbh = ex.stand_mean_dbh, stand_top_ht = ex.stand_top_ht, full_output = ex.full_output, weib_a = ex.weib_a, weib_k = ex.weib_k, ci = ex.ci, ci_value = ex.ci-value, cr_width = ex.cr_width, cr_depth = ex.cr_depth, stand_cr_width = ex.stand_cr_width, stand_cr_depth = ex.stand_cr_depth, soil_group = ex.soil_group, rooting = ex.rooting, dist_edge = ex.dist_edge, gap_size = ex.gap_size, spacing_before = ex.spacing_before, years_since_thin = ex.years_since_thin, moe = ex.moe, mor = ex.mor, fknot = ex.fknot, stem_vol = ex.stem_vol, crown_vol = ex.crown_vol, stem_density = ex.stem_density, crown_density = ex.crown_density, c_reg = ex.c_reg, c_drag = ex.c_drag, n_drag = ex.n_drag, drag_upper_limit = ex.drag_upper_limit, snow_depth = ex.snow_depth, snow_density = ex.snow_density, ro = ex.ro, x = ex.x, y = ex.y, z = ex.z, dams = ex.dams)))
library(tidyr)
out_tmc_df <- unnest(out_tmc)
