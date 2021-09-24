cws <- function(model){
  ###############################################################
  # initialisation
  ###############################################################

  # load packages
  # library("devtools")
  # devtools::install_github("tom-locatelli/fgr", build_vignettes = FALSE)
  require(fgr)
  require(dplyr)
  require(ggplot2)
  require(tidyr)

  # load pre-disturbance stands
  initPath <- paste0('./data/', model, '/init/')
  csvFile <- list.files(path = initPath, pattern = '\\.csv$')
  df <- read.csv(paste0(initPath, csvFile), sep = ';')
  df$simID <- as.character(df$simID)
  df$species <- as.factor(df$species)
  # add tree id
  df$tree <- c(1:nrow(df))



  df <- df[df$simID %in% unique(df$simID)[1:12],]


  ###############################################################
  # convert sp codes and calculate stand level variables
  ###############################################################

  # create correspondance between sp latin names and fgr sp codes
  # fgr::species_parameters
  fgrSpCode <- data.frame(latinName = c('Fagus sylvatica', 'Quercus petraea', 'Picea abies', 'Pinus sylvestris'),
                          fgrSpCode = c('BE', 'OK', 'NS', 'SP'),
                          Imaestro = c('fasy', 'qupe', 'piab', 'pisy'))
  #

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
  # calculate normalised 'BAl' competition index
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

  ###############################################################
  # set soil features
  ###############################################################

  df$soil <- 1 # 1 = soil class A
  df$roots <- 2 # 2 = roots can penetrate to >=80 cm


  ###############################################################
  # calculate critical wind speed (cws) for all trees
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
  cws <- lapply(c(1:nrow(df75)), damage, df75)
  cws <- data.frame(matrix(unlist(cws), nrow=length(cws), byrow=TRUE))
  colnames(cws) <- c('tree', 'cws_ms')

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

  ###############################################################
  # save
  ###############################################################

  saveRDS(df , paste0(tempPath, '/cws.rds'))

  # plot cws values for 12 first stands
  pl1 <- ggplot() +
    geom_point(data = df[df$simID %in% unique(df$simID)[1:12],], aes(x = D_cm, y = cws_kmh, col = latinName)) +
    # geom_hline(yintercept = 75, col = 'red') +
    # geom_hline(yintercept = 80, col = 'red') +
    # geom_hline(yintercept = 85, col = 'red') +
    facet_wrap(. ~ simID) +
    theme_bw()
  pl1
  # save plot
  ggsave(file = paste0(evalPath, '/cws12stands.pdf'), plot = pl1, width = 10, height = 10)

}
