wsDamages <- function(method, thresholds){

  ###############################################################
  # initialisation
  ###############################################################

  # load packages
  require(tidyverse)
  require(ggplot2)

  # load critical wind speed data
  df <- readRDS(paste0(tempPath, '/cws.rds'))


  ###############################################################
  # calculate BA damages depending on wind speed
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
  ws <- c(seq(50, 100, by = 1))
  damdf <- lapply(ws, windDamages, df)
  damSite <- data.frame(simID = (1:nrow(damdf[[1]])))
  damdf <- as.data.frame(do.call(cbind, damdf))
  damdf <- cbind(damSite, damdf)
  colnames(damdf) <- c('simID', paste0('ws', ws))
  damdf <- damdf %>% pivot_longer(cols = starts_with('ws'), names_to = 'ws',
                                  values_to = 'damagedBA', names_prefix = 'ws')
  damdf$ws <- as.numeric(damdf$ws)

  # thresholds
  if(method == 'ws'){
    damdfThresholds <- damdf %>% filter(ws %in% thresholds)
  } else if(method == "dBA"){
    thresh <- damdf %>% group_by(ws) %>% summarise(median = median(damagedBA)) %>%
                                         mutate(thresh1 = abs(median-thresholds[1]),
                                                thresh2 = abs(median-thresholds[2]),
                                                thresh3 = abs(median-thresholds[3]))
    thresh1 <- as.numeric(thresh %>% filter(thresh1 == min(thresh1)) %>% summarise(ws))
    thresh2 <- as.numeric(thresh %>% filter(thresh2 == min(thresh2)) %>% summarise(ws))
    thresh3 <- as.numeric(thresh %>% filter(thresh3 == min(thresh3)) %>% summarise(ws))
    damdfThresholds <- damdf %>% filter(ws %in% c(thresh1, thresh2, thresh3))
  }

  # plot damaged BA = f(ws)
  pl1 <- ggplot() +
  geom_boxplot(data = damdf, aes(x = ws, y = damagedBA, group = ws), fill = 'grey', width = 0.8) +
  geom_boxplot(data = damdfThresholds, aes(x = ws, y = damagedBA, group = ws), col = 'red', fill = 'pink', width = 0.8) +
  theme_light() +
  theme(legend.position = "none") +
  scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10)) +
  scale_x_continuous(minor_breaks = seq(50 , 120, 5), breaks = seq(50, 120, 5))
  pl1

  # save plot
  ggsave(file = paste0(evalPath, '/wsDamages.pdf'), plot = pl1, width = 15, height = 10)

  # save wind speed modalities
  if(method == 'ws'){
    ws <- thresholds
  } else if(method == 'dBA'){
    ws <- c(thresh1, thresh2, thresh3)
  }
  saveRDS(ws , paste0(tempPath, '/ws.rds'))

}
