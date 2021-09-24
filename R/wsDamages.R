wsDamages <- function(){

  ###############################################################
  # initialisation
  ###############################################################

  # load packages
  require(dplyr)
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
  ggsave(file = paste0(evalPath, '/wsDamages.pdf'), plot = pl1, width = 10, height = 10)

}
