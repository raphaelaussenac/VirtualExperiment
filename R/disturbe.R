disturbe <- function(ws){

  ###############################################################
  # initialisation
  ###############################################################

  # load packages
  require(dplyr)
  require(ggplot2)

  # load critical wind speed data
  df <- readRDS(paste0(tempPath, '/cws.rds'))

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

  # disturbe stands
  disturbed <- lapply(ws, disturb, df)
  disturbed <- bind_rows(disturbed)

  # save
  disturbed$postDisturbance <- 'true'
  write.csv(disturbed, paste0(modPath, '/', model, 'Disturbed.csv'), row.names = FALSE)

  ###############################################################
  # evaluation
  ###############################################################

  # plot distribution of post-disturbance BA
  # calculate stand BA
  ba <- disturbed %>% group_by(simID) %>%
                            summarise(BA = sum((pi * (D_cm/200)^2) * weight)) %>%
                            mutate(ws = ifelse(substr(simID, 1, 2) =='W1', ws[1], ifelse(substr(simID, 1, 2) =='W2', ws[2], ws[3])))
  #
  # plot BA of disturbed stands depending on their associated ws
  pl1 <- ggplot() +
  geom_boxplot(data = ba, aes(x = ws, y = BA, group = ws)) +
  ylab('post-dist BA') +
  theme_bw() +
  scale_x_continuous(minor_breaks = seq(70 , 90, 5), breaks = seq(70, 90, 5))
  pl1
  # save plot
  ggsave(file = paste0(evalPath, '/postDistBA.pdf'), plot = pl1, width = 10, height = 10)

  # compare tree dbh pre/post disturbance
  # create column with wind speed
  disturbed <- disturbed %>% group_by(simID) %>%
                              mutate(ws = ifelse(substr(simID, 1, 2) =='W1', ws[1], ifelse(substr(simID, 1, 2) =='W2', ws[2], ws[3])))
  # plot pre and post disturbance distribution of tree dbh
  pl2 <- ggplot() +
    geom_histogram(data = df, aes(x = D_cm)) +
    geom_histogram(data = disturbed, aes(x = D_cm), fill = 'orange', alpha = 0.2) +
    facet_wrap(. ~ ws) +
    theme_bw()
  pl2
  ggsave(file = paste0(evalPath, '/dbhPreVSpPost.pdf'), plot = pl2, width = 10, height = 10)

}
