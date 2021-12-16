plotHetRes <- function(metric){

  ###############################################################
  # initialisation
  ##############################################################

  # load packages
  require(dplyr)
  require(stringr)
  require(ggplot2)

  # load heterogeneity - resilience metrics
  df <- readRDS(paste0(tempPath, '/hetResMet.rds'))
  # add specific columns with modalities
  modalities <- data.frame(str_split(df$simulationId, '-', simplify = TRUE))
  colnames(modalities) <- c('ws', 'cl', 'cd', 'gi', 'dg')
  df <- cbind(df, modalities)

  # plot interactions
  df <- df  %>% mutate(cl = case_when(cl == 'CL1' ~ 'peak', #68
                                        cl == 'CL2' ~ 'hotter', #77
                                        cl == 'CL3' ~ 'wetter',
                                        cl == 'CL4' ~ 'hotter-wetter'),
                         NclassSpini = as.factor(NclassSpini)) %>%
                  rename(climate = cl)
  df$climate <- factor(df$cl, levels = c('peak', 'hotter', 'wetter', 'hotter-wetter'))

  ###############################################################
  # plot het - res relationship
  ###############################################################

  # recovery distribution
  pl1 <- ggplot(data = df) +
  geom_histogram(aes(x = get(metric)), bins = 100) +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/resilience.jpg'), plot = pl1, width = 10, height = 10)

  # wind speed
  pl2 <- ggplot(data = df, aes(x = ws, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/effectWS.jpg'), plot = pl2, width = 10, height = 10)

  # climate
  pl3 <- ggplot(data = df, aes(x = climate, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/effectCL.jpg'), plot = pl3, width = 10, height = 10)

  # composition
  pl4 <- ggplot(data = df, aes(x = reorder(cd,-get(metric), na.rm = TRUE), y = get(metric), -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/effectCD.jpg'), plot = pl4, width = 10, height = 10)

  # gini
  pl5 <- ggplot(data = df, aes(x = gi, y = get(metric), -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/effectGI.jpg'), plot = pl5, width = 10, height = 10)

  # dg
  pl6 <- ggplot(data = df, aes(x = dg, y = get(metric), -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/effectDg.jpg'), plot = pl6, width = 10, height = 10)

  # sp richness
  pl7 <- ggplot(data = df, aes(x = NclassSpini, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw() #+
  # ggsave(file = paste0(resultPath, '/effectDI.jpg'), plot = pl7, width = 10, height = 10)

  # f(gini, by dg, ws)
  pl8 <- ggplot(data = df, aes(x = gi, y = get(metric))) +
  geom_boxplot() +
  scale_fill_manual(values = c('chartreuse2', 'orangered', 'turquoise1', 'darkorchid2')) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white')) #+
  # ggsave(file = paste0(resultPath, '/giniNested.jpg'), plot = pl8, width = 10, height = 10)

  # f(gini, by dg, ws and cl)
  pl9 <- ggplot(data = df, aes(x = gi, y = get(metric))) +
  geom_boxplot(aes(fill = climate)) +
  scale_fill_manual(values = c('chartreuse2', 'orangered', 'turquoise1', 'darkorchid2')) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'), legend.position = 'bottom') #+
  # ggsave(file = paste0(resultPath, '/giniClimNested.jpg'), plot = pl9, width = 10, height = 10)

  # f(sp richness, by dg, ws)
  pl10 <- ggplot(data = df, aes(x = NclassSpini, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'))
  # ggsave(file = paste0(resultPath, '/spNested.jpg'), plot = pl10, width = 10, height = 10)

  # f(sp richness, by dg, ws)
  pl11 <- ggplot(data = df, aes(x = NclassSpini, y = get(metric))) +
  geom_boxplot(aes(fill = gi)) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'), legend.position = 'bottom')
  # ggsave(file = paste0(resultPath, '/spGiNested.jpg'), plot = pl11, width = 10, height = 10)

  # save all plots in a single pdf
  pdf(paste0(resultPath, '/', metric, '.pdf'), width = 10, height = 10)
  print(pl1)
  print(pl2)
  print(pl3)
  print(pl4)
  print(pl5)
  print(pl6)
  print(pl7)
  print(pl8)
  print(pl9)
  print(pl10)
  print(pl11)
  dev.off()

}


evalHetRes <- function(metric){
  lapply(metric, plotHetRes)
}
