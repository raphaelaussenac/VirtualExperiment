plotHetRes <- function(metric, var){

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
  df <- df %>% mutate(cl = case_when(cl == 'CL1' ~ 'peak', #68
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
  xlab(paste(var, '-', metric)) +
  theme_bw() #+

  # wind speed
  pl2 <- ggplot(data = df, aes(x = ws, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  ylab(paste(var, '-', metric)) +
  theme_bw()

  # climate
  pl3 <- ggplot(data = df, aes(x = climate, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  ylab(paste(var, '-', metric)) +
  theme_bw()

  # composition
  pl4 <- ggplot(data = df, aes(x = reorder(cd,-get(metric), na.rm = TRUE), y = get(metric), -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  ylab(paste(var, '-', metric)) +
  theme_bw()

  # gini
  pl5 <- ggplot(data = df, aes(x = gi, y = get(metric), -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  ylab(paste(var, '-', metric)) +
  theme_bw()

  # dg
  pl6 <- ggplot(data = df, aes(x = dg, y = get(metric), -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  ylab(paste(var, '-', metric)) +
  theme_bw()

  # sp richness
  pl7 <- ggplot(data = df, aes(x = NclassSpini, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  ylab(paste(var, '-', metric)) +
  theme_bw()

  # f(gini, by dg, ws)
  pl8 <- ggplot(data = df, aes(x = gi, y = get(metric))) +
  geom_boxplot() +
  scale_fill_manual(values = c('chartreuse2', 'orangered', 'turquoise1', 'darkorchid2')) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  ylab(paste(var, '-', metric)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'))

  # f(gini, by dg, ws and cl)
  pl9 <- ggplot(data = df, aes(x = gi, y = get(metric))) +
  geom_boxplot(aes(fill = climate)) +
  scale_fill_manual(values = c('chartreuse2', 'orangered', 'turquoise1', 'darkorchid2')) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  ylab(paste(var, '-', metric)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'), legend.position = 'bottom')

  # f(sp richness, by dg, ws)
  pl10 <- ggplot(data = df, aes(x = NclassSpini, y = get(metric))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  ylab(paste(var, '-', metric)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'))


  # f(sp richness, by dg, ws)
  pl11 <- ggplot(data = df, aes(x = NclassSpini, y = get(metric))) +
  geom_boxplot(aes(fill = gi)) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  ylab(paste(var, '-', metric)) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'), legend.position = 'bottom')

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


evalHetRes <- function(metric, var){
  lapply(metric, plotHetRes, var)
}
