evalHetRes <- function(){

  ###############################################################
  # initialisation
  ###############################################################

  # load packages
  require(dplyr)
  require(stringr)
  require(glmulti)
  require(ggplot2)
  require(sjPlot)
  require(see)

  # load heterogeneity - resilience metrics
  df <- readRDS(paste0(tempPath, '/hetResMet.rds'))
  # add specific columns with modalities
  modalities <- data.frame(str_split(df$simulationId, '-', simplify = TRUE))
  colnames(modalities) <- c('ws', 'cl', 'cd', 'gi', 'dg')
  df <- cbind(df, modalities)

  # plot interactions
  df2 <- df  %>% mutate(cl = case_when(cl == 'CL1' ~ 'peak', #68
                                        cl == 'CL2' ~ 'hotter', #77
                                        cl == 'CL3' ~ 'wetter',
                                        cl == 'CL4' ~ 'hotter-wetter'),
                         NclassSpini = as.factor(NclassSpini)) %>%
                  rename(climate = cl)
  df2$climate <- factor(df2$cl, levels = c('peak', 'hotter', 'wetter', 'hotter-wetter'))

  ###############################################################
  # plot het - res relationship
  ###############################################################

  # recovery distribution
  pl1 <- ggplot(data = df) +
  geom_histogram(aes(x = DegreeRecovery)) +
  theme_bw()
  ggsave(file = paste0(evalPath, '/resilience.pdf'), plot = pl1, width = 10, height = 10)

  # wind speed
  pl2 <- ggplot(data = df, aes(x = ws, y = DegreeRecovery)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw()
  ggsave(file = paste0(evalPath, '/effectWS.pdf'), plot = pl2, width = 10, height = 10)

  # climate
  pl3 <- ggplot(data = df, aes(x = cl, y = DegreeRecovery)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw()
  ggsave(file = paste0(evalPath, '/effectCL.pdf'), plot = pl3, width = 10, height = 10)

  # composition
  pl4 <- ggplot(data = df, aes(x = reorder(cd,-DegreeRecovery, na.rm = TRUE), y = DegreeRecovery, -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw()
  ggsave(file = paste0(evalPath, '/effectCD.pdf'), plot = pl4, width = 10, height = 10)

  # gini
  pl5 <- ggplot(data = df, aes(x = gi, y = DegreeRecovery, -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw()
  ggsave(file = paste0(evalPath, '/effectGI.pdf'), plot = pl5, width = 10, height = 10)

  # dg
  pl6 <- ggplot(data = df, aes(x = dg, y = DegreeRecovery, -cd, na.rm = TRUE)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw()
  ggsave(file = paste0(evalPath, '/effectDg.pdf'), plot = pl6, width = 10, height = 10)

  # sp richness
  pl7 <- ggplot(data = df, aes(x = as.factor(NclassSpini), y = DegreeRecovery)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
  theme_bw()
  ggsave(file = paste0(evalPath, '/effectDI.pdf'), plot = pl7, width = 10, height = 10)

  # recovery = f(gini, by dg, ws and cl)
  pl8 <- ggplot(data = df2, aes(x = gi, y = DegreeRecovery)) +
  geom_boxplot(aes(fill = climate)) +
  scale_fill_manual(values = c('chartreuse2', 'orangered', 'turquoise1', 'darkorchid2')) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'), legend.position = 'bottom')
  ggsave(file = paste0(evalPath, '/interactionsCL.pdf'), plot = pl8, width = 10, height = 10)

  # recovery = f(gini, by dg, ws and sp richness)
  pl9 <- ggplot(data = df2, aes(x = NclassSpini, y = DegreeRecovery)) +
  geom_boxplot(aes(fill = gi)) +
  stat_summary(fun = mean, geom = 'point', shape = 19, size = 5, col = 'black') +
  facet_wrap(dg~ws, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(colour = 'black', fill = 'white'), legend.position = 'bottom') +
  scale_fill_discrete(name = "species richness")
  ggsave(file = paste0(evalPath, '/interactionsDI.pdf'), plot = pl9, width = 10, height = 10)


}

# # recovery = f(sp richness, by dg, ws and cl)
# ggplot(data = df, aes(x = as.factor(NclassSpini), y = DegreeRecovery)) +
# geom_boxplot(aes(fill = cl)) +
# stat_summary(fun = mean, geom = 'point', shape = 20, size = 5, color = 'red', fill = 'red') +
# facet_wrap(dg~ws, nrow = 1) +
# theme_bw() +
# theme(strip.background = element_rect(colour = 'black', fill = 'white'))
#

###############################################################
# model het - res relationship
###############################################################
#
# library(lme4)
# mod <- lmer(DegreeRecovery ~ cl + ws + dg + gi + NclassSpini + (gi | cl/dg/ws), data = df, REML = TRUE)
#
#
# df$NclassSpini <- as.factor(df$NclassSpini)
#
#
# # change categorical variables into continuous variables
# df <- df %>% mutate(wsNum = case_when(ws == 'W1' ~ 64, #68
#                                    ws == 'W2' ~ 73, #77
#                                    ws == 'W3' ~ 80), #86
#                     giNum = case_when(gi == 'G1' ~ 0.3,
#                                    gi == 'G2' ~ 0.4,
#                                    gi == 'G3' ~ 0.5,
#                                    gi == 'G4' ~ 0.6,),
#                     dgNum = case_when(dg == 'D1' ~ 30,
#                                    dg == 'D2' ~ 40))
# #
#
# mod <- lm(DegreeRecovery ~ cl + ws + dg + giNum + NclassSpini + giNum:dg + giNum:ws, data = df)
#
# plot_model(mod, type = 'pred', terms = c('giNum', 'NclassSpini', 'ws', 'dg'))
#
# mod <- glmulti(DegreeRecovery ~ ws + cl + cd + gi + dg + NclassSpini, data = df, level = 1, method = 'g',
#                crit = aic, plotty = TRUE, report = TRUE,
#                family = gaussian(link = 'identity'))
# #
