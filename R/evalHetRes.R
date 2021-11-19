
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

###############################################################
# plot het - res relationship
###############################################################

pdf(paste0(evalPath, '/rplot.pdf'))

# recovery distribution
hist(df$DegreeRecovery)

# wind speed
pl1 <- ggplot(data = df, aes(x = ws, y = DegreeRecovery)) +
geom_boxplot() +
stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
theme_bw()
pl1
# climate
pl2 <- ggplot(data = df, aes(x = cl, y = DegreeRecovery)) +
geom_boxplot() +
stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
theme_bw()
pl2
# composition
pl3 <- ggplot(data = df, aes(x = reorder(cd,-DegreeRecovery, na.rm = TRUE), y = DegreeRecovery, -cd, na.rm = TRUE)) +
geom_boxplot() +
stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
theme_bw()
pl3
# gini
pl4 <- ggplot(data = df, aes(x = gi, y = DegreeRecovery, -cd, na.rm = TRUE)) +
geom_boxplot() +
stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
theme_bw()
pl4
# dg
pl5 <- ggplot(data = df, aes(x = dg, y = DegreeRecovery, -cd, na.rm = TRUE)) +
geom_boxplot() +
stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
theme_bw()
pl5
# sp richness
pl6 <- ggplot(data = df, aes(x = as.factor(df$NclassSpini), y = DegreeRecovery)) +
geom_boxplot() +
stat_summary(fun.y = mean, geom = "point", shape = 20, size = 5, color = "red", fill = "red") +
theme_bw()
pl6

dev.off()


###############################################################
# model het - res relationship
###############################################################

df$NclassSpini <- as.factor(df$NclassSpini)

mod <- lm(DegreeRecovery ~ cl + ws + dg + gi + cd, data = df)

plot_model(mod, type = "pred", terms = c('cd', 'ws', 'dg'))

# TODO: passer en continue

mod <- glmulti(DegreeRecovery ~ ws + cl + cd + gi + dg + NclassSpini, data = df, level = 1, method = 'g',
               crit = aic, plotty = TRUE, report = TRUE,
               family = gaussian(link = "identity"))
#
