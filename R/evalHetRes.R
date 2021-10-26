
###############################################################
# initialisation
###############################################################

# load packages
require(dplyr)
require(stringr)
require(glmulti)

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
hist(df$DegreeRecovery)
boxplot(df$DegreeRecovery ~ df$ws)
boxplot(df$DegreeRecovery ~ df$cl)
boxplot(df$DegreeRecovery ~ df$cd)
boxplot(df$DegreeRecovery ~ df$gi)
boxplot(df$DegreeRecovery ~ df$dg)
boxplot(df$DegreeRecovery ~ df$NclassSpini)
dev.off()


###############################################################
# model het - res relationship
###############################################################

df$NclassSpini <- as.factor(df$NclassSpini)

mod <- lm(DegreeRecovery ~ ws + cl + cd + gi + dg, data = df)



mod <- glmulti(DegreeRecovery ~ ws + cl + cd + gi + dg + NclassSpini, data = df, level = 1, method = 'h',
               crit = aic, plotty = TRUE, report = TRUE,
               family = gaussian(link = "identity"))
#
