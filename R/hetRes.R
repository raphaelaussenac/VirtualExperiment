###############################################################
# initialisation
###############################################################

# load packages
# library(devtools)
# devtools::install_github('raphaelaussenac/forestdiversity', build_vignettes=TRUE)
require(forestdiversity)
require(dplyr)
require(ggplot2)
require(data.table)
require(doParallel)

# load init data
initPath <- paste0('./data/', model, '/init/')
csvFile <- list.files(path = initPath, pattern = '\\.csv$')
init <- read.csv(paste0(initPath, csvFile), sep = ';')
# duplicate init stands (without W modalities) and add W2, W3 and W4 to simIDs
nrowInit <-  nrow(init)
init <- rbind(init, init, init)
init$simID <- paste0(c(rep('W2-', nrowInit), rep('W3-', nrowInit), rep('W4-', nrowInit)), init$simID)

# load disturbed data
dist <- read.csv(paste0(modPath, '/', model, 'Disturbed.csv'))

# retrieve list of simulation files
simPath <- paste0('./data/', model, '/sim')
simList <- list.files(path = simPath, pattern = '\\.csv$')

# species code correspondence
spCor <- data.frame(spSalem = c(9, 3, 52, 62), species = c('fasy', 'qupe', 'pisy', 'piab'))

# recdf <- data.frame()
hetRes <- function(i, simPath, init, dist, sim, spCor){
  # load sim data
  sim <- read.csv(paste0(simPath, '/', i), sep = ';')
  ID <- unique(sim$simID)
  # rbind init, dist and sim
  df <- rbind(init[init$simID == ID,], dist[dist$simID == ID,], sim)
  # format sim
  df <- df %>% left_join(spCor, by = 'species') %>%
                 mutate(simID = as.integer(1),
                        postThinning = as.factor(postThinning),
                        postDisturbance = as.factor(postDisturbance),
                        species = as.integer(spSalem),
                        X = NA,
                        Y = NA) %>%
                 select(-comment, -spSalem) %>%
                 rename(site = simID) %>%
                 relocate(c(X,Y), .after = V_m3)
  #
  # format function from 'forestdiversity'
  df2 <- format_salem(df, Out = 'HillNb')
  # plot(df2, Nvar='V_m3', RecTime=20, normalize='baseline')

  # recovery metrics
  rec <- EventResilience(df2, Nvar='V_m3', RecTime = 20, normalize = 'impact')
  rec$simulationId <- ID
  # recdf <- rbind(recdf, rec)
  return(rec)

}

# start_time <- Sys.time()
# test <- lapply(simList, hetRes, simPath, init, dist, sim, spCor)
# nameList <- names(test[[1]])
# test <- data.frame(matrix(unlist(test), nrow = length(test), byrow = TRUE))
# colnames(test) <- nameList
# end_time <- Sys.time()
# end_time - start_time
#
# ---> 2 min pour 50 ppt --> 1.004 h pour 1440 ppt

# run calculation in parallel
start_time <- Sys.time()
cl <- makeCluster(8)
registerDoParallel(cl)
test <- foreach(i = simList, .combine = 'rbind', .packages = c('forestdiversity', 'dplyr')) %dopar% {hetRes(i = i, simPath = simPath, init = init, dist = dist, sim = sim, spCor = spCor)}
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time
# ---> 1.151585 min pour 50 ppt --> 0.5527608 h pour 1440 ppt
