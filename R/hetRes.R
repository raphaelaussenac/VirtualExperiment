hetRes <- function(model){

  ###############################################################
  # initialisation
  ###############################################################

  # load packages
  # library(devtools)
  # devtools::install_gitlab('arnaud.guyennon/forestdiversity', build_vignettes=TRUE)
  require(forestdiversity)
  require(dplyr)
  require(ggplot2)
  require(data.table)
  require(doParallel)

  # load init data
  if(model == 'salem'){
    initPath <- paste0('./data/init/', model, '/')
  } else {
    initPath <- paste0('./data/init/otherModels/')
  }
  csvFile <- list.files(path = initPath, pattern = '\\.csv$')
  init <- read.csv(paste0(initPath, csvFile), sep = ';')
  # duplicate init stands (without W modalities) and add W1, W2 and W3 to simIDs
  nrowInit <- nrow(init)
  init <- rbind(init, init, init)
  init$simID <- paste0(c(rep('W1-', nrowInit), rep('W2-', nrowInit), rep('W3-', nrowInit)), init$simID)

  # load disturbed data
  csvFile <- list.files(path = modPath, pattern = '\\.csv$')
  dist <- read.csv(paste0(modPath, '/', csvFile))

  # retrieve list of simulation files
  simPath <- paste0('./data/sim/', model, '/')
  simList <- list.files(path = simPath, pattern = '\\.csv$')

  # species code correspondence
  spCor <- data.frame(spSalem = c(9, 3, 52, 62), species = c('fasy', 'qupe', 'pisy', 'piab'))


#   # TODO: checker le nombre de init / dist / sim
  length(unique(init$simID))
  length(unique(dist$simID))
  length(simList)
#   # TODO: que se passe-t-il quand il n'y a pas de dist?
#   unique(init$simID)[!(unique(init$simID) %in% unique(dist$simID))]
#   i <- "samsaraPostDist_Reformated_Disturbed_W1-CL1-CD1-G1-D1.csv"
#
# ---> ajouter un warning

  # recdf <- data.frame()
  hetResMet <- function(i, simPath, init, dist, sim, spCor){
    # load sim data
    sim <- read.csv(paste0(simPath, '/', i), sep = ';')
    ID <- sim$simID[1]
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
    df2 <- format_salem(df, Out = 'HillNb', ClassIni = 7.5) #TODO: define ClassIni = 0?
    # plot(df2, Nvar='V_m3', RecTime=20, normalize='baseline')

    # recovery metrics
    rec <- EventResilience(df2, Nvar='V_m3', RecTime = 20, normalize = 'baseline')
    rec$simulationId <- ID
    # recdf <- rbind(recdf, rec)
    return(rec)

  }

  # run calculation in parallel
  start_time <- Sys.time()
  cl <- makeCluster(5)
  registerDoParallel(cl)
  df <- foreach(i = simList, .combine = 'rbind', .packages = c('forestdiversity', 'dplyr')) %dopar% {hetResMet(i = i, simPath = simPath, init = init, dist = dist, sim = sim, spCor = spCor)}
  stopCluster(cl)
  end_time <- Sys.time()
  end_time - start_time

  # save
  saveRDS(df , paste0(tempPath, '/hetResMet.rds'))

}
