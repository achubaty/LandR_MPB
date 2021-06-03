## pre-simulation preparation

do.call(SpaDES.core::setPaths, paths3)

timesFit <- list(start = 2010, end = 2011) ## 2010-2016
paramsFit <- list(
  canWind = list(
    climateModel = "GCM4", ## TODO: parametrize using config
    climateScenario = "4.5", ## TODO: parametrize using config
    .plotInitialTime = timesFit$start,
    .plotInterval = NA,
    #.tempdir = scratchDir,
    years = seq(timesFit$start, timesFit$end, 1)
  ),
  mpbClimateData = list(
    climateModel = "GCM4", ## TODO: parametrize using config
    climateScenario = "4.5", ## TODO: parametrize using config
    suitabilityIndex = "R",    ## Can be "G", "S", "L", "R"
    .maxMemory = maxMemory,
    .useCache = eventCaching,
    .plotInitialTime = NA#,
    #.tempdir = scratchDir
  ),
  mpbMassAttacksData = list(
    .maxMemory = maxMemory,
    .useCache = eventCaching,
    .plotInitialTime = NA#,
    #.tempdir = scratchDir
  ),
  mpbPine = list(
    lowMemory = lowMemory,
    .maxMemory = maxMemory,
    .useCache = eventCaching,
    .plotInitialTime = NA#,
    #.tempdir = scratchDir
  ),
  mpbRedTopGrowth = list(
    .useCache = eventCaching,
    .plotInitialTime = NA,
    dataset = "Boone2011"
  ),
  mpbRedTopSpread = list(
    advectionDir = 90,
    advectionMag = 1000,
    bgSettlingProp = 0.1,
    meanDist = 1000,
    type = if (Require:::isWindows()) "runOnce" else "runOnce" # "runOnce"#  "optim" "nofit"
  )
)

objects3 <- list(
  studyArea = simOutPreamble$studyArea,
  studyAreaFit = simOutPreamble$studyAreaFit ## TODO: pass this explicitly as studyArea
)

modules3 <- list(
  "mpbClimateData", "mpbPine",
  "mpbMassAttacksData",
  "mpbRedTopGrowth",
  "mpbRedTopSpread"#,
  #"mpbManagement"
)

MPBfit <- #Cache(
  simInitAndSpades(
    times = timesFit,
    params = paramsFit,
    modules = modules3,
    objects = objects3,
    loadOrder = unlist(modules3)#,
    # events = "init"
    # useCache = "overwrite"
  )

if (FALSE) {
  projDir <- getwd()
  objectiveFunction <- function(params) {
    data.table::setDTthreads(1L)

    RenvironFile <- file.path(projDir, ".Renviron")
    if (file.exists(RenvironFile)) readRenviron(RenvironFile)

    SpaDES.core::setPaths(
      cachePath = file.path(projDir, "cache"),
      inputPath = file.path(projDir, "inputs"),
      modulePath = file.path(projDir, "modules"),
      outputPath = file.path(projDir, "outputs"),
      rasterPath = file.path(scratchDir, "rasters")
    )
    lowMemory <- TRUE
    maxMemory <- 5e+9

    raster::rasterOptions(default = TRUE)
    options(
      "rasterMaxMemory" = maxMemory,
      "rasterTmpDir" = Paths$rasterPath,
      "reproducible.cacheSaveFormat" = "rds", ## can be "qs" or "rds"
      "reproducible.conn" = cacheDBconn
    )

    # Sys.sleep(runif(1, 1, 100)) ## random delay to mitigate database locked errors

    sim <- reproducible::retry(quote({
      simInit(times = timesFit, params = paramsFit, modules = modules,
              #objects = objects,
              #useCache = "overwrite",
              loadOrder = unlist(modules)
      )
    }))

    params(sim)[["mpbRedTopSpread"]][["advectionDir"]] <- params[1]
    params(sim)[["mpbRedTopSpread"]][["advectionMag"]] <- params[2]
    params(sim)[["mpbRedTopSpread"]][["bgSettlingProp"]] <- params[3]
    params(sim)[["mpbRedTopSpread"]][["meanDist"]] <- params[4]

    simOut <- spades(sim, .plotInitialTime = NA, debug = FALSE)

    ## TODO: below currently does total attack area. more refined criteria needed
    ## e.g., area and shape metrics from `landscapemetrics` package?

    ## simulated attack area
    atks <- simOut$massAttacksDT
    nPix <- atks[ATKTREES > 0, .N]
    atkAreaSim <- nPix * prod(res(simOut$rasterToMatch)) / (100^2) ## area in ha

    ## attacked area from data
    atksRas <- simOut$massAttacksMap[[paste0("X", timesFit$end)]]
    atks <- data.table(ID = 1L:ncell(atksRas), ATKTREES = atksRas[])
    nPix <- atks[ATKTREES > 0, .N] ## total number of pixels
    atkAreaData <- nPix * prod(res(simOut$rasterToMatch)) / (100^2) ## area in ha

    ## sum negative log likelihood for attacked pixels


    ## TODO: something other than simple sum of squares?
    metric <- (atkAreaData - atkAreaSim)^2 #+ (SNLL / 10^3)
    return(metric)
  }

  params4POM <- data.frame(
    name = c("advectionDir", "advectionMag", "bgSettlingProp", "meanDist"),
    lower = c(  0.000,   10, 0.01,   100),
    upper = c(359.999, 1000, 0.20, 10000), ## TODO: refine these upper and lower limits
    stringsAsFactors = FALSE
  )

  packages4POM <- lapply(unlist(modules3), function(m) reqdPkgs(MPBfit, m)) %>%
    unlist() %>%
    unique() %>%
    grep("@", ., invert = TRUE, value = TRUE) %>%
    c("amc", "LandR", "pemisc", "SpaDES.core")

  N <- 10 * nrow(params4POM) ## need 10 populations per parameter
  cl <- parallel::makeCluster(min(N, 20))  ## forking doesn't work with data.table

  parallel::clusterExport(cl, varlist = c("modules", "paramsFit", "packages4POM", "projDir", "scratchDir", "timesFit"))
  parallel::clusterEvalQ(
    cl, {
      for (i in packages4POM)
        library(i, character.only = TRUE)
    }
  )
  outPOM <- DEoptim(fn = objectiveFunction,
                    control = DEoptim::DEoptim.control(
                      cluster = cl, ## see ArdiaD/DEoptim#3
                      #foreachArgs = c(.packages = packages4POM), ## only when parallelType = 2
                      initialpop = NULL,
                      itermax = 50,
                      #packages = as.list(packages4POM),
                      parallelType = 1, ## 0 = single thread; 1 = parallel; 2 = foreach
                      #parVar = list("modules", "paramsFit", "timesFit", "Paths"),
                      VTR = 0
                    ),
                    lower = params4POM$lower,
                    upper = params4POM$upper
  )
  parallel::stopCluster(cl)

  qs::qsave(outPOM, file.path(Paths$outputPath, "outPOM.qs"))
  #saveSimList(MPBfit, file.path(Paths$outputPath, "test.qs"))
  #MPBfit <- loadSimList(file.path(Paths$outputPath, "test.qs"))

  ## MPB spread fit Summarios
  paramNames <- c("advectionDir", "advectionMag", "bgSettlingProp", "meanDist")

  bestValues <- as.data.frame(outPOM[["member"]][["bestmemit"]])
  colnames(bestValues) <- paramNames

  bestFitVals <- as.data.frame(t(outPOM[["optim"]][["bestmem"]]))
  colnames(bestFitVals) <- paramNames

  plot(bestValues$advectionDir)
  points(bestFitVals$advectionDir, col = "red", pch = 19)

  plot(bestValues$advectionMag)
  points(bestFitVals$advectionMag, col = "red", pch = 19)

  plot(bestValues$bgSettlingProp)
  points(bestFitVals$bgSettlingProp, col = "red", pch = 19)

  plot(bestValues$meanDist)
  points(bestFitVals$meanDist, col = "red", pch = 19)
}
