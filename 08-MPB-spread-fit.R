## pre-simulation preparation

do.call(SpaDES.core::setPaths, paths3)

timesFit <- list(start = 2010, end = 2020) ## 2010-2016
times <- list(start = 2010, end = 2020) ## 2010-2016
timesForecast <- list(start = 2020, end = 2030) ## 2010-2016

times <- timesForecast

paramsFit <- list(
  .globals = list(.plots = "png"),
  mpbClimateData = list(
    suitabilityIndex = "R",    ## Can be "G", "S", "L", "R"
    .maxMemory = maxMemory,
    .useCache = eventCaching
    # .plots = ""
    # .plotInitialTime = NA#,
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
  mpbRedTopSpread = list(
    # p_advectionDir = 90, # not used anymore
    p_advectionMag = 1000,
    p_meanDist = 1000,
    maxDistance = 1e5,
    dispersalKernel = "twodt",# "Generalized Gamma", # Weibull3
    # .plots = "screen",
    type = if (Require:::isWindows() || amc::isRstudio()) "validate" else "predict" # "predict" "validate" "runOnce"#  "optim" "nofit" "fit"
  )
)


objects3 <- list(
  studyArea = simOutPreamble$studyArea,
  studyAreaFit = simOutPreamble$studyAreaFit, ## TODO: pass this explicitly as studyArea
  absk = simOutPreamble$absk
)

modules3 <- list(
  "mpbClimateData", "mpbPine",
  "mpbMassAttacksData",
  "mpbRedTopSpread"#,
  #"mpbManagement"
)



if (!(Require:::isWindows() || amc::isRstudio()) && grepl("optim", tolower(paramsFit$mpbRedTopSpread$type )))
  MPBfit <- Cache(
    simInitAndSpades,
    times = timesFit,
    params = paramsFit,
    modules = modules3,
    objects = objects3,
    loadOrder = unlist(modules3),
    .cacheExtra = moduleCodeFiles(paths3, modules3)
    # events = "init"
    # useCache = "overwrite"
  )

#########################################################################################
# For Forecasting -- use a specific fit_mpbSpreadOptimizer object
DEoutFileList <- dir(dirname(paths3$outputPath), pattern = "DEout", full.names = TRUE)
fit_mpbSpreadOptimizer <- readRDS(DEoutFileList[[24]]) # 24 is currently best of my files (Eliot)
objects3 <- append(objects3,
                   list(fit_mpbSpreadOptimizer = fit_mpbSpreadOptimizer))
paramsFit$mpbRedTopSpread$dispersalKernel <- "Weibull3"
outputs <- setDF(rbindlist(list(
  data.frame(objectName = "fit_mpbSpreadOptimizer", saveTime = timesFit$end),
  data.table("ROCList", saveTime = timesFit$end)
), use.names = FALSE))
# paramsFit$mpbRedTopSpread$type <- "validate"

if (paramsFit$mpbRedTopSpread$type == "validate") {
  message(crayon::green("RUNNING ", paramsFit$mpbRedTopSpread$type))
  MPBpredict <- Cache(
    simInitAndSpades,
    times = times,
    params = paramsFit,
    modules = modules3,
    objects = objects3,
    outputs = outputs,
    loadOrder = unlist(modules3),
    .cacheExtra = moduleCodeFiles(paths3, modules3)
    # events = "init"
    # useCache = "overwrite"
  )
}

# ROLLING FORECASTING WINDOWS
if (paramsFit$mpbRedTopSpread$type == "predict" && times$start < 2020) {
  paramsFit$mpbRedTopSpread$type <- "predict"
  message(crayon::green("Running Rolling Forecasting"))
  todo <- rbindlist(lapply(2:10, function(x) data.table(rollingWindow = x, sy = 2010:(2020-x))))
  todo[, runName := paste0("RW", rollingWindow, "_", "X", sy)]
  # todo <- todo[45,]
  MPBpredictRolling <- parallel::mcMap(runName = todo$runName, rollingWindow = todo$rollingWindow, sy = todo$sy,
                                       mc.cores = 4,
                                       function(runName, rollingWindow, sy) {
                                         rwInd <- paste0("RW", rollingWindow)
                                         times <- list(start = sy, end = sy + rollingWindow)
                                         message(crayon::green("RUNNING ", paramsFit$mpbRedTopSpread$type))
                                         syNam <- paste0("X", sy)
                                         out <- Cache(
                                           simInitAndSpades,
                                           times = times,
                                           params = paramsFit,
                                           modules = modules3,
                                           objects = objects3,
                                           outputs = outputs,
                                           loadOrder = unlist(modules3),
                                           .cacheExtra = moduleCodeFiles(paths3, modules3)
                                           # events = "init"
                                           # useCache = "overwrite"
                                         )
                                         rm(climateSuitabilityMaps, envir = out)
                                         saveSimList(out, filename = file.path(outputPath(out), paste0("mpbRollingSim_", runName, ".qs")))
                                         return(out)
                                       }
  )
  set(todo, NULL, "ROC", sapply(MPBpredictRolling, function(sim) mean(sim$ROCList[[1]]$ROC)))
  forecastHorizonFn <- function(todo) {
    ggplot(todo, aes(x = rollingWindow, y = ROC)) +
      geom_point() +
      geom_smooth() +
      theme_bw() +
      xlab("Forecast horizon; years into the future") +
      ylab("AUC (Area under the Receiver Operating Curve)")
  }
  Plots(fn = forecastHorizonFn, todo = todo, type = paramsFit$.globals$.plots,
        filename = file.path(paths3$outputPath, "figures", paste0("Forecast Horizon")))
}


# FORECASTING with CLIMATE UNCERTAINTY

paramsFit$mpbRedTopSpread$type <- "predict"
paramsFit$mpbClimateData$.useCache <- ".inputObjects"
message(crayon::green("Forecasting with climate variation"))

out <- simInit(##AndExperiment2,
  times = times,
  params = paramsFit,
  modules = modules3,
  objects = objects3,
  outputs = outputs,
#  replicates = 6,
  loadOrder = unlist(modules3)#,
 # .cacheExtra = moduleCodeFiles(paths3, modules3)
  # events = "init"
  # useCache = "overwrite"
)
nreps <- 10
RNGkind("L'Ecuyer-CMRG")
# out2 <- lapply(seq(nreps), function(rep) spades(Copy(out)))
out2 <- mclapply(seq(nreps), function(rep) spades(Copy(out)), mc.cores = min(4, nreps))
out3 <- lapply(out2, function(sim) sim$predictedDT[, list(meanATKTREES = mean(ATKTREES, na.rm = TRUE)), by = "layerName"])
out4 <- rbindlist(out3, idcol = "rep")
saveRDS(out4, file = file.path(paths3$outputPath, "Forecasts with ConfInt.rds"))

plot_smoothPoint <- function(dt)  {
  ggplot(dt, aes(y = meanATKTREES, x = as.integer(gsub("X", "", layerName)))) +
    geom_point() +
    geom_smooth() +
    scale_y_continuous(trans = "log10") +
    theme_bw() +
    xlab("Year") +
    ylab("Forecasted Mean Attack Density (per km2 of attacked pixels)")
}

Plots(dt = out4, fn = plot_smoothPoint, type = paramsFit$.globals$.plots,
      filename = file.path(paths3$outputPath, "figures", paste0("Forecast Abundance")))

# UPLOAD FILES
# if (FALSE) {
  Require::Require("googledrive")
  local_files <- dir(file.path(paths3$outputPath, "figures"), full.names = TRUE)
  driveFolder <- as_id("1L2lNlsNa6DFaC9fUFwz6MyZw3mKSWQzd")
  files <- map(local_files, ~ drive_upload(.x, path = driveFolder, overwrite = TRUE))
# }


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

    params(sim)[["mpbRedTopSpread"]][["p_advectionDir"]] <- params[1]
    params(sim)[["mpbRedTopSpread"]][["p_advectionMag"]] <- params[2]
    # params(sim)[["mpbRedTopSpread"]][["bgSettlingProp"]] <- params[3]
    params(sim)[["mpbRedTopSpread"]][["p_meanDist"]] <- params[4]

    simOut <- spades(sim, .plotInitialTime = NA, debug = FALSE)

    ## TODO: below currently does total attack area. more refined criteria needed
    ## e.g., area and shape metrics from `landscapemetrics` package?

    ## simulated attack area
    atks <- simOut$massAttacksDT
    nPix <- atks[ATKTREES > 0, .N]
    atkAreaSim <- nPix * prod(res(simOut$rasterToMatch)) / (100^2) ## area in ha

    ## attacked area from data
    atksRas <- simOut$massAttacksStack[[paste0("X", timesFit$end)]]
    atks <- data.table(ID = 1L:ncell(atksRas), ATKTREES = atksRas[])
    nPix <- atks[ATKTREES > 0, .N] ## total number of pixels
    atkAreaData <- nPix * prod(res(simOut$rasterToMatch)) / (100^2) ## area in ha

    ## sum negative log likelihood for attacked pixels


    ## TODO: something other than simple sum of squares?
    metric <- (atkAreaData - atkAreaSim)^2 #+ (SNLL / 10^3)
    return(metric)
  }

  params4POM <- data.frame(
    name = c("p_advectionDir", "p_advectionMag", #"bgSettlingProp",
             "p_meanDist"),
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
  paramNames <- c("p_advectionDir", "p_advectionMag",
                  #"bgSettlingProp",
                  "p_meanDist")

  bestValues <- as.data.frame(outPOM[["member"]][["bestmemit"]])
  colnames(bestValues) <- paramNames

  bestFitVals <- as.data.frame(t(outPOM[["optim"]][["bestmem"]]))
  colnames(bestFitVals) <- paramNames

  plot(bestValues$p_advectionDir)
  points(bestFitVals$p_advectionDir, col = "red", pch = 19)

  plot(bestValues$p_advectionMag)
  points(bestFitVals$p_advectionMag, col = "red", pch = 19)

  #plot(bestValues$bgSettlingProp)
  #points(bestFitVals$bgSettlingProp, col = "red", pch = 19)

  plot(bestValues$p_meanDist)
  points(bestFitVals$p_meanDist, col = "red", pch = 19)
}
