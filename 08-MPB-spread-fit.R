## pre-simulation preparation

do.call(SpaDES.core::setPaths, paths3)

runName <- if (Require:::isWindows() || amc::isRstudio()) "fit" else "backcast" # "predict" "validate" "runOnce"#  "optim" "nofit" "fit"
climateMapRandomize = NULL
times <- list(start = 2010, end = 2020) ## 2010-2016

if (runName %in% c("fit", "runOnce")) {
  type <- "DEoptim"
}
if (runName %in% c("validate", "parameter")) {
  type <- "validate"
}
if (runName %in% "backcast") {
  times <- list(start = 2010, end = 2020) ## 2010-2016
  type <- "predict"
}
if (runName %in% "runOnce") {
  type <- "runOnce"
}
if (runName %in% "forecast") {
  times <- list(start = 2020, end = 2030) ## 2010-2016
  type <- "predict"
  climateMapRandomize = TRUE
}


paramsFit <- list(
  .globals = list(.plots = "png"),
  mpbClimateData = list(
    suitabilityIndex = "R",    ## Can be "G", "S", "L", "R"
    .maxMemory = maxMemory,
    .useCache = "init"
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
    dispersalKernel = "Weibull3",# "Generalized Gamma", # Weibull3
    # .plots = "screen",
    type = type
  )
)


objects3 <- list(
  studyArea = simOutPreamble$studyArea,
  studyAreaFit = simOutPreamble$studyAreaFit, ## TODO: pass this explicitly as studyArea
  absk = simOutPreamble$absk,
  climateMapRandomize = climateMapRandomize
)

modules3 <- list(
  "mpbClimateData", "mpbPine",
  "mpbMassAttacksData",
  "mpbRedTopSpread"#,
  #"mpbManagement"
)



if (type %in% c("DEoptim", "optim", "runOnce")) {
  message(crayon::green("RUNNING ", type))
  MPBfit <- Cache(
    simInitAndSpades,
    times = times,
    params = paramsFit,
    modules = modules3,
    objects = objects3,
    loadOrder = unlist(modules3),
    .cacheExtra = moduleCodeFiles(paths3, modules3),
    useCloud = TRUE, userTags = c("MPB 08 Fit")
    cloudFolderID = "175NUHoqppuXc2gIHZh5kznFi6tsigcOX" # Eliot's Gdrive: Hosted/BioSIM/ folder
    # events = "init"
    # useCache = "overwrite"
  )
} else {

  #########################################################################################
  # For Forecasting -- use a specific fit_mpbSpreadOptimizer object
  DEoutFileList <- dir(dirname(paths3$outputPath), pattern = "DEout", full.names = TRUE)
  fit_mpbSpreadOptimizer <- readRDS(DEoutFileList[[24]]) # 24 is currently best of my files (Eliot)
  objects3 <- append(objects3,
                     list(fit_mpbSpreadOptimizer = fit_mpbSpreadOptimizer))
  paramsFit$mpbRedTopSpread$dispersalKernel <- "Weibull3"
  outputs <- setDF(rbindlist(list(
    data.frame(objectName = "fit_mpbSpreadOptimizer", saveTime = times$end),
    data.table("ROCList", saveTime = times$end)
  ), use.names = FALSE))
  # paramsFit$mpbRedTopSpread$type <- "validate"

  if (runName == "validate") {
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


  RNGkind("L'Ecuyer-CMRG")
  ## Test a parameter varying
  if (runName == "parameter") {
    message(crayon::green("RUNNING", runName))
    todo <- data.table(tpps = 1:4/100)
    sleeps <- seq_len(NROW(todo))
    # browser()
    outParams <- lapply(X = todo$tpps, # sleep = sleeps,
                        # mc.cores = pmin(4, length(sleeps)),
                        paramsFit = paramsFit,
                        function(X, paramsFit) {#}, sleep) {
                          # Sys.sleep(sleep)
                          paramsFit$mpbRedTopSpread$thresholdPineProportion <- X
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

                        })
    set(todo, NULL, "ROC", sapply(outParams, function(sim) mean(sim$ROCList[[1]]$ROC)))
  }



  # ROLLING BACKCASTING WINDOWS
  if (runName == "backcast" && times$start < 2020) {
    paramsFit$mpbRedTopSpread$type <- "predict"
    paramsFit$mpbRedTopSpread$coresForPrediction <- 23
    message(crayon::green("Running Rolling Forecasting"))
    todo <- rbindlist(lapply(10:1, function(x) data.table(rollingWindow = x, sy = 2010:(2020-x))))
    todo[, runName := paste0("RW", rollingWindow, "_", "X", sy)]
    # todo <- todo[25:55,]
    rasterOptions(maxmemory = 6e10)
    # showCache("cd35c61c23c7c92a") # first one with 10 year window
    # Require(unique(unlist(packages(paths = paths3$modulePath, modules = unlist(modules3)))))
    MPBpredictRolling <- Map(runName = todo$runName, rollingWindow = todo$rollingWindow, sy = todo$sy,
                             #  mc.cores = 23, mc.preschedule = FALSE,
                             function(runName, rollingWindow, sy) {
                               times <- list(start = sy, end = sy + rollingWindow)
                               syNam <- paste0("X", sy)
                               message(crayon::green("RUNNING ", paramsFit$mpbRedTopSpread$type, " on ", syNam))
                               opts <- options(spades.recoveryMode = FALSE)
                               rasterOptions(maxmemory = 6e10)
                               on.exit(try(options(opts), silent = TRUE))
                               simInitForRolling <- try(
                                 Cache(simInit,
                                       times = times,
                                       params = paramsFit,
                                       modules = modules3,
                                       objects = objects3,
                                       outputs = outputs,
                                       loadOrder = unlist(modules3)#,
                                       # .cacheExtra = moduleCodeFiles(paths3, modules3)
                                 ))
                               out <- try(Cache(spades, simInitForRolling))
                               if (is(out, "try-error")) {
                                 message(crayon::red(paste0("failed on '", syNam, "' on pid: ", Sys.getpid())))
                               } else {
                                 options(opts)
                                 rm(climateSuitabilityMaps, windSpeedStack, windDirStack, pineDT, envir = envir(out))
                                 saveSimList(out, filename = file.path(outputPath(out), paste0("mpbRollingSim_", runName, ".qs")))
                               }
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


  if (runName %in% "forecast") {
    ##################################################################
    # FORECASTING setup
    ##################################################################

    paramsFit$mpbRedTopSpread$type <- "predict"
    paramsFit$mpbClimateData$.useCache <- ".inputObjects"
    paramsFit$mpbRedTopSpread$coresForPrediction <- 10
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


    ##################################################################
    # FORECASTING with CLIMATE UNCERTAINTY
    ##################################################################
    nreps <- 10
    cpus <- 3
    opts <- options(spades.recoveryMode = FALSE)
    out2 <- lapply(seq(nreps), function(rep) spades(Copy(out)))
    options(opts)
    # out2 <- mclapply(seq(nreps), function(rep) spades(Copy(out)), mc.cores = min(cpus, nreps))
    out3 <- lapply(out2, function(sim) sim$predictedDT[, list(meanATKTREES = mean(ATKTREES, na.rm = TRUE)), by = "layerName"])
    out4 <- rbindlist(out3, idcol = "rep")

    # Clean up some large objects
    # lapply(out2, function(sim) rm(climateSuitabilityMaps, windSpeedStack, windDirStack, pineDT, envir = envir(sim)))


    lapply(seq_along(out2), function(ind)
      saveSimList(out2[[ind]], file.path(paths3$outputPath, paste0("Replicate Forecast Sims_",
                                                                   paddedFloatToChar(ind, 3), ".qs"))))

    # Read them back in
    fls <- dir(file.path("outputs", "run01"), pattern = "Replicate.+\\.qs", full.names = TRUE)
    names(fls) <- basename(fls)
    sims <- lapply(fls, loadSimList)



    binaries <- binaryStacks(sims, stackName = "predictedStack", propPineRasName = "propPineRas", thresholdAttackTreesMinDetectable = 4)
    pa <- probAttack(binaries)

    pred <- pa[[tail(names(pa), 1)]]
    pred[pred[] == 0] <- NA
    propPineRas <- sims[[1]]$propPineRas
    propPineRas[propPineRas[] == 0] <- NA


    viewMode <- suppressMessages(identical(tmap_mode(), "view"))
    tmap_style("white")
    t1 <- tm_shape(sims[[1]]$absk) + tm_polygons(alpha = 0) + tm_graticules(alpha = 0.2) #tm_grid(projection = st_crs(4326), alpha = 0.15)
    t1 <- t1 + tm_shape(sims[[1]]$studyArea) + tm_polygons(alpha = 0, border.col = "lightblue")
    if (viewMode)
      t1 <- tm_basemap("OpenStreetMap") + t1 # only with tmap_mode("view")
    #t5 <- tm_tiles("OpenStreetMap")
    brks <- 0:5/5
    # t2 <- tm_shape(mam) + tm_raster(title = "Accumulated Damage", alpha = 0.8, palette = "YlOrRd",
    #                                 style = "fixed", breaks = brks, legend.show = FALSE)
    t3 <- tm_shape(propPineRas) + tm_raster(title = "Pine Cover (prop)", alpha = 0.6, palette = "Greens")
    # t3b <- t3 + tm_legend(show = FALSE)
    t4 <- tm_shape(pred) + tm_raster(title = "Prob. mass attack", alpha = 0.8, palette = "YlOrRd", style = "fixed", breaks = brks)
    tpred <- t3 + t4 + t1 + tm_legend(show = TRUE, position = c("right", "top"))
    tpred <- tpred + tm_layout(title = "Forecasted probability of mass attack by 2030")
    tpred

    fn1 <- file.path(paths3$out, "figures", paste0("Forecasted risk maps to 2030.png"))
    suppressWarnings(
      Cache(tmap_save, tpred, filename = fn1,
            width = 8, height = 5, units = "in", dpi = 300, omitArgs = "filename")
    )
    local_files <- fn1

    plot_smoothPoint <- function(dt)  {
      dtSumm <- dt[, list(ci = sd(meanATKTREES)/sqrt(.N) * 1.96,
                          meanATKTREES = mean(meanATKTREES)), by = "layerName"]
      dtSumm[, `:=`(lowCI = meanATKTREES - ci,
                    hiCI = meanATKTREES + ci)]
      brks <- unique(as.integer(gsub("X", "", dt$layerName)))
      ggplot(dtSumm, aes(y = meanATKTREES, x = as.integer(gsub("X", "", layerName)))) +
        geom_point() +
        geom_smooth(
          aes(ymin = lowCI, ymax = hiCI),
          stat = "identity") +
        scale_x_continuous(breaks = brks) +
        geom_point(data = dt) +
        scale_y_continuous(trans = "log10") +
        theme_bw() +
        xlab("Year") +
        ylab("Forecasted Mean Attack Density (per km2 of attacked pixels)")
    }

    Plots(dt = out4, fn = plot_smoothPoint, type = paramsFit$.globals$.plots,
          filename = file.path(paths3$outputPath, "figures", paste0("Forecast Abundance")))


    ########## DISPLACEMENT TABLES

    dispTables <- lapply(sims, function(sim) {
      startToEnd <- paste0(start(sim), " to ", end(sim), "_", Sys.time())
      centroidPredicted <- centroidChange(sim$predictedStack, sim$propPineRas)
      centroidPredicted[, variable := paste0("X", as.numeric(gsub("X", "", variable)) - 1)]
      setnames(centroidPredicted, old = c("yAtMax", "xAtMax"), new = c("yPredicted", "xPredicted"))

      centroidData <- centroidChange(sim$massAttacksStack, sim$propPineRas)
      setnames(centroidData, old = c("yAtMax", "xAtMax"), new = c("yData", "xData"))
      centroids <- centroidData[centroidPredicted]

      setnames(centroids, "variable", "layerName")
      centroidsLong <- melt(centroids, measure = patterns("Data|Pred"))
      centroidsLong[, type := gsub("x|y", "", variable)]
      centroidsLong[, xy := gsub("^(x|y).+", "\\1", variable)]
      centroidsLong[, xy := ifelse(xy == "x", "East", "North")]
      centroidsLong[, UTM := value]
      centroidsLong[, value := c(NA, diff(value))/1e3, by = c("variable")]

      # Cache(Plots, centroidsLong, plot_CentroidShift, title = "Predicted vs. Observed centroid displacment each year",
      #       ggsaveArgs = list(width = 8, height = 10, units = "in", dpi = 300),
      #       filename = paste0("forecast", ": Centroid Displacement Pred vs Observed ",
      #                         startToEnd), omitArgs = "filename")


      # Displacement Table
      centroids[, EastObs := c(NA, diff(xData)/1e3)]
      centroids[, NorthObs := c(NA, diff(yData)/1e3)]
      centroids[, EastPred := c(NA, diff(xPredicted)/1e3)]
      centroids[, NorthPred := c(NA, diff(yPredicted)/1e3)]
      centroids <- na.omit(centroids, cols = "EastPred")
      corEastDisplacement <- cor(centroids$EastObs, centroids$EastPred, method = "spearman")
      corNorthDisplacement <- cor(centroids$NorthObs, centroids$NorthPred, method = "spearman")
      sim$correlationsDisplacement <- c(East = corEastDisplacement, North = corNorthDisplacement)

      if (end(sim) - start(sim) >= 10) {
        centroids[, FiveYrPd := rep(c(paste0(layerName[1],"/",layerName[5]), paste0(layerName[6],"/",layerName[10])), each = 5)]

        centroids5Y <- centroids[, lapply(.SD, function(x) sum(x)), by = FiveYrPd,
                                 .SDcols = c("EastObs", "NorthObs", "EastPred", "NorthPred")]
        centroids <- rbindlist(list(centroids, centroids5Y), fill = TRUE)
        centroids[is.na(layerName), layerName := FiveYrPd]
      }
      centroids2 <- as.data.table(t(centroids[, list(NorthObs, NorthPred, EastObs, EastPred)]))
      setnames(centroids2, centroids$layerName)
      centroids2 <- cbind(layerName = c("NorthObs", "NorthPred", "EastObs", "EastPred"), centroids2)
      centroids2
    })

    dispTablesDT <- rbindlist(dispTables)
    dispTablesDT <- dispTablesDT[grep("Pred", layerName)]
    dispTableMn <- dispTablesDT[, append(list(type = "mean"), lapply(.SD, function(x) round(mean(x), 0))), by = "layerName"]
    dispTableLow <- dispTablesDT[, append(list(type = "low"), lapply(.SD, function(x) round(mean(x) - sd(x)/sqrt(.N), 0))), by = "layerName"]
    dispTableHigh <- dispTablesDT[, append(list(type = "high"), lapply(.SD, function(x) round(mean(x) + sd(x)/sqrt(.N), 0))), by = "layerName"]
    dispTabs <- rbindlist(list(dispTableMn, dispTableLow, dispTableHigh))
    dispT1 <- melt(dispTabs, id.vars = c("layerName", "type"))
    dispT2 <- dcast(dispT1, layerName + variable ~ type, value.var = "value")
    dispT2[, label := paste0(mean, " (", low, " - ", high, ")")]
    set(dispT2, NULL, c("high", "low", "mean"), NULL)
    dispT3 <- dcast(dispT2, layerName ~ variable)
    dispT3[, layerName := gsub("Pred", "", layerName)]

    library(gt)
    old <- grep("^X", colnames(dispT3), value = TRUE)
    setnames(dispT3, old = old, new = gsub("X", "", old))
    setnames(dispT3, old = "layerName", new = "Direction")
    tabWithCI <- dispT3 %>%
      gt() %>%
      tab_spanner(
        label = "Annual displacement (km/y)",
        columns = grep(value = TRUE, "^.{4,4}$", colnames(dispT3))
      ) %>%
      tab_spanner(
        label = "Net displacement (km/5 y)",
        columns = grep(value = TRUE, "^.+/.+", colnames(dispT3))
      )
    startToEnd <- paste0(times$start, " to ", times$end, "_", Sys.time())

    gtsave(tabWithCI,
           filename = file.path(outputPath(sim), "figures",
                                paste0("forecast: displacementTable ", startToEnd, ".png")))









    # UPLOAD FILES
    if (FALSE) {
      Require::Require("googledrive")
      local_files <- dir(file.path(paths3$outputPath, "figures"), full.names = TRUE)
      local_files <- dir(file.path(paths3$outputPath, "figures"), pattern =  "forecast:.+displacementTable.+2020.+2030.+06-30", full.names = TRUE)

      driveFolder <- as_id("1L2lNlsNa6DFaC9fUFwz6MyZw3mKSWQzd")
      files <- lapply(local_files, function(fn) drive_upload(fn, path = driveFolder, overwrite = TRUE))
    }
  }
}

