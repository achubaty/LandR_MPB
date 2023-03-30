source("05-google-ids.R")

gid_mpbfit <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "mpbSpreadFit", gid]
upload_mpbfit <- config$args[["reupload"]] | length(gid_mpbfit) == 0

## pre-simulation preparation
runNameMPB <- if ("fit" %in% config$context[["mode"]]) "fit" else "forecast" ## TODO: allow run standalone; allow backcast etc.

climateMapRandomize <- NULL
mpbFitTimes <- list(start = 2010, end = 2020)

if (runNameMPB %in% c("fit", "runOnce")) {
  type <- "DEoptim"
}
if (runNameMPB %in% c("validate", "parameter")) {
  type <- "validate"
}
if (runNameMPB %in% "backcast") {
  mpbFitTimes <- list(start = 2010, end = 2020)
  type <- "predict"
}
if (runNameMPB %in% "runOnce") {
  type <- "runOnce"
}
if (runNameMPB %in% "forecast") {
  mpbFitTimes <- list(start = 2020, end = 2030)
  type <- "predict"
  climateMapRandomize = TRUE
}

mpbFitParams <- list(
  mpbClimateData = config$params[["mpbClimateData"]],
  mpbMassAttacksData = config$params[["mpbMassAttacksData"]],
  ## mpbPine was run with Biomass_borealDataPrep etc. earlier
  mpbRedTopSpread = config$params[["mpbRedTopSpread"]]
)

mpbFitObjects <- list(
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaFit = simOutPreamble[["studyAreaFit"]], ## TODO: pass this explicitly as studyArea
  absk = simOutPreamble[["absk"]],
  climateMapRandomize = climateMapRandomize
)

mpbFitModules <- list(
  "mpbClimateData",
  "mpbMassAttacksData",
  "mpbRedTopSpread"
)

if (type %in% c("DEoptim", "optim", "runOnce")) {
  message(crayon::green("RUNNING ", type))
  MPBfit <- Cache(
    simInitAndSpades,
    times = mpbFitTimes,
    params = mpbFitParams,
    modules = modules3,
    objects = objects3,
    loadOrder = unlist(modules3),
    .cacheExtra = moduleCodeFiles(paths3, modules3),
    #useCloud = TRUE,
    userTags = c("mpbSpreadFit")
    #cloudFolderID = cloudCacheFolderID # Eliot's Gdrive: Hosted/BioSIM/ folder
    # events = "init"
    # useCache = "overwrite"
  )
} else {
  #########################################################################################
  ## for forecasting -- use a specific fit_mpbSpreadOptimizer object
  DEoutFileList <- dir(dirname(paths3$outputPath), pattern = "DEout", full.names = TRUE)
  fit_mpbSpreadOptimizer <- readRDS(DEoutFileList[[24]]) ## TODO: don't have '24' -- Eliot's best
  objects3 <- append(objects3,
                     list(fit_mpbSpreadOptimizer = fit_mpbSpreadOptimizer))
  mpbFitParams$mpbRedTopSpread$dispersalKernel <- "Weibull3"
  outputs <- setDF(rbindlist(list(
    data.frame(objectName = "fit_mpbSpreadOptimizer", saveTime = mpbFitTimes$end),
    data.table("ROCList", saveTime = mpbFitTimes$end)
  ), use.names = FALSE))
  # mpbFitParams$mpbRedTopSpread$type <- "validate"

  if (runNameMPB == "validate") {
    message(crayon::green("RUNNING ", mpbFitParams$mpbRedTopSpread$type))
    MPBpredict <- Cache(
      simInitAndSpades,
      times = mpbFitTimes,
      params = mpbFitParams,
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
  if (runNameMPB == "parameter") {
    message(crayon::green("RUNNING", runNameMPB))
    todo <- data.table(tpps = 1:4/100)
    sleeps <- seq_len(NROW(todo))
    # browser()
    outParams <- lapply(X = todo$tpps, # sleep = sleeps,
                        # mc.cores = pmin(4, length(sleeps)),
                        mpbFitParams = mpbFitParams,
                        function(X, mpbFitParams) {#}, sleep) {
                          # Sys.sleep(sleep)
                          mpbFitParams$mpbRedTopSpread$thresholdPineProportion <- X
                          MPBpredict <- Cache(
                            simInitAndSpades,
                            times = mpbFitTimes,
                            params = mpbFitParams,
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
  if (runNameMPB == "backcast" && mpbFitTimes$start < 2020) {
    mpbFitParams$mpbRedTopSpread$type <- "predict"
    mpbFitParams$mpbRedTopSpread$coresForPrediction <- 23
    message(crayon::green("Running Rolling Forecasting"))
    todo <- rbindlist(lapply(10:1, function(x) data.table(rollingWindow = x, sy = 2010:(2020 - x))))
    todo[, runNameMPB := paste0("RW", rollingWindow, "_", "X", sy)]
    rasterOptions(maxmemory = 6e10)
    MPBpredictRolling <- Map(runNameMPB = todo$runNameMPB, rollingWindow = todo$rollingWindow, sy = todo$sy,
                             #  mc.cores = 23, mc.preschedule = FALSE,
                             function(runNameMPB, rollingWindow, sy) {
                               times <- list(start = sy, end = sy + rollingWindow)
                               syNam <- paste0("X", sy)
                               message(crayon::green("RUNNING ", mpbFitParams$mpbRedTopSpread$type, " on ", syNam))
                               opts <- options(spades.recoveryMode = FALSE)
                               rasterOptions(maxmemory = 6e10)
                               on.exit(try(options(opts), silent = TRUE))
                               simInitForRolling <- try(
                                 Cache(simInit,
                                       times = times,
                                       params = mpbFitParams,
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
                                 saveSimList(out, filename = file.path(outputPath(out), paste0("mpbRollingSim_", runNameMPB, ".qs")))
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
    Plots(fn = forecastHorizonFn, todo = todo, type = mpbFitParams$.globals$.plots,
          filename = file.path(paths3$outputPath, "figures", paste0("Forecast Horizon")))
  }


  if (runNameMPB %in% "forecast") {
    ##################################################################
    # FORECASTING setup
    ##################################################################

    mpbFitParams$mpbRedTopSpread$type <- "predict"
    mpbFitParams$mpbClimateData$.useCache <- ".inputObjects"
    mpbFitParams$mpbRedTopSpread$coresForPrediction <- 10
    message(crayon::green("Forecasting with climate variation"))

    out <- simInit(##AndExperiment2,
      times = mpbFitTimes,
      params = mpbFitParams,
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

    Plots(dt = out4, fn = plot_smoothPoint, type = mpbFitParams$.globals$.plots,
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
    startToEnd <- paste0(mpbFitTimes$start, " to ", mpbFitTimes$end, "_", Sys.time())

    gtsave(tabWithCI,
           filename = file.path(outputPath(sim), "figures",
                                paste0("forecast: displacementTable ", startToEnd, ".png")))

    # UPLOAD FILES
    if (isTRUE(upload_mpbfit)) {
      Require::Require("googledrive")
      local_files <- dir(file.path(paths3$outputPath, "figures"), full.names = TRUE)
      local_files <- dir(file.path(paths3$outputPath, "figures"), pattern =  "forecast:.+displacementTable.+2020.+2030.+06-30", full.names = TRUE)

      driveFolder <- gid_mpbfit ## TODO: was `as_id("1L2lNlsNa6DFaC9fUFwz6MyZw3mKSWQzd")` for standalone MPB
      files <- lapply(local_files, function(fn) drive_upload(fn, path = driveFolder, overwrite = TRUE))
    }
  }
}
