################################################################################
## main simulation
################################################################################

times <- config$args[["simYears"]]

dynamicModules <- list(
  "historicFires",
  "fireSense_dataPrepPredict",
  "fireSense",
  "fireSense_IgnitionPredict",
  "fireSense_EscapePredict",
  "fireSense_SpreadPredict",
  "Biomass_core",
  "Biomass_regeneration",
  "gmcsDataPrep"
)

dynamicParams <- list(
  .globals = config$params[[".globals"]],
  Biomass_core = config$params[["Biomass_core"]],
  Biomass_regeneration = config$params[["Biomass_regeneration"]],
  fireSense = config$params[["fireSense"]],
  fireSense_dataPrepPredict = config$params[["fireSense_dataPrepPredict"]],
  fireSense_EscapePredict = config$params[["fireSense_EscapePredict"]],
  fireSense_IgnitionPredict = config$params[["fireSense_IgnitionPredict"]],
  fireSense_SpreadPredict = config$params[["fireSense_SpreadPredict"]],
  gmcsDataPrep = config$params[["gmcsDataPrep"]],
  historicFires = config$params[["historicFires"]],
  mpbClimateData = config$params[["mpbClimateData"]],
  mpbMassAttacksData = config$params[["mpbMassAttacksData"]],
  ## mpbPine was run with Biomass_borealDataPrep etc. earlier
  mpbRedTopSpread = config$params[["mpbRedTopSpread"]]
)

dynamicParams[[".globals"]][["pineSpToUse"]] <- c("Pinu_con" , "Pinu_ban")
dynamicParams[[".globals"]][["sppEquivCol"]] <- simOutPreamble[["sppEquivCol"]]
dynamicParams[[".globals"]][["stemsPerHaAvg"]] <- 1125 ## TODO: get from params list in config
dynamicParams[["fireSense_dataPrepPredict"]][["missingLCCgroup"]] <- simOutPreamble[["missingLCCgroup"]]

dynamicParams[["mpbRedTopSpread"]] <- append(as.list(apply(mpbFitParams$fit_mpbSpreadOptimizer$member$pop, 2, mean)),
                                list(type = c("predict"),
                                     coresForPrediction = 7,
                                     .useCache = "")) ## TODO: confirm these; set number of cores flexibly

## TODO: use simOutPreamble$sppEquiv to get sppNameVector
species <- Cache(LandR::speciesInStudyArea, biomassMaps2011[["studyArea"]])
sppNameVector <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
sppNameVector <- grep("Lari_Lya", sppNameVector, invert = TRUE, value = TRUE)

dynamicObjects <- list(
  .runName = config$context[["runName"]],
  ATAstack = simOutPreamble[["ATAstack"]],
  biomassMap = biomassMaps2011$biomassMap,
  climateComponentsTouse = fSsimDataPrep[["climateComponentsToUse"]],
  CMInormal = simOutPreamble[["CMInormal"]],
  CMIstack = simOutPreamble[["CMIstack"]],
  cohortData = fSsimDataPrep[["cohortData2011"]],
  columnsForPixelGroups = c("ecoregionGroup", "speciesCode", "age", "B"),
  covMinMax_spread = spreadOut[["covMinMax_spread"]],
  covMinMax_ignition = ignitionOut[["covMinMax_ignition"]],
  ecoregion = biomassMaps2011[["ecoregion"]],
  ecoregionMap = biomassMaps2011[["ecoregionMap"]],
  flammableRTM = fSsimDataPrep[["flammableRTM"]],
  fireSense_IgnitionFitted = ignitionOut[["fireSense_IgnitionFitted"]],
  fireSense_EscapeFitted = escapeOut[["fireSense_EscapeFitted"]],
  fireSense_SpreadFitted = spreadOut[["fireSense_SpreadFitted"]],
  landcoverDT = fSsimDataPrep[["landcoverDT"]],
  nonForest_timeSinceDisturbance = fSsimDataPrep[["nonForest_timeSinceDisturbance2011"]],
  minRelativeB = biomassMaps2011[["minRelativeB"]],
  PCAveg = fSsimDataPrep[["PCAveg"]],
  pixelGroupMap = biomassMaps2011[["pixelGroupMap"]],
  #pixelGroupMap = fSsimDataPrep[["pixelGroupMap2011"]], ## TODO: why is this slightly off?
  projectedClimateLayers = simOutPreamble[["projectedClimateRasters"]],
  rasterToMatch = biomassMaps2011[["rasterToMatch"]],
  rasterToMatchLarge = biomassMaps2011[["rasterToMatchLarge"]],
  rescaleFactor = 1 / fSsimDataPrep@params$fireSense_dataPrepFit$igAggFactor^2,
  species = biomassMaps2011[["species"]],
  speciesEcoregion = biomassMaps2011[["speciesEcoregion"]],
  speciesLayers = biomassMaps2011[["speciesLayers"]], ## TODO: does Biomass_core actually need this?
  sppColorVect = biomassMaps2011[["sppColorVect"]],
  sppNameVector = sppNameVector,
  sppEquiv = fSsimDataPrep[["sppEquiv"]],
  standAgeMap = biomassMaps2011[["standAgeMap"]],
  studyArea = biomassMaps2011[["studyArea"]],
  studyAreaLarge = biomassMaps2011[["studyAreaLarge"]],
  studyAreaPSP = simOutPreamble[["studyAreaPSP"]],
  studyAreaReporting = biomassMaps2011[["studyAreaReporting"]],
  sufficientLight = biomassMaps2011[["sufficientLight"]],
  terrainDT = fSsimDataPrep[["terrainDT"]],
  vegComponentsToUse = fSsimDataPrep[["vegComponentsToUse"]]
)

rastersToSaveAnnually <- c(
  "ANPPMap",
  "burnMap",
  "fireSense_EscapePredicted",
  "fireSense_IgnitionPredicted",
  "fireSense_SpreadPredicted",
  "mortalityMap",
  "pixelGroupMap",
  "rstCurrentBurn",
  "simulatedBiomassMap"
)

annualRasters <- data.frame(
  expand.grid(
    objectName = rastersToSaveAnnually,
    saveTime = seq(times$start, times$end, 1),
    fun = "writeRaster",
    package = "raster"
  ),
  stringsAsFactors = FALSE
)
annualRasters$file <- paste0(annualRasters$objectName, "_", annualRasters$saveTime, ".tif")
annualRasters$arguments <- I(list(list(overwrite = TRUE, progress = FALSE)))

objectsToSaveAnnually <- c(
  "cohortData" ## data.table
)

annualObjects <- data.frame(
  expand.grid(
    objectName = objectsToSaveAnnually,
    saveTime = seq(times$start, times$end, 1),
    fun = "qsave",
    package = "qs"
  ),
  stringsAsFactors = FALSE
)
annualObjects$file <- paste0(annualObjects$objectName, "_", annualObjects$saveTime, ".qs")
annualObjects$arguments <- I(list(list()))

objectNamesToSaveAtEnd <- c(
  "gcsModel",
  "burnSummary",
  "mcsModel",
  "species",
  "speciesEcoregion",
  "simulationOutput"
)

finalYearOutputs <- data.frame(
  objectName = objectNamesToSaveAtEnd,
  saveTime = times$end,
  fun = "qsave",
  package = "qs",
  file = paste0(objectNamesToSaveAtEnd, ".qs"),
  stringsAsFactors = FALSE
)
finalYearOutputs$arguments <- I(list(list()))

dynamicOutputs <- rbind(annualRasters, annualObjects, finalYearOutputs)

## delete unused objects, including previous simLists to free up memory
rm(
  preambleObjects, simOutPreamble,   ## 06-studyArea.R
  biomassMaps2001,                   ## 07a-dataPrep_2001.R
  biomassMaps2011,                   ## 07b-dataPrep_2011.R
  fSdataPrepObjects, fSsimDataPrep,  ## 07c-dataPrep_fS.R
  ignitionFitObjects, ignitionOut,   ## 08a-ignitionFit.R
  escapeFitObjects, escapeOut,       ## 08b-escapeFit.R
  spreadFitObjects, spreadOut        ## 08c-spreadFit.R
)

fsim <- simFile(config$context[["runName"]], config$paths[["outputPath"]], ext = "qs")

tryCatch({
  mainSim <- simInitAndSpades(
    times = times,
    modules = dynamicModules,
    objects = dynamicObjects,
    outputs = dynamicOutputs,
    params = dynamicParams,
    loadOrder = unlist(dynamicModules),
    debug = list(file = list(file = file.path(config$paths[["logPath"]], "sim.log"),
                             append = TRUE), debug = 1)
  )

  capture.output(warnings(), file = file.path(config$paths[["logPath"]], "warnings.txt"), split = TRUE)
}, error = function(e) {
  capture.output(traceback(), file = file.path(config$paths[["logPath"]], "traceback_mainSim.txt"), split = TRUE)

  DBI::dbDisconnect(getOption("reproducible.conn"))

  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("ERROR in simulation `", config$context[["runName"]], "` on host `", .nodename, "`.\n",
             "```\n", e$message, "\n```"),
      channel = config$args[["notifications"]][["slackChannel"]], preformatted = FALSE
    )

    stop(e$message)
  }
})

if (isUpdated(mainSim)) {
  mainSim@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)

  message("Saving simulation to: ", fsim)
  saveSimList(sim = mainSim, filename = fsim, fileBackend = 2)

  # save simulation stats -----------------------------------------------------------------------

  elapsed <- elapsedTime(mainSim)
  data.table::fwrite(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.csv"))
  qs::qsave(elapsed, file.path(config$paths[["logPath"]], "elapsedTime_summaries.qs"))

  memory <- memoryUse(mainSim, max = TRUE)
  data.table::fwrite(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.csv"))
  qs::qsave(memory, file.path(config$paths[["logPath"]], "memoryUsed_summaries.qs"))

  # archive and upload --------------------------------------------------------------------------
  resultsDir <- config$paths[["outputPath"]]

  ## make annual standAgeMaps from cohortData
  # lapply(2011:2100, function(year) {
  #   cohortData <- qs::qread(file = file.path(resultsDir, paste0("cohortData_", year, "_year", year, ".qs")))
  #   cohortData[, bWeightedAge := floor(sum(age * B) / sum(B) / 10) * 10, .(pixelGroup)]
  #   cohortDataReduced <- cohortData[, c("pixelGroup", "bWeightedAge")]
  #   cohortDataReduced <- unique(cohortDataReduced)
  #   pixelGroupMap <- raster(file.path(resultsDir, paste0("pixelGroupMap_", year, "_year", year, ".tif")))
  #   names(pixelGroupMap) <- "pixelGroup"
  #   standAgeMap <- rasterizeReduced(cohortDataReduced, pixelGroupMap, "bWeightedAge", mapCode = "pixelGroup")
  #   writeRaster(standAgeMap, filename = file.path(resultsDir, paste0("standAgeMap_", year, ".tif")), overwrite = TRUE)
  #   TRUE
  # }) ## TODO: currently fails with error: "double free or corruption (!prev)"

  tarball <- paste0(resultsDir, ".tar.gz")
  #archive::archive_write_dir(archive = tarball, dir = resultsDir) ## doesn't work
  utils::tar(tarball, resultsDir, compression = "gzip") ## TODO: use archive pkg

  ## we will upload at the end to prevent timeouts from delaying subsequent sims
}

# end-of-sim notifications --------------------------------------------------------------------

SpaDES.project::notify_slack(config$context[["runName"]], config$args[["notifications"]][["slackChannel"]])

