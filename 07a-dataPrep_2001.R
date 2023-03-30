source("05-google-ids.R")

gid_biomassMaps2001 <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "biomassMaps2001", gid]
upload_biomassMaps2001 <- config$args[["reupload"]] | length(gid_biomassMaps2001) == 0

year <- 2001

dataPrepModules2001 <- list(
  "Biomass_speciesData",
  "Biomass_speciesFactorial",
  "Biomass_borealDataPrep",
  "Biomass_speciesParameters"
) ## TODO: use config$modules

## TODO: why isn't updating .globals sufficient to update indiv mmodule values???
config$params <- list(
  .globals = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesData = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesFactorial = list(
    .plotInitialTime = year
  ),
  Biomass_borealDataPrep = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesParameters = list(
    .plotInitialTime = year
  )
)

dataPrepParams2001 <- list(
  .globals = config$params[[".globals"]],
  Biomass_speciesData = config$params[["Biomass_speciesData"]],
  Biomass_speciesFactorial = config$params[["Biomass_speciesFactorial"]],
  Biomass_borealDataPrep = config$params[["Biomass_borealDataPrep"]],
  Biomass_speciesParameters = config$params[["Biomass_speciesParameters"]]
)

dataPrepObjects <- list(
  .runName = config$context[["runName"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  rstLCC = simOutPreamble[["LCC"]],
  standAgeMap = simOutPreamble[["standAgeMap2001"]],
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

dataPrepOutputs2001 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(config$context[["studyAreaName"]], "_",
                c("cohortData2001_fireSense.rds",
                  "pixelGroupMap2001_fireSense.rds",
                  "speciesLayers2001_fireSense.rds",
                  "standAgeMap2001_borealDataPrep.rds",
                  "rawBiomassMap2001_borealDataPrep.rds"))
)

fbiomassMaps2001 <- simFile(paste0("biomassMaps2001_", config$context[["studyAreaName"]]), config$paths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_biomassMaps2001)) {
  if (!file.exists(fbiomassMaps2001)) {
    googledrive::drive_download(file = as_id(gid_biomassMaps2001), path = fbiomassMaps2001)
  }
  biomassMaps2001 <- loadSimList(fbiomassMaps2001)

  ## TODO: fix these upstream
  biomassMaps2001[["sufficientLight"]] <- as.data.frame(biomassMaps2001[["sufficientLight"]])
} else {
  biomassMaps2001 <- Cache(
    simInitAndSpades,
    times = list(start = year, end = year),
    params = dataPrepParams2001,
    modules = dataPrepModules2001,
    objects = dataPrepObjects,
    loadOrder = unlist(dataPrepModules2001),
    # outputs = dataPrepOutputs2001,
    .plots = NA,
    useCloud = FALSE, #config$args[["cloud"]][["useCloud"]],
    cloudFolderID = NULL,#config$args[["cloud"]][["cacheDir"]],
    userTags = c("dataPrep2001", config$context[["studyAreaName"]])
  )

  if (isUpdated(biomassMaps2001)) {
    biomassMaps2001@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(biomassMaps2001, fbiomassMaps2001,
                fileBackend = ifelse(isTRUE(config$args[["reupload"]]), 2, 0))
  }

  if (isTRUE(upload_biomassMaps2001)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fbiomassMaps2001, path = as_id(gdriveURL), name = basename(fbiomassMaps2001))
    gid_biomassMaps2001 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "biomassMaps2001", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2001),
      gdriveSims
    )
  }
}

## restore original studyAreaName
config$params[[".globals"]][[".studyAreaName"]] <- config$context[["studyAreaName"]]
config$update()

## PLOTTING
if ("screen" %in% config$params[[".globals"]][[".plots"]]) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers2001[["speciesLayers"]])
}
