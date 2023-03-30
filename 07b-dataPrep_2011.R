source("05-google-ids.R")

gid_biomassMaps2011 <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "biomassMaps2011", gid]
upload_biomassMaps2011 <- config$args[["reupload"]] | length(gid_biomassMaps2011) == 0

year <- 2011

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

dataPrepParams2011 <- dataPrepParams2001

## begin param updates
dataPrepParams2011[[".globals"]][["dataYear"]] <- year
dataPrepParams2011[[".globals"]][[".plotInitialTime"]] <- year
dataPrepParams2011[[".globals"]][[".studyAreaName"]] <- paste0(config$context[["studyAreaName"]], "_", year)

dataPrepParams2011[["Biomass_speciesData"]][["dataYear"]] <- year
dataPrepParams2011[["Biomass_speciesData"]][[".plotInitialTime"]] <- year
dataPrepParams2011[["Biomass_speciesData"]][["types"]] <- "KNN" ## TODO: is this correct? what year for ONFRI?
dataPrepParams2011[["Biomass_speciesData"]][[".studyAreaName"]] <- paste0(config$context[["studyAreaName"]], "_", year)

dataPrepParams2011[["Biomass_speciesFactorial"]][[".plotInitialTime"]] <- year

dataPrepParams2011[["Biomass_borealDataPrep"]][["dataYear"]] <- year
dataPrepParams2011[["Biomass_borealDataPrep"]][[".plotInitialTime"]] <- year
dataPrepParams2011[["Biomass_borealDataPrep"]][[".studyAreaName"]] <- paste0(config$context[["studyAreaName"]], "_", year)

dataPrepParams2011[["Biomass_speciesParameters"]][[".plotInitialTime"]] <- year
## end pram updates

dataPrepObjects[["standAgeMap"]] <- simOutPreamble[["standAgeMap2011"]]

dataPrepOutputs2011 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(config$context[["studyAreaName"]], "_",
                c("cohortData2011_fireSense.rds",
                  "pixelGroupMap2011_fireSense.rds",
                  "speciesLayers2011_fireSense.rds",
                  "standAgeMap2011_borealDataPrep.rds",
                  "rawBiomassMap2011_borealDataPrep.rds"))
)

fbiomassMaps2011 <- simFile(paste0("biomassMaps2011_", config$context[["studyAreaName"]]), config$paths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_biomassMaps2011)) {
  if (!file.exists(fbiomassMaps2011)) {
    googledrive::drive_download(file = as_id(gid_biomassMaps2011), path = fbiomassMaps2011)
  }
  biomassMaps2011 <- loadSimList(fbiomassMaps2011)

  ## TODO: fix these upstream
  biomassMaps2011[["sufficientLight"]] <- as.data.frame(biomassMaps2011[["sufficientLight"]])
} else {
  biomassMaps2011 <- Cache(
    simInitAndSpades,
    times = list(start = year, end = year),
    params = dataPrepParams2011,
    modules = dataPrepModules,
    objects = dataPrepObjects,
    loadOrder = unlist(dataPrepModules),
    #outputs = dataPrepOutputs2011,
    useCloud = FALSE, #config$args[["cloud"]][["useCloud"]],
    cloudFolderID = NULL,#config$args[["cloud"]][["cacheDir"]],
    userTags = c("dataPrep2011", config$context[["studyAreaName"]])
  )

  if (isUpdated(biomassMaps2011)) {
    biomassMaps2011@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(biomassMaps2011, fbiomassMaps2011,
                fileBackend = ifelse(isTRUE(config$args[["reupload"]]), 2, 0))
  }

  if (isTRUE(upload_biomassMaps2011)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fbiomassMaps2011, path = as_id(gdriveURL), name = basename(fbiomassMaps2011))
    gid_biomassMaps2011 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "biomassMaps2011", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2011),
      gdriveSims
    )
  }
}

rm(dataPrepObjects, dataPrepOutputs2001, dataPrepParams2001, dataPrepOutputs2011, dataPrepParams2011)

## restore original studyAreaName
config$params[[".globals"]][[".studyAreaName"]] <- config$context[["studyAreaName"]]
config$update()
