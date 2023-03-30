source("05-google-ids.R")

gid_fSsimDataPrep <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "fSsimDataPrep", gid]
upload_fSsimDataPrep <- config$args[["reupload"]] | length(gid_fSsimDataPrep) == 0

## WildFire raster - for now we will supply this data as WWH gang has not made it publicly available
# wildfire2020 <- prepInputs(
#   url = "https://drive.google.com/file/d/1Vc4cOY1jOS1y8P20S14nYBJWwkRj_SPL/",
#   targetFile = "Fire_1985-2020_ROF.dat",
#   fun = "raster::raster",
#   rasterToMatch = simOutPreamble$rasterToMatch,
#   alsoExtract = c("Fire_1985-2020_ROF.dat.ovr",
#                   "Fire_1985-2020_ROF.dat.aux.xml",
#                   "Fire_1985-2020_ROF.dat.vat.cpg",
#                   "Fire_1985-2020_ROF.hdr"),
#   destinationPath = config$paths[["inputPath"]]
# )

# wildfire2020 <- raster::setMinMax(wildfire2020)

fSdataPrepParams <- list(
  fireSense_dataPrepFit = config$params[["fireSense_dataPrepFit"]]
)

fSdataPrepParams[["fireSense_dataPrepFit"]][["forestedLCC"]] <- simOutPreamble[["fireSenseForestedLCC"]]
fSdataPrepParams[["fireSense_dataPrepFit"]][["missingLCCgroup"]] <- simOutPreamble[["missingLCCGroup"]]
fSdataPrepParams[["fireSense_dataPrepFit"]][["nonflammableLCC"]] <- simOutPreamble[["nonflammableLCC"]]
fSdataPrepParams[["fireSense_dataPrepFit"]][["sppEquivCol"]] <- simOutPreamble[["sppEquivCol"]]

simOutPreamble[["rasterToMatch"]] <- raster::mask(simOutPreamble[["rasterToMatch"]], simOutPreamble[["studyArea"]])
standAgeMap2001 <- postProcess(biomassMaps2001[["standAgeMap"]], rasterToMatch = simOutPreamble[["rasterToMatch"]])
standAgeMap2011 <- postProcess(biomassMaps2011[["standAgeMap"]], rasterToMatch = simOutPreamble[["rasterToMatch"]])
rstLCC <- postProcess(biomassMaps2011[["rstLCC"]], rasterToMatch = simOutPreamble[["rasterToMatch"]])
rstLCC[] <- as.integer(rstLCC[])

fSdataPrepObjects <- list(
  .runName = config$context[["runName"]],
  cohortData2001 = biomassMaps2001[["cohortData"]],
  cohortData2011 = biomassMaps2011[["cohortData"]],
  # fireRaster = wildfire2020,
  nonForestedLCCGroups = simOutPreamble[["nonForestLCCGroups"]],
  historicalClimateRasters = simOutPreamble[["historicalClimateRasters"]],
  pixelGroupMap2001 = biomassMaps2001[["pixelGroupMap"]],
  pixelGroupMap2011 = biomassMaps2011[["pixelGroupMap"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rstLCC = rstLCC,
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap2001 = standAgeMap2001,
  standAgeMap2011 = standAgeMap2011,
  studyArea = simOutPreamble[["studyArea"]]
)

amc::.gc()

ffSsimDataPrep <- simFile(paste0("fSsimDataPrep_", config$context[["studyAreaName"]]), config$paths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]])) {
  if (!file.exists(ffSsimDataPrep)) {
    googledrive::drive_download(file = as_id(gid_fSsimDataPrep), path = ffSsimDataPrep)
  }
  fSsimDataPrep <- loadSimList(ffSsimDataPrep)
} else {
  fSsimDataPrep <- Cache(
    simInitAndSpades,
    times =  list(start = 2011, end = 2011),
    params = fSdataPrepParams,
    objects = fSdataPrepObjects,
    modules = "fireSense_dataPrepFit", ## TODO: use config$modules
    useCloud = FALSE, #config$args[["cloud"]][["useCloud"]],
    cloudFolderID = NULL, #config$args[["cloud"]][["cacheDir"]],
    userTags = c("fireSense_dataPrepFit", config$context[["studyAreaName"]])
  )

  if (isUpdated(fSsimDataPrep)) {
    fSsimDataPrep@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(fSsimDataPrep, ffSsimDataPrep,
                fileBackend = ifelse(isTRUE(config$args[["reupload"]]), 2, 0)
                )
  }
}

if (isTRUE(upload_fSsimDataPrep)) {
  source("05-google-ids.R")

  fdf <- googledrive::drive_put(media = ffSsimDataPrep, path = as_id(gdriveURL), name = basename(ffSsimDataPrep))
  gid_fSsimDataPrep <- as.character(fdf$id)
  rm(fdf)
  gdriveSims <- update_googleids(
    data.table(studyArea = config$context[["studyAreaName"]], simObject = "fSsimDataPrep",  runID = NA,
               gcm = NA, ssp = NA, gid = gid_fSsimDataPrep),
    gdriveSims
  )

  source("R/upload_fSDatPrepFit_vegCoeffs.R")
}

rm(rstLCC, standAgeMap2001, standAgeMap2011)
