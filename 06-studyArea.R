source("05-google-ids.R")

gid_preamble <- gdriveSims[studyArea == .studyAreaName & simObject == "simOutPreamble" &
                             gcm == .climateGCM & ssp == .climateSSP, gid]
upload_preamble <- config$context[["rep"]] == 1 & (config$args[["reupload"]] | length(gid_preamble) == 0)

preambleObjects <- list(
  .runName = config$context[["runName"]]
)

preambleParams <- list(
  # config$params[[".globals"]],
  canClimateData = config$params[["canClimateData"]],
  LandR_MPB_studyArea = config$params[["LandR_MPB_studyArea"]]
)

preambleModules <- list("LandR_MPB_studyArea", "canClimateData") ## TODO: use config$modules


fsimOutPreamble <- simFile(paste0("simOutPreamble_", config$context[["studyAreaName"]],
                                  "_", config$context[["climateGCM"]],
                                  "_", config$context[["climateSSP"]]),
                           prjPaths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_preamble)) {
  if (!file.exists(fsimOutPreamble)) {
    googledrive::drive_download(file = as_id(gid_preamble), path = fsimOutPreamble)
  }
  simOutPreamble <- loadSimList(fsimOutPreamble)
} else {
  simOutPreamble <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = preambleParams,
    modules = preambleModules,
    loadOrder = unlist(preambleModules),
    objects = preambleObjects,
    #.cacheExtra = moduleCodeFiles(config$paths, preambleModules)
  )

  if (isUpdated(simOutPreamble)) {
    simOutPreamble@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(simOutPreamble, fsimOutPreamble,
                fileBackend = ifelse(isTRUE(config$args[["reupload"]]), 2, 0)
                )
    amc::.gc()
  }

  if (isTRUE(upload_preamble)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fsimOutPreamble, path = as_id(gdriveURL), name = basename(fsimOutPreamble))
    gid_preamble <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "simOutPreamble", runID = NA,
                 gcm = config$context[["climateGCM"]], ssp = config$context[["climateSSP"]], gid = gid_preamble),
      gdriveSims
    )
  }
}

firstRunMDCplots <- if (config$context[["rep"]] == 1 && config$args[["reupload"]]) TRUE else FALSE

if (isTRUE(firstRunMDCplots)) {
  ggMDC <- fireSenseUtils::compareMDC(
    historicalMDC = simOutPreamble[["historicalClimateRasters"]][["MDC"]],
    projectedMDC = simOutPreamble[["projectedClimateRasters"]][["MDC"]],
    flammableRTM = simOutPreamble[["flammableRTM"]]
  )
  fggMDC <- file.path(config$paths[["outputPath"]], "figures",
                      paste0("compareMDC_", config$context[["studyAreaName"]], "_",
                             config$context[["climateGCM"]], "_",
                             config$context[["climateSSP"]], ".png"))
  checkPath(dirname(fggMDC), create = TRUE)

  ggplot2::ggsave(plot = ggMDC, filename = fggMDC)

  if (isTRUE(upload_preamble)) {
    source("05-google-ids.R")

    googledrive::drive_put(
      media = fggMDC,
      path = unique(as_id(gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "results", gid])),
      name = basename(fggMDC)
    )
  }
}

nSpecies <- length(unique(simOutPreamble[["sppEquiv"]][["LandR"]]))
