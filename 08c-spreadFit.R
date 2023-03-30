source("05-google-ids.R")

gid_spreadOut <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "spreadOut" & runID == config$context[["rep"]], gid]
upload_spreadOut <- config$args[["reupload"]] | length(gid_spreadOut) == 0

## TODO: remove this workaround
fSsimDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]][[1]] <-
  as.data.table(fSsimDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]][[1]])

extremeVals <- 4
lowerParamsNonAnnual <- rep(-extremeVals, times = ncol(fSsimDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]][[1]]) - 1)
lowerParamsAnnual <- c(-extremeVals, -extremeVals)
upperParamsNonAnnual <- rep(extremeVals, times = length(lowerParamsNonAnnual))
upperParamsAnnual <- c(0, extremeVals) ## youngAge <= 0
lowerParams <- c(lowerParamsAnnual, lowerParamsNonAnnual)
upperParams <- c(upperParamsAnnual, upperParamsNonAnnual)

## Spread log function bounds

## for logistic3p
# lower <- c(0.22, 0.001, 0.001, lowerParams)
# upper <- c(0.29, 10, 10, upperParams)

lower <- c(0.25, 0.2, 0.1, lowerParams)
upper <- c(0.265, 2, 4, upperParams) ## NOTE: 2022-03-04: lowered to 0.265 from 0.276 b/c too burny
dfT <- cbind(c("lower", "upper"), t(data.frame(lower, upper)))
message("Upper and Lower parameter bounds are:")
Require:::messageDF(dfT)

spreadFitParams <- list(
  fireSense_SpreadFit = config$params[["fireSense_SpreadFit"]]
)

spreadFitParams[["fireSense_SpreadFit"]][["lower"]] <- lower
spreadFitParams[["fireSense_SpreadFit"]][["upper"]] <- upper
spreadFitParams[["fireSense_SpreadFit"]][["maxFireSpread"]] <- max(0.28, upper[1])
spreadFitParams[["fireSense_SpreadFit"]][["NP"]] <- length(spreadFitParams[["fireSense_SpreadFit"]][["cores"]]) ## TODO: autoupdate config

spreadFitObjects <- list(
  fireBufferedListDT = fSsimDataPrep[["fireBufferedListDT"]],
  firePolys = fSsimDataPrep[["firePolys"]],
  fireSense_annualSpreadFitCovariates = fSsimDataPrep[["fireSense_annualSpreadFitCovariates"]],
  fireSense_nonAnnualSpreadFitCovariates = fSsimDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]],
  fireSense_spreadFormula = fSsimDataPrep[["fireSense_spreadFormula"]],
  flammableRTM = fSsimDataPrep[["flammableRTM"]],
  #parsKnown = spreadOut[["fireSense_SpreadFitted"]][["meanCoef"]],
  rasterToMatch = fSsimDataPrep[["rasterToMatch"]],
  spreadFirePoints = fSsimDataPrep[["spreadFirePoints"]],
  studyArea = fSsimDataPrep[["studyArea"]]
)

fspreadOut <- simFile(paste0("spreadOut_", config$context[["studyAreaName"]], "_", config$context[["rep"]]),
                      config$paths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_spreadOut)) {
  if (!file.exists(fspreadOut)) {
    googledrive::drive_download(file = as_id(gid_spreadOut), path = fspreadOut)
  }
  spreadOut <- loadSimList(fspreadOut)
} else {
  spreadOut <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = spreadFitParams,
    modules = "fireSense_SpreadFit",
    objects = spreadFitObjects
  )

  #if (isUpdated(spreadOut)) {
    spreadOut@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(spreadOut, fspreadOut,
                fileBackend = ifelse(isTRUE(config$args[["reupload"]]), 2, 0)
                )
  #}

  if (isTRUE(upload_spreadOut)) {
    source("05-google-ids.R")

    tempdir(check = TRUE) ## TODO: why is this dir being removed in the first place?
    fdf <- googledrive::drive_put(media = fspreadOut, path = as_id(gdriveURL), name = basename(fspreadOut))
    gid_spreadOut <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "spreadOut", runID = config$context[["rep"]],
                 gcm = NA, ssp = NA, gid = gid_spreadOut),
      gdriveSims
    )
  }

  source("R/upload_spreadFit.R")

  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("`fireSense_SpreadFit` for `", config$context[["runName"]], "` completed on host `", Sys.info()[["nodename"]], "`."),
      channel = config$args[["notifications"]][["slackChannel"]], preformatted = FALSE
    )
  }
}
