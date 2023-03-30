source("05-google-ids.R")

gid_ignitionOut <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "ignitionOut", gid]
upload_ignitionOut <- config$args[["reupload"]] | length(gid_ignitionOut) == 0

## ub and lb have to be provided for now

biggestObj <- as.numeric(object.size(fSsimDataPrep[["fireSense_ignitionCovariates"]])) / 1e6 * 1.2

## TODO: add to config & test:
#config$params[["fireSense_IgnitionFit"]] <- list(rescalers = NA, rescaleVars = FALSE) ## ok? no CIs on plots
#config$params[["fireSense_IgnitionFit"]] <- list(rescalers = NA, rescaleVars = TRUE) ## ok to use??
config$params[["fireSense_IgnitionFit"]] <- list(rescalers = NULL, rescaleVars = TRUE) ## ok to use

ignitionFitParams <- list(
  fireSense_IgnitionFit = config$params[["fireSense_IgnitionFit"]]
)

if (grepl("ROF", config$context[["studyAreaName"]])) {
  ## NOTE: drop youngAge and class2 (rare on landscape and driving crazy ignitions)
  form <- paste("ignitions ~ FenPlus:MDC + BogSwamp:MDC + class3:MDC +",
                "FenPlus:pw(MDC, k_FnP) + BogSwamp:pw(MDC, k_BgS) + class3:pw(MDC, k_cl3) - 1")
} else {
  form <- fSsimDataPrep[["fireSense_ignitionFormula"]]
}

ignitionFitParams[["fireSense_IgnitionFit"]][["fireSense_ignitionFormula"]] <- form

## if using binomial need to pass theta to lb and ub
if (grepl("negative.binomial", ignitionFitParams[["fireSense_IgnitionFit"]][["family"]][1])) {
  ignitionFitParams[["fireSense_IgnitionFit"]][["lb"]] <- list(
    coef = 0,
    knots = list(MDC = round(quantile(fSsimDataPrep[["fireSense_ignitionCovariates"]][["MDC"]], probs = 0.05), digits = 0))
  )
  ignitionFitParams[["fireSense_IgnitionFit"]][["ub"]] <- list(
    coef = 20,
    knots = list(MDC = round(quantile(fSsimDataPrep[["fireSense_ignitionCovariates"]][["MDC"]], probs = 0.8), digits = 0))
  )
}

ignitionFitObjects <- list(
  fireSense_ignitionCovariates = fSsimDataPrep[["fireSense_ignitionCovariates"]],
  ignitionFitRTM = fSsimDataPrep[["ignitionFitRTM"]]
)

fignitionOut <- simFile(paste0("ignitionOut_", config$context[["studyAreaName"]]), config$paths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_ignitionOut)) {
  if (!file.exists(fignitionOut)) {
    googledrive::drive_download(file = as_id(gid_ignitionOut), path = fignitionOut)
  }
  ignitionOut <- loadSimList(fignitionOut)
} else {
  ignitionOut <- Cache(
    simInitAndSpades,
    times = list(start = 0, end = 1),
    params = ignitionFitParams,
    modules = "fireSense_IgnitionFit",
    objects = ignitionFitObjects,
    userTags = c("ignitionFit")
  )

  if (isUpdated(ignitionOut)) {
    ignitionOut@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(sim = ignitionOut, filename = fignitionOut,
                fileBackend = ifelse(isTRUE(config$args[["reupload"]]), 2, 0)
                )
  }

  if (isTRUE(upload_ignitionOut)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fignitionOut, path = as_id(gdriveURL), name = basename(fignitionOut))
    gid_ignitionOut <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "ignitionOut", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_ignitionOut),
      gdriveSims
    )
  }

  firstRunIgnitionFit <- if (config$context[["rep"]] == 1) TRUE else FALSE

  if (isTRUE(firstRunIgnitionFit)) {
    source("R/upload_ignitionFit.R")
  }

  # end-of-sim notifications --------------------------------------------------------------------

  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("fireSense_IgnitionFit for `", config$context[["studyAreaName"]], "` completed on host `", Sys.info()[["nodename"]], "`."),
      channel = config$args[["notifications"]][["slackChannel"]], preformatted = FALSE
    )
  }
}

