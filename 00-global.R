# project basics ------------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT
if (file.exists("LandR_MPB.Renviron")) readRenviron("LandR_MPB.Renviron") ## database credentials

.ncores <- min(parallel::detectCores() / 2, 24L)
.nodename <- Sys.info()[["nodename"]]
.user <- Sys.info()[["user"]]

###### allow setting run context info from outside this script (e.g., bash script) -----------------
if (exists(".mode", .GlobalEnv)) {
  stopifnot(all(.mode %in% c("development", "fit", "postprocess", "production")))
} else {
  .mode <- c("development")

  if (.user %in% c("achubaty") && grepl("for-cast[.]ca", .nodename)) {
    .mode <- append(.mode, "fit")
  }
}

if (exists(".climateGCM", .GlobalEnv)) {
  stopifnot(.climateGCM %in% c("CanESM5", "CNRM-ESM2-1"))
} else {
  .climateGCM <- "CanESM5"
}

if (exists(".climateSSP", .GlobalEnv)) {
  stopifnot(.climateSSP %in% c(245, 370, 585))
} else {
  .climateSSP <- 370
}

if (exists(".rep", .GlobalEnv)) {
  .rep <- if ("postprocess" %in% .mode) NA_integer_ else as.integer(.rep)
} else {
  .rep <- if ("postprocess" %in% .mode) NA_integer_ else 1L
}

if (exists(".res", .GlobalEnv)) {
  stopifnot(.res %in% c(125, 250))
} else {
  .res <- 250
}

if (!exists(".studyAreaName", .GlobalEnv)) {
  ## ecoprovs in ABSK corresponding to ecoregions in preamble: 9.1, 9.2
  .studyAreaName <- "ABSK_9.2" ## TODO: avoid needing to specify both ecoregions and ecoprovs
}
#####

prjDir <- switch(.user,
                 # user1 = "/path/to/my/projects/LandR_MPB"
                 "~/GitHub/LandR_MPB")

stopifnot(identical(normalizePath(prjDir), normalizePath(getwd())))

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cloud.r-project.org")
)

# install and load packages -------------------------------------------------------------------

pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  install.packages("remotes")
}

Require.version <- "PredictiveEcology/Require@dev-stable"
if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
    packageVersion("Require", lib.loc = .libPaths()[1]) < "0.2.6.9004") {
  remotes::install_github(Require.version)
}

library(Require)

setLinuxBinaryRepo()

## WARNING: Require doesn't respect upgrade = FALSE; reinstall affected packages manually if needed

## TODO: restore this once Require fixed + upgraded
# Require(c(
#   "PredictiveEcology/SpaDES.project@transition (>= 0.0.7.9018)", ## TODO: use development once merged
#   "PredictiveEcology/SpaDES.config@development (>= 0.0.2.9068)"
# ), upgrade = FALSE, standAlone = TRUE)

if (!"SpaDES.project" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  remotes::install_github("PredictiveEcology/SpaDES.project@607f3fb1e32970c568a5e7a0ea3ca26a83237321", upgrade = FALSE)
}
require("SpaDES.project")

if (!"SpaDES.config" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  remotes::install_github("PredictiveEcology/SpaDES.config@development", upgrade = FALSE)
}
require("SpaDES.config")

if (!"BioSIM" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  ## https://sourceforge.net/p/mrnfforesttools/biosimclient/wiki/BioSIM-R/#requirements
  # install.packages("https://sourceforge.net/projects/repiceasource/files/latest",
  #                  repos = NULL,  type = "source")
  # install.packages("https://sourceforge.net/projects/biosimclient.mrnfforesttools.p/files/latest",
  #                  repos = NULL,  type = "source")
  remotes::install_github("CWFC-CCFB/J4R@v1.1.8") ## v1.1.9 broken on ubuntu
  remotes::install_github("RNCan/BioSimClient_R")
}

## TODO: Require fails to install modulePkgs correctly - manually install these:
if (FALSE) {
  install.packages(c(
    "SpatialPack", "knitr", "details", "rmarkdown", "htmlwidgets", "htmlTable", "leaflet",
    "widgetframe", "Hmisc", "leafem", "leafsync", "rms", "tmap", "spatialEco"
  ), repos = "https://cloud.r-project.org")

  install.packages(c(
    "future", "dplyr", "disk.frame"
  ), repos = "https://cloud.r-project.org")
}
modulePkgs <- unname(unlist(packagesInModules(modulePath = file.path(prjDir, "modules"))))
modulePkgs <- unique(gsub("development", "dev-stable", modulePkgs)) ## TODO: temporary version; update when working

otherPkgs <- c("archive", "details", "DBI", "s-u/fastshp",
               "PredictiveEcology/fireSenseUtils@dev-stable",
               "future", "future.callr",
               "PredictiveEcology/LandR@dev-stable",
               "ianmseddy/LandR.CS@dev-stable",
               "logging",
               "PredictiveEcology/map@dev-stable",
               "PredictiveEcology/pemisc@dev-stable",
               "Rcpp (>= 1.0.10)",
               "PredictiveEcology/quickPlot@dev-stable",
               "PredictiveEcology/reproducible@dev-stable (>= 1.2.16.9024)",
               "PredictiveEcology/Require@dev-stable",
               "RPostgres", "slackr",
               "PredictiveEcology/SpaDES.core@dev-stable (>= 1.1.1)",
               "PredictiveEcology/SpaDES.tools@dev-stable",
               "terra (>= 1.7-3)")

Require(unique(c(modulePkgs, otherPkgs)), require = FALSE, standAlone = TRUE, upgrade = FALSE)

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr", "SpaDES.core",
          "googledrive", "httr", "LandR", "magrittr", "sessioninfo", "slackr"),
        upgrade = FALSE, standAlone = TRUE)

# configure project ---------------------------------------------------------------------------

config <- SpaDES.config::useConfig(projectName = "LandRfS", projectPath = prjDir,
                                   climateGCM = .climateGCM, climateSSP = .climateSSP,
                                   mode = .mode, rep = .rep, res = .res,
                                   studyAreaName = .studyAreaName)

## apply user and machine context settings here
source("02-studyArea-config.R")
config$args <- config.studyArea$args
config$modules <- modifyList2(config$modules, config.studyArea$modules) ## TODO: update in SpaDES.config
config$options <- config.studyArea$options
config$params <- config.studyArea$params
config$paths <- config.studyArea$paths
config$update()
config$validate()

## apply user and machine context settings here
source("02-user-config.R")
config$args <- config.user$args
#config$modules <- config.user$modules ## no modules should differ among users/machines
config$options <- config.user$options
config$params <- config.user$params
config$paths <- config.user$paths

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(config$context)
names(config$modules)

# project paths -------------------------------------------------------------------------------
config$paths
stopifnot(identical(checkPath(config$paths[["projectPath"]]), getwd()))

checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below

prjPaths <- SpaDES.config::paths4spades(config$paths)

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "eastern-boreal", tryEmail = config$args[["cloud"]][["googleUser"]])

## helper functions
source("R/cache_helpers.R")

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, prjPaths)

if (config$args[["delayStart"]] > 0) {
  message(crayon::green("\nStaggered job start: delaying by", config$args[["delayStart"]], "minutes."))
  Sys.sleep(config$args[["delayStart"]]*60)
}

if (!"postprocess" %in% config$context[["mode"]]) {
  if ("fit" %in% config$context[["mode"]]) {
    config$args[["usePrerun"]] <- FALSE
    config$args[["reupload"]] <- TRUE
  } else {
    config$args[["usePrerun"]] <- TRUE
    config$args[["reupload"]] <- FALSE
  }

  source("06-studyArea.R")
  source("07a-dataPrep_2001.R")

  if ("fit" %in% config$context[["mode"]]) {
    opt <- options(spades.memoryUseInterval = FALSE) ## TODO: periodically stalls during mem use setup; disable temporarily
  }
  source("07b-dataPrep_2011.R") ## run mpbPine here
  source("07c-dataPrep_fS.R")

  source("08-MPB-spread-fit.R")
  source("08a-ignitionFit.R")
  source("08b-escapeFit.R")

  # if ("fit" %in% config$context[["mode"]]) {
  #   options(opt)
  # }

  if ("fit" %in% config$context[["mode"]]) {
    config$args[["usePrerun"]] <- FALSE
    config$args[["reupload"]] <- TRUE

    for (i in config$params[[".globals"]][["reps"]]) {
      config$context[["rep"]] <- i
      config$update()
      config$validate()

      logPath <- checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below
      prjPaths <- SpaDES.config::paths4spades(config$paths)

      ## prerun all spreadfits, for use with main sim runs on another machine

      if (file.exists("Rplots.pdf")) {
        unlink("Rplots.pdf")
      }

      do.call(SpaDES.core::setPaths, prjPaths)

      source("08c-spreadFit.R")

      if (file.exists("Rplots.pdf")) {
        file.rename("Rplots.pdf", file.path(figPath, sprintf("spreadFit_plots_%s.pdf", config$context[["runName"]])))
      }
    }
  } else {
    source("08c-spreadFit.R")
    source("09-main-sim.R")
  }
} else {
  source("10-post-processing.R")
}

relOutputPath <- SpaDES.config:::.getRelativePath(prjPaths[["outputPath"]], prjDir)
rrFile <- file.path(relOutputPath, "INFO.md")
cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
cat(SpaDES.project::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

DBI::dbDisconnect(getOption("reproducible.conn"))
