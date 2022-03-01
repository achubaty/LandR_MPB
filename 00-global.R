if (!exists("pkgDir")) {
  topLevelPkgDir <- if (Sys.info()[["user"]] == "emcintir") {
    "../packages_MPB_SK"
  } else {
    "packages"
  }
  pkgDir <- file.path(topLevelPkgDir, version$platform,
                      paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))

  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir, recursive = TRUE)
  }
  .libPaths(pkgDir)
}

#devtools::install("../reproducible")
#devtools::load_all("~/GitHub/SpaDES.tools")
#devtools::load_all("~/GitHub/SpaDES.core")

switch(Sys.info()[["user"]],
       "achubaty" = Sys.setenv(R_CONFIG_ACTIVE = "alex"),
       "emcintir" = Sys.setenv(R_CONFIG_ACTIVE = "eliot"),
       Sys.setenv(R_CONFIG_ACTIVE = "default")
)
#Sys.getenv("R_CONFIG_ACTIVE") ## verify

source("03-packages.R")
source("01-init.R")
source("02-paths.R")
source("04-options.R")
#source("05-google-ids.R") ## gets sourced at top of each script 06, 07x, 08x

if (delayStart > 0) {
  message(crayon::green("\nStaggered job start: delaying by", delayStart, "minutes."))
  Sys.sleep(delayStart*60)
}

source("06-studyArea.R")
#source("07-dataPrep.R") ## skip for now
source("08-MPB-spread-fit.R")
# source("09-main-sim.R")
