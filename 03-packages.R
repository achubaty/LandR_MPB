## set CRAN repos; use binary linux packages if on Ubuntu
local({
  options(Ncpus = parallel::detectCores() / 2)
  options("repos" = c(CRAN = "https://cran.rstudio.com"))

  if (Sys.info()["sysname"] == "Linux" && grepl("Ubuntu", utils::osVersion)) {
    .os.version <- strsplit(system("lsb_release -c", intern = TRUE), ":\t")[[1]][[2]]
    .user.agent <- paste0(
      "R/", getRversion(), " R (",
      paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]),
      ")"
    )
    options(repos = c(CRAN = paste0("https://packagemanager.rstudio.com/all/__linux__/",
                                    .os.version, "/latest")))
    options(HTTPUserAgent = .user.agent)
  }
})
Require("PredictiveEcology/SpaDES.install (>= 0.0.4.9000)")
out <- makeSureAllPackagesInstalled(modulePath = "modules")
Require(c("data.table", "plyr", "pryr", "raster")) ## ensure plyr loaded before dplyr or there will be problems
Require("PredictiveEcology/SpaDES.core@rasterToMemoryUpdates (>= 1.0.6.9023)",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core

Require("achubaty/amc (>= 0.2.0)", require = FALSE, which = c("Suggests", "Imports", "Depends"))
Require("jimhester/archive", upgrade = FALSE)
Require("slackr", require = FALSE)

