## Ubuntu 20.04 ships with GDAL 3.0 and PROJ 6.3, which is what RSPM builds with
## If using newer versions from ubuntu-gis ppa, need to install spatial packages from source
need_reinstall_spatial <- tryCatch(!nzchar(rgdal::getGDALVersionInfo()), error = function(e) TRUE)
gdal_version <- numeric_version(system("pkg-config --modversion gdal", intern = TRUE))
proj_version <- numeric_version(system("pkg-config --modversion proj", intern = TRUE))
if (need_reinstall_spatial) {
  spatialPkgs <- c("rgdal", "rgeos", "sf", "sp", "raster", "terra")
  if (gdal_version > "3.2" | proj_version > "7.2") {
    install.packages(spatialPkgs, repos = "https://cran.rstudio.com") ## install from source
  } else {
    install.packages(spatialPkgs) ## install from RSPM binaries
  }
}

if (!require("BioSIM", quietly = TRUE)) {
  ## https://sourceforge.net/p/mrnfforesttools/biosimclient/wiki/BioSIM-R/#requirements
  install.packages("https://sourceforge.net/projects/repiceasource/files/latest",
                   repos = NULL,  type = "source")
  install.packages("https://sourceforge.net/projects/biosimclient.mrnfforesttools.p/files/latest",
                   repos = NULL,  type = "source")
}

Require("PredictiveEcology/SpaDES.install (>= 0.0.4.9000)")
out <- makeSureAllPackagesInstalled(modulePath = "modules")
Require(c("data.table", "plyr", "pryr", "raster", "parallel")) ## ensure plyr loaded before dplyr or there will be problems
Require("SpaDES.core (>= 1.0.8)",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core

Require("achubaty/amc (>= 0.2.0)", require = FALSE, which = c("Suggests", "Imports", "Depends"))
Require("jimhester/archive", upgrade = FALSE)
Require("slackr", require = FALSE)
# Require("PredictiveEcology/SpaDES.experiment@development")
