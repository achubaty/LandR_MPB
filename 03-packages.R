## Ubuntu 20.04 ships with GDAL 3.0 and PROJ 6.3, which is what RSPM builds with
## If using newer versions from ubuntu-gis ppa, need to install spatial packages from source
gdal_version <- numeric_version(system("pkg-config --modversion gdal", intern = TRUE))
proj_version <- numeric_version(system("pkg-config --modversion proj", intern = TRUE))
if (gdal_version > "3.2" | proj_version > "7.2") {
  spatialPkgs <- c("rgdal", "rgeos", "sf", "sp", "raster", "terra")
  install.packages(spatialPkgs, repos = "https://cran.rstudio.com")
}

Require("PredictiveEcology/SpaDES.install (>= 0.0.4.9000)")
out <- makeSureAllPackagesInstalled(modulePath = "modules")
Require(c("data.table", "plyr", "pryr", "raster")) ## ensure plyr loaded before dplyr or there will be problems
Require("SpaDES.core (>= 1.0.8)",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core

Require("achubaty/amc (>= 0.2.0)", require = FALSE, which = c("Suggests", "Imports", "Depends"))
Require("jimhester/archive", upgrade = FALSE)
Require("slackr", require = FALSE)

if (!require("BioSIM", quietly = TRUE)) {
  ## https://sourceforge.net/p/mrnfforesttools/biosimclient/wiki/BioSIM-R/#requirements
  install.packages("https://sourceforge.net/projects/repiceasource/files/latest",
                   repos = NULL,  type = "source")
  install.packages("https://sourceforge.net/projects/biosimclient.mrnfforesttools.p/files/latest",
                   repos = NULL,  type = "source")
}
