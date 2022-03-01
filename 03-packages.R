if (!require("Require")) {install.packages("Require")}
## Ubuntu 20.04 ships with GDAL 3.0 and PROJ 6.3, which is what RSPM builds with
## If using newer versions from ubuntu-gis ppa, need to install spatial packages from source
# if (!Require:::isWindows()) {
#   need_reinstall_spatial <- tryCatch(!nzchar(rgdal::getGDALVersionInfo()), error = function(e) TRUE)
#   gdal_version <- numeric_version(system("pkg-config --modversion gdal", intern = TRUE))
#   proj_version <- numeric_version(system("pkg-config --modversion proj", intern = TRUE))
#   if (need_reinstall_spatial) {
#     spatialPkgs <- c("rgdal", "rgeos", "sf", "sp", "raster", "terra")
#     if (gdal_version > "3.2" | proj_version > "7.2") {
#       install.packages(spatialPkgs, repos = "https://cran.rstudio.com") ## install from source
#     } else {
#       install.packages(spatialPkgs) ## install from RSPM binaries
#     }
#   }
# }

# Installs First
Require::Require("PredictiveEcology/SpaDES.install@development (>= 0.0.4.9000)")
installSpaDES(upgrade = TRUE) # this will do spatial packages
if (FALSE) {
  installSpatialPackages() # may want to do this manually
}
out <- makeSureAllPackagesInstalled(modulePath = "modules")
Require("achubaty/amc (>= 0.2.0)", require = FALSE, which = c("Suggests", "Imports", "Depends"))
Require("slackr", require = FALSE)


# Installs and Loads
Require(c("data.table", "plyr", "pryr", "raster", "config")) ## ensure plyr loaded before dplyr or there will be problems
if (!require("BioSIM", quietly = TRUE)) {
  ## https://sourceforge.net/p/mrnfforesttools/biosimclient/wiki/BioSIM-R/#requirements
  install.packages("https://sourceforge.net/projects/repiceasource/files/latest",
                   repos = NULL,  type = "source")
  install.packages("https://sourceforge.net/projects/biosimclient.mrnfforesttools.p/files/latest",
                   repos = NULL,  type = "source")
}
Require("SpaDES.core (>= 1.0.8)",
        which = c("Suggests", "Imports", "Depends"), upgrade = FALSE) # need Suggests in SpaDES.core


# Require("jimhester/archive", upgrade = FALSE)
# Require("PredictiveEcology/SpaDES.experiment@development")
