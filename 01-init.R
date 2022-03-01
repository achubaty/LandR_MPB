if (file.exists(".Renviron")) readRenviron(".Renviron")

.starttime <- Sys.time()

cacheDir <- config::get("paths")[["cachedir"]]
cacheFormat <- config::get("cacheformat")
codeChecks <- config::get("codechecks")
delayStart <- config::get("delaystart")
eventCaching <- c(".inputObjects", "init")
lowMemory <- config::get("lowmemory")
messagingNumCharsModule <- config::get("messagingNumCharsModule")
newGoogleIDs <- FALSE ## gets rechecked/updated for each script (06, 07x, 08x) based on script 05
reproducibleAlgorithm <- config::get("reproduciblealgorithm")
run <- config::get("run")
scratchDir <- config::get("paths")[["scratchdir"]]
showSimilarDepth <- config::get("showsimilardepth")
suitabilityIndex <- config::get("mpbsuitabilityindex")
useMemoise <- config::get("usememoise")
usePlot <- config::get("plot")
userInputPaths <- config::get("inputpaths")
usePrerun <- config::get("useprerun")
useRequire <- config::get("userequire")
recoveryMode <- config::get("recoverymode")
DTthreads <- config::get("dtthreads")

## default paths
inputDir <- "inputs"
moduleDir <- "modules"
outputDir <- "outputs"
scratchDir <- config::get("paths")[["scratchdir"]]

## runName
if (!exists("runName")) {
  runName <- sprintf("run%02d", run) ## TODO: update for MPB climate scenario
} else {
  studyAreaName <- strsplit(runName, "_")[[1]][1]
  run <- as.numeric(substr(runName, nchar(runName) - 1, nchar(runName)))
}
