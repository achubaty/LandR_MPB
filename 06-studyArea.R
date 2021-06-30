## run module(s) to create study areas etc.

do.call(SpaDES.core::setPaths, paths1)

modules1 <- list("LandR_MPB_studyArea")

objects1 <- list()

parameters1 <- list(
  MPB_SK_studyArea = list(
    .plotInitialTime = NA
  )
)


# Digest the code files
# mcf <- CacheDigest(moduleCodeFiles, quick = FALSE)$outputHash

simOutPreamble <- Cache(
  simInitAndSpades,
  times = list(start = 0, end = 1),
  params = parameters1,
  modules = modules1,
  objects = objects1,
  paths = paths1,
  debug = 1,
  omitArgs = c("debug", "paths"),
  .cacheExtra = moduleCodeFiles(paths1, modules1) # new fn in SpaDES.core (>= 1.0.8.9004)
  #useCache = "overwrite", ## TODO: remove this workaround
  #useCloud = useCloudCache,
  #cloudFolderID = cloudCacheFolderID
)
fsimOutPreamble <- file.path(Paths$outputPath, "ml_preamble.rds")
saveSimList(simOutPreamble, fsimOutPreamble, fileBackend = 2)
