## run module(s) to create study areas etc.

do.call(SpaDES.core::setPaths, paths1)

modules1 <- list("MPB_SK_studyArea")

objects1 <- list()

parameters1 <- list(
  MPB_SK_studyArea = list(
    .plotInitialTime = NA
  )
)

simOutPreamble <- Cache(
  simInitAndSpades,
  times = list(start = 0, end = 1),
  params = parameters1,
  modules = modules1,
  objects = objects1,
  paths = paths1,
  debug = 1,
  omitArgs = c("debug", "paths")#,
  #useCache = "overwrite", ## TODO: remove this workaround
  #useCloud = useCloudCache,
  #cloudFolderID = cloudCacheFolderID
)
fsimOutPreamble <- file.path(Paths$outputPath, "ml_preamble.rds")
saveSimList(simOutPreamble, fsimOutPreamble, fileBackend = 2)
