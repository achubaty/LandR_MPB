## run the simulation
do.call(SpaDES.core::setPaths, paths3)

data.table::setDTthreads(1)

modules4 <- unique(c(unlist(modules3), c("Biomass_borealDataPrep", "Biomass_core", "Biomass_regeneration")))
timesPredict <- list(start = 2016, end = 2018) ## 2017-2020

paramsPredict <- paramsFit
paramsPredict[["mpbRedTopSpread"]] <-
  modifyList(paramsPredict[["mpbRedTopSpread"]], as.list(apply(MPBfit$fit_mpbSpreadOptimizer$member$pop, 2, mean)))

species <- LandR::speciesInStudyArea(objects3$studyArea)
objects3$sppNameVector <- grep("_Spp", species$speciesList, invert = TRUE, value = TRUE)
objects3$studyAreaLarge <- objects3$studyArea

paramsPredict$Biomass_regeneration$fireInitialTime <- timesPredict$start
# tryCatch({
  mySimOut <- Cache(simInitAndSpades, times = timesPredict, #cl = cl,
                    params = paramsPredict,
                    modules = modules4,
                    #outputs = outputs3,
                    objects = objects3,
                    paths = paths3,
                    loadOrder = unlist(modules3),
                    #debug = list(file = list(file = file.path(Paths$outputPath, "sim.log"),
                    #                         append = TRUE), debug = 1),
                    useCloud = FALSE,
                    cloudFolderID = cloudCacheFolderID,
                    omitArgs = c("paths"),
                    .plots = "png",
                    .plotInitialTime = timesPredict$start)
# }, error = function(e) {
#   if (requireNamespace("slackr") & file.exists("~/.slackr")) {
#     slackr::slackr_setup()
#     slackr::text_slackr(
#       paste0("ERROR in simulation `", runName, "` on host `", Sys.info()[["nodename"]], "`.\n",
#              "```\n", e$message, "\n```"),
#       channel = config::get("slackchannel"), preformatted = FALSE
#     )
#     stop(e$message)
#   }
# })
#
# cat(capture.output(warnings()), file = file.path(Paths$outputPath, "warnings.txt"))
#
# fsim <- simFile("mySimOut", Paths$outputPath, SpaDES.core::end(mySimOut), "qs")
# message("Saving simulation to: ", fsim)
# saveSimList(sim = mySimOut, filename = fsim)


## simulation diagrams
if (FALSE) {
  clearPlot()
  vcol <- sapply(names(V(depsGraph(MPB))), function(v) {
    if (v == "_INPUT_") {
      "orange"
    } else if (v %in% c("mpbRedTopGrowth", "mpbRedTopSpread", "mpbManagement")) {
      "pink"
    } else {
      "lightblue"
    }
  })
  moduleDiagram(MPB, vertex.color = vcol, vertex.size = 30)

  clearPlot()
  objectDiagram(MPB)

  clearPlot()
  eventDiagram(MPB2)
}
