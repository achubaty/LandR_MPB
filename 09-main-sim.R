## run the simulation
do.call(SpaDES.core::setPaths, paths3)

data.table::setDTthreads(useParallel)

timesPredict <- list(start = 2016, end = 2016) ## 2017-2020

paramsPredict <- paramsFit
paramsPredict[["mpbRedTopSpread"]][["advectionDir"]] <- bestFitVals$advectionDir
paramsPredict[["mpbRedTopSpread"]][["advectionMag"]] <- bestFitVals$advectionMag
paramsPredict[["mpbRedTopSpread"]][["bgSettlingProp"]] <- bestFitVals$bgSettlingProp
paramsPredict[["mpbRedTopSpread"]][["meanDist"]] <- bestFitVals$meanDist

tryCatch({
  mySimOut <- Cache(simInitAndSpades, times = timesPredict, #cl = cl,
                    params = paramsPredict,
                    modules = modules3,
                    #outputs = outputs3,
                    objects = objects3,
                    paths = paths3,
                    loadOrder = unlist(modules3),
                    debug = list(file = list(file = file.path(Paths$outputPath, "sim.log"),
                                             append = TRUE), debug = 1),
                    useCloud = FALSE,
                    cloudFolderID = cloudCacheFolderID,
                    omitArgs = c("debug", "paths", ".plotInitialTime"),
                    .plotInitialTime = .plotInitialTime)
}, error = function(e) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::text_slackr(
      paste0("ERROR in simulation `", runName, "` on host `", Sys.info()[["nodename"]], "`.\n",
             "```\n", e$message, "\n```"),
      channel = config::get("slackchannel"), preformatted = FALSE
    )
    stop(e$message)
  }
})

cat(capture.output(warnings()), file = file.path(Paths$outputPath, "warnings.txt"))

fsim <- simFile("mySimOut", Paths$outputPath, SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim)


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
