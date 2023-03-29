Require::Require("fs")
Require::Require("googledrive")

source("05-google-ids.R")

filesToUpload <- c(
  paste0("figures/ignition_NumFiresFitted_", config$context[["studyAreaName"]], ".png"),
  paste0("figures/IgnitionRatePer100km2_", config$context[["studyAreaName"]], ".png")
)

lapply(filesToUpload, function(f) {
  retry(quote(drive_put(file.path(config$paths[["outputPath"]], f), unique(as_id(gid_results)))),
        retries = 5, exponentialDecayBase = 2)
})
