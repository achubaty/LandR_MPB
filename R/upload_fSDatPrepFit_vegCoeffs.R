Require::Require("reproducible")
Require::Require("googledrive")

source("05-google-ids.R")

filesToUpload <- c(
  paste0("fireSense_SpreadFit_veg_coeffs_", config$context[["studyAreaName"]], ".txt")
)

gid_results <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "results", gid]
lapply(filesToUpload, function(f) {
  if (file.exists(f))
    retry(quote(drive_put(file.path("outputs", config$context[["studyAreaName"]], f), unique(as_id(gid_results)))),
          retries = 5, exponentialDecayBase = 2)
})
