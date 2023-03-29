Require::Require("fs")
Require::Require("googledrive")

source("05-google-ids.R")
tempdir(check = TRUE) ## TODO: figure out why this gets deleted; required for upload

try(file_move(
  file.path(config$paths[["outputPath"]], "figures", "spreadFit_coeffs.png"),
  file.path(config$paths[["outputPath"]], "figures", sprintf("spreadFit_coeffs_%s_run_%02d.png",
                                                             config$context[["studyAreaName"]], config$context[["rep"]]))
))

filesToUpload <- c(
  file.path(config$paths[["outputPath"]], "figures", sprintf("spreadFit_coeffs_%s_run_%02d.png",
                                                             config$context[["studyAreaName"]], config$context[["rep"]]))
)

gid_results <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "results", gid]
lapply(filesToUpload, function(f) {
  retry(quote(drive_put(f, unique(as_id(gid_results), basename(f)))), retries = 5, exponentialDecayBase = 2)
})
