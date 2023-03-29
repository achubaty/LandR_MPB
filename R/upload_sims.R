library("Require")
Require(c("googledrive", "purrr"))

drive_auth(email = "achubaty@for-cast.ca", use_oob = quickPlot::isRstudioServer())

source("05-google-ids.R")

studyAreas <- c("ABSK_MPB_9.1", "ABSK_MPB_9.2")
lapply(studyAreas, function(sA) {
  gid_results <- as_id(gdriveSims[studyArea == sA & simObject == "results", gid])

  files2upload <- list.files("outputs", paste0(sA, "_(CanESM5|CNRM-ESM2-1).*[.]tar.gz$"), full.names = TRUE)
  files2upload <- set_names(files2upload, basename(files2upload))

  prevUploaded <- drive_ls(gid_results)
  toUpload <- files2upload[!(basename(files2upload) %in% prevUploaded$name)]
  uploaded <- map(toUpload, ~ drive_upload(.x, path = gid_results))
})
