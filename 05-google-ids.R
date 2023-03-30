## Google Drive locations for pre-run simulation objects

gdriveSims <- data.table::fread("05-google-ids.csv")

lvls <- c("simOutPreamble", "biomassMaps2001", "biomassMaps2011", "fSsimDataPrep", "mpbSpreadFit",
          "ignitionOut", "escapeOut", "spreadOut", "results")
data.table::set(gdriveSims, NULL, "simObject", factor(gdriveSims$simObject, levels = lvls))
data.table::setkeyv(gdriveSims, c("studyArea", "simObject", "runID", "gcm", "ssp"))

gdriveAllResults <- as_id("1gp0EDw0l3aiif0Eev9MP_IzDYJ9gcgWJ")

gdriveURL <- gid_results <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "results", gid]

update_googleids <- function(x, gdriveSims) {
  gdriveSims_updated <- rbind(gdriveSims, x)
  gdriveSims_updated <- unique(gdriveSims_updated)
  setorder(gdriveSims_updated)
  fwrite(x = gdriveSims_updated, file = "05-google-ids.csv")

  return(gdriveSims_updated)
}

