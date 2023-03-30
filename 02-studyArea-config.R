## project-specific studyArea and other config

config.studyArea <- list(
  args = list(
    simYears = list(start = 2018, end = 2030)
  ),
  modules = list(
    LandR_MPB_studyArea = "LandR_MPB_studyArea"
  ),
  options = list(
    ## TODO
  ),
  params = list(
    .globals = list(
      sppEquivCol = "LandR"
    ),
    Biomass_borealDataPrep = list(
      subsetDataAgeModel = 50,
      subsetDataBiomassModel = 50
    ),
    canClimateData = list(
      studyAreaName = c("AB", "SK")
    ),
    LandR_MPB_studyArea = list(
      studyAreaName = config$context[["studyAreaName"]],
      .useCache = FALSE #".inputObjects"
    )
  ),
  paths = list(
    ## TODO
  )
)

