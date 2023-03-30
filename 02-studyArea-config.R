## project-specific studyArea and other config; additional modules not in LandR-fS config

historicFireYears <- 2011:2020

config.studyArea <- list(
  args = list(
    simYears = list(start = 2018, end = 2030)
  ),
  modules = list(
    LandR_MPB_studyArea = "LandR_MPB_studyArea",
    mpbClimateData = "mpbClimateData",
    mpbPine = "mpbPine",
    mpbMassAttacksData = "mpbMassAttacksData",
    mpbRedTopSpread = "mpbRedTopSpread"
  ),
  options = list(
    rasterMaxMemory = 6e+10 ## TODO: is this too high?
  ),
  params = list(
    .globals = list(
      sppEquivCol = "LandR",
      .runInitialTime = max(historicFireYears) + 1 ## fireSense simulates fires for years w/o data; TODO: add to config
    ),
    Biomass_borealDataPrep = list(
      subsetDataAgeModel = 50,
      subsetDataBiomassModel = 50
    ),
    canClimateData = list(
      studyAreaName = c("AB", "SK")
    ),
    historicFires = list( ## TODO: add to config
      staticFireYears = historicFireYears
    ),
    LandR_MPB_studyArea = list(
      studyAreaName = config$context[["studyAreaName"]],
      .useCache = FALSE #".inputObjects"
    ),
    mpbClimateData = list(
      suitabilityIndex = "R",    ## Can be "G", "S", "L", "R"
      .maxMemory = 6e+10,
      .useCache = "init"
    ),
    mpbMassAttacksData = list(
      .maxMemory = 6e+10,
      .useCache = c(".inputObjects", "init"),
      .plotInitialTime = NA
    ),
    mpbPine = list(
      lowMemory = FALSE,
      .maxMemory = 6e+10,
      .useCache = c(".inputObjects", "init"),
      .plotInitialTime = NA#,
      #.tempdir = scratchDir
    ),
    mpbRedTopSpread = list(
      # p_advectionDir = 90, # not used anymore
      p_advectionMag = 1000,
      p_meanDist = 1000,
      maxDistance = 1e5,
      dispersalKernel = "Weibull3", ## "Generalized Gamma"
      # .plots = "screen",
      type = if ("fit" %in% config$context[["mode"]]) "fit" else "forecast" ## TODO: allow backcast etc.
    )
  ),
  paths = list(
    ## TODO
  )
)

