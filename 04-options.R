## cache database connection (requires reproducbile >= 1.0.0)
cacheDBconn <- if (config::get("cachedb") == "sqlite") {
  NULL
} else if (config::get("cachedb") == "postgresql") {
  Require("RPostgres")
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 host = Sys.getenv("PGHOST"),
                 port = Sys.getenv("PGPORT"),
                 dbname = Sys.getenv("PGDATABASE"),
                 user = Sys.getenv("PGUSER"),
                 password = Sys.getenv("PGPASSWORD"))
} else {
  stop("Unsupported cache database type '", config::get("cachedb"), "'")
}

rep <- config::get("rep")

maxMemory <- 6e+10

rasterOptions(default = TRUE)
opts <- options(
  "fftempdir" = scratchDir,
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = config::get("assertions"),
  "rasterMaxMemory" = maxMemory,
  "rasterTmpDir" = scratchDir,
  "reproducible.cachePath" = file.path(scratchDir, "cache"),
  "reproducible.cacheSaveFormat" = cacheFormat, ## can be "qs" or "rds"
  "reproducible.conn" = cacheDBconn,
  "reproducible.destinationPath" = normPath(paths1$inputPath),
  "reproducible.futurePlan" = FALSE,
  "reproducible.inputPaths" = userInputPaths,
  "reproducible.nThreads" = 2,
  "reproducible.overwrite" = TRUE,
  "reproducible.quick" = FALSE,
  "reproducible.showSimilar" = TRUE,
  "reproducible.showSimilarDepth" = showSimilarDepth,
  "reproducible.useCache" = TRUE,
  "reproducible.useCloud" = TRUE,
  "reproducible.useGDAL" = FALSE, ## NOTE: GDAL can be faster, but mixing GDAL with raster causes inconsistencies
  "reproducible.useMemoise" = useMemoise,
  "reproducible.useNewDigestAlgorithm" = 2,
  "reproducible.useRequire" = TRUE,
  "spades.moduleCodeChecks" = FALSE,
  "spades.nThreads" = 4,
  "spades.recoveryMode" = recoveryMode,
  "spades.restartR.restartDir" = paths3$outputPath,
  "spades.useRequire" = FALSE # Don't use Require... meaning assume all pkgs installed
)

setDTthreads(DTthreads)

httr::set_config(httr::config(http_version = 0))

token <- NA_character_

if (is.na(token) || !file.exists(token))
  message(crayon::red("No Google service token found; authenticating with user token..."))

drive_auth(email = config::get("googleuser"), use_oob = quickPlot::isRstudioServer(),
           cache = config::get("googleauthcache"))

message(crayon::silver("Authenticating as: "), crayon::green(drive_user()$emailAddress))
