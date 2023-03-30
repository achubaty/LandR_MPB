## user + machine specific configs

.fitUsing <- if (grepl("for-cast[.]ca", .nodename)) 1 else 0

config.user <- switch(
  .user,

  ## Alex ------------------------------------------------------------------------------------------
  achubaty = list(
    args = list(
      cloud = list(
        googleUser = "achubaty@for-cast.ca",
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = "@alex.chubaty"
      ),
      reupload = TRUE,
      usePrerun = FALSE
    ),
    options = list(
      reproducible.cacheSaveFormat = "qs",
      reproducible.conn = SpaDES.config::dbConnCache("postgresql")
    ),
    params = list(
      fireSense_IgnitionFit = list(
        cores = switch(
          .nodename,
          "pinus.for-cast.ca" = 8L, ## ~200 GB
          "picea.for-cast.ca" = 16L,
          "pseudotsuga.for-cast.ca" = 16L, ## ~400 GB
          1L
        )
      ),
      fireSense_SpreadFit = list(
        cores = switch(
          .nodename,
          "pinus.for-cast.ca" = {
            switch(
              .fitUsing,
              `1` = rep("pseudotsuga.for-cast.ca", 100),
              `2` = c(rep("picea.for-cast.ca", 32), rep("pseudotsuga.for-cast.ca", 68)),
              `3` = c(rep("localhost", 8), rep("picea.for-cast.ca", 25), rep("pseudotsuga.for-cast.ca", 67)),
              `4` = c(rep("localhost", 32), rep("picea.for-cast.ca", 48), rep("pseudotsuga.for-cast.ca", 20)),
              `5` = c(rep("localhost", 32), rep("pseudotsuga.for-cast.ca", 68))
            )
          },
          "picea.for-cast.ca" = {
            switch(
              .fitUsing,
              `2` = c(rep("localhost", 68), rep("pinus.for-cast.ca", 32)),
              `3` = c(rep("localhost", 25), rep("pinus.for-cast.ca", 8), rep("pseudotsuga.for-cast.ca", 67)),
              `4` = c(rep("localhost", 48), rep("pinus.for-cast.ca", 32), rep("pseudotsuga.for-cast.ca", 20)),
              `5` = c(rep("pinus.for-cast.ca", 32), rep("pseudotsuga.for-cast.ca", 68))
            )
          },
          "pseudotsuga.for-cast.ca" = {
            rep("localhost", 100)
          }
        )
      )
    ),
    paths = list(
      scratchPath = switch(.nodename,
                           `larix.for-cast.ca` = file.path("/tmp/scratch", basename(prjDir)),
                           file.path("/mnt/scratch", .user, basename(prjDir)))
    )
  ),

  ## TODO: eliot config

  ## docker (user rstudio) -------------------------------------------------------------------------
  rstudio = list(
    args = list(
      cloud = list(
        googleUser = "", ## TODO
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = "" ## TODO
      )
    ),
    paths = list(
      cachePath = "cache_sqlite"
    )
  ),

  ## default (i.e, no changes) ---------------------------------------------------------------------
  list()
)
