library(sits)

# define a classification routine
do_classify <- function(proj_dir, cube, roi = NULL, version = "v1",
                        memsize = 20, multicores = 20) {

  stopifnot(dir.exists(proj_dir))

  model_file <- paste0(proj_dir, "/model", "/model.rds")
  output_dir <- paste0(proj_dir, "/class")

  stopifnot(file.exists(model_file))

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  stopifnot(dir.exists(output_dir))

  model <- readRDS(file = model_file)

  probs <- sits::sits_classify(data = cube, ml_model = model, roi = roi,
                               memsize = memsize, multicores = multicores,
                               output_dir = output_dir, version = version)

  maps <- sits::sits_label_classification(probs, output_dir = output_dir,
                                          version = version)

  list(probs = probs, maps = maps)
}

# # ---- script run ----
#
# proj_dir <- "/Public/cerrado"
#
# # ---- classification ----
#
# # CHECK for BDC_ACCESS_TOKEN environment variable
#
# lc8_cube <- sits_cube(type        = "BDC",
#                       url         = "http://datacube-005.dpi.inpe.br:8010/stac/",
#                       name        = "cerrado",
#                       bands       = c("BAND1", "BAND2", "BAND3", "BAND4",
#                                       "BAND5", "BAND6", "BAND7", "EVI",
#                                       "NDVI", "FMASK4"),
#                       collection  = "LC8_30_16D_STK-1",
#                       tiles       = c("042050", "044049", "044046"),
#                       start_date  = "2017-09-01",
#                       end_date    = "2018-08-31")
#
# library(randomForest)
# cerrado <- do_classify(proj_dir = proj_dir, cube = lc8_cube,
#                        memsize = 100, multicores = 40)
