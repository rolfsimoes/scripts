library(sits)

do_train <- function(proj_dir, ml_method) {

  stopifnot(dir.exists(proj_dir))
  stopifnot(is.function(ml_method))

  samples_file <- paste0(proj_dir, "/samples", "/samples.rds")
  stopifnot(file.exists(samples_file))

  model_outfile <- paste0(proj_dir, "/model", "/model.rds")

  stopifnot(!file.exists(model_outfile))

  samples <- readRDS(samples_file)

  model <- sits::sits_train(samples, ml_method = ml_method)

  saveRDS(model, file = model_outfile)

  model_outfile
}


# ---- script run ----
# library(sits)
#
# proj_dir <- "/Public/cerrado"
#
# do_train(proj_dir = proj_dir, ml_method = sits_rfor(num_trees = 2000))
