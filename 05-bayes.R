img_open <- function(img_lst, blk_rows, blk_cols, ovr_rows = 0, ovr_cols = 0) {

  img <- sf:::CPL_read_gdal(img_lst[[1]],
                            options = character(0),
                            driver = character(0),
                            read_data = FALSE,
                            NA_value = numeric(0),
                            RasterIO_parameters = list())

  img_rows <- img$rows[[2]]
  img_cols <- img$cols[[2]]

  x_off <- ceiling(seq(1, img_cols, by = blk_cols))
  x_size <- diff(c(x_off, img_cols + 1))
  ovr_x_off <- c(1, c(x_off[-1] - ovr_cols))
  ovr_x_size <- c(x_off[-1] + ovr_cols, img_cols) - ovr_x_off

  y_off <- ceiling(seq(1, img_rows, by = blk_rows))
  y_size <- diff(c(y_off, img_rows + 1))
  ovr_y_off <- c(1, c(y_off[-1] - ovr_rows))
  ovr_y_size <- c(y_off[-1] + ovr_rows, img_rows) - ovr_y_off

  structure(list(
    x_blocks = length(x_off),
    y_blocks = length(y_off),
    x_off = x_off,
    y_off = y_off,
    x_size = x_size,
    y_size = y_size,
    ovr_x_off = ovr_x_off,
    ovr_y_off = ovr_y_off,
    ovr_x_size = ovr_x_size,
    ovr_y_size = ovr_y_size,
    img_lst = img_lst
  ), class = "img")
}

img_read_block <- function(img, x_block, y_block) {

  stopifnot(inherits(img, "img"))
  stopifnot(x_block >= 1)
  stopifnot(y_block >= 1)
  stopifnot(x_block <= img$x_blocks)
  stopifnot(y_block <= img$y_blocks)

  options <- list(nXOff = img$ovr_x_off[[x_block]],
                  nYOff = img$ovr_y_off[[y_block]],
                  nXSize = img$ovr_x_size[[x_block]],
                  nYSize = img$ovr_y_size[[y_block]],
                  nBufXSize = img$ovr_x_size[[x_block]],
                  nBufYSize = img$ovr_y_size[[y_block]])

  data <- sf:::CPL_read_gdal(img$img_lst[[1]],
                            options = character(0),
                            driver = character(0),
                            read_data = TRUE,
                            NA_value = numeric(0),
                            RasterIO_parameters = options)

  structure(list(
    data = attr(data, "data"),
    driver = data$driver[[1]],
    datatype = data$datatype,
    wkt = data$crs$wkt,
    geotransform = data$geotransform,
    x_from = img$x_off[[x_block]],
    y_from = img$y_off[[y_block]],
    x_off = img$x_off[[x_block]] - img$ovr_x_off[[x_block]] + 1,
    y_off = img$y_off[[y_block]] - img$ovr_y_off[[y_block]] + 1,
    x_size = img$x_size[[x_block]],
    y_size = img$y_size[[y_block]]
  ), class = "img_chunk")
}

img <- img_open(logit1_outfile, blk_rows = 50, blk_cols = 50,
                ovr_rows = 4, ovr_cols = 4)

test <- img_read_block(img, 2, 2)

img_write_block <- function(test, file = "test.tif") {

  sf:::CPL_write_gdal(x,
                      fname = file,
                      driver = "GTiff",
                      options = "COMPRESS=LZW",
                      Type = "Float32",
                      dims = , IntegerVector from,
                      NumericVector gt, CharacterVector p4s, NumericVector na_val,
                      bool create = true, bool only_create = false)
  test$data[test$x_off + seq_len(test$x_size),
            test$y_off + seq_len(test$y_size),]
}

chunk_io_fun <- function(block, x, fun, ...) {

  # crop adding overlaps
  x <- raster::crop(x, raster::extent(x,
                                      r1 = block$r1,
                                      r2 = block$r2,
                                      c1 = 1,
                                      c2 = ncol(x)))

  # process it
  res <- fun(x, ...)
  stopifnot(inherits(res, c("RasterLayer", "RasterStack", "RasterBrick")))

  # crop removing overlaps
  res <- raster::crop(res, raster::extent(res,
                                          r1 = block$orig1,
                                          r2 = block$orig2,
                                          c1 = 1,
                                          c2 = ncol(res)))

  # export to temp file
  filename <- tempfile(fileext = ".tif")
  raster::writeRaster(res, filename = filename, overwrite = TRUE)

  filename
}

split_clusterR <- function(x, n_tiles, pad_rows, fun,
                           args = NULL, export = NULL, cl = NULL, ...) {

  stopifnot(n_tiles > 0)
  stopifnot(pad_rows >= 0)

  breaks <- ceiling(seq(1, nrow(x) + 1, length.out = n_tiles + 1))
  breaks <- mapply(list,
                   r1 = ifelse(breaks - pad_rows <= 0, 1,
                               breaks - pad_rows)[seq_len(n_tiles)],
                   r2 = ifelse(breaks + pad_rows - 1 > nrow(x), nrow(x),
                               breaks + pad_rows - 1)[-1:0], SIMPLIFY = FALSE,
                   orig1 = ifelse(breaks - pad_rows <= 0, 1,
                                  pad_rows + 1)[seq_len(n_tiles)],
                   orig2 = ifelse(breaks - pad_rows <= 0,
                                  breaks[-1:0] - breaks[seq_len(n_tiles)],
                                  breaks[-1:0] - breaks[seq_len(n_tiles)]
                                  + pad_rows + 1)[-1:0])

  # if (is.null(cl)) {
  #
  #   cl <- raster::getCluster()
  #   on.exit(raster::returnCluster(), add = TRUE)
  #   stopifnot(!is.null(cl))
  # }

  # export

  # if (!is.null(export)) {
  #
  #   parallel::clusterExport(cl, export)
  # }

  # start process cluster
  pb <- txtProgressBar(max = length(breaks) + 1, style = 3)

  .arg_fun <- function(i) {

    setTxtProgressBar(pb, i)
    c(list(b = breaks[[i]]), x = x, fun = fun, args)
  }

  tmp_tiles <- snow::dynamicClusterApply(cl = cl,
                                         fun = cluster_io_fun,
                                         n = length(breaks),
                                         argfun = .arg_fun)

  setTxtProgressBar(pb, length(breaks) + 1)
  close(pb)
  on.exit(unlink(tmp_tiles))
  # end process cluster

  # merge to save final result with '...' parameters
  message("Merging files...", appendLF = TRUE)
  do.call(raster::merge, c(lapply(tmp_tiles, raster::brick), list(...)))
}

# ---- Gaussian filter ----
gauss_experiment <- function(b2, window) {

  # .libPaths("/home/rolf.simoes/R/x86_64-pc-linux-gnu-library/4.0")
  # require("sits")

  stopifnot(inherits(b2, "RasterBrick"))
  stopifnot(is.matrix(window))

  # generate cube smooth
  bb <- raster::brick(b2, nl = raster::nlayers(b2))

  for (i in seq_len(raster::nlayers(b2))) {

    # layer <- matrix(
    #     as.matrix(b2[[i]][]),
    #     nrow = raster::nrow(b2),
    #     ncol = raster::ncol(b2),
    #     byrow = TRUE)
    #
    # ds <- sits:::kernel_estimator(data = layer, kernel = window)
    ds <- raster::focal(b2[[i]], w = window)

    bb[[i]] <- ds
  }

  raster::calc(bb, fun = function(x) {
    x / rowSums(x) * 10000
  })
}

process_gaussian <- function(probs, window, sigma_param) {

  probs_file <- raster::filename(probs)

  # filename
  cube_filename <- paste0(dirname(probs_file),
                          "/gauss_post",
                          "_w", dim(window)[[1]],
                          "_sig", paste0(sigma_param),
                          ".tif")
  # process
  message("Processing Gaussian Smoothing...", appendLF = TRUE)
  gauss <-
    split_clusterR(probs,
                   n_tiles = 100,
                   pad_rows = ceiling(nrow(window) / 2) - 1,
                   fun = gauss_experiment,
                   args = list(window = window),
                   filename = cube_filename,
                   options = "COMPRESS=LZW",
                   datatype = "INT2U",
                   overwrite = TRUE)

  # generate map
  map_filename <- paste0(dirname(probs_file),
                         "/gauss_map",
                         "_w", dim(window)[[1]],
                         "_sig", paste0(sigma_param),
                         ".tif")

  map_gauss <-
    split_clusterR(gauss,
                   n_tiles = 100,
                   pad_rows = 0,
                   fun = raster::which.max,
                   filename = map_filename,
                   options = "COMPRESS=LZW",
                   datatype = "INT1U",
                   overwrite = TRUE)

  list(map = map_gauss,
       gauss = gauss,
       sigma = sigma_param)
}

# ---- bilinear filter ----
bilinear_experiment <- function(b2, window, tau, scale_factor = 0.0001) {

  .libPaths("/home/rolf.simoes/R/x86_64-pc-linux-gnu-library/4.0")
  require("sits")

  stopifnot(inherits(b2, "RasterBrick"))
  stopifnot(is.matrix(window))

  # generate cube smooth
  bb <- raster::brick(b2, nl = raster::nlayers(b2))

  for (i in seq_len(raster::nlayers(b2))) {

    layer <- matrix(
      as.matrix(b2[[i]][]),
      nrow = raster::nrow(b2),
      ncol = raster::ncol(b2),
      byrow = TRUE)

    ds <- sits:::kernel_estimator_non_linear(data = layer,
                                             kernel = window,
                                             tau = tau,
                                             scale_factor = scale_factor)
    # ds <- raster::focal(b2[[i]], w = window)

    bb[[i]][] <- c(ds)
  }

  raster::calc(bb, fun = function(x) {
    x * 10000
  })
}

process_bilinear <- function(probs, window, sigma_param, tau_param) {

  probs_file <- raster::filename(probs)

  # filename
  cube_filename <- paste0(dirname(probs_file),
                          "/bilin_post",
                          "_w", dim(window)[[1]],
                          "_sig", paste0(sigma_param),
                          "_tau", paste0(tau_param),
                          ".tif")
  # process
  message("Processing Bilinear Smoothing...", appendLF = TRUE)
  bilin <-
    split_clusterR(probs,
                   n_tiles = 100,
                   pad_rows = ceiling(nrow(window) / 2) - 1,
                   fun = bilinear_experiment,
                   args = list(window = window,
                               tau = tau_param,
                               scale_factor = 0.0001),
                   filename = cube_filename,
                   options = "COMPRESS=LZW",
                   datatype = "FLT8S",
                   overwrite = TRUE)

  # generate map
  map_filename <- paste0(dirname(probs_file),
                         "/bilin_map",
                         "_w", dim(window)[[1]],
                         "_sig", paste0(sigma_param),
                         "_tau", paste0(tau_param),
                         ".tif")

  map_bilin <-
    split_clusterR(bilin,
                   n_tiles = 100,
                   pad_rows = 0,
                   fun = raster::which.max,
                   filename = map_filename,
                   options = "COMPRESS=LZW",
                   datatype = "INT1U",
                   overwrite = TRUE)

  list(map = map_bilin,
       gauss = bilin,
       sigma = sigma_param,
       tau = tau_param)
}

# ---- Bayesian filter ----
bayes_experiment <- function(b2, window, nu, sigma, covar) {

  stopifnot(inherits(b2, "RasterBrick"))
  stopifnot(is.matrix(window))
  stopifnot(is.matrix(sigma))


  d <- unname(raster::values(b2))

  # process
  ds <- bmv::bayes_multiv_smooth(m = d,
                                 m_nrow = raster::nrow(b2),
                                 m_ncol = raster::ncol(b2),
                                 w = window,
                                 sigma = sigma,
                                 nu = nu,
                                 covar = covar)

  # generate cube smooth
  bb <- raster::brick(b2, nl = raster::nlayers(b2))
  bb[] <- ds

  bb
}

process_bayesian <- function(logit, window, sigma_param, covar, cl) {

  nu <- 1

  stopifnot(is.matrix(window))
  stopifnot(is.matrix(sigma))

  logit_file <- raster::filename(logit)
  out_dir <- paste0(dirname(logit_file), "/",
                    sub("^(.*)\\..*$", "\\1", basename(logit_file)))
  if (!dir.exists(out_dir)) {
    message(paste0("Creating dir '", out_dir, "'"), appendLF = TRUE)
    dir.create(out_dir)
  }
  stopifnot(dir.exists(out_dir))

  # filename
  cube_filename <- paste0(out_dir,
                          "/bnm",
                          "_w", dim(window)[[1]],
                          "_detsig", paste0(round(det(sigma_param), 4)),
                          ".tif")
  # process
  message("Processing Bayesian Smoothing...", appendLF = TRUE)
  post_bayes_mv <-
    split_clusterR(logit,
                   n_tiles = length(cl),
                   pad_rows = ceiling(nrow(window) / 2) - 1,
                   fun = bayes_experiment,
                   args = list(window = window,
                               sigma = sigma_param,
                               nu = nu,
                               covar = covar),
                   cl = cl,
                   filename = cube_filename,
                   options = "COMPRESS=LZW",
                   datatype = "FLT4S",
                   overwrite = TRUE)

  # save covariance matrix used for smooth
  covmtx_filename <- paste0(out_dir,
                            "/bnm",
                            "_w", dim(window)[[1]],
                            "_detsig", paste0(round(det(sigma_param), 4)),
                            ".rds")
  saveRDS(sigma_param, file = covmtx_filename)

  # generate map
  map_filename <- paste0(out_dir,
                         "/map",
                         "_w", dim(window)[[1]],
                         "_detsig", paste0(round(det(sigma_param), 4)),
                         ".tif")

  map_post_bayes_mv <-
    split_clusterR(post_bayes_mv,
                   n_tiles = length(cl),
                   pad_rows = 0,
                   fun = raster::which.max,
                   cl = cl,
                   filename = map_filename,
                   options = "COMPRESS=LZW",
                   datatype = "INT1U",
                   overwrite = TRUE)

  list(map = map_post_bayes_mv,
       post_bayes = post_bayes_mv,
       sigma = sigma_param)
}

# ---- b2 functions ----

b2_fix_probs <- function(probs, ...) {
  stopifnot(inherits(probs, "RasterBrick"))

  raster::calc(probs, function(x) {

    maxprob <- 10000 - ncol(x) + 1
    x[x == 0] <- 1
    x[x > maxprob] <- maxprob
    x
  }, ...)
}

b2_logit <- function(probs, ...) {
  stopifnot(inherits(probs, "RasterBrick"))

  raster::calc(probs, fun = function(x) {

    log(x / (rowSums(x) - x))
  }, ...)
}

b2_export_functions <- function(cl) {
  snow::clusterExport(cl, list = list(
    "b2_logit",
    "b2_fix_probs"
  ))
}

# ---- other functions ----

matrix_const <- function(const, dim) {

  stopifnot(is.numeric(const))
  sigma <- matrix(0, nrow = dim,
                  ncol = dim)
  diag(sigma) <- const
  sigma
}

# ---- run script ----

library(snow)
cl <- snow::makeSOCKcluster(20)



library(raster)
probs1 <- raster::brick("public/rolf/uncertainty/class/uncertainty_077095_probs_2018_7_2019_7_roi1.tif")
logit1_outfile <- paste0(dirname(raster::filename(probs1)),
                         "/logit_",
                         basename(raster::filename(probs1)))
if (file.exists(logit1_outfile))
  logit1 <- raster::brick(logit1_outfile)

logit1 <-
  probs1 %>%
  split_clusterR(n_tiles = 20,
                 pad_rows = 0,
                 fun = function(x) {
                   b2_logit(b2_fix_probs(x))
                 },
                 export = c("b2_logit", "b2_fix_probs"),
                 filename = logit1_outfile,
                 options = "COMPRESS=LZW",
                 datatype = "FLT4S",
                 cl = cl)

# 9 x 9 window
window <- matrix(1, nrow = 9, ncol = 9)

# sigma
sigma <- matrix(c(10,  0,  0,  0,  0,
                   0, 10,  0,  0,  0,
                   0,  0, 10,  0,  0,
                   0,  0,  0, 10,  0,
                   0,  0,  0,  0, 10), nrow = 5, ncol = 5, byrow = TRUE)
# labels:
#  1 Abandoned
#  2 Deforestation
#  3 Forest
#  4 NatNonForest
#  5 Pasture

bnm1 <-
  logit1 %>%
  process_bayesian(window = window,
                   sigma_param = sigma,
                   covar = TRUE,
                   cl = cl)

sits_labels(readRDS("~/public/rolf/uncertainty/samples/samples.rds"))

snow::stopCluster(cl)



raster::rasterOptions(progress = "text", timer = TRUE,
                      format = "GTiff")
raster::beginCluster(20)

logit_file <- paste0("/home/alber.ipia/Documents/",
                     "sits_classify_S2_10_16D_STK_077095/",
                     "results/paper_defor/",
                     "bmv_logit_S2_10_16D_STK_077095_probs_2018_7.tif")
probs_file <- paste0("/home/alber.ipia/Documents/",
                     "sits_classify_S2_10_16D_STK_077095/",
                     "results/paper_defor/",
                     "bmv_fixprobs_S2_10_16D_STK_077095_probs_2018_7.tif")

logit1 <- raster::brick(logit_file)
probs1 <- raster::brick(probs_file)


#### EXPER1: constant smooth ####
window <- raster::focalWeight(logit1, d = 45, type = "rectangle")
window[window > 0] <- 1
window

# use LOGIT
process_bayesian(logit = logit1, window = window,
                 sigma_param = matrix_const(10, 4),
                 covar = FALSE, nu = 1)


#### EXPER2: multivariate bayes ####

# sigma: from probs.R
sigma_file <- paste0("/home/alber.ipia/Documents/",
                     "sits_classify_S2_10_16D_STK_077095/results/paper_defor/",
                     "bmv_post_w15_nu1_covTRUE.rds")
sigma <- readRDS(sigma_file)
det(sigma)
# create window
window <- raster::focalWeight(logit1, d = 45, type = "rectangle")
window[window > 0] <- 1
window[ceiling(nrow(window)/2), ceiling(ncol(window)/2)] <- 0
window

# use LOGIT
res2 <- process_bayesian(logit = logit1, window = window,
                         sigma_param = sigma, covar = TRUE, nu = 1)

#### EXPER3: gaussian ####

# sits gauss window
# window_size <- 15
# sigma <- 3
# gauss_kernel <- matrix(1, nrow = window_size, ncol = window_size)
# center_i <- floor(window_size / 2 + 1)
# center_j <- floor(window_size / 2 + 1)
# for (i in 1:window_size) {
#     for (j in 1:window_size) {
#         h <- (i - center_i)^2 + (j - center_j)^2
#         gauss_kernel[i,j] <- exp(-h/(2*sigma^2))
#     }
# }

# create window
sigma_param <- 2
window <- raster::focalWeight(logit1,
                              d = c(sigma_param * raster::xres(logit1), 45),
                              type = "Gauss")
image(window)

# use PROBS
res3 <- process_gaussian(probs = probs1, window = window,
                         sigma_param = sigma_param)


#### EXPER4: bilinear filtering ####

sigma_param <- 2
window <- raster::focalWeight(logit1,
                              d = c(sigma_param * raster::xres(logit1), 45),
                              type = "Gauss")
image(window)

tau_param <- 0.25

# use PROBS
res3 <- process_bilinear(probs = probs1, window = window,
                         sigma_param = sigma_param, tau_param = tau_param)

#### end experiments ####

raster::endCluster()

plot(res$map)
plot(res$logit)

source("~/dbxcli.R")
dbxcli_put(local_dir = "~/uncertainty/class/actv1",
           dest_dir = "/Rolf/uncertainty/class/actv1",
           pattern = "^(logit_mv|map_mv).*\\.(tif|csv)$",
           dry_run = F)

# plot original map (before smoothing)
plot(generate_map(raster::values(b2), b2))





# b <- raster::brick("~/uncertainty/class/actv1/bayes_logit_s2_stac_077095_probs_2018_7_2019_7_roi1.tif")
# b2 <- raster::crop(b, raster::extent(b, r1 = 600, r2 = 1200, c1 = 400, c2 = 1200))
# plot(b2)
# d <- unname(raster::values(b2))
# d <- unname(raster::values(b))
# dw <- neighborhood(m = d, m_nrow = raster::nrow(b), m_ncol = raster::ncol(b),
#                    w = focalWCirc,
#                    m_i = 600, m_j = 400);image(matrix(dw[,1], nrow = 15))
# var(dw)

# from probs.R script
# sigma <- unname(sigma0)
# dw[5,]

# c(post_mean_x(x = dw[5,], sigma = sigma, mu0 = apply(dw, 2, mean),
#               sigma0 = cov(dw), nu = 20))

# d[1:4,]
# b[2]
#
#######################################

# compute logit

# load data
# probs_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/S2_10_16D_STK_077095_probs_2018_7.tif"
#
# raster::beginCluster(n = 20)
#
# raster::rasterOptions(datatype = "INT2U", progress = "text", timer = TRUE,
#                       format = "GTiff")
#
# probs1 <-
#     raster::brick(probs_file) %>%
#     raster::clusterR(fun = fix_probs,
#                      filename = paste0(dirname(probs_file),
#                                        "/bmv_fixprobs_",
#                                        basename(probs_file)),
#                      options = "COMPRESS=LZW",
#                      overwrite = TRUE)
#
# raster::rasterOptions(datatype = "FLT8S", progress = "text", timer = TRUE,
#                       format = "GTiff")
#
# odds1 <-
#     probs1 %>%
#     raster::clusterR(fun = compute_odds,
#                      filename = paste0(dirname(probs_file),
#                                        "/bmv_odds_",
#                                        basename(probs_file)),
#                      options = "COMPRESS=LZW",
#                      overwrite = TRUE)
#
# logit1 <-
#     odds1 %>%
#     raster::clusterR(fun = log,
#                      filename = paste0(dirname(probs_file),
#                                        "/bmv_logit_",
#                                        basename(probs_file)),
#                      options = "COMPRESS=LZW",
#                      overwrite = TRUE)

# b <- raster::brick("~/uncertainty/class/actv1/bayes_logit_s2_stac_077095_probs_2018_7_2019_7_roi1.tif")
# b2 <- raster::crop(b, raster::extent(b,
#                                      r1 = 600, r2 = 1200,
#                                      c1 = 400, c2 = 1200))


#########################################
# raster::rasterOptions(datatype = "FLT8S", progress = "text", timer = TRUE,
#                       format = "GTiff")
#
# raster::writeRaster(bb, filename = "~/uncertainty/class/actv1/bayes_multivar.tif",
#                     options = c("COMPRESS=LZW"),
#                     overwrite = TRUE)
#
# library(raster)
#
