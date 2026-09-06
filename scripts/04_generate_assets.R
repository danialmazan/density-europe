#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ragg)
  library(sf)
  library(terra)
  library(yaml)
})

root <- normalizePath(getwd())
config <- read_yaml(file.path(root, "config.yml"))
raw_dir <- file.path(root, "data", "raw")
asset_dir <- file.path(root, "site", "assets")
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

population <- rast(file.path(raw_dir, "ESTAT_Census_2021_V3.tiff"))[["T"]]
land <- rast(file.path(raw_dir, "ESTAT_Census_2021_LAND_SURFACE_V3.tiff"))
population <- ifel(land > 0, population, NA)
boundaries <- st_read(file.path(raw_dir, "CNTR_RG_20M_2024_3035.geojson"), quiet = TRUE)
expected <- unlist(config$analysis$expected_countries)
boundaries <- boundaries[boundaries$CNTR_ID %in% expected, ]

breaks <- c(-0.5, 0.5, 10, 100, 1000, 5000, Inf)
colours <- unlist(config$map$colours)
paper <- "#f4f0e8"
ink <- "#27302c"

draw_raster <- function(x, outlines, extent, width, height, output, inset = FALSE) {
  agg_png(output, width = width, height = height, units = "px", res = 144, background = paper)
  on.exit(dev.off(), add = TRUE)
  par(mar = rep(0, 4), xaxs = "i", yaxs = "i", bg = paper)
  plot(crop(x, extent), col = colours, breaks = breaks, axes = FALSE, legend = FALSE, box = FALSE, maxcell = Inf)
  crop_box <- st_bbox(c(
    xmin = xmin(extent), xmax = xmax(extent), ymin = ymin(extent), ymax = ymax(extent)
  ), crs = st_crs(3035))
  plot(st_geometry(st_crop(outlines, crop_box)), add = TRUE, border = ink, lwd = max(0.35, width / 2600), col = NA)
  if (inset) box(col = ink, lwd = 1)
}

render_europe <- function(width) {
  height <- round(width * 0.69)
  output <- file.path(asset_dir, sprintf("europe-density-%d.png", width))
  draw_raster(population, boundaries, ext(2500000, 6600000, 1300000, 5450000), width, height, output)
}

render_spain <- function(width) {
  height <- round(width * 0.70)
  output <- file.path(asset_dir, sprintf("spain-density-%d.png", width))
  agg_png(output, width = width, height = height, units = "px", res = 144, background = paper)
  par(mar = rep(0, 4), xaxs = "i", yaxs = "i", bg = paper)
  mainland_extent <- ext(2350000, 3850000, 1450000, 2500000)
  spain <- boundaries[boundaries$CNTR_ID == "ES", ]
  spain_vector <- vect(spain)
  mainland_population <- mask(crop(population, mainland_extent), spain_vector)
  plot(mainland_population, col = colours, breaks = breaks, axes = FALSE, legend = FALSE, box = FALSE, maxcell = Inf)
  mainland_box <- st_bbox(c(xmin = 2350000, xmax = 3850000, ymin = 1450000, ymax = 2500000), crs = st_crs(3035))
  plot(st_geometry(st_crop(spain, mainland_box)), add = TRUE, border = ink, lwd = max(0.5, width / 2200), col = NA)
  par(fig = c(0.02, 0.25, 0.04, 0.29), new = TRUE, mar = rep(0, 4), xaxs = "i", yaxs = "i")
  canary_extent <- ext(1500000, 2050000, 900000, 1180000)
  canary_population <- mask(crop(population, canary_extent), spain_vector)
  plot(canary_population, col = colours, breaks = breaks, axes = FALSE, legend = FALSE, box = FALSE, maxcell = Inf)
  canary_box <- st_bbox(c(xmin = 1500000, xmax = 2050000, ymin = 900000, ymax = 1180000), crs = st_crs(3035))
  plot(st_geometry(st_crop(spain, canary_box)), add = TRUE, border = ink, lwd = max(0.5, width / 2200), col = NA)
  box(col = ink, lwd = 1)
  dev.off()
}

widths <- c(960L, 1600L, 2400L)
invisible(lapply(widths, render_europe))
invisible(lapply(widths, render_spain))

cwebp <- Sys.which("cwebp")
if (nzchar(cwebp)) {
  pngs <- list.files(asset_dir, pattern = "density-[0-9]+[.]png$", full.names = TRUE)
  for (png in pngs) {
    webp <- sub("[.]png$", ".webp", png)
    status <- system2(cwebp, c("-quiet", "-q", "82", shQuote(png), "-o", shQuote(webp)))
    if (status != 0) stop("cwebp failed for ", png, call. = FALSE)
  }
}

cat("Responsive Europe and Spain map assets generated.\n")
