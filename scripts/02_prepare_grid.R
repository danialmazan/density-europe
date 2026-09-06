#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
})

root <- normalizePath(getwd())
source(file.path(root, "R", "grid_helpers.R"))
config <- read_yaml(file.path(root, "config.yml"))
raw_dir <- file.path(root, "data", "raw")
interim_dir <- file.path(root, "data", "interim")
derived_dir <- file.path(root, "data", "derived")
dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

zip_path <- file.path(raw_dir, "ESTAT_Census_2021_country.csv.zip")
stop_if(!file.exists(zip_path), "Country archive is missing; run make data first.")

members <- unzip(zip_path, list = TRUE)$Name
members <- members[grepl("CENSUS_GRID_N_[A-Z]{2}_2021[.]csv$", members)]
codes <- substr(basename(members), 15L, 16L)
expected <- unlist(config$analysis$expected_countries)
stop_if(!setequal(codes, expected), "Country files do not match the expected 30-country inventory.")

read_country <- function(member, code) {
  command <- sprintf(
    "unzip -p %s %s | awk -F, 'NR==1 || $3==\"T\"'",
    shQuote(zip_path), shQuote(member)
  )
  x <- fread(cmd = command, showProgress = FALSE, encoding = "UTF-8")
  setnames(x, sub("^﻿", "", names(x)))
  required <- c("STAT", "SPATIAL", "OBS_VALUE", "LAND_SURFACE", "POPULATED", "SPECIAL_VALUE")
  stop_if(!all(required %in% names(x)), paste("Missing columns for", code))
  x <- x[STAT == "T"]
  x[, country_code := code]
  x[, source_spatial_id := SPATIAL]
  x[, is_unallocated := grepl("unallocated$", SPATIAL, ignore.case = TRUE)]
  x[, grid_id := sub(paste0("^", code, "_"), "", SPATIAL)]
  x[, population_raw := as.numeric(OBS_VALUE)]
  x[, population := fifelse(population_raw >= 0, population_raw, NA_real_)]
  x[, missing_reason := fifelse(population_raw == -8888, "confidential",
    fifelse(population_raw == -9999, "other_unavailable", NA_character_))]
  x[, land_surface_km2 := as.numeric(LAND_SURFACE)]
  x[, populated := as.integer(POPULATED)]
  x[, northing := fifelse(!is_unallocated, parse_grid_coordinate(grid_id, "northing"), NA_real_)]
  x[, easting := fifelse(!is_unallocated, parse_grid_coordinate(grid_id, "easting"), NA_real_)]
  x[, .(
    country_code, source_spatial_id, grid_id, is_unallocated,
    population_raw, population, missing_reason, land_surface_km2,
    populated, northing, easting
  )]
}

country_tables <- vector("list", length(members))
for (i in seq_along(members)) {
  message(sprintf("Reading %s (%d/%d)", codes[i], i, length(members)))
  country_tables[[i]] <- read_country(members[i], codes[i])
}
cells <- rbindlist(country_tables, use.names = TRUE)
setorder(cells, country_code, is_unallocated, grid_id)

stop_if(anyDuplicated(cells[, .(country_code, source_spatial_id)]) > 0, "Duplicate country/grid records found.")
stop_if(any(!cells$is_unallocated & (!is.finite(cells$northing) | !is.finite(cells$easting))), "Grid coordinate parsing failed.")

saveRDS(cells, file.path(interim_dir, "country_grid_cells.rds"), compress = "xz")

inventory <- cells[, .(
  spatial_cells = sum(!is_unallocated),
  unallocated_records = sum(is_unallocated),
  missing_confidential = sum(missing_reason == "confidential", na.rm = TRUE),
  missing_other = sum(missing_reason == "other_unavailable", na.rm = TRUE),
  zero_population_but_populated = sum(!is_unallocated & population == 0 & populated == 1, na.rm = TRUE),
  populated_zero_land = sum(!is_unallocated & populated == 1 & land_surface_km2 <= 0, na.rm = TRUE),
  land_area_km2 = sum(land_surface_km2[!is_unallocated], na.rm = TRUE),
  allocated_population = sum(population[!is_unallocated], na.rm = TRUE),
  unallocated_population = sum(population[is_unallocated], na.rm = TRUE)
), by = country_code]
inventory[, country := unname(country_names[country_code])]
setcolorder(inventory, c("country_code", "country", setdiff(names(inventory), c("country_code", "country"))))
fwrite(inventory, file.path(derived_dir, "country_coverage.csv"))

cat(sprintf("Prepared %s country-cell records across %d countries.\n", format(nrow(cells), big.mark = ","), uniqueN(cells$country_code)))
