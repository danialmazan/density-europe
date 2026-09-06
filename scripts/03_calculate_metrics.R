#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(yaml)
})

root <- normalizePath(getwd())
source(file.path(root, "R", "grid_helpers.R"))
config <- read_yaml(file.path(root, "config.yml"))
interim_dir <- file.path(root, "data", "interim")
derived_dir <- file.path(root, "data", "derived")
qa_dir <- file.path(root, "output", "qa")
site_dir <- file.path(root, "site")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

cells <- as.data.table(readRDS(file.path(interim_dir, "country_grid_cells.rds")))
thresholds <- as.numeric(unlist(config$analysis$thresholds))
targets <- as.numeric(unlist(config$analysis$population_targets))

metric_for_country <- function(code) {
  all_rows <- cells[country_code == code]
  x <- all_rows[is_unallocated == FALSE]
  valid <- x[is.finite(population)]
  total_land <- sum(x$land_surface_km2, na.rm = TRUE)
  allocated_population <- sum(valid$population)
  unallocated_population <- sum(all_rows[is_unallocated == TRUE]$population, na.rm = TRUE)
  total_population <- allocated_population + unallocated_population
  pw_mean <- sum(valid$population^2) / allocated_population
  adjusted_keep <- is.finite(valid$land_surface_km2) & valid$land_surface_km2 > 0
  adjusted_population <- sum(valid$population[adjusted_keep])
  adjusted_density <- valid$population[adjusted_keep] / valid$land_surface_km2[adjusted_keep]
  adjusted_pw_mean <- sum(valid$population[adjusted_keep] * (valid$population[adjusted_keep] / valid$land_surface_km2[adjusted_keep])) / adjusted_population
  out <- data.table(
    country_code = code,
    country = unname(country_names[code]),
    cells = nrow(x),
    population_allocated = allocated_population,
    population_unallocated = unallocated_population,
    population_total = total_population,
    allocated_population_share = allocated_population / total_population,
    land_area_km2 = total_land,
    conventional_density = total_population / total_land,
    conventional_density_allocated = allocated_population / total_land,
    occupied_cell_share = mean(x$populated == 1, na.rm = TRUE),
    positive_cell_share = mean(x$population > 0, na.rm = TRUE),
    occupied_land_share = sum(x$land_surface_km2[x$populated == 1], na.rm = TRUE) / total_land,
    population_weighted_mean = pw_mean,
    population_weighted_median = weighted_quantile_step(valid$population, valid$population, 0.5),
    population_weighted_mean_land_adjusted = adjusted_pw_mean,
    population_weighted_median_land_adjusted = weighted_quantile_step(adjusted_density, valid$population[adjusted_keep], 0.5),
    land_adjusted_population_share = adjusted_population / allocated_population,
    resident_density_uplift = pw_mean / (total_population / total_land),
    effective_occupied_area_km2 = allocated_population / pw_mean,
    effective_occupied_area_share = (allocated_population / pw_mean) / total_land,
    land_weighted_gini = weighted_gini_by_land(valid$population, valid$land_surface_km2),
    missing_t_cells = sum(!is.finite(x$population)),
    zero_population_but_populated = sum(x$population == 0 & x$populated == 1, na.rm = TRUE),
    populated_zero_land = sum(x$populated == 1 & x$land_surface_km2 <= 0, na.rm = TRUE)
  )
  for (threshold in thresholds) {
    key <- paste0("ge", threshold)
    selected <- valid$population >= threshold
    out[[paste0(key, "_cell_share")]] <- mean(selected)
    out[[paste0(key, "_land_share")]] <- sum(valid$land_surface_km2[selected], na.rm = TRUE) / total_land
    out[[paste0(key, "_population_share")]] <- sum(valid$population[selected]) / allocated_population
  }
  concentration <- rbindlist(lapply(targets, function(target) {
    as.data.table(land_for_population_share(valid$population, valid$land_surface_km2, target))
  }))
  for (i in seq_len(nrow(concentration))) {
    label <- sprintf("%d", round(concentration$target[i] * 100))
    out[[paste0("land_for_", label, "pct_population")]] <- concentration$land_share[i]
    out[[paste0("land_for_", label, "pct_population_lower")]] <- concentration$lower_land_share[i]
    out[[paste0("land_for_", label, "pct_population_upper")]] <- concentration$upper_land_share[i]
  }
  adjusted_concentration <- rbindlist(lapply(targets, function(target) {
    as.data.table(land_for_population_share(
      valid$population[adjusted_keep], valid$land_surface_km2[adjusted_keep],
      target, adjusted_density
    ))
  }))
  for (i in seq_len(nrow(adjusted_concentration))) {
    label <- sprintf("%d", round(adjusted_concentration$target[i] * 100))
    out[[paste0("land_for_", label, "pct_population_land_adjusted")]] <- adjusted_concentration$land_share[i]
  }
  out
}

metrics <- rbindlist(lapply(sort(unique(cells$country_code)), metric_for_country), fill = TRUE)

rank_specs <- list(
  conventional_density = FALSE,
  occupied_land_share = FALSE,
  population_weighted_mean = TRUE,
  population_weighted_median = TRUE,
  resident_density_uplift = TRUE,
  land_for_50pct_population = FALSE,
  land_for_80pct_population = FALSE,
  land_for_90pct_population = FALSE,
  land_weighted_gini = TRUE,
  effective_occupied_area_share = FALSE
)
rankings <- rbindlist(lapply(names(rank_specs), function(metric) {
  values <- metrics[[metric]]
  descending <- rank_specs[[metric]]
  data.table(
    country_code = metrics$country_code,
    country = metrics$country,
    metric = metric,
    value = values,
    rank = frank(if (descending) -values else values, ties.method = "min", na.last = "keep")
  )
}))

curves <- rbindlist(lapply(metrics$country_code, function(code) {
  x <- cells[country_code == code & is_unallocated == FALSE & is.finite(population)]
  curve <- as.data.table(concentration_curve(x$population, x$land_surface_km2))
  curve[, `:=`(country_code = code, country = unname(country_names[code]))]
  curve
}))
setcolorder(curves, c("country_code", "country", "population_share", "land_share"))

distribution_source <- cells[is_unallocated == FALSE & is.finite(population)]
distribution_source[, bucket_lower := pmin(floor(population / 1000) * 1000, 20000)]
density_distribution <- distribution_source[, .(
  population = sum(population),
  grid_cells = .N
), by = .(country_code, bucket_lower)]
density_distribution <- merge(
  CJ(country_code = sort(unique(metrics$country_code)), bucket_lower = seq(0, 20000, 1000), unique = TRUE),
  density_distribution,
  by = c("country_code", "bucket_lower"),
  all.x = TRUE,
  sort = TRUE
)
density_distribution[is.na(population), `:=`(population = 0, grid_cells = 0L)]
density_distribution[, country := unname(country_names[country_code])]
density_distribution[, population_share := population / sum(population), by = country_code]
density_distribution[, bucket_label := fifelse(
  bucket_lower == 20000,
  "20,000+",
  sprintf("%s–%s", format(bucket_lower, big.mark = ",", scientific = FALSE, trim = TRUE), format(bucket_lower + 999, big.mark = ",", scientific = FALSE, trim = TRUE))
)]
setcolorder(density_distribution, c("country_code", "country", "bucket_lower", "bucket_label", "population", "population_share", "grid_cells"))

top_cells <- cells[is_unallocated == FALSE & is.finite(population)][order(-population)][1:100]
top_cells[, country := unname(country_names[country_code])]
top_cells <- top_cells[, .(country_code, country, grid_id, population, land_surface_km2, populated, easting, northing)]

border_audit <- cells[is_unallocated == FALSE, .(
  reporting_countries = uniqueN(country_code),
  country_codes = paste(sort(unique(country_code)), collapse = "-"),
  reported_population = sum(population, na.rm = TRUE),
  reported_land_surface_km2 = sum(land_surface_km2, na.rm = TRUE)
), by = grid_id][reporting_countries > 1]
setorder(border_audit, -reporting_countries, grid_id)

reference_json <- fromJSON(file.path(root, "data", "raw", "demo_urespop_2021.json"), simplifyVector = FALSE)
geo_positions <- unlist(reference_json$dimension$geo$category$index)
reference_values <- unlist(reference_json$value)
national_reference <- data.table(
  country_code = names(geo_positions),
  position = as.integer(geo_positions)
)
national_reference[, national_population_reference := as.numeric(reference_values[match(as.character(position), names(reference_values))])]
national_reference[, national_reference_label := reference_json$label]
metrics <- merge(metrics, national_reference[, .(country_code, national_population_reference, national_reference_label)], by = "country_code", all.x = TRUE, sort = FALSE)
metrics[, national_population_difference := population_total - national_population_reference]
metrics[, national_population_difference_share := national_population_difference / national_population_reference]

fwrite(metrics, file.path(derived_dir, "country_metrics.csv"))
fwrite(rankings, file.path(derived_dir, "country_rankings.csv"))
fwrite(curves, file.path(derived_dir, "concentration_curves.csv"))
fwrite(density_distribution, file.path(derived_dir, "density_distribution.csv"))
fwrite(top_cells, file.path(derived_dir, "top_cells.csv"))
fwrite(border_audit, file.path(derived_dir, "border_cell_audit.csv"))

threshold_distribution <- rbindlist(lapply(seq_len(nrow(metrics)), function(i) {
  rbindlist(lapply(thresholds, function(threshold) {
    key <- paste0("ge", threshold)
    data.table(
      country_code = metrics$country_code[i],
      country = metrics$country[i],
      threshold = threshold,
      cell_share = metrics[[paste0(key, "_cell_share")]][i],
      land_share = metrics[[paste0(key, "_land_share")]][i],
      population_share = metrics[[paste0(key, "_population_share")]][i]
    )
  }))
}))
fwrite(threshold_distribution, file.path(derived_dir, "threshold_distribution.csv"))

peer_codes <- unlist(config$analysis$peers)
peer_metrics <- metrics[country_code %in% peer_codes, .(
  country_code, country, conventional_density,
  population_weighted_mean, population_weighted_mean_land_adjusted,
  population_weighted_median, population_weighted_median_land_adjusted,
  land_for_50pct_population, land_for_50pct_population_land_adjusted,
  land_for_90pct_population, land_for_90pct_population_land_adjusted,
  land_adjusted_population_share, allocated_population_share,
  national_population_reference, national_population_difference_share
)]
peer_rankings <- rankings[country_code %in% peer_codes]
fwrite(peer_metrics, file.path(qa_dir, "method_sensitivity.csv"))

display_metrics <- metrics[, .(
  country_code, country, population_total, land_area_km2,
  conventional_density, occupied_land_share,
  population_weighted_mean, population_weighted_median,
  resident_density_uplift, land_for_50pct_population,
  land_for_80pct_population, land_for_90pct_population,
  land_weighted_gini, effective_occupied_area_share,
  allocated_population_share
)]

payload <- list(
  generated = format(Sys.Date(), "%Y-%m-%d"),
  source = list(title = config$source$title, version = config$source$version, release_date = config$source$release_date),
  metrics = display_metrics,
  rankings = rankings[metric %in% c("conventional_density", "occupied_land_share", "population_weighted_median", "resident_density_uplift", "land_for_50pct_population")],
  curves = curves,
  density_distribution = density_distribution
)
writeLines(
  paste0("window.DENSITY_EU_DATA = ", toJSON(payload, dataframe = "rows", auto_unbox = TRUE, digits = 10, na = "null"), ";"),
  file.path(site_dir, "data.js")
)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
qa_columns <- c(
  "country", "conventional_density", "occupied_land_share",
  "population_weighted_mean", "population_weighted_median",
  "resident_density_uplift", "land_for_50pct_population",
  "land_for_90pct_population", "land_weighted_gini",
  "allocated_population_share", "missing_t_cells", "populated_zero_land"
)
qa_table <- metrics[match(c(peer_codes, setdiff(metrics$country_code, peer_codes)), country_code), ..qa_columns]
header <- paste(sprintf("<th>%s</th>", html_escape(names(qa_table))), collapse = "")
rows <- apply(qa_table, 1, function(row) paste0("<tr>", paste(sprintf("<td>%s</td>", html_escape(row)), collapse = ""), "</tr>"))
writeLines(c(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>Candidate metrics QA</title><style>body{font:14px system-ui;margin:2rem;color:#17201d}table{border-collapse:collapse;white-space:nowrap}th,td{padding:.45rem .65rem;border-bottom:1px solid #ccd2ce;text-align:right}th:first-child,td:first-child{text-align:left;position:sticky;left:0;background:white}thead{position:sticky;top:0;background:#183229;color:white}div{overflow:auto;max-height:85vh}</style></head><body>",
  "<h1>Candidate country metrics</h1><p>Spain and requested peers appear first. Values are unrounded audit outputs.</p><div><table><thead><tr>",
  header, "</tr></thead><tbody>", rows, "</tbody></table></div></body></html>"
), file.path(qa_dir, "candidate_metrics.html"))

cat("Candidate metrics calculated.\n")
print(metrics[country_code %in% peer_codes, .(
  country, conventional_density, occupied_land_share,
  population_weighted_median, resident_density_uplift,
  land_for_50pct_population, land_for_90pct_population
)])
cat("\nSpain ranks:\n")
print(rankings[country_code == "ES"])
