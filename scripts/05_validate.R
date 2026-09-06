#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(xml2)
  library(yaml)
})

root <- normalizePath(getwd())
config <- read_yaml(file.path(root, "config.yml"))
derived <- file.path(root, "data", "derived")
site <- file.path(root, "site")
qa <- file.path(root, "output", "qa")
dir.create(qa, recursive = TRUE, showWarnings = FALSE)

checks <- list()
record <- function(name, condition, detail) {
  passed <- isTRUE(condition)
  checks[[length(checks) + 1L]] <<- list(name = name, passed = passed, detail = as.character(detail))
  cat(sprintf("[%s] %s — %s\n", if (passed) "PASS" else "FAIL", name, detail))
}

metrics <- fread(file.path(derived, "country_metrics.csv"))
rankings <- fread(file.path(derived, "country_rankings.csv"))
coverage <- fread(file.path(derived, "country_coverage.csv"))
thresholds <- fread(file.path(derived, "threshold_distribution.csv"))
distribution <- fread(file.path(derived, "density_distribution.csv"))
expected <- unlist(config$analysis$expected_countries)

record("country inventory", setequal(metrics$country_code, expected) && nrow(metrics) == 30L, "30 expected EU/EFTA country files")
record("coverage inventory", setequal(coverage$country_code, expected), "coverage table matches configured countries")
record("threshold inventory", nrow(thresholds) == 30L * length(config$analysis$thresholds), "one row per country and threshold")
record("density-band inventory", nrow(distribution) == 30L * 21L, "21 complete density bands for each country")
distribution_totals <- distribution[, .(share = sum(population_share)), by = country_code]
record("density-band conservation", all(abs(distribution_totals$share - 1) < 1e-9), "population shares sum to one by country")
record("published population values", all(metrics$missing_t_cells == 0), "no missing national T values in spatial cells")
record("unallocated disclosure", all(metrics$allocated_population_share >= 0.98), sprintf("minimum allocated share %.3f%%", 100 * min(metrics$allocated_population_share)))

spain <- metrics[country_code == "ES"]
spain_ranks <- rankings[country_code == "ES"]
rank_of <- function(metric_name) spain_ranks[metric == metric_name]$rank
record("Spain conventional-density claim", rank_of("conventional_density") <= 15, sprintf("rank %.0f of 30 from lowest density", rank_of("conventional_density")))
record("Spain populated-land claim", rank_of("occupied_land_share") == 3, "third-smallest populated-land share")
record("Spain resident-median claim", rank_of("population_weighted_median") == 1, "highest population-weighted median")
record("Spain 90-percent claim", rank_of("land_for_90pct_population") == 1, "smallest land share for 90% of residents")
record("Spain allocation", spain$allocated_population_share > 0.999, sprintf("%.4f%% allocated", 100 * spain$allocated_population_share))

required_assets <- c(
  sprintf("europe-density-%d.%s", rep(c(960, 1600, 2400), each = 2), rep(c("png", "webp"), 3)),
  sprintf("spain-density-%d.%s", rep(c(960, 1600, 2400), each = 2), rep(c("png", "webp"), 3))
)
asset_paths <- file.path(site, "assets", required_assets)
record("responsive maps", all(file.exists(asset_paths) & file.info(asset_paths)$size > 1000), "12 non-empty PNG/WebP assets")

doc <- read_html(file.path(site, "index.html"))
record("document language", xml_attr(xml_find_first(doc, "//html"), "lang") == "en", "English is the clean-URL default")
record("canonical URL", xml_attr(xml_find_first(doc, "//link[@rel='canonical']"), "href") == "https://danielalmazan.com/density-europe/", "canonical is configured")
record("image alternatives", all(nzchar(xml_attr(xml_find_all(doc, "//main//img"), "alt"))), "all story images have alt text")
record("language controls", length(xml_find_all(doc, "//*[@data-lang='en' or @data-lang='es']")) == 2L, "EN/ES controls present")
record("map legend labels", length(xml_find_all(doc, "//*[@data-i18n='legendTitle']")) == 2L, "both map legends identify the measurement")
record("portfolio return link", xml_attr(xml_find_first(doc, "//footer//a[contains(@class,'footer-home')]"), "href") == "https://danielalmazan.com/", "footer links to danielalmazan.com")

js <- readLines(file.path(site, "app.js"), warn = FALSE)
record("Spanish route behavior", any(grepl('searchParams.set\\("lang","es"\\)', js)), "?lang=es is preserved in the URL")
record("Spanish copy", any(grepl("¿País vacío", js, fixed = TRUE)), "Spanish title and article copy embedded")
record("focus-country set", any(grepl('focusCodes=["ES","FR","PT","IT","DE","NL","EL"]', js, fixed = TRUE)), "Greece replaces Finland, Sweden and Belgium")

data_js <- paste(readLines(file.path(site, "data.js"), warn = FALSE), collapse = "\n")
json_text <- sub("^window[.]DENSITY_EU_DATA = ", "", data_js)
json_text <- sub(";[[:space:]]*$", "", json_text)
payload <- fromJSON(json_text)
record("web-data inventory", nrow(payload$metrics) == 30L, "30 countries embedded in site/data.js")
record("web density bands", nrow(payload$density_distribution) == 630L, "density distributions embedded for all 30 countries")
record("web-data provenance", identical(payload$source$version, "3.0"), "source version embedded")

passed <- sum(vapply(checks, `[[`, logical(1), "passed"))
report <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  passed = passed,
  total = length(checks),
  success = passed == length(checks),
  checks = checks
)
write_json(report, file.path(qa, "validation.json"), pretty = TRUE, auto_unbox = TRUE)

if (!report$success) stop(sprintf("Validation failed: %d of %d checks passed.", passed, length(checks)), call. = FALSE)
cat(sprintf("Validation passed: %d/%d checks.\n", passed, length(checks)))
