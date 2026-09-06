country_names <- c(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria", CH = "Switzerland",
  CY = "Cyprus", CZ = "Czechia", DE = "Germany", DK = "Denmark",
  EE = "Estonia", EL = "Greece", ES = "Spain", FI = "Finland",
  FR = "France", HR = "Croatia", HU = "Hungary", IE = "Ireland",
  IT = "Italy", LI = "Liechtenstein", LT = "Lithuania", LU = "Luxembourg",
  LV = "Latvia", MT = "Malta", NL = "Netherlands", NO = "Norway",
  PL = "Poland", PT = "Portugal", RO = "Romania", SE = "Sweden",
  SI = "Slovenia", SK = "Slovakia"
)

stop_if <- function(condition, message) {
  if (isTRUE(condition)) stop(message, call. = FALSE)
}

weighted_quantile_step <- function(value, weight, probability) {
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  value <- value[keep]
  weight <- weight[keep]
  stop_if(!length(value), "Weighted quantile has no positive-weight observations.")
  order_index <- order(value)
  value <- value[order_index]
  weight <- weight[order_index]
  value[which(cumsum(weight) >= sum(weight) * probability)[1]]
}

parse_grid_coordinate <- function(grid_id, axis) {
  pattern <- if (axis == "northing") ".*mN(-?[0-9]+)E.*" else ".*E(-?[0-9]+)$"
  suppressWarnings(as.numeric(sub(pattern, "\\1", grid_id)))
}

weighted_gini_by_land <- function(population, land_area) {
  keep <- is.finite(population) & population >= 0 & is.finite(land_area) & land_area > 0
  population <- population[keep]
  land_area <- land_area[keep]
  total_population <- sum(population)
  total_land <- sum(land_area)
  if (!length(population) || total_population <= 0 || total_land <= 0) return(NA_real_)
  density <- population / land_area
  order_index <- order(density, population)
  x <- c(0, cumsum(land_area[order_index]) / total_land)
  y <- c(0, cumsum(population[order_index]) / total_population)
  1 - 2 * sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

land_for_population_share <- function(population, land_area, target, density = population) {
  keep <- is.finite(population) & population >= 0 & is.finite(land_area) & land_area >= 0 & is.finite(density)
  x <- data.table::data.table(
    density = density[keep], population = population[keep], land_area = land_area[keep]
  )
  x <- x[, .(group_population = sum(population), land_area = sum(land_area)), by = density]
  data.table::setorder(x, -density)
  total_population <- sum(x$group_population)
  total_land <- sum(x$land_area)
  cumulative_population <- cumsum(x$group_population)
  cumulative_land <- cumsum(x$land_area)
  index <- which(cumulative_population >= total_population * target)[1]
  population_before <- if (index == 1) 0 else cumulative_population[index - 1]
  land_before <- if (index == 1) 0 else cumulative_land[index - 1]
  remaining_population <- total_population * target - population_before
  fraction <- if (x$group_population[index] > 0) remaining_population / x$group_population[index] else 0
  interpolated_land <- land_before + fraction * x$land_area[index]
  data.frame(
    target = target,
    land_share = interpolated_land / total_land,
    lower_land_share = land_before / total_land,
    upper_land_share = cumulative_land[index] / total_land
  )
}

concentration_curve <- function(population, land_area, steps = 100L, density = population) {
  keep <- is.finite(population) & population >= 0 & is.finite(land_area) & land_area >= 0 & is.finite(density)
  x <- data.table::data.table(
    density = density[keep], population = population[keep], land_area = land_area[keep]
  )
  x <- x[, .(group_population = sum(population), land_area = sum(land_area)), by = density]
  data.table::setorder(x, -density)
  total_population <- sum(x$group_population)
  total_land <- sum(x$land_area)
  cumulative_population <- c(0, cumsum(x$group_population) / total_population)
  cumulative_land <- c(0, cumsum(x$land_area) / total_land)
  targets <- seq(0, 1, length.out = steps + 1L)
  data.frame(
    population_share = targets,
    land_share = approx(cumulative_population, cumulative_land, xout = targets, ties = "ordered", rule = 2)$y
  )
}
