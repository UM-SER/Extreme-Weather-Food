# ==============================================================================
# 10_rebuild_tmmx_from_netcdf.R
# Purpose: Re-extract county daily tmmx from the raw gridMET NetCDF files with
#          the same method as 02_process_weather_nc.R, then check
#          (a) does it equal data/Weather/county_daily_tmmx_2018_2023.rds?
#          (b) does the rebuilt extreme-heat route index match panel_od_year_truck_sctg01_07.rds?
# Read-only on existing data; writes to tables/diagnostics/rebuild/
# ==============================================================================
suppressMessages({library(terra); library(sf); library(exactextractr); library(data.table); library(stringr)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
RB   <- file.path(OUT, "rebuild"); dir.create(RB, recursive = TRUE, showWarnings = FALSE)
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "10_log.txt"), split = TRUE)

counties <- st_read(file.path(ROOT, "data/Shapefile/cb_2017_us_county_20m.shp"), quiet = TRUE)
counties <- st_transform(counties, 4326); counties$GEOID <- pad5(counties$GEOID)

out_file <- file.path(RB, "rebuilt_tmmx_daily.rds")
if (!file.exists(out_file)) {
  res <- list()
  for (yr in 2018:2023) {
    f <- file.path(ROOT, sprintf("data/Weather/gridmet_raw/tmmx/tmmx_%d.nc", yr))
    r <- rast(f)
    cat(yr, ": layers", nlyr(r), " first time:", as.character(time(r)[1]), "\n")
    vals <- exact_extract(r, counties, "mean", progress = FALSE)
    ds <- seq(as.Date(paste0(yr, "-01-01")), as.Date(paste0(yr, "-12-31")), by = "day")
    k <- min(ncol(vals), length(ds)); vals <- vals[, 1:k]; ds <- ds[1:k]
    colnames(vals) <- as.character(ds)
    dt <- as.data.table(vals); dt[, GEOID := counties$GEOID]
    dl <- melt(dt, id.vars = "GEOID", variable.name = "date", value.name = "temp")
    dl[, `:=`(date = as.Date(as.character(date)), temp = temp - 273.15)]
    res[[as.character(yr)]] <- dl
    rm(r, vals, dt); gc()
  }
  rebuilt <- rbindlist(res); saveRDS(rebuilt, out_file)
} else rebuilt <- readRDS(out_file)

# (a) compare with existing daily file
old <- readRDS(file.path(ROOT, "data/Weather/county_daily_tmmx_2018_2023.rds")); setDT(old)
old[, `:=`(GEOID = pad5(GEOID), date = as.Date(date))]
cmp <- merge(rebuilt, old[, .(GEOID, date, temp_old = temp)], by = c("GEOID", "date"))
cmp[, year := as.integer(format(date, "%Y"))]
cat("\n(a) rebuilt vs existing county_daily_tmmx_2018_2023.rds\n")
print(cmp[, .(n = .N, share_equal = mean(abs(temp - temp_old) < 1e-4, na.rm = TRUE),
              mean_diff = mean(temp - temp_old, na.rm = TRUE), max_abs = max(abs(temp - temp_old), na.rm = TRUE)), by = year][order(year)])

# (b) rebuild route heat index for all routes and compare with panel
bh <- fread(file.path(ROOT, "data/Weather/county_baseline_tmmx_jja_1988_2017.csv"))[, GEOID := pad5(GEOID)]
rt <- fread(file.path(ROOT, "data/Route/od_route_county_segments.csv"), colClasses = list(character = c("dms_orig_GEOID","dms_dest_GEOID","geoid")))
rt[, `:=`(geoid = pad5(geoid), unit = paste0(pad5(dms_orig_GEOID), pad5(dms_dest_GEOID)))]
panel <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07.rds")))
cnty_units <- unique(panel[!grepl("^000", dms_orig_GEOID) & !grepl("^000", dms_dest_GEOID)]$unit)

route_index <- function(daily) {
  d <- merge(daily, bh, by = "GEOID")[, year := as.integer(format(date, "%Y"))]
  cnt <- d[, .(n = sum(temp > mean_JJA + 3 * sd_JJA, na.rm = TRUE)), by = .(GEOID, year)]
  m <- merge(rt[unit %in% cnty_units, .(unit, geoid)], cnt, by.x = "geoid", by.y = "GEOID", all.x = TRUE, allow.cartesian = TRUE)
  m[is.na(n), n := 0]
  m[, .(val = mean(n)), by = .(unit, year)]
}
score <- function(idx, label) {
  x <- merge(idx, panel[, .(unit, year, p = avgHighTempFreq_3sd)], by = c("unit", "year"))
  cat(sprintf("\n(b) %s: n=%d  share exact=%.3f  corr=%.4f\n", label, nrow(x), mean(abs(x$val - x$p) < 1e-6), cor(x$val, x$p)))
  print(x[, .(exact = round(mean(abs(val - p) < 1e-6), 3), mean_diff_panel_minus = round(mean(p - val), 3)), by = year][order(year)])
}
score(route_index(rebuilt), "rebuilt from NetCDF")
score(route_index(old[, .(GEOID, date, temp)]), "existing daily rds")
sink()
