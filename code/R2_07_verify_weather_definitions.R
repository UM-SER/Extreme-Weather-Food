# ==============================================================================
# 07_verify_weather_definitions.R
# Purpose: Check which thresholds generated the weather variables in
#          panel_od_year_truck_sctg01_07.rds, by rebuilding route indices from the daily
#          county files and comparing with the panel (read-only).
#   - Wind: > 8 m/s (manuscript) vs > 10 m/s (code)
#   - Heat: tmmx > mean_JJA + 3*sd_JJA, sd = interannual SD of JJA means
#   - Precip: > 50 mm
# ==============================================================================
suppressMessages({library(data.table); library(dplyr); library(stringr)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "07_log.txt"), split = TRUE)

panel <- readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07.rds"))
rt <- fread(file.path(ROOT, "data/Route/od_route_county_segments.csv"),
            colClasses = list(character = c("dms_orig_GEOID", "dms_dest_GEOID", "geoid")))
rt[, `:=`(geoid = pad5(geoid), unit = paste0(pad5(dms_orig_GEOID), pad5(dms_dest_GEOID)))]

set.seed(1)
units_chk <- sample(unique(panel$unit[panel$dms_orig_GEOID != panel$dms_dest_GEOID &
                                      !grepl("^000", panel$dms_orig_GEOID) & !grepl("^000", panel$dms_dest_GEOID)]), 300)
rt_s <- rt[unit %in% units_chk]
ctys <- unique(rt_s$geoid)

route_mean <- function(counts) {           # counts: GEOID, year, n
  m <- merge(rt_s[, .(unit, geoid)], counts, by.x = "geoid", by.y = "GEOID", all.x = TRUE, allow.cartesian = TRUE)
  m[is.na(n), n := 0]
  m[, .(val = mean(n)), by = .(unit, year)]
}
compare <- function(rebuilt, var, label) {
  x <- merge(rebuilt, as.data.table(panel)[unit %in% units_chk, .(unit, year, panel = get(var))], by = c("unit", "year"))
  cat(sprintf("%-32s n=%d  corr=%.4f  max|diff|=%.4f  share exact(<1e-6)=%.3f\n",
              label, nrow(x), cor(x$val, x$panel), max(abs(x$val - x$panel)), mean(abs(x$val - x$panel) < 1e-6)))
}

vs <- readRDS(file.path(ROOT, "data/Weather/county_daily_vs_2018_2023.rds")); setDT(vs)
vs <- vs[GEOID %in% ctys][, year := as.integer(format(as.Date(date), "%Y"))]
compare(route_mean(vs[vs > 8,  .(n = .N), by = .(GEOID, year)]), "avgHighVsFreq", "Wind > 8 m/s")
compare(route_mean(vs[vs > 10, .(n = .N), by = .(GEOID, year)]), "avgHighVsFreq", "Wind > 10 m/s")
rm(vs); gc()

pr <- readRDS(file.path(ROOT, "data/Weather/county_daily_pr_2018_2023.rds")); setDT(pr)
pr <- pr[GEOID %in% ctys][, year := as.integer(format(as.Date(date), "%Y"))]
compare(route_mean(pr[pr > 50, .(n = .N), by = .(GEOID, year)]), "avgHighPrecFreq", "Precip > 50 mm")
rm(pr); gc()

tx <- readRDS(file.path(ROOT, "data/Weather/county_daily_tmmx_2018_2023.rds")); setDT(tx)
tx <- tx[GEOID %in% ctys][, year := as.integer(format(as.Date(date), "%Y"))]
bh <- fread(file.path(ROOT, "data/Weather/county_baseline_tmmx_jja_1988_2017.csv"))[, GEOID := pad5(GEOID)]
tx <- merge(tx, bh, by = "GEOID")
compare(route_mean(tx[temp > mean_JJA + 3 * sd_JJA, .(n = .N), by = .(GEOID, year)]), "avgHighTempFreq_3sd", "Hot: mean_JJA + 3*sd_JJA")
cat("\nBaseline sd_JJA (interannual SD of JJA mean tmax), deg C:\n"); print(summary(bh$sd_JJA))
cat("Implied hot threshold above JJA mean (3*sd), deg C:\n"); print(summary(3 * bh$sd_JJA))
sink()
