# ==============================================================================
# 11_check_route_mean_denominator.R
# Purpose: In 03_calculate_weighted_index.R, heat/cold county counts keep every
#          county-year (zeros included), but precipitation/wind counts keep only
#          county-years with >= 1 event. Test which definition the panel's avg*
#          and distw* variables follow:
#            "all"  = mean over ALL traversed counties (zeros included)
#            "pos"  = mean over traversed counties with >= 1 event that year
# Read-only.
# ==============================================================================
suppressMessages({library(data.table); library(stringr)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "11_log.txt"), split = TRUE)

panel <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07.rds")))
rt <- fread(file.path(ROOT, "data/Route/od_route_county_segments.csv"), colClasses = list(character = c("dms_orig_GEOID","dms_dest_GEOID","geoid")))
rt[, `:=`(geoid = pad5(geoid), unit = paste0(pad5(dms_orig_GEOID), pad5(dms_dest_GEOID)))]
units <- unique(panel[!grepl("^000", dms_orig_GEOID) & !grepl("^000", dms_dest_GEOID) & dms_orig_GEOID != dms_dest_GEOID]$unit)
R <- rt[unit %in% units, .(unit, geoid, w = distance / total_distance)]
grid <- CJ(i = seq_len(nrow(R)), year = 2018:2023); grid <- cbind(R[grid$i], year = grid$year)

check <- function(cnt, avg_var, distw_var, label) {
  m <- merge(grid, cnt, by.x = c("geoid", "year"), by.y = c("GEOID", "year"), all.x = TRUE)
  idx <- m[, .(avg_all = mean(fifelse(is.na(n), 0, n)),
               avg_pos = if (any(!is.na(n))) mean(n, na.rm = TRUE) else 0,
               distw   = sum(w * fifelse(is.na(n), 0, n))), by = .(unit, year)]
  x <- merge(idx, panel[, .(unit, year, pa = get(avg_var), pd = get(distw_var))], by = c("unit", "year"))
  ex <- function(a, b) round(mean(abs(a - b) < 1e-6), 3)
  cat(sprintf("%-14s n=%d | avg var: match 'all' %.3f, match 'pos' %.3f | distw var: match %.3f\n",
              label, nrow(x), ex(x$avg_all, x$pa), ex(x$avg_pos, x$pa), ex(x$distw, x$pd)))
  cat(sprintf("               mean of panel avg = %.3f ; correct 'all' mean = %.3f ; corr(panel, all) = %.3f\n",
              mean(x$pa), mean(x$avg_all), cor(x$pa, x$avg_all)))
  invisible(x)
}
daily <- function(v) { d <- readRDS(file.path(ROOT, sprintf("data/Weather/county_daily_%s_2018_2023.rds", v))); setDT(d)
  d[, `:=`(GEOID = pad5(GEOID), year = as.integer(format(as.Date(date), "%Y")))]; d }

pr <- daily("pr"); x_pr <- check(pr[pr > 50, .(n = .N), by = .(GEOID, year)], "avgHighPrecFreq", "distwHighPrecFreq", "Heavy precip"); rm(pr); gc(verbose = FALSE)
vs <- daily("vs"); x_vs <- check(vs[vs > 10, .(n = .N), by = .(GEOID, year)], "avgHighVsFreq", "distwHighVsFreq", "High wind");   rm(vs); gc(verbose = FALSE)

bh <- fread(file.path(ROOT, "data/Weather/county_baseline_tmmx_jja_1988_2017.csv"))[, GEOID := pad5(GEOID)]
tx <- merge(daily("tmmx"), bh, by = "GEOID")
check(tx[, .(n = sum(temp > mean_JJA + 3 * sd_JJA, na.rm = TRUE)), by = .(GEOID, year)], "avgHighTempFreq_3sd", "distwHighTempFreq_3sd", "Extreme hot"); rm(tx); gc(verbose = FALSE)
bc <- fread(file.path(ROOT, "data/Weather/county_baseline_tmmn_djf_1988_2017.csv"))[, GEOID := pad5(GEOID)]
tn <- merge(daily("tmmn"), bc, by = "GEOID")
check(tn[, .(n = sum(temp < mean_DJF - 3 * sd_DJF, na.rm = TRUE)), by = .(GEOID, year)], "avgLowTempFreq_3sd", "distwLowTempFreq_3sd", "Extreme cold"); rm(tn); gc(verbose = FALSE)

# save corrected precip/wind route means for a possible re-estimation
fix <- merge(x_pr[, .(unit, year, avgHighPrecFreq_all = avg_all)], x_vs[, .(unit, year, avgHighVsFreq_all = avg_all)], by = c("unit", "year"))
saveRDS(fix, file.path(OUT, "corrected_avg_precip_wind_county_routes.rds"))
sink()
