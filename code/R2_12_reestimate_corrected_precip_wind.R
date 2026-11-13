# ==============================================================================
# 12_reestimate_corrected_precip_wind.R
# Purpose: Rebuild avgHighPrecFreq and avgHighVsFreq as the mean over ALL counties
#          a route traverses (zero-event counties count as 0), for every route,
#          then re-estimate the baseline model. panel_od_year_truck_sctg01_07.rds is not changed.
#          State-level zones ("000xx" intra-zone routes) use the mean over all
#          counties in the state, zeros included.
# ==============================================================================
suppressMessages({library(data.table); library(stringr); library(fixest)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "12_log.txt"), split = TRUE)

panel <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07.rds")))
# 2026-10-26: AK/HI routes kept so all tables use the full sample (N = 74,238)
# panel <- panel[!dms_orig %in% c(20, 151, 159) & !dms_dest %in% c(20, 151, 159)]
rt <- fread(file.path(ROOT, "data/Route/od_route_county_segments.csv"), colClasses = list(character = c("dms_orig_GEOID","dms_dest_GEOID","geoid")))
rt[, `:=`(geoid = pad5(geoid), unit = paste0(pad5(dms_orig_GEOID), pad5(dms_dest_GEOID)))]
R <- rt[unit %in% panel$unit, .(unit, geoid)]
cat("Routes in panel:", uniqueN(panel$unit), " with route-table rows:", uniqueN(R$unit), "\n")

county_counts <- function(v, thr) {
  d <- readRDS(file.path(ROOT, sprintf("data/Weather/county_daily_%s_2018_2023.rds", v))); setDT(d)
  d[, `:=`(GEOID = pad5(GEOID), year = as.integer(format(as.Date(date), "%Y")))]
  cc <- d[, .(n = sum(get(v) > thr, na.rm = TRUE)), by = .(GEOID, year)]          # all county-years, zeros kept
  st <- cc[, .(n = mean(n)), by = .(GEOID = pad5(substr(GEOID, 1, 2)), year)]     # state means incl. zeros
  rbind(cc, st)
}
route_avg <- function(cc, name) {
  g <- CJ(i = seq_len(nrow(R)), year = 2018:2023); g <- cbind(R[g$i], year = g$year)
  m <- merge(g, cc, by.x = c("geoid", "year"), by.y = c("GEOID", "year"), all.x = TRUE)
  cat(name, ": route-county-years with no weather record:", sum(is.na(m$n)), "\n")
  m[, .(val = mean(n, na.rm = TRUE)), by = .(unit, year)][, setnames(.SD, "val", name)]
}
pr <- route_avg(county_counts("pr", 50), "avgHighPrecFreq_all")
vs <- route_avg(county_counts("vs", 10), "avgHighVsFreq_all")
d <- merge(merge(panel, pr, by = c("unit", "year"), all.x = TRUE), vs, by = c("unit", "year"), all.x = TRUE)
# gridMET does not cover Alaska/Hawaii (and one route lacks route-table rows): keep the panel value there
cat("Rows without a corrected value (kept at panel value):", d[!is.finite(avgHighPrecFreq_all), .N], "\n")
d[!is.finite(avgHighPrecFreq_all), avgHighPrecFreq_all := avgHighPrecFreq]
d[!is.finite(avgHighVsFreq_all), avgHighVsFreq_all := avgHighVsFreq]
cat("Panel rows:", nrow(d), " missing corrected precip:", sum(is.na(d$avgHighPrecFreq_all)), " wind:", sum(is.na(d$avgHighVsFreq_all)), "\n")
cat("\nMeans: precip panel", round(mean(d$avgHighPrecFreq), 3), "corrected", round(mean(d$avgHighPrecFreq_all, na.rm = TRUE), 3),
    "| wind panel", round(mean(d$avgHighVsFreq), 3), "corrected", round(mean(d$avgHighVsFreq_all, na.rm = TRUE), 3), "\n")
cat("SD:    precip panel", round(sd(d$avgHighPrecFreq), 3), "corrected", round(sd(d$avgHighPrecFreq_all, na.rm = TRUE), 3),
    "| wind panel", round(sd(d$avgHighVsFreq), 3), "corrected", round(sd(d$avgHighVsFreq_all, na.rm = TRUE), 3), "\n")

x_old <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd", "avgHighPrecFreq", "avgHighVsFreq")
x_new <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd", "avgHighPrecFreq_all", "avgHighVsFreq_all")
dict <- c(total_tons = "Tons", total_tmiles = "Ton-miles", avgHighTempFreq_3sd = "Extreme hot days", avgLowTempFreq_3sd = "Extreme cold days",
          avgHighPrecFreq = "Heavy precipitation days", avgHighVsFreq = "High wind days",
          avgHighPrecFreq_all = "Heavy precipitation days", avgHighVsFreq_all = "High wind days")
for (y in c("total_tons", "total_tmiles")) {
  f <- function(x) feols(as.formula(paste(y, "~", paste(x, collapse = " + "), "| unit + year")), cluster = ~unit, data = d)
  ms <- list("Panel as built" = f(x_old), "Corrected precip & wind" = f(x_new))
  print(etable(ms, dict = dict, signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05)))
  etable(ms, dict = dict, signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
         title = paste("Baseline with corrected route averages for precipitation and wind:", dict[y]),
         file = file.path(OUT, paste0("F_corrected_precip_wind_", y, ".txt")), replace = TRUE)
}
saveRDS(d[, .(unit, year, avgHighPrecFreq_all, avgHighVsFreq_all)], file.path(OUT, "corrected_avg_precip_wind.rds"))
sink()
