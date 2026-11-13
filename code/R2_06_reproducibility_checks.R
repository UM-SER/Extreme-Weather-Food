# ==============================================================================
# 06_reproducibility_checks.R
# Purpose: Evidence for Reviewer #2, Major Issue 2 (Comments 17-26).
#   A. Sample-size reconciliation (C21)
#   B. Routing endpoints actually used by route101125.R (C19)
#   C. Road vs great-circle distance; flag suspect routes (C21/C23)
#   D. FAF dist_band vs OSRM route distance (C24)
#   E. Robustness: drop suspect routes, drop intra-zone routes,
#      distance-weighted exposure (C21/C23). Baseline is NOT modified.
#   F. OSRM package defaults (C18)
# Read-only on all existing data; writes only to tables/diagnostics
# ==============================================================================

suppressMessages({
  library(dplyr); library(sf); library(stringr); library(data.table)
  library(fixest); library(readxl); library(tidyr)
})

ROOT    <- "."
DATA    <- file.path(ROOT, "data")
OUT     <- file.path(ROOT, "tables/diagnostics")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
log_file <- file.path(OUT, "06_log.txt")
sink(log_file, split = TRUE)

ak_hi <- c(20, 151, 159)
weather_vars <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd", "avgHighPrecFreq", "avgHighVsFreq")

# --- Load ---------------------------------------------------------------------
panel_raw <- readRDS(file.path(DATA, "panel_od_year_truck_sctg01_07_all_routes.rds"))  # 2026-11-08: all 12,373 routes, before the contiguous-U.S. restriction
# 2026-10-26: AK/HI routes kept so all tables use the full sample (N = 74,238)
# panel <- panel_raw %>% filter(!dms_orig %in% ak_hi & !dms_dest %in% ak_hi)
panel <- panel_raw
rt <- fread(file.path(DATA, "Route/od_route_county_segments.csv"),
            colClasses = list(character = c("dms_orig_GEOID", "dms_dest_GEOID", "geoid")))
rt[, `:=`(dms_orig_GEOID = pad5(dms_orig_GEOID), dms_dest_GEOID = pad5(dms_dest_GEOID))]
rt[, unit := paste0(dms_orig_GEOID, dms_dest_GEOID)]
gpkg <- st_read(file.path(DATA, "Route/od_route_geometries_conus.gpkg"), quiet = TRUE)
gpkg_units <- paste0(pad5(gpkg$dms_orig_GEOID), pad5(gpkg$dms_dest_GEOID))
lookup <- readRDS(file.path(DATA, "Route/faf_zone_lookup.rds")) %>%
  mutate(final_geoid = pad5(final_geoid))

# ==============================================================================
# A. Sample size (C21)
# ==============================================================================
cat("\n=== A. SAMPLE SIZE ===\n")
yrs_per_route <- table(table(panel$unit))
sample_tab <- data.frame(
  object = c("Regression panel, raw", "Regression panel, excl. AK/HI",
             "Route-county table (od_route_county_segments.csv)", "Map geometry file (od_route_geometries_conus.gpkg)"),
  routes = c(n_distinct(panel_raw$unit), n_distinct(panel$unit), uniqueN(rt$unit), length(unique(gpkg_units))),
  obs    = c(nrow(panel_raw), nrow(panel), NA, NA)
)
print(sample_tab)
cat("Years per route (excl. AK/HI):\n"); print(yrs_per_route)
cat("Rows per year:\n"); print(table(panel$year))
cat("Panel routes absent from od_route_county_segments:", sum(!unique(panel_raw$unit) %in% rt$unit), "\n")
fwrite(sample_tab, file.path(OUT, "A_sample_reconciliation.csv"))

# ==============================================================================
# B. Routing endpoints (C19) — replicate route101125.R logic
#    5-digit GEOID -> st_point_on_surface(county polygon)
#    state GEOID (000xx) -> st_centroid(state polygon)
# ==============================================================================
cat("\n=== B. ROUTING ENDPOINTS ===\n")
cty <- st_read(file.path(DATA, "Shapefile/cb_2017_us_county_20m.shp"), quiet = TRUE) %>%
  mutate(GEOID = pad5(GEOID)) %>% st_transform(4269)
sts <- st_read(file.path(DATA, "Shapefile/cb_2018_us_state_5m.shp"), quiet = TRUE) %>%
  mutate(GEOID = pad5(STATEFP)) %>% st_transform(4269)

meta <- read_excel(file.path(DATA, "FAF/FAF5_metadata.xlsx"), sheet = "FAF Zone (Domestic)")
zone_type <- meta %>%
  transmute(FAF_ID = sprintf("%03d", as.numeric(`Numeric Label`)),
            zone_type = case_when(grepl("Remainder", `Long Description`) ~ "Remainder of State",
                                  grepl("CFS Area", `Long Description`) ~ "CFS Metro Area",
                                  TRUE ~ "Whole-State Zone"))

sf_use_s2(FALSE)
pts_cty <- suppressWarnings(st_point_on_surface(cty)) %>% st_coordinates() %>% as.data.frame() %>%
  mutate(GEOID = cty$GEOID, rep_county = cty$NAME)
pts_st  <- suppressWarnings(st_centroid(sts)) %>% st_coordinates() %>% as.data.frame() %>%
  mutate(GEOID = sts$GEOID, rep_county = paste0("(state centroid) ", sts$NAME))
pts <- bind_rows(pts_cty, pts_st) %>% rename(route_lon = X, route_lat = Y)
sf_use_s2(TRUE)

endpoints <- lookup %>%
  left_join(zone_type, by = "FAF_ID") %>%
  left_join(pts, by = c("final_geoid" = "GEOID")) %>%
  transmute(FAF_ID, zone_name = Clean_Name, zone_type, representative_GEOID = final_geoid,
            representative_unit = rep_county,
            point_method = ifelse(grepl("^000", final_geoid), "state polygon centroid",
                                  "point-on-surface of representative county"),
            route_lon = round(route_lon, 5), route_lat = round(route_lat, 5),
            cfs_internal_point_lon = round(lon, 5), cfs_internal_point_lat = round(lat, 5))
cat("Zones:", nrow(endpoints), " by method:\n"); print(table(endpoints$point_method))
cat("Zones with missing routing point:", sum(is.na(endpoints$route_lon)), "\n")
# distance between routing point and CFS internal point (how far off the representative county is)
e_ok <- endpoints %>% filter(!is.na(route_lon), !is.na(cfs_internal_point_lon))
d_off <- as.numeric(st_distance(st_as_sf(e_ok, coords = c("route_lon", "route_lat"), crs = 4269),
                                st_as_sf(e_ok, coords = c("cfs_internal_point_lon", "cfs_internal_point_lat"), crs = 4269),
                                by_element = TRUE)) / 1609.34
endpoints$offset_miles_vs_cfs_internal_point <- NA_real_
endpoints$offset_miles_vs_cfs_internal_point[match(e_ok$FAF_ID, endpoints$FAF_ID)] <- round(d_off, 1)
cat("Offset (miles) between routing point and CFS-area internal point, CFS Metro zones:\n")
print(summary(endpoints$offset_miles_vs_cfs_internal_point[endpoints$zone_type == "CFS Metro Area"]))
fwrite(endpoints, file.path(OUT, "B_faf_zone_routing_points.csv"))

# ==============================================================================
# C. Road vs great-circle distance; suspect routes
# ==============================================================================
cat("\n=== C. ROUTE DISTANCE CHECKS ===\n")
od <- panel %>% distinct(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID, total_distance, dist_cat) %>%
  mutate(orig_id = sprintf("%03d", dms_orig), dest_id = sprintf("%03d", dms_dest)) %>%
  left_join(endpoints %>% select(FAF_ID, lo = route_lon, la = route_lat), by = c("orig_id" = "FAF_ID")) %>%
  left_join(endpoints %>% select(FAF_ID, lo2 = route_lon, la2 = route_lat), by = c("dest_id" = "FAF_ID")) %>%
  mutate(intra_zone = dms_orig_GEOID == dms_dest_GEOID,
         in_map_file = unit %in% gpkg_units)
ok <- !od$intra_zone & !is.na(od$lo) & !is.na(od$lo2)
od$gc_miles <- NA_real_
od$gc_miles[ok] <- as.numeric(st_distance(st_as_sf(od[ok, ], coords = c("lo", "la"), crs = 4269),
                                          st_as_sf(od[ok, ], coords = c("lo2", "la2"), crs = 4269),
                                          by_element = TRUE)) / 1609.34
od$road_gc_ratio <- od$total_distance / od$gc_miles
n_cty <- rt[, .(n_counties = uniqueN(geoid)), by = unit]
od <- od %>% left_join(as.data.frame(n_cty), by = "unit")

ratio_tab <- od %>% filter(!intra_zone) %>% group_by(in_map_file) %>%
  summarise(routes = n(), median_ratio = median(road_gc_ratio, na.rm = TRUE),
            p05 = quantile(road_gc_ratio, .05, na.rm = TRUE), p95 = quantile(road_gc_ratio, .95, na.rm = TRUE),
            share_ratio_below_1.02 = mean(road_gc_ratio < 1.02, na.rm = TRUE),
            median_counties = median(n_counties, na.rm = TRUE), .groups = "drop")
print(ratio_tab)
cat("Intra-zone routes (origin = destination zone):", sum(od$intra_zone), "\n")
cat("Counties traversed per inter-zone route:\n"); print(summary(od$n_counties[!od$intra_zone]))
cat("Route distance (miles), inter-zone:\n"); print(summary(od$total_distance[!od$intra_zone]))
fwrite(ratio_tab, file.path(OUT, "C_road_vs_greatcircle.csv"))
fwrite(od %>% filter(!in_map_file) %>%
         select(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID, total_distance, gc_miles, road_gc_ratio, n_counties),
       file.path(OUT, "C_routes_not_in_map_file.csv"))

# ==============================================================================
# D. FAF dist_band vs OSRM distance (C24)
# ==============================================================================
cat("\n=== D. DISTANCE CLASSIFICATION ===\n")
od <- od %>% mutate(osrm_band = cut(total_distance, c(-Inf, 500, 1000, Inf),
                                    labels = c("<500 mi", "500-1000 mi", ">1000 mi"), right = FALSE))
xt <- od %>% filter(!intra_zone) %>% count(FAF_dist_cat = dist_cat, osrm_band) %>%
  pivot_wider(names_from = osrm_band, values_from = n, values_fill = 0)
print(xt)
agree <- od %>% filter(!intra_zone) %>%
  summarise(agree = mean((dist_cat == "Short" & osrm_band == "<500 mi") |
                         (dist_cat == "Medium" & osrm_band == "500-1000 mi") |
                         (dist_cat == "Long" & osrm_band == ">1000 mi")))
cat("Share of inter-zone routes where FAF class = OSRM class:", round(agree$agree, 3), "\n")
cat("Intra-zone routes by FAF class:\n"); print(table(od$dist_cat[od$intra_zone]))
fwrite(xt, file.path(OUT, "D_distband_vs_osrm.csv"))

# ==============================================================================
# E. Robustness regressions (baseline spec unchanged)
# ==============================================================================
cat("\n=== E. ROBUSTNESS ===\n")
distw_vars <- c("distwHighTempFreq_3sd", "distwLowTempFreq_3sd", "distwHighPrecFreq", "distwHighVsFreq")
cat("distw vars present:", all(distw_vars %in% names(panel)), "\n")
suspect <- od$unit[!od$in_map_file]
intra   <- od$unit[od$intra_zone]

fit <- function(y, x, d) feols(as.formula(paste(y, "~", paste(x, collapse = " + "), "| unit + year")),
                               cluster = ~unit, data = d)
dict <- c(total_tons = "Tons", total_tmiles = "Ton-miles",
          avgHighTempFreq_3sd = "Extreme hot days", avgLowTempFreq_3sd = "Extreme cold days",
          avgHighPrecFreq = "Heavy precipitation days", avgHighVsFreq = "High wind days",
          distwHighTempFreq_3sd = "Extreme hot days (dist-weighted)", distwLowTempFreq_3sd = "Extreme cold days (dist-weighted)",
          distwHighPrecFreq = "Heavy precipitation days (dist-weighted)", distwHighVsFreq = "High wind days (dist-weighted)")
hdr <- list("Sample / exposure" = c("Baseline", "Mapped routes only",
                                    "Excl. intra-zone routes", "Distance-weighted exposure"))
my_signif <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)   # same levels as the manuscript tables
for (y in c("total_tons", "total_tmiles")) {
  ms <- list(fit(y, weather_vars, panel),
             fit(y, weather_vars, panel %>% filter(!unit %in% suspect)),
             fit(y, weather_vars, panel %>% filter(!unit %in% intra)))
  if (all(distw_vars %in% names(panel))) ms[[4]] <- fit(y, distw_vars, panel)
  print(etable(ms, dict = dict, signif.code = my_signif))
  etable(ms, dict = dict, headers = list("Sample / exposure" = hdr[[1]][seq_along(ms)]),
         signif.code = my_signif,
         title = paste("Route-construction robustness:", dict[y]),
         file = file.path(OUT, paste0("E_robustness_", y, ".txt")), replace = TRUE)
}

# ==============================================================================
# F. OSRM defaults (C18)
# ==============================================================================
cat("\n=== F. OSRM PACKAGE DEFAULTS (installed version) ===\n")
if (requireNamespace("osrm", quietly = TRUE)) {
  suppressMessages(library(osrm))
  cat("osrm version:", as.character(packageVersion("osrm")), "\n")
  cat("osrm.server:", getOption("osrm.server"), "\n")
  cat("osrm.profile:", getOption("osrm.profile"), "\n")
} else cat("osrm not installed\n")
cat("od_route_county_segments.csv last modified:", format(file.mtime(file.path(DATA, "Route/od_route_county_segments.csv"))), "\n")
cat("od_route_geometries_conus.gpkg last modified:", format(file.mtime(file.path(DATA, "Route/od_route_geometries_conus.gpkg"))), "\n")

sink()
message("Done. Outputs in ", OUT)
