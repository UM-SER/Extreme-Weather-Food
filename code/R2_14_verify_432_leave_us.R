# ==============================================================================
# 14_verify_432_leave_us.R
# Purpose: Re-request a sample of the 432 unmapped routes from OSRM (same
#          endpoints and defaults as route101125.R) and measure how much of each
#          route lies outside the contiguous-U.S. polygon. Read-only on project
#          data; ~40 requests to the public OSRM server.
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(stringr); library(osrm); library(data.table)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "14_log.txt"), split = TRUE)

ep <- fread(file.path(OUT, "B_faf_zone_routing_points.csv"))
miss <- fread(file.path(OUT, "C_routes_not_in_map_file.csv"))
g <- st_read(file.path(ROOT, "data/Route/od_route_geometries_conus.gpkg"), quiet = TRUE) %>% st_drop_geometry()
gu <- paste0(pad5(g$dms_orig_GEOID), pad5(g$dms_dest_GEOID))
pan <- readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07_all_routes.rds")) %>%
  filter(!dms_orig %in% c(20,151,159) & !dms_dest %in% c(20,151,159)) %>% distinct(unit, dms_orig, dms_dest)
mapped <- pan %>% filter(unit %in% gu, dms_orig != dms_dest)

states <- st_read(file.path(ROOT, "data/Shapefile/cb_2018_us_state_5m.shp"), quiet = TRUE) %>%
  filter(!STATEFP %in% c("02","15","60","66","69","72","78")) %>% st_transform(5070)
conus <- st_union(states)

pt <- function(z) { e <- ep[FAF_ID == as.integer(z)]; stopifnot(nrow(e) == 1); st_sfc(st_point(c(e$route_lon, e$route_lat)), crs = 4269) }
measure <- function(o, d) {
  r <- NULL; for (k in 1:3) { r <- tryCatch(osrmRoute(src = pt(o), dst = pt(d), overview = "full"), error = function(e) NULL); Sys.sleep(1.5); if (!is.null(r)) break; Sys.sleep(5) }
  if (is.null(r)) return(data.table(o, d, ok = FALSE, road_mi = NA, outside_mi = NA, lon_max = NA, lat_max = NA, lat_min = NA))
  rp <- st_transform(r, 5070); L <- as.numeric(st_length(rp)) / 1609.34
  outside <- suppressWarnings(st_difference(rp, conus)); Lo <- if (length(outside)) sum(as.numeric(st_length(outside))) / 1609.34 else 0
  bb <- st_bbox(r)
  data.table(o, d, ok = TRUE, road_mi = round(L), outside_mi = round(Lo, 1), lon_max = round(bb$xmax, 2), lat_max = round(bb$ymax, 2), lat_min = round(bb$ymin, 2))
}
set.seed(7)
s_miss <- miss[sample(.N, 25)]
s_map  <- mapped[sample(nrow(mapped), 10), ]
cat("== 25 of the 432 unmapped routes ==\n")
rm1 <- rbindlist(lapply(seq_len(nrow(s_miss)), function(i) measure(sprintf("%03d", s_miss$dms_orig[i]), sprintf("%03d", s_miss$dms_dest[i]))))
print(rm1)
cat("\n== 10 mapped routes (control) ==\n")
rm2 <- rbindlist(lapply(seq_len(nrow(s_map)), function(i) measure(sprintf("%03d", s_map$dms_orig[i]), sprintf("%03d", s_map$dms_dest[i]))))
print(rm2)
cat("\nUnmapped sample: share with >1 mile outside contiguous US:", round(mean(rm1$outside_mi > 1, na.rm = TRUE), 2),
    " | latitude max > 49 (north of border):", sum(rm1$lat_max > 49.0, na.rm = TRUE),
    " | latitude min < 32.5 (south of AZ/CA border):", sum(rm1$lat_min < 32.5, na.rm = TRUE), "\n")
cat("Mapped sample: share with >1 mile outside:", round(mean(rm2$outside_mi > 1, na.rm = TRUE), 2), "\n")
sink()
