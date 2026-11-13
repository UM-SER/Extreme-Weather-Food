# ==============================================================================
# R2_18_why_258_routes_missing.R
# Purpose: Explain why 258 routes that lie within the contiguous U.S. (R2_16)
#          were missing from the original map file od_route_geometries_conus.gpkg.
#          The original route script (archive_route_generation_original_2025-10-11.R)
#          requested routes with osrmRoute() defaults, i.e. OSRM's *simplified*
#          route line, and kept a route only if that line lies strictly within the
#          union of contiguous-U.S. counties (tl_2017_us_county; st_within, no tolerance).
#          Here the same test uses TIGER 2017 state polygons (48 states + DC), which
#          share the outer boundary of the county file (the county file is 120 MB and
#          not included in the repository).
#          This script applies that same rule to the 258 routes:
#            (1) to their full road lines from R2_16, and
#            (2) to simplified lines requested again from OSRM (osrmRoute defaults).
#          If (1) passes and (2) fails, the simplified line, not the road, leaves the U.S.
# Input:  data/Route/route_conus_check.csv, data/Route/od_route_geometries_rerouted.gpkg,
#         data/Route/faf_zone_routing_points.csv,
#         data/Shapefile/tl_2017_us_state.shp (TIGER/Line 2017 states)
# Output: tables/diagnostics/18_why_258_routes_missing.csv and _log.txt
# Needs internet access (public OSRM server). Results may differ by a few routes
# from run to run because OSRM uses current OpenStreetMap data.
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(data.table); library(osrm)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
log_file <- file.path(OUT, "18_why_258_routes_missing_log.txt")
sink(log_file, split = TRUE)

st <- st_read(file.path(ROOT, "data/Shapefile/tl_2017_us_state.shp"), quiet = TRUE) %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))
mainland <- st_union(st_transform(st, 4269))

chk  <- fread(file.path(ROOT, "data/Route/route_conus_check.csv"), colClasses = list(character = "unit"))
u258 <- chk[source == "requested again (OSRM)" & within_conus == TRUE]
cat("Routes tested:", nrow(u258), "\n")

# (1) full road lines from R2_16
full <- st_read(file.path(ROOT, "data/Route/od_route_geometries_rerouted.gpkg"), quiet = TRUE) %>%
  filter(unit %in% u258$unit) %>% st_transform(4269)
w_full <- st_within(full, mainland, sparse = FALSE)[, 1]
cat("(1) Full road lines, original rule: within", sum(w_full), "of", nrow(full), "\n")

# (2) simplified lines (osrmRoute defaults, as in the original script)
ep <- fread(file.path(ROOT, "data/Route/faf_zone_routing_points.csv"))
pt <- function(z) { e <- ep[FAF_ID == as.integer(z)]; st_sfc(st_point(c(e$route_lon, e$route_lat)), crs = 4269) }
res <- vector("list", nrow(u258))
for (i in seq_len(nrow(u258))) {
  o <- sprintf("%03d", u258$dms_orig[i]); d <- sprintf("%03d", u258$dms_dest[i]); r <- NULL
  for (k in 1:4) {
    r <- tryCatch(osrmRoute(src = pt(o), dst = pt(d)), error = function(e) NULL)
    Sys.sleep(1.2); if (!is.null(r)) break; Sys.sleep(5 * k)
  }
  res[[i]] <- data.table(unit = u258$unit[i],
                         full_line_within = w_full[match(u258$unit[i], full$unit)],
                         simplified_requested = !is.null(r),
                         simplified_line_within = if (is.null(r)) NA else
                           st_within(st_transform(st_geometry(r), 4269), mainland, sparse = FALSE)[1, 1])
}
res <- rbindlist(res)
fwrite(res, file.path(OUT, "18_why_258_routes_missing.csv"))
cat("(2) Simplified lines, original rule:\n"); print(res[, .N, by = .(simplified_requested, simplified_line_within)])
cat("Run date:", format(Sys.Date()), "\n")
sink()
