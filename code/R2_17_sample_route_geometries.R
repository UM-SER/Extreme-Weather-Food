# ==============================================================================
# R2_17_sample_route_geometries.R
# Purpose: One route-line file for the analysis sample, so that all maps show the
#          same routes as the regressions. Uses the FULL OSRM road lines saved by
#          R2_19 (outside the repository, see FULL_LINES_DIR) and keeps the routes
#          with within_conus == TRUE in route_conus_check_fullgeom.csv (R2_20).
#          Lines are simplified to 200 m for mapping (file size). Same-zone routes
#          are not routed; as in the original map file they get a zero-length line
#          at the zone's routing point, so every sample route is in the file.
#          No OSRM requests.
# 2026-11-11: switched from the stored simplified lines (+ R2_16 re-requests) to
#          the full-line routing of R2_19.
# Input:  data/Route/route_conus_check_fullgeom.csv; FULL_LINES_DIR/lines_*.gpkg
# Output: data/Route/od_route_geometries_sample.gpkg (unit, dms_orig_GEOID,
#         dms_dest_GEOID; EPSG:4269)
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(data.table)})
ROOT <- "."
LINES_DIR <- Sys.getenv("FULL_LINES_DIR", "../revision/results/full_geometry_lines")

chk  <- fread(file.path(ROOT, "data/Route/route_conus_check_fullgeom.csv"), colClasses = list(character = "unit"))
keep  <- chk[within_conus == TRUE & source == "routed (full road line)", unit]
intra <- chk[within_conus == TRUE & source == "intra-zone (not routed)"]
ep <- fread(file.path(ROOT, "data/Route/faf_zone_routing_points.csv"))

files <- list.files(LINES_DIR, "^lines_.*\\.gpkg$", full.names = TRUE)
lines <- do.call(rbind, lapply(files, st_read, quiet = TRUE))
st_geometry(lines) <- "geometry"
lines <- lines %>% filter(unit %in% keep) %>%
  mutate(geometry = st_simplify(geometry, dTolerance = 200)) %>%
  st_transform(4269) %>% select(unit)
pts <- ep[match(intra$dms_orig, FAF_ID)]
intra_lines <- st_sf(unit = intra$unit, geometry = st_sfc(lapply(seq_len(nrow(pts)), function(i)
  st_linestring(matrix(rep(c(pts$route_lon[i], pts$route_lat[i]), each = 2), ncol = 2))), crs = 4269))
lines <- rbind(lines, intra_lines) %>%
  mutate(dms_orig_GEOID = substr(unit, 1, 5), dms_dest_GEOID = substr(unit, 6, 10), .after = unit)
stopifnot(nrow(lines) == sum(chk$within_conus), !anyDuplicated(lines$unit))

st_write(lines, file.path(ROOT, "data/Route/od_route_geometries_sample.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message("Saved ", nrow(lines), " route lines (", length(keep), " routed + ", nrow(intra), " same-zone)")
