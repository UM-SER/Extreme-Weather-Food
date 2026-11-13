# ==============================================================================
# R2_16_check_routes_within_conus.R
# Purpose: R2 Comment 21. Decide for every route in the panel whether its truck
#          route lies entirely within the contiguous U.S.
#   Rule: a route is kept if its OSRM road path has no more than TOL miles outside
#         the contiguous U.S. (union of the TIGER/Line 2017 state polygons of the
#         48 states and DC; these polygons include coastal and Great Lakes water,
#         so bridges and causeways count as inside). Routes to or from Alaska or
#         Hawaii are excluded.
#   Part A: the 11,938 routes stored in od_route_geometries_conus.gpkg (no requests).
#   Part B: the other panel routes are requested again from the OSRM server with
#           the same endpoints (faf_zone_routing_points.csv) and the same defaults
#           as the original route script, and tested with the same rule.
# Output: data/Route/route_conus_check.csv, one row per route; 06_build_panel.R
#         reads it to define the sample. data/Route/od_route_geometries_rerouted.gpkg
#         holds the route lines from Part B.
# Note: Part B queries the public OSRM server, which routes on current
#       OpenStreetMap data, so a re-run can return slightly different paths. The
#       stored output is what the analysis uses.
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(stringr); library(data.table); library(osrm)})
ROOT <- "."
TOL  <- 1      # miles outside the contiguous U.S. allowed (border-hugging roads)
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
ak_hi <- c(20, 151, 159)
OUT_CSV  <- file.path(ROOT, "data/Route/route_conus_check.csv")
OUT_GPKG <- file.path(ROOT, "data/Route/od_route_geometries_rerouted.gpkg")

# --- Contiguous-U.S. polygon ---------------------------------------------------
conus <- st_read(file.path(ROOT, "data/Shapefile/tl_2017_us_state.shp"), quiet = TRUE) %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>%
  st_transform(5070) %>% st_make_valid() %>% st_union()
outside_miles <- function(g) {            # g: sfc of lines in EPSG:5070
  d <- suppressWarnings(st_difference(g, conus))
  if (length(d) == 0) return(0)
  sum(as.numeric(st_length(d))) / 1609.34
}

# --- Routes in the panel (before any restriction) ------------------------------
routes <- readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07_all_routes.rds")) %>%
  distinct(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID)
gpkg <- st_read(file.path(ROOT, "data/Route/od_route_geometries_conus.gpkg"), quiet = TRUE) %>%
  mutate(unit = paste0(pad5(dms_orig_GEOID), pad5(dms_dest_GEOID))) %>%
  filter(unit %in% routes$unit) %>% distinct(unit, .keep_all = TRUE) %>% st_transform(5070)

# --- Part A: stored route lines -------------------------------------------------
message("Part A: ", nrow(gpkg), " stored route lines")
inside <- st_within(st_geometry(gpkg), conus, sparse = FALSE)[, 1]
a <- data.table(unit = gpkg$unit,
                road_miles = as.numeric(st_length(gpkg)) / 1609.34,
                outside_miles = 0,
                source = "stored (od_route_geometries_conus.gpkg)")
for (i in which(!inside)) a$outside_miles[i] <- outside_miles(st_geometry(gpkg)[i])
message("  stored lines not entirely within: ", sum(!inside))

# --- Part B: request the other routes again ------------------------------------
ep <- fread(file.path(ROOT, "data/Route/faf_zone_routing_points.csv"))
pt <- function(z) { e <- ep[FAF_ID == as.integer(z)]; st_sfc(st_point(c(e$route_lon, e$route_lat)), crs = 4269) }
todo <- routes %>% filter(!unit %in% gpkg$unit, !dms_orig %in% ak_hi, !dms_dest %in% ak_hi)
message("Part B: ", nrow(todo), " routes to request")
b_lines <- list(); b <- vector("list", nrow(todo))
for (i in seq_len(nrow(todo))) {
  o <- sprintf("%03d", todo$dms_orig[i]); d <- sprintf("%03d", todo$dms_dest[i])
  r <- NULL
  for (k in 1:4) {
    r <- tryCatch(osrmRoute(src = pt(o), dst = pt(d), overview = "full"), error = function(e) NULL)
    Sys.sleep(1.2); if (!is.null(r)) break; Sys.sleep(5 * k)
  }
  if (is.null(r)) {
    b[[i]] <- data.table(unit = todo$unit[i], road_miles = NA_real_, outside_miles = NA_real_, source = "request failed")
  } else {
    g <- st_transform(st_geometry(r), 5070)
    b[[i]] <- data.table(unit = todo$unit[i], road_miles = as.numeric(st_length(g)) / 1609.34,
                         outside_miles = outside_miles(g), source = "requested again (OSRM)")
    b_lines[[length(b_lines) + 1]] <- st_sf(unit = todo$unit[i], geometry = g)
  }
  if (i %% 50 == 0) message("  ", i, " / ", nrow(todo))
}
b <- rbindlist(b)
if (length(b_lines)) st_write(do.call(rbind, b_lines), OUT_GPKG, delete_dsn = TRUE, quiet = TRUE)

# --- Combine and classify ---------------------------------------------------------
c_akhi <- routes %>% filter(dms_orig %in% ak_hi | dms_dest %in% ak_hi) %>%
  transmute(unit, road_miles = NA_real_, outside_miles = NA_real_, source = "Alaska or Hawaii")
res <- rbind(a, b, as.data.table(c_akhi))
res <- merge(as.data.table(routes), res, by = "unit", all.x = TRUE)
res[, within_conus := !is.na(outside_miles) & outside_miles <= TOL & source != "Alaska or Hawaii"]
res[, `:=`(road_miles = round(road_miles, 1), outside_miles = round(outside_miles, 2))]
fwrite(res, OUT_CSV)

cat("\nRule: road path within the contiguous U.S. (at most", TOL, "mile outside)\n")
print(res[, .N, by = .(source, within_conus)][order(source)])
cat("\nOutside miles among routes that fail the rule:\n")
print(summary(res[within_conus == FALSE & !is.na(outside_miles), outside_miles]))
cat("\nRoutes kept:", sum(res$within_conus), "of", nrow(res), "\n")
