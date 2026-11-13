# ==============================================================================
# R2_19_full_geometry_check.R
# Purpose: Re-route every panel route with OSRM's FULL road geometry
#          (overview = "full") and redo, on the full line,
#            (a) the contiguous-U.S. check of R2_16 (same boundary, same 1-mile rule), and
#            (b) the county segments of 02_generate_route_table.R (same counties,
#                same projection, same intersection), so that the exposure built
#                from simplified lines can be compared with the full-line version.
#          The original routes (02 and the map file) used osrmRoute() defaults,
#          i.e. OSRM's simplified line; R2_16 checked the 11,938 stored lines on
#          that simplified line. This script checks all routes on the road itself.
#
# Nothing existing is overwritten. Outputs go to data/Route/full_geometry/:
#   route_conus_check_full.csv          one row per route (as route_conus_check.csv)
#   od_route_county_segments_full.csv   county segments on the full line (as 02)
#   _progress/chunk_*.rds               checkpoints; re-running resumes
# Route lines (several GB at full detail) are saved outside the repository,
# lightly simplified (20 m), to LINES_DIR.
#
# Needs internet access (public OSRM server, about 1 request per second;
# about 4 hours for all routes). Endpoints: data/Route/faf_zone_routing_points.csv,
# as in R2_16.
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(stringr); library(data.table); library(osrm)})
setDTthreads(1)
ROOT      <- "."
TOL       <- 1
ak_hi     <- c(20, 151, 159)
OUT_DIR   <- file.path(ROOT, "data/Route/full_geometry")
PROG_DIR  <- file.path(OUT_DIR, "_progress")
LINES_DIR <- Sys.getenv("FULL_LINES_DIR", "../revision/results/full_geometry_lines")
CHUNK     <- 200
dir.create(PROG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LINES_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Boundaries (same as R2_16 and 02) -------------------------------------------
conus <- st_read(file.path(ROOT, "data/Shapefile/tl_2017_us_state.shp"), quiet = TRUE) %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>%
  st_transform(5070) %>% st_make_valid() %>% st_union()
counties_proj <- st_read(file.path(ROOT, "data/Shapefile/cb_2017_us_county_20m.shp"), quiet = TRUE) %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, "left", "0")) %>% st_transform(5070)
outside_miles <- function(g) {
  d <- suppressWarnings(st_difference(g, conus))
  if (length(d) == 0) return(0)
  sum(as.numeric(st_length(d))) / 1609.34
}

# --- Routes ---------------------------------------------------------------------
routes <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07_all_routes.rds")))
routes <- unique(routes[, .(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID)])
routes[, akhi := dms_orig %in% ak_hi | dms_dest %in% ak_hi]
routes[, intra := dms_orig == dms_dest]
todo <- routes[!akhi & !intra][order(unit)]
TEST <- nzchar(Sys.getenv("R2_19_TEST"))          # test mode: 3 routes, scratch folders
if (TEST) { todo <- todo[c(1, 5000, 9000)]; OUT_DIR <- PROG_DIR <- LINES_DIR <- Sys.getenv("R2_19_TEST")
            dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE) }
ep <- fread(file.path(ROOT, "data/Route/faf_zone_routing_points.csv"))
pt <- function(z) { e <- ep[FAF_ID == as.integer(z)]; st_sfc(st_point(c(e$route_lon, e$route_lat)), crs = 4269) }

route_one <- function(i) {
  o <- sprintf("%03d", todo$dms_orig[i]); d <- sprintf("%03d", todo$dms_dest[i]); u <- todo$unit[i]
  r <- NULL
  for (k in 1:5) {
    t0 <- Sys.time()
    r <- tryCatch(osrmRoute(src = pt(o), dst = pt(d), overview = "full"), error = function(e) NULL)
    wait <- 1.05 - as.numeric(difftime(Sys.time(), t0, units = "secs")); if (wait > 0) Sys.sleep(wait)
    if (!is.null(r)) break
    Sys.sleep(10 * k)
  }
  if (is.null(r)) return(list(chk = data.table(unit = u, osrm_road_miles = NA_real_, osrm_minutes = NA_real_,
                                               line_miles = NA_real_, outside_miles = NA_real_, status = "request failed"),
                              seg = NULL, line = NULL))
  g <- st_transform(st_geometry(r), 5070)
  seg <- suppressWarnings(st_intersection(st_sf(geometry = g), counties_proj))
  seg <- data.table(unit = u, geoid = seg$GEOID, name = seg$NAME,
                    distance = as.numeric(st_length(seg)) / 1609.344)
  list(chk  = data.table(unit = u, osrm_road_miles = r$distance / 1.609344, osrm_minutes = r$duration,
                         line_miles = as.numeric(st_length(g)) / 1609.344,
                         outside_miles = outside_miles(g), status = "ok"),
       seg  = seg,
       line = st_sf(unit = u, geometry = st_simplify(g, dTolerance = 20)))
}

# --- Run in chunks (resumable) ----------------------------------------------------
n_chunks <- ceiling(nrow(todo) / CHUNK)
message("Routes to request: ", nrow(todo), " in ", n_chunks, " chunks")
for (ch in seq_len(n_chunks)) {
  f <- file.path(PROG_DIR, sprintf("chunk_%03d.rds", ch))
  if (file.exists(f)) next
  idx <- ((ch - 1) * CHUNK + 1):min(ch * CHUNK, nrow(todo))
  out <- lapply(idx, route_one)
  saveRDS(list(chk = rbindlist(lapply(out, `[[`, "chk")),
               seg = rbindlist(lapply(out, `[[`, "seg"))), f)
  lines <- Filter(Negate(is.null), lapply(out, `[[`, "line"))
  if (length(lines)) st_write(do.call(rbind, lines), file.path(LINES_DIR, sprintf("lines_%03d.gpkg", ch)),
                              delete_dsn = TRUE, quiet = TRUE)
  message(format(Sys.time(), "%H:%M"), "  chunk ", ch, "/", n_chunks, " done")
}

# --- Combine ----------------------------------------------------------------------
parts <- lapply(list.files(PROG_DIR, "^chunk_.*\\.rds$", full.names = TRUE), readRDS)
chk <- rbindlist(lapply(parts, `[[`, "chk"))
seg <- rbindlist(lapply(parts, `[[`, "seg"))
stopifnot(!anyDuplicated(chk$unit), nrow(chk) == nrow(todo))
if (TEST) { print(chk); print(seg[, .N, by = unit]); quit(save = "no") }

other <- rbind(routes[akhi == TRUE,  .(unit, status = "Alaska or Hawaii")],
               routes[akhi == FALSE & intra == TRUE, .(unit, status = "intra-zone (not routed)")], fill = TRUE)
chk <- rbind(chk, other, fill = TRUE)
chk <- merge(routes[, .(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID)], chk, by = "unit")
chk[, within_conus_full := status == "intra-zone (not routed)" |
                          (status == "ok" & outside_miles <= TOL)]
old <- fread(file.path(ROOT, "data/Route/route_conus_check.csv"), colClasses = list(character = "unit"))
chk <- merge(chk, old[, .(unit, within_conus_old = within_conus, old_source = source)], by = "unit", all.x = TRUE)
num <- c("osrm_road_miles", "osrm_minutes", "line_miles", "outside_miles")
chk[, (num) := lapply(.SD, round, 2), .SDcols = num]
fwrite(chk, file.path(OUT_DIR, "route_conus_check_full.csv"))

seg[, distance := round(distance, 2)]
seg[, total_distance := round(sum(distance), 2), by = unit]
fwrite(seg, file.path(OUT_DIR, "od_route_county_segments_full.csv"))

sink(if (TEST) file.path(OUT_DIR, "log.txt") else file.path(ROOT, "tables/diagnostics/19_full_geometry_check_log.txt"), split = TRUE)
cat("Full-geometry check, run", format(Sys.Date()), "\n\n")
print(chk[, .N, by = status])
cat("\nOld rule (route_conus_check.csv) vs full-line rule:\n")
print(chk[, .N, by = .(within_conus_old, within_conus_full)][order(within_conus_old, within_conus_full)])
cat("\nOutside miles among routed lines that fail:\n")
print(summary(chk[status == "ok" & within_conus_full == FALSE, outside_miles]))
cat("\nRoutes in sample under full-line rule:", sum(chk$within_conus_full), "\n")
sink()
