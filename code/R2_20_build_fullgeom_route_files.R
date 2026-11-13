# ==============================================================================
# R2_20_build_fullgeom_route_files.R
# Purpose: Turn the full-geometry routing of R2_19 into the route files used by
#          the main pipeline (steps 4-6 and all tables and figures):
#   data/Route/od_route_county_segments_fullgeom.csv
#       county segments on the FULL OSRM road line, in the format of
#       02_generate_route_table.R (dms_orig_GEOID, dms_dest_GEOID, geoid, name,
#       distance, total_distance). Routes that are not routed keep their rows
#       from od_route_county_segments.csv: same-zone routes (one row,
#       distance = 1, as in 02) and the 3 Alaska/Hawaii routes.
#   data/Route/route_conus_check_fullgeom.csv
#       one row per panel route: road miles, miles outside the contiguous U.S.
#       (on the full line), source, and within_conus (the sample rule:
#       at most 1 mile outside; same-zone routes count as within).
# Input:  data/Route/full_geometry/route_conus_check_full.csv and
#         od_route_county_segments_full.csv (R2_19), data/Route/od_route_county_segments.csv
# No OSRM requests.
# ==============================================================================
suppressMessages({library(data.table)})
setDTthreads(1)
ROOT <- "."
FG   <- file.path(ROOT, "data/Route/full_geometry")
pad5 <- function(x) sprintf("%05d", as.integer(x))

chk  <- fread(file.path(FG, "route_conus_check_full.csv"),
              colClasses = list(character = c("unit", "dms_orig_GEOID", "dms_dest_GEOID")))
segf <- fread(file.path(FG, "od_route_county_segments_full.csv"), colClasses = list(character = c("unit", "geoid")))
old  <- fread(file.path(ROOT, "data/Route/od_route_county_segments.csv"),
              colClasses = list(character = c("dms_orig_GEOID", "dms_dest_GEOID", "geoid")))
old[, `:=`(dms_orig_GEOID = pad5(dms_orig_GEOID), dms_dest_GEOID = pad5(dms_dest_GEOID))]
old[, unit := paste0(dms_orig_GEOID, dms_dest_GEOID)]

# --- County segments ---------------------------------------------------------------
stopifnot(all(chk[status == "ok", unit] %in% segf$unit))
not_routed <- chk[status != "ok", unit]
segf[, `:=`(dms_orig_GEOID = substr(unit, 1, 5), dms_dest_GEOID = substr(unit, 6, 10))]
seg <- rbind(segf[, .(dms_orig_GEOID, dms_dest_GEOID, geoid, name, distance, total_distance)],
             old[unit %in% not_routed, .(dms_orig_GEOID, dms_dest_GEOID, geoid, name, distance, total_distance)])
seg[, geoid := pad5(geoid)]
setorder(seg, dms_orig_GEOID, dms_dest_GEOID, geoid)
fwrite(seg, file.path(ROOT, "data/Route/od_route_county_segments_fullgeom.csv"))

# --- Sample rule -------------------------------------------------------------------
out <- chk[, .(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID,
               road_miles = osrm_road_miles, outside_miles,
               source = fifelse(status == "ok", "routed (full road line)", status),
               within_conus = within_conus_full)]
fwrite(out, file.path(ROOT, "data/Route/route_conus_check_fullgeom.csv"))

cat("Route-table routes:", uniqueN(seg[, paste0(dms_orig_GEOID, dms_dest_GEOID)]), "\n")
print(out[, .N, by = .(source, within_conus)])
cat("Routes in sample:", sum(out$within_conus), "\n")
