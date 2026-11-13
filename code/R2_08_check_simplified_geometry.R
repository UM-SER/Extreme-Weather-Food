# ==============================================================================
# 08_check_simplified_geometry.R
# Purpose: osrmRoute() was called with its default overview = "simplified", so
#          county miles were measured on a generalized line. Compare the length
#          of that line (od_route_county_segments total_distance) with the road distance
#          OSRM reports for the full route (gpkg `distance`, km). Read-only.
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(data.table); library(stringr)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "08_log.txt"), split = TRUE)

g <- st_read(file.path(ROOT, "data/Route/od_route_geometries_conus.gpkg"), quiet = TRUE)
g$unit <- paste0(pad5(g$dms_orig_GEOID), pad5(g$dms_dest_GEOID))
g$geom_miles <- as.numeric(st_length(st_transform(g, 5070))) / 1609.34
gd <- st_drop_geometry(g) %>% transmute(unit, osrm_road_miles = distance / 1.609344, osrm_minutes = duration, geom_miles)

rt <- fread(file.path(ROOT, "data/Route/od_route_county_segments.csv"), colClasses = list(character = c("dms_orig_GEOID","dms_dest_GEOID")))
rt[, unit := paste0(pad5(dms_orig_GEOID), pad5(dms_dest_GEOID))]
rtd <- rt[, .(table_miles = first(total_distance)), by = unit]

x <- merge(as.data.table(gd), rtd, by = "unit")[osrm_road_miles > 0]
x[, `:=`(ratio_table = table_miles / osrm_road_miles, ratio_gpkg_geom = geom_miles / osrm_road_miles)]
cat("Routes compared:", nrow(x), "\n\n")
cat("route_table length / OSRM road distance:\n"); print(quantile(x$ratio_table, c(.01,.05,.25,.5,.75,.95,.99)))
cat("\ngpkg line length / OSRM road distance:\n"); print(quantile(x$ratio_gpkg_geom, c(.01,.05,.25,.5,.75,.95,.99)))
cat("\nBy OSRM road distance band (median ratio_table):\n")
print(x[, .(n = .N, median_ratio = median(ratio_table)), by = .(band = cut(osrm_road_miles, c(0,100,250,500,1000,2000,Inf)))][order(band)])
cat("\nImplied average speed (mph), median:", median(x$osrm_road_miles / (x$osrm_minutes / 60)), "\n")
sink()
