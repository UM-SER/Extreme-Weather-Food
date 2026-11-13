# ==============================================================================
# 13_profile_432_routes.R
# Purpose: Why do 432 regression routes lack a geometry in od_route_geometries_conus.gpkg?
#          Profile them vs mapped routes: same zone, same state, state-level zones,
#          distance, FAF band. Read-only.
# ==============================================================================
suppressMessages({library(sf); library(dplyr); library(stringr); library(data.table)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")
sink(file.path(OUT, "13_log.txt"), split = TRUE)

g <- st_read(file.path(ROOT, "data/Route/od_route_geometries_conus.gpkg"), quiet = TRUE) %>% st_drop_geometry()
gu <- paste0(pad5(g$dms_orig_GEOID), pad5(g$dms_dest_GEOID))
p <- readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07_all_routes.rds")) %>%
  filter(!dms_orig %in% c(20, 151, 159) & !dms_dest %in% c(20, 151, 159)) %>%
  distinct(unit, dms_orig, dms_dest, dms_orig_GEOID, dms_dest_GEOID, total_distance, dist_cat) %>%
  mutate(mapped = unit %in% gu,
         same_zone  = dms_orig == dms_dest,
         same_state = floor(dms_orig / 10) == floor(dms_dest / 10),
         state_zone = grepl("^000", dms_orig_GEOID) | grepl("^000", dms_dest_GEOID))
cat("Routes:", nrow(p), " mapped:", sum(p$mapped), " missing:", sum(!p$mapped), "\n\n")
print(p %>% group_by(mapped) %>% summarise(n = n(), same_zone = sum(same_zone), same_state_diff_zone = sum(same_state & !same_zone),
                                          involves_state_zone = sum(state_zone), median_miles = median(total_distance), .groups = "drop"))
cat("\nFAF distance class among missing:\n"); print(table(p$dist_cat[!p$mapped]))
cat("\nShare missing by origin-zone count (top 10 origin zones):\n")
print(p %>% filter(!mapped) %>% count(dms_orig, sort = TRUE) %>% head(10))

# was the missing set present in the older December gpkg?
old <- file.path(ROOT, "data/Route/routes_within_joined_20251211.gpkg")
if (file.exists(old)) {
  o <- st_read(old, quiet = TRUE) %>% st_drop_geometry()
  ou <- paste0(pad5(o$dms_orig_GEOID), pad5(o$dms_dest_GEOID))
  cat("\nOlder gpkg (2025-12-11): routes", length(unique(ou)), "; of the 432 missing, present there:", sum(p$unit[!p$mapped] %in% ou), "\n")
}
o2 <- file.path(ROOT, "data/Route/routes_within_20251211.gpkg")
if (file.exists(o2)) {
  o <- st_read(o2, quiet = TRUE) %>% st_drop_geometry()
  cat("routes_within_20251211.gpkg cols:", paste(names(o), collapse = ", "), " rows:", nrow(o), "\n")
}
# Panel used to build routes (route101125 used Panel_sum_subset_cleaned): which panel version had these OD?
for (f in c("Panel_sum_subset_cleaned.rds", "Panel_sum_subset_cleaned20251112.rds")) {
  ff <- file.path(ROOT, "data/result", f)
  if (file.exists(ff)) { q <- readRDS(ff); u <- if ("unit" %in% names(q)) unique(as.character(q$unit)) else character(0)
    cat(f, ": units", length(u), "; of 432 missing present:", sum(p$unit[!p$mapped] %in% u), "; of mapped present:", sum(p$unit[p$mapped] %in% u), "\n") }
}
sink()
