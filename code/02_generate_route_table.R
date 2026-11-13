# ==============================================================================
# 02_generate_route_table.R
# Purpose: Route every truck O-D pair with OSRM and record the counties each
#          route traverses and the miles driven within each county.
# Output:  data/Route/od_route_county_segments.csv
#          (dms_orig_GEOID, dms_dest_GEOID, geoid, name, distance, total_distance)
#
# Cleaned extraction of the routing block in archive/route_generation_original_2025-10-11.R
# (lines 1112-1239), which produced the od_route_county_segments.csv used in the paper
# (file dated 2025-10-11). Logic is unchanged:
#   - Endpoint of a FAF zone = st_point_on_surface() of its representative county
#     (5-digit GEOID from data/Route/faf_zone_lookup.rds), or the polygon
#     centroid of the state for whole-state zones (GEOID 000xx).
#   - osrm::osrmRoute() with package defaults: server
#     https://routing.openstreetmap.de/, profile "car" (fastest route on the
#     OpenStreetMap road network).
#   - Route intersected with 2017 county boundaries in EPSG:5070; segment
#     lengths in miles. Same-zone pairs get one row with distance = 1.
# NOTE: re-running queries a live OSRM server whose OpenStreetMap snapshot changes
#       over time, so new routes may differ slightly from the archived table.
# ==============================================================================

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(units)
library(purrr)
library(osrm)
library(tigris)

pad5 <- function(x) str_pad(as.character(x), 5, "left", "0")

counties_sf <- st_read("./data/Shapefile/cb_2017_us_county_20m.shp", quiet = TRUE) %>%
  mutate(GEOID = pad5(GEOID))
states_sf <- states(cb = TRUE) %>% st_transform(4269) %>% mutate(GEOID = pad5(STATEFP))
counties_proj <- st_transform(counties_sf, 5070)

# --- O-D pairs: domestic truck shipments of SCTG 01-07 with positive tons ---
lookup <- readRDS("./data/Route/faf_zone_lookup.rds") %>%
  distinct(FAF_ID, final_geoid) %>% mutate(final_geoid = pad5(final_geoid))
faf <- read_csv("./data/FAF/FAF5.6.1_2018-2023.csv",
                col_types = cols(.default = "n"))
od <- faf %>%
  filter(trade_type == 1, sctg2 %in% 1:7, dms_mode == 1) %>%
  filter(if_any(starts_with("tons_"), ~ .x > 0)) %>%
  distinct(dms_orig, dms_dest) %>%
  mutate(o = sprintf("%03d", dms_orig), d = sprintf("%03d", dms_dest)) %>%
  left_join(lookup, by = c("o" = "FAF_ID")) %>% rename(dms_orig_GEOID = final_geoid) %>%
  left_join(lookup, by = c("d" = "FAF_ID")) %>% rename(dms_dest_GEOID = final_geoid) %>%
  filter(!is.na(dms_orig_GEOID), !is.na(dms_dest_GEOID)) %>%
  distinct(dms_orig_GEOID, dms_dest_GEOID)

endpoint <- function(id) {
  if (startsWith(id, "000")) st_centroid(filter(states_sf, GEOID == id))
  else st_point_on_surface(filter(counties_sf, GEOID == id))
}

route_one <- function(orig_id, dest_id) {
  if (orig_id == dest_id) {
    nm <- if (startsWith(orig_id, "000")) "State Route" else counties_sf$NAME[counties_sf$GEOID == orig_id][1]
    return(tibble(dms_orig_GEOID = orig_id, dms_dest_GEOID = dest_id, geoid = orig_id,
                  name = nm, distance = 1, total_distance = 1))
  }
  route <- tryCatch(osrmRoute(src = endpoint(orig_id), dst = endpoint(dest_id)),
                    error = function(e) NULL)
  if (is.null(route)) {
    return(tibble(dms_orig_GEOID = orig_id, dms_dest_GEOID = dest_id, geoid = NA_character_,
                  name = NA_character_, distance = NA_real_, total_distance = NA_real_))
  }
  seg <- st_intersection(st_transform(route, 5070), counties_proj) %>%
    mutate(distance = as.numeric(set_units(st_length(geometry), "mi")))
  seg %>% st_drop_geometry() %>%
    transmute(dms_orig_GEOID = orig_id, dms_dest_GEOID = dest_id, geoid = GEOID, name = NAME,
              distance, total_distance = sum(seg$distance, na.rm = TRUE))
}

route_table <- pmap_dfr(list(od$dms_orig_GEOID, od$dms_dest_GEOID), route_one) %>%
  mutate(distance = round(distance, 2), total_distance = round(total_distance, 2))

write_csv(route_table, "./data/Route/od_route_county_segments_rerun.csv")
