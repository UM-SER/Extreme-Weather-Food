# ==============================================================================
# 00_build_location_lookup.R
# Purpose: Build Master Coordinate Lookup.
#          Standardizes State IDs to Census Format (e.g. Alaska = "00002").
# ==============================================================================

library(sf)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# --- CONFIG ---
PROJECT_DIR <- "."
MAP_DIR     <- file.path(PROJECT_DIR, "data/Shapefile")
FAF_META    <- file.path(PROJECT_DIR, "data/FAF/FAF5_metadata.xlsx")
METRO_SHP   <- file.path(MAP_DIR, "2017_CFS_Metro_Areas.shp")
STATE_SHP   <- file.path(MAP_DIR, "cb_2018_us_state_5m.shp")
OUTPUT_RDS  <- file.path(PROJECT_DIR, "data/Route/faf_zone_lookup.rds")

# --- LOAD DATA ---
message("Loading Maps & Metadata...")
meta <- read_excel(FAF_META, sheet = "FAF Zone (Domestic)") %>%
  mutate(
    FAF_ID = sprintf("%03d", as.numeric(`Numeric Label`)),
    State_FIPS = substr(FAF_ID, 1, 2),
    Clean_Name = str_trim(gsub("\r", "", `Long Description`))
  )

metro_map <- st_read(METRO_SHP, quiet = TRUE)
metro_data <- metro_map %>% st_drop_geometry() %>%
  dplyr::select(CFS17_NAME, GEOID, INTPTLAT, INTPTLON) %>%
  mutate(lat_metro = as.numeric(INTPTLAT), lon_metro = as.numeric(INTPTLON), geoid_metro = as.character(GEOID)) %>%
  dplyr::select(CFS17_NAME, geoid_metro, lat_metro, lon_metro)

state_map <- st_read(STATE_SHP, quiet = TRUE)
state_centroids <- state_map %>% st_centroid() %>% st_coordinates() %>% as.data.frame() %>%
  bind_cols(state_map %>% st_drop_geometry() %>% dplyr::select(STATEFP, NAME)) %>%
  rename(lon_state = X, lat_state = Y) %>%
  mutate(
    STATEFP = str_pad(as.character(STATEFP), 2, "left", "0"),
    # CRITICAL: Census Standard ID (00002)
    geoid_state = str_pad(STATEFP, 5, "left", "0") 
  )

# --- JOIN ---
message("Building Lookup...")
final_table <- meta %>%
  left_join(metro_data, by = c("Clean_Name" = "CFS17_NAME")) %>%
  left_join(state_centroids, by = c("State_FIPS" = "STATEFP")) %>%
  mutate(
    final_lat = coalesce(lat_metro, lat_state),
    final_lon = coalesce(lon_metro, lon_state),
    # Priority: Metro ID -> Census State ID
    final_geoid = coalesce(geoid_metro, geoid_state),
    match_source = case_when(!is.na(lat_metro) ~ "Metro", !is.na(lat_state) ~ "State", TRUE ~ "FAILED")
  )

# --- SAVE ---
final_output <- final_table %>%
  dplyr::select(FAF_ID, Clean_Name, lat = final_lat, lon = final_lon, final_geoid, match_source)

saveRDS(final_output, OUTPUT_RDS)
message(paste("Lookup saved to:", OUTPUT_RDS))