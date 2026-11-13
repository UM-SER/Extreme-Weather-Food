# ==============================================================================
# 05_faf_merge_analysis.R
# Purpose: Final Assembly of Regression Panel.
#          - Calculates BOTH Tons Share and Ton-Miles Share.
#          - Merges Weather, SVI, and Distance.
# ==============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(data.table)

# --- 1. CONFIGURATION ---
PROJECT_DIR <- "."

# Inputs
FAF_CSV      <- file.path(PROJECT_DIR, "data/FAF/FAF5.6.1_2018-2023.csv")
WEATHER_IDX  <- file.path(PROJECT_DIR, "data/Weather/od_route_weather_exposure.rds") 
SVI_IDX      <- file.path(PROJECT_DIR, "data/SVI/od_route_svi.rds")            
LOOKUP_FILE  <- file.path(PROJECT_DIR, "data/Route/faf_zone_lookup.rds") 
ROUTE_FILE   <- file.path(PROJECT_DIR, "data/Route/od_route_county_segments.csv") # Validated Table

# Output
OUTPUT_FILE  <- file.path(PROJECT_DIR, "data/panel_od_year_truck_sctg01_07.rds")

# --- 2. LOAD & PROCESS FAF DATA (ALL MODES) ---
message("Processing FAF Data (Calculating Shares)...")

# Load raw (Keep all modes initially)
faf <- read_csv(FAF_CSV, col_types = cols(.default = "n")) 

# Filter: Domestic Trade, Ag/Food Commodities (1-7)
faf_subset <- faf %>%
  filter(trade_type == 1, sctg2 %in% 1:7)

# Pivot Long (Years)
faf_long <- faf_subset %>%
  pivot_longer(
    cols = matches("^(tons_|value_|tmiles_)"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(year = as.integer(year))

# --- 3. AGGREGATE & CALCULATE SHARE ---
message("Calculating Truck Shares (Tons & Ton-Miles)...")

faf_agg <- faf_long %>%
  group_by(dms_orig, dms_dest, year) %>%
  summarise(
    dist_band = max(dist_band, na.rm = TRUE),
    # Total (All Modes)
    all_tons   = sum(tons, na.rm = TRUE),
    all_tmiles = sum(tmiles, na.rm = TRUE),
    
    # Truck Only (Mode 1)
    truck_tons   = sum(tons[dms_mode == 1], na.rm = TRUE),
    truck_tmiles = sum(tmiles[dms_mode == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate Share (0 to 1)
    truck_share_tons   = ifelse(all_tons == 0, 0, truck_tons / all_tons),
    truck_share_tmiles = ifelse(all_tmiles == 0, 0, truck_tmiles / all_tmiles) # <-- ADDED
  )

# --- 3b. DISTANCE BAND FROM TRUCK RECORDS ---
# FAF reports dist_band per record, and it can differ across modes (and across
# truck records) for the same O-D pair. Each route gets one time-invariant band:
# the band that carries the most truck tons over 2018-2023 (ties: longer band).
truck_band <- faf_long %>%
  filter(dms_mode == 1) %>%
  group_by(dms_orig, dms_dest, dist_band) %>%
  summarise(band_tons = sum(tons, na.rm = TRUE), .groups = "drop") %>%
  arrange(dms_orig, dms_dest, desc(band_tons), desc(dist_band)) %>%
  distinct(dms_orig, dms_dest, .keep_all = TRUE) %>%
  select(dms_orig, dms_dest, truck_dist_band = dist_band)

faf_agg <- faf_agg %>%
  left_join(truck_band, by = c("dms_orig", "dms_dest")) %>%
  mutate(dist_band = truck_dist_band) %>%
  select(-truck_dist_band)

# --- 4. FILTER FOR TRUCK ANALYSIS ---
message("Filtering for valid truck routes...")

# We keep routes where Truck Volume > 0 so 'total_tons' is valid for regression
faf_truck_panel <- faf_agg %>%
  filter(truck_tons > 0) %>%
  rename(
    total_tons = truck_tons,      # Truck Volume
    total_tmiles = truck_tmiles   # Truck Ton-Miles
  )

# --- 5. STANDARDIZE IDs & MERGE ---
message("Merging Context Data...")

lookup <- readRDS(LOOKUP_FILE) %>% dplyr::select(FAF_ID, final_geoid) %>% distinct()

# Map IDs
panel_mapped <- faf_truck_panel %>%
  mutate(orig_3d = sprintf("%03d", dms_orig), dest_3d = sprintf("%03d", dms_dest)) %>%
  left_join(lookup, by = c("orig_3d" = "FAF_ID")) %>% rename(dms_orig_GEOID = final_geoid) %>%
  left_join(lookup, by = c("dest_3d" = "FAF_ID")) %>% rename(dms_dest_GEOID = final_geoid) %>%
  filter(!is.na(dms_orig_GEOID) & !is.na(dms_dest_GEOID)) %>%
  mutate(
    dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"),
    dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0")
  )

# Load External Indices
weather <- readRDS(WEATHER_IDX) %>% 
  mutate(year = as.integer(year), 
         dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"), 
         dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0"))

svi <- readRDS(SVI_IDX) %>% 
  mutate(dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"), 
         dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0"))

# Load Distances
dist_dt <- fread(ROUTE_FILE, colClasses = list(character = c("dms_orig_GEOID", "dms_dest_GEOID")))
dist_lookup <- dist_dt[, .(total_distance = mean(total_distance, na.rm=TRUE)), 
                       by = .(dms_orig_GEOID, dms_dest_GEOID)] %>%
  mutate(dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"), 
         dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0"))

# Final Merge
final_panel <- panel_mapped %>%
  left_join(weather, by = c("dms_orig_GEOID", "dms_dest_GEOID", "year")) %>%
  left_join(svi, by = c("dms_orig_GEOID", "dms_dest_GEOID")) %>%
  left_join(dist_lookup, by = c("dms_orig_GEOID", "dms_dest_GEOID"))

# --- 6. CLEANUP ---
message("Finalizing Panel...")

final_panel <- final_panel %>%
  mutate(
    unit = paste0(dms_orig_GEOID, dms_dest_GEOID),
    across(matches("^(distw|avg)"), ~replace_na(., 0)),
    total_distance = replace_na(total_distance, 0),
    
    # SVI Categories
    svi_cat = cut(avgSVI, 
                  breaks = quantile(avgSVI, probs = seq(0, 1, 0.25), na.rm=TRUE),
                  labels = c("Low", "Low-Medium", "Medium-High", "High"),
                  include.lowest = TRUE),
    
    dist_cat = case_when(
      dist_band %in% c(1, 2, 3) ~ "Short",
      dist_band %in% c(4, 5)    ~ "Medium",
      dist_band %in% c(6, 7, 8) ~ "Long",
      TRUE ~ "Unknown"
    )
  )

saveRDS(final_panel, OUTPUT_FILE)
message(paste("DONE! Panel saved to:", OUTPUT_FILE))
