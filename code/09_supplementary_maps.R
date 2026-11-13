# ==============================================================================
# 09_supplementary_maps.R
# Purpose: 
#   1. Add FAF IDs and Distance Band (Truck + Food Only).
#   2. Classify Routes (Short/Med/Long).
#   3. Generate Supplementary Figures S2-S4 (Maps + Bar Charts).
#      **NOTE:** Uses 'avg*' variables to match Figures 1-3.
# ==============================================================================

library(dplyr)
library(stringr)
library(readr)
library(data.table)
library(sf)
library(ggplot2)
library(patchwork)
library(tidyr)

# --- 1. CONFIGURATION ---
PROJECT_DIR <- "."
DATA_RESULT <- file.path(PROJECT_DIR, "data")
MAP_DIR     <- file.path(PROJECT_DIR, "data/Shapefile")
ROUTE_DIR   <- file.path(PROJECT_DIR, "data/Route")

# Inputs
LOOKUP_FILE <- file.path(ROUTE_DIR, "faf_zone_lookup.rds")
FAF_RAW_CSV <- file.path(PROJECT_DIR, "data/FAF/FAF5.6.1_2018-2023.csv")
INPUT_DATA  <- file.path(DATA_RESULT, "panel_od_year_truck_sctg01_07.rds")
STATE_SHP   <- file.path(MAP_DIR, "cb_2018_us_state_5m.shp")

# Output Directory
SUPP_DIR <- file.path(PROJECT_DIR, "figures", "supplementary")
if(!dir.exists(SUPP_DIR)) dir.create(SUPP_DIR, recursive = TRUE)

# ==============================================================================
# PART 1: PREPARE ROUTE DATA (FAF IDs & BANDS)
# ==============================================================================

# --- 1.1 LOAD ROUTES (If not in environment) ---
# We try to load the GPKG if the object is missing
if(!exists("routes_within_joined")) {
  GPKG_PATH <- file.path(ROUTE_DIR, "od_route_geometries_conus.gpkg")
  if(file.exists(GPKG_PATH)) {
    message("Loading routes from GPKG...")
    routes_within_joined <- st_read(GPKG_PATH, quiet = TRUE)
  } else {
    stop("Object 'routes_within_joined' not found and GPKG is missing. Please run Script 02/08 first.")
  }
}

# The route geometry file already carries dist_band; Part 1 only runs if it is missing.
if (!"dist_band" %in% names(routes_within_joined)) {
# --- 1.2 PREPARE LOOKUP ---
lookup <- readRDS(LOOKUP_FILE)
lookup_clean <- lookup %>%
  mutate(
    FAF_ID = str_pad(as.character(FAF_ID), 3, "left", "0"),
    final_geoid = str_pad(as.character(final_geoid), 5, "left", "0")
  ) %>%
  select(FAF_ID, final_geoid) %>%
  distinct() 

# --- 1.3 EXTRACT DISTANCE BANDS (TRUCK + FOOD) ---
message("Extracting Distance Bands from FAF...")
faf_dist_info <- read_csv(FAF_RAW_CSV, 
                          col_select = c("dms_orig", "dms_dest", "dms_mode", "dist_band", "sctg2")) %>%
  filter(dms_mode == 1) %>%           # Truck
  mutate(sctg2 = as.numeric(sctg2)) %>%
  filter(sctg2 >= 1 & sctg2 <= 7) %>% # Food/Ag
  select(dms_orig, dms_dest, dist_band) %>% 
  distinct() %>% 
  group_by(dms_orig, dms_dest) %>%
  slice(1) %>% 
  ungroup() %>%
  mutate(
    dms_orig = str_pad(as.character(dms_orig), 3, "left", "0"),
    dms_dest = str_pad(as.character(dms_dest), 3, "left", "0")
  )

# --- 1.4 MERGE INTO ROUTES ---
message("Merging Bands into Spatial Data...")
routes_final <- routes_within_joined %>%
  mutate(
    dms_orig_GEOID = str_pad(as.character(dms_orig_GEOID), 5, "left", "0"),
    dms_dest_GEOID = str_pad(as.character(dms_dest_GEOID), 5, "left", "0")
  ) %>%
  left_join(lookup_clean, by = c("dms_orig_GEOID" = "final_geoid")) %>%
  rename(dms_orig_FAF = FAF_ID) %>%
  left_join(lookup_clean, by = c("dms_dest_GEOID" = "final_geoid")) %>%
  rename(dms_dest_FAF = FAF_ID) %>%
  left_join(faf_dist_info, by = c("dms_orig_FAF" = "dms_orig", "dms_dest_FAF" = "dms_dest"))

# Update Object
routes_within_joined <- routes_final
}  # input data in data/Route are never overwritten

# ==============================================================================
# PART 2: CLASSIFICATION & VERIFICATION
# ==============================================================================

# --- 2.0 USE THE PANEL'S DISTANCE BAND ---
# The distance band comes from the panel (truck records, dominant band; see
# 06_build_panel.R) so that the maps use the same groups as the regressions.
panel_band <- readRDS(INPUT_DATA) %>% distinct(unit, dist_band)
routes_within_joined <- routes_within_joined %>%
  mutate(unit = paste0(str_pad(as.character(dms_orig_GEOID), 5, "left", "0"),
                       str_pad(as.character(dms_dest_GEOID), 5, "left", "0"))) %>%
  select(-any_of("dist_band")) %>%
  left_join(panel_band, by = "unit")

# --- 2.1 CLASSIFY ---
routes_classified <- routes_within_joined %>%
  mutate(
    dist_cat = case_when(
      dist_band %in% c(1, 2, 3) ~ "Short (Band 1-3)",
      dist_band %in% c(4, 5)    ~ "Medium (Band 4-5)",
      dist_band %in% c(6, 7, 8) ~ "Long (Band 6-8)",
      TRUE ~ "Unknown"
    )
  )

# --- 2.2 VERIFY ---
summary_counts <- routes_classified %>%
  st_drop_geometry() %>%
  distinct(dms_orig_FAF, dms_dest_FAF, dist_cat) %>% 
  count(dist_cat, name = "n_routes")

print(summary_counts)
message("Total Unique Routes: ", sum(summary_counts$n_routes))

# --- 2.3 CREATE SUBSAMPLES ---
sub_short  <- routes_classified %>% filter(dist_cat == "Short (Band 1-3)")
sub_medium <- routes_classified %>% filter(dist_cat == "Medium (Band 4-5)")
sub_long   <- routes_classified %>% filter(dist_cat == "Long (Band 6-8)")

# ==============================================================================
# PART 3: GENERATE PLOTS (Figures S2-S4)
# ==============================================================================

# --- 3.1 LOAD PANEL DATA ---
df_panel_master <- readRDS(INPUT_DATA)
us_states <- st_read(STATE_SHP, quiet = TRUE) %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

svi_route_colors <- c("Low"="#008f91", "Low-Medium"="#4489c8", "Medium-High"="#ffcd44", "High"="#ed7e7a")

# --- 3.2 PLOTTING FUNCTION ---
create_figure_for_subsample <- function(sub_df, file_suffix) {
  
  # 1. Identify IDs
  target_ids <- sub_df %>% 
    st_drop_geometry() %>% 
    select(dms_orig_GEOID, dms_dest_GEOID) %>% 
    distinct()
  
  message(paste0(">>> Plotting ", file_suffix, " (n=", nrow(target_ids), ")"))
  
  # 2. Filter Data
  map_subset <- sub_df # Already filtered spatially
  bar_subset <- df_panel_master %>% semi_join(target_ids, by = c("dms_orig_GEOID", "dms_dest_GEOID"))
  
  # 3. Aggregation (USING AVG VARIABLES)
  # *** CRITICAL FIX: Changed distw* to avg* to match Figures 1-3 ***
  weather_summary <- bar_subset %>%
    rename(SviGroup = svi_cat) %>%
    filter(!is.na(SviGroup)) %>%
    group_by(SviGroup) %>%
    summarise(
      HighTemp = mean(avgHighTempFreq_3sd, na.rm = TRUE), # Was distw
      LowTemp  = mean(avgLowTempFreq_3sd,  na.rm = TRUE), # Was distw
      HighPrec = mean(avgHighPrecFreq,     na.rm = TRUE), # Was distw
      HighWind = mean(avgHighVsFreq,       na.rm = TRUE)  # Was distw
    ) %>% ungroup() %>%
    mutate(SviGroup = recode(SviGroup, "Low SVI" = "Low", "Medium-Low" = "Low-Medium"))
  
  # 4. Plots
  # Map
  map_plot <- ggplot() +
    geom_sf(data = us_states, fill = "white", color = "black", size = 0.6) + 
    geom_sf(data = map_subset, aes(color = svi_group), size = 0.6, alpha = 0.9, key_glyph = "path") +
    scale_color_manual(values = svi_route_colors, breaks = names(svi_route_colors), name = "Route by SVI", drop = FALSE, na.value = "grey85") +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1, linetype = "solid", fill = NA))) +
    coord_sf(datum = NA) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", legend.key.width = unit(1, "cm"), 
          panel.grid = element_blank(), axis.text = element_blank())
  
  # Bars
  bar_w <- 0.6
  lock_axis <- function(breaks) {
    scale_y_continuous(breaks = breaks, limits = range(breaks), expand = expansion(mult = c(0, 0), add = 0))
  }
  
  plot_bar <- function(data, y_var, title) {
    ggplot(data, aes(x = SviGroup, y = .data[[y_var]], fill = SviGroup)) +
      geom_col(width = bar_w) + 
      geom_text(aes(label = sprintf("%.2f", .data[[y_var]])), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = "bold") +
      scale_fill_manual(values = svi_route_colors) +
      labs(y = title, x = NULL) + theme_minimal() + 
      theme(legend.position = "none", axis.text.x = element_text(size=8), axis.title.y = element_text(size=8, face="bold"))
  }
  
  p1 <- plot_bar(weather_summary, "HighTemp", "Mean Extreme Hot Days")
  p2 <- plot_bar(weather_summary, "LowTemp", "Mean Extreme Cold Days")
  p3 <- plot_bar(weather_summary, "HighPrec", "Mean Heavy Precip Days") + lock_axis(c(0, 1, 2, 3, 3.5))
  p4 <- plot_bar(weather_summary, "HighWind", "Mean High Wind Days") + lock_axis(c(0, 1, 2, 3))
  
  final_plot <- (map_plot / (p1 | p2) / (p3 | p4)) + 
    plot_layout(heights = c(1.6, 1, 1)) + plot_annotation(tag_levels = 'a') & 
    theme(plot.tag = element_text(size = 12, face = "bold"), plot.tag.position = c(0.01, 0.99))
  
  out_png <- file.path(SUPP_DIR, paste0(file_suffix, ".png"))
  ggsave(out_png, final_plot, width = 7.2, height = 8.8, dpi = 300)
}

# --- 3.3 EXECUTE ---
create_figure_for_subsample(sub_short,  "FigureS2_Short")
create_figure_for_subsample(sub_medium, "FigureS3_Medium")
create_figure_for_subsample(sub_long,   "FigureS4_Long")

message("DONE! Supplementary Figures S2-S4 generated (using Avg-Weighted variables).")