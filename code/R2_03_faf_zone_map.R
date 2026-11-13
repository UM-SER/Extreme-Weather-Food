# ==============================================================================
# 03_faf_zone_map.R
# Comment 1a — Map all 132 FAF5 domestic zones (contiguous US)
#
# Coverage:
#   119 zones  → CFS Metro / Remainder-of-State shapefiles (matched by GEOID)
#    12 zones  → Full-state polygon from tl_2017_us_state.shp
#     1 zone   → Alaska (dms_orig GEOID 00002), excluded from contiguous map
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)

R          <- "."
MAP_DIR    <- file.path(R, "data/Shapefile")
PANEL_FILE <- file.path(R, "data/panel_od_year_truck_sctg01_07.rds")
OUT_DIR    <- file.path(R, "figures", "supplementary")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ==============================================================================
# 1. Build FAF zone → GEOID crosswalk from panel
# ==============================================================================
df    <- readRDS(PANEL_FILE)
xwalk <- df %>%
  select(dms_orig, dms_orig_GEOID) %>%
  distinct() %>%
  mutate(GEOID_str = formatC(as.integer(dms_orig_GEOID), width = 5, flag = "0"))

cat("FAF zones:", nrow(xwalk), "\n")

# Split into matched (standard county FIPS) and synthetic (00xxx = whole-state)
xwalk_std  <- xwalk %>% filter(!grepl("^00", GEOID_str))   # 119 zones
xwalk_fake <- xwalk %>% filter(grepl("^00", GEOID_str))    # 13 zones (incl. AK)
# Extract state FIPS from synthetic GEOID (last 2 chars)
xwalk_fake <- xwalk_fake %>%
  mutate(state_fips = substr(GEOID_str, 4, 5))

cat("Standard GEOID zones:", nrow(xwalk_std), "\n")
cat("Whole-state zones:   ", nrow(xwalk_fake), "\n")

# ==============================================================================
# 2. Load CFS shapefiles and join 119 zones
# ==============================================================================
metro     <- st_read(file.path(MAP_DIR, "2017_CFS_Metro_Areas.shp"),    quiet = TRUE)
remainder <- st_read(file.path(MAP_DIR, "2017_Remainder_of_States.shp"), quiet = TRUE)
cfs_all   <- rbind(
  metro[, c("GEOID","CFS17_NAME","geometry")],
  remainder[, c("GEOID","CFS17_NAME","geometry")]
) %>% distinct(GEOID, .keep_all = TRUE)   # drop any duplicate GEIDs

zones_cfs <- cfs_all %>%
  inner_join(xwalk_std[, c("dms_orig","GEOID_str")], by = c("GEOID" = "GEOID_str")) %>%
  rename(zone_name = CFS17_NAME)

cat("CFS zones matched:", nrow(zones_cfs), "\n")

# ==============================================================================
# 3. Load state shapefile and get whole-state polygons for 12 zones
#    (exclude Alaska state FIPS 02 — not in contiguous US)
# ==============================================================================
states_shp <- st_read(file.path(MAP_DIR, "tl_2017_us_state.shp"), quiet = TRUE)

# Exclude non-contiguous states/territories for the background too
excl_fips <- c("02","15","60","66","69","72","78")
states_cont <- states_shp %>% filter(!STATEFP %in% excl_fips)

# Whole-state FAF zones in contiguous US
xwalk_fake_cont <- xwalk_fake %>% filter(state_fips != "02")   # drop AK
cat("Whole-state zones in contiguous US:", nrow(xwalk_fake_cont), "\n")

zones_state <- states_cont %>%
  inner_join(xwalk_fake_cont[, c("dms_orig","state_fips")],
             by = c("STATEFP" = "state_fips")) %>%
  mutate(zone_name = paste0("Remainder of ", NAME)) %>%
  select(dms_orig, zone_name, geometry)

# ==============================================================================
# 4. Combine into one sf object (all 131 contiguous FAF zones)
# ==============================================================================
zones_cfs_sel   <- zones_cfs   %>% select(dms_orig, zone_name, geometry)
zones_combined  <- rbind(zones_cfs_sel, zones_state)

cat("Total zones in map:", nrow(zones_combined), "(expect 129 = 132 minus Alaska and the two Hawaii zones, which have no route in the sample)\n")

# Project to Albers Equal Area
proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m"
zones_p  <- st_transform(zones_combined, crs = proj)
states_p <- st_transform(states_cont,    crs = proj)

# Crop to contiguous US bounding box (removes Pacific islands, Caribbean outliers)
bbox_cont <- st_bbox(c(xmin = -2500000, xmax = 2800000,
                        ymin = -1400000, ymax = 1600000),
                     crs = st_crs(zones_p))
zones_p  <- st_crop(zones_p,  bbox_cont)
states_p <- st_crop(states_p, bbox_cont)

# ==============================================================================
# 5. Plot — two versions
# ==============================================================================

# ---- 5a. Simple zone-type map (metro vs remainder) ----
# Tag zone type using zone_name: "Remainder of..." identifies non-metro zones
zones_p <- zones_p %>%
  mutate(zone_type = case_when(
    dms_orig %in% zones_state$dms_orig          ~ "Whole-State Zone",
    grepl("Remainder", zone_name, ignore.case = TRUE) ~ "Remainder of State",
    TRUE                                          ~ "CFS Metro Area"
  ))

# Count zones by type for legend labels
zone_counts <- table(zones_p$zone_type)
n_metro     <- as.integer(zone_counts["CFS Metro Area"])
n_remainder <- as.integer(zone_counts["Remainder of State"])
n_whole     <- as.integer(zone_counts["Whole-State Zone"])

p1 <- ggplot() +
  geom_sf(data = zones_p,
          aes(fill = zone_type),
          color = "white", linewidth = 0.12) +
  geom_sf(data = states_p,
          fill = NA, color = "gray20", linewidth = 0.45) +
  scale_fill_manual(
    values = c(
      "CFS Metro Area"      = "#2166ac",
      "Remainder of State"  = "#92c5de",
      "Whole-State Zone"    = "#d1e5f0"
    ),
    labels = c(
      "CFS Metro Area"     = paste0("CFS Metro Area (N=", n_metro, ")"),
      "Remainder of State" = paste0("Remainder of State (N=", n_remainder, ")"),
      "Whole-State Zone"   = paste0("Whole-State Zone (N=", n_whole, ")")
    ),
    name = "FAF5 Zone Type"
  ) +
  labs(
    title   = "FAF5 Zones in Contiguous United States",
    caption = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0.5,
                                   margin = margin(b = 14)),
    legend.position = "bottom",
    legend.key.size = unit(0.45, "cm"),
    legend.text     = element_text(size = 9),
    legend.title    = element_text(face = "bold", size = 9),
    plot.margin     = margin(10, 12, 8, 12)
  )

ggsave(file.path(OUT_DIR, "FigureS1_FAF_Zones.png"),
       p1, width = 10, height = 6.2, dpi = 300, bg = "white")
message("Saved: Figure_FAF_Zones.png")

# ---- 5b. Route-frequency heatmap (number of destination zones per origin) ----
FAF_CSV <- file.path(R, "data/FAF/FAF5.6.1_2018-2023.csv")
if (file.exists(FAF_CSV)) {  # this panel needs the raw FAF5 CSV
faf <- read.csv(FAF_CSV)
route_count <- faf %>%
  filter(trade_type == 1, sctg2 %in% 1:7, dms_mode == 1) %>%
  group_by(dms_orig) %>%
  summarise(n_dest = n_distinct(dms_dest), .groups = "drop")

zones_heat <- zones_p %>%
  left_join(route_count, by = "dms_orig")

p2 <- ggplot() +
  geom_sf(data = zones_heat,
          aes(fill = n_dest),
          color = "white", linewidth = 0.12) +
  geom_sf(data = states_p,
          fill = NA, color = "gray20", linewidth = 0.45) +
  scale_fill_distiller(
    palette   = "YlOrRd",
    direction = 1,
    name      = "Destination\nzones served",
    na.value  = "gray80"
  ) +
  labs(
    title    = "Food Freight Reach by Origin FAF Zone",
    subtitle = "Number of distinct destination zones receiving truck-based food freight (SCTG 1–7), 2018–2023",
    caption  = "Sources: FAF5 (BTS/FHWA); authors' calculations"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle   = element_text(size = 8.5, hjust = 0.5, color = "gray35",
                                   margin = margin(b = 4)),
    plot.caption    = element_text(size = 7,   hjust = 1,   color = "gray50"),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm"),
    legend.title    = element_text(face = "bold", size = 9),
    legend.text     = element_text(size = 8),
    plot.margin     = margin(10, 12, 8, 12)
  )

ggsave(file.path(OUT_DIR, "Figure_FAF_Zones_RouteCount.png"),
       p2, width = 10, height = 6.2, dpi = 300, bg = "white")
message("Saved: Figure_FAF_Zones_RouteCount.png")
}

# Summary
cat("\n=== Map summary ===\n")
cat("Zones plotted (contiguous US):  ", nrow(zones_p), "\n")
cat("Zone type breakdown:\n")
print(table(zones_p$zone_type))
if (exists("route_count")) cat("Route count range: ", min(route_count$n_dest), "–", max(route_count$n_dest), "\n")
if (exists("route_count")) cat("Mean destinations per origin:   ", round(mean(route_count$n_dest), 1), "\n")
message("\nAll done.")
