# ==============================================================================
# 04_generate_svi_index.R
# Purpose: Generate Route-Based SVI Indices (Static).
# Method:  Uses 2022 SVI as the baseline for all routes.
#          - Calculates Distance Weighted SVI (distwSVI).
#          - Calculates Simple Average SVI (avgSVI).
#          - Handles State Aggregation for "000xx" generic route IDs.
# Output:  od_route_svi.rds
# ==============================================================================

library(data.table)
library(dplyr)
library(stringr)

# --- 1. CONFIGURATION ---
PROJECT_DIR <- "."

# Inputs
SVI_FILE    <- file.path(PROJECT_DIR, "data/SVI/county_svi_2022.csv")
ROUTE_FILE <- file.path(PROJECT_DIR, "data/Route/od_route_county_segments_fullgeom.csv")  # 2026-11-11: full road lines (R2_19/R2_20); was od_route_county_segments.csv (simplified lines, 02)
# Output
OUTPUT_FILE <- file.path(PROJECT_DIR, "data/SVI/od_route_svi.rds")

# --- 2. PREPARE SVI DATA ---
message("Loading and Preparing SVI Data...")

if(!file.exists(SVI_FILE)) stop("SVI File missing! Check path.")

# Load SVI
dt_svi <- read.csv(SVI_FILE, stringsAsFactors = FALSE)
setDT(dt_svi)

# Filter for 2022 Baseline
if("year" %in% names(dt_svi)) {
  dt_svi <- dt_svi[year == 2022, .(GEOID, svi)]
} else {
  # If no year column, ensure we just take ID and SVI columns
  dt_svi <- dt_svi[, .(GEOID, svi)]
}

# Standardize GEOID Padding
dt_svi[, GEOID := str_pad(as.character(GEOID), 5, "left", "0")]

# --- 3. STATE AGGREGATION (For Generic Routes) ---
message("Calculating State Averages for Generic Routes...")

# Create State ID (first 2 digits)
dt_svi[, STATEFP := substr(GEOID, 1, 2)]

# Calculate Mean SVI per State
dt_state <- dt_svi[, .(svi = mean(svi, na.rm=TRUE)), by = .(STATEFP)]

# Format State ID as 5-digit GEOID (e.g. "02" -> "00002")
dt_state[, GEOID := str_pad(STATEFP, 5, "left", "0")]
dt_state[, STATEFP := NULL]

# Combine County SVI + State SVI into one lookup table
dt_svi_final <- rbind(dt_svi[, .(GEOID, svi)], dt_state)

# --- 4. MAP TO ROUTES ---
message("Mapping SVI to Routes...")

# Load Routes (Force character IDs)
routes <- fread(ROUTE_FILE, 
                colClasses = list(character = c("dms_orig_GEOID", "dms_dest_GEOID", "geoid")))

# Ensure Padding
routes[, geoid := str_pad(geoid, 5, "left", "0")]
routes[, dms_orig_GEOID := str_pad(dms_orig_GEOID, 5, "left", "0")]
routes[, dms_dest_GEOID := str_pad(dms_dest_GEOID, 5, "left", "0")]

# Merge Route Segments with SVI Lookup
# Left join ensures we keep the route structure
merged <- merge(routes, dt_svi_final, by.x="geoid", by.y="GEOID", all.x=TRUE)

# Check for missing SVI coverage
missing_count <- nrow(merged[is.na(svi)])
if(missing_count > 0) {
  message(paste("Warning:", missing_count, "segments are missing SVI data. Filling with mean."))
  global_mean <- mean(dt_svi_final$svi, na.rm=TRUE)
  merged[is.na(svi), svi := global_mean]
}

# --- 5. CALCULATE INDICES ---
message("Calculating Final Indices...")

final_index <- merged[, .(
  # Weighted by distance in each county
  distwSVI = sum((distance / total_distance) * svi, na.rm = TRUE),
  # Simple average of all counties on route
  avgSVI   = mean(svi, na.rm = TRUE)
), by = .(dms_orig_GEOID, dms_dest_GEOID)]

# --- 6. SAVE ---
saveRDS(final_index, OUTPUT_FILE)
message(paste("DONE! Route SVI Index saved to:", OUTPUT_FILE))
print(head(final_index))
