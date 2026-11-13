# ==============================================================================
# 03_generate_weather_indices.R
# Purpose: Generate Route-Based Weather Indices for Freight Analysis.
#
# METHODOLOGY:
# 1. Dual Thresholds (Sensitivity Check):
#    a. Sensitive: Temp > (Mean + 3 * Seasonal_SD)
#    b. Robust:    Temp > (Mean + 5.0 deg C)
#
# 2. Dual Aggregations (Exposure Check):
#    a. Distance Weighted ('distw...'): Weights by miles driven in county.
#    b. Simple Average ('avg...'): Unweighted mean of counties on route.
#
# 3. Robustness Fixes:
#    - Aggregates County -> State weather for generic '000xx' IDs.
#    - Forces character classes to prevent ID corruption.
#    - Pads all GEOIDs to 5 digits.
# ==============================================================================

library(data.table)
library(dplyr)
library(stringr)

# --- 1. CONFIGURATION ---
PROJECT_DIR <- "."
WEATHER_DIR <- file.path(PROJECT_DIR, "data/Weather") 
HIST_DIR    <- file.path(PROJECT_DIR, "data/Weather")

ROUTE_FILE <- file.path(PROJECT_DIR, "data/Route/od_route_county_segments.csv")
OUTPUT_FILE <- file.path(WEATHER_DIR, "od_route_weather_exposure.rds")

# New Daily Weather Files (Step 02 Output)
WEATHER_FILES <- list(
  tmmx = file.path(WEATHER_DIR, "county_daily_tmmx_2018_2023.rds"),
  tmmn = file.path(WEATHER_DIR, "county_daily_tmmn_2018_2023.rds"),
  pr   = file.path(WEATHER_DIR, "county_daily_pr_2018_2023.rds"),
  vs   = file.path(WEATHER_DIR, "county_daily_vs_2018_2023.rds")
)

# Historical Baseline Files (Step 00 Output)
# Use 'baseline_hot_clean.csv' if you created the Excel fix, otherwise original.
BASELINE_FILES <- list(
  hot  = file.path(HIST_DIR, "county_baseline_tmmx_jja_1988_2017.csv"),
  cold = file.path(HIST_DIR, "county_baseline_tmmn_djf_1988_2017.csv")
)

# --- 2. HELPER FUNCTIONS ---

load_baseline <- function(path) {
  tryCatch({
    # Use standard CSV read to avoid some permission locking issues
    df <- read.csv(path, stringsAsFactors = FALSE)
    setDT(df)
    # Ensure standard padding (1001 -> "01001")
    df[, GEOID := str_pad(as.character(GEOID), 5, "left", "0")]
    return(df)
  }, error = function(e) stop(paste("FATAL: Could not read file.", e$message)))
}

add_state_averages <- function(dt_counts) {
  # Logic: Create "State Average" weather for routes that only have State IDs (000xx)
  dt_counts[, STATEFP := substr(GEOID, 1, 2)]
  
  # Identify all count columns dynamically
  count_cols <- names(dt_counts)[grepl("n_days", names(dt_counts))]
  
  # Calculate Mean of all counties in the state
  dt_state <- dt_counts[, lapply(.SD, mean, na.rm=TRUE), 
                        by = .(STATEFP, year), 
                        .SDcols = count_cols]
  
  # Format State ID as 5-digit GEOID (e.g. "02" -> "00002")
  dt_state[, GEOID := str_pad(STATEFP, 5, "left", "0")]
  dt_state[, STATEFP := NULL]
  
  # Append to the main table
  dt_final <- rbind(dt_counts, dt_state, fill=TRUE)
  if("STATEFP" %in% names(dt_final)) dt_final[, STATEFP := NULL]
  
  return(dt_final)
}

# --- 3. CALCULATE COUNTY-LEVEL EXTREMES ---
message("Calculating County Weather Events...")

# A. EXTREME HEAT (TMMX)
message("  1/4: Extreme Heat...")
dt_hot <- readRDS(WEATHER_FILES$tmmx)
base_hot <- load_baseline(BASELINE_FILES$hot)

dt_hot <- merge(dt_hot, base_hot, by="GEOID")
dt_hot[, year := as.integer(format(date, "%Y"))]

# Calculate Counts (Dual Thresholds)
counts_hot <- dt_hot[, .(
  n_days_3sd = sum(temp > (mean_JJA + 3 * sd_JJA), na.rm=TRUE), # Sensitive
  n_days_5c  = sum(temp > (mean_JJA + 5.0), na.rm=TRUE)         # Robust
), by = .(GEOID, year)]

counts_hot <- add_state_averages(counts_hot)
rm(dt_hot, base_hot); gc() 

# B. EXTREME COLD (TMMN)
message("  2/4: Extreme Cold...")
dt_cold <- readRDS(WEATHER_FILES$tmmn)
base_cold <- load_baseline(BASELINE_FILES$cold)

dt_cold <- merge(dt_cold, base_cold, by="GEOID")
dt_cold[, year := as.integer(format(date, "%Y"))]

counts_cold <- dt_cold[, .(
  n_days_3sd = sum(temp < (mean_DJF - 3 * sd_DJF), na.rm=TRUE),
  n_days_5c  = sum(temp < (mean_DJF - 5.0), na.rm=TRUE)
), by = .(GEOID, year)]

counts_cold <- add_state_averages(counts_cold)
rm(dt_cold, base_cold); gc()

# C. PRECIPITATION (PR)
message("  3/4: High Precip...")
dt_pr <- readRDS(WEATHER_FILES$pr)
dt_pr[, year := as.integer(format(date, "%Y"))]
counts_pr <- dt_pr[pr > 50, .(n_days = .N), by = .(GEOID, year)]
counts_pr <- add_state_averages(counts_pr)
rm(dt_pr); gc()

# D. WIND (VS)
message("  4/4: High Wind...")
dt_vs <- readRDS(WEATHER_FILES$vs)
dt_vs[, year := as.integer(format(date, "%Y"))]
counts_vs <- dt_vs[vs > 10, .(n_days = .N), by = .(GEOID, year)]
counts_vs <- add_state_averages(counts_vs)
rm(dt_vs); gc()

# --- 4. MAP TO ROUTES (BOTH AGGREGATIONS) ---
message("Mapping to Routes (Weighted & Simple Average)...")

# *** CRITICAL FIX: Force character IDs to prevent "00002" -> 2 corruption ***
routes <- fread(ROUTE_FILE, 
                colClasses = list(character = c("dms_orig_GEOID", "dms_dest_GEOID", "geoid")))

# Safety Padding
routes[, geoid := str_pad(geoid, 5, "left", "0")]
routes[, dms_orig_GEOID := str_pad(dms_orig_GEOID, 5, "left", "0")]
routes[, dms_dest_GEOID := str_pad(dms_dest_GEOID, 5, "left", "0")]

# Master Aggregation Function
calc_index_all <- function(route_dt, weather_counts_dt, prefix) {
  
  merged <- merge(route_dt, weather_counts_dt, by.x="geoid", by.y="GEOID", 
                  all.x=TRUE, allow.cartesian=TRUE)
  
  count_cols <- names(weather_counts_dt)[grepl("n_days", names(weather_counts_dt))]
  
  # Fill NAs (0 days of extreme weather)
  for(col in count_cols) {
    set(merged, i=which(is.na(merged[[col]])), j=col, value=0)
  }
  
  # 1. Pre-Calculate Distance Weighted Values
  w_cols <- c()
  for(col in count_cols) {
    w_name <- paste0("w_", col)
    merged[, (w_name) := (distance / total_distance) * get(col)]
    w_cols <- c(w_cols, w_name)
  }
  
  # 2. Dynamic Aggregation Construction
  aggs <- list()
  
  # Add Weighted Sums (distw)
  for(wc in w_cols) {
    orig_suffix <- sub("w_n_days", "", wc) 
    out_name <- paste0("distw", prefix, "Freq", orig_suffix)
    aggs[[out_name]] <- call("sum", as.name(wc), na.rm=TRUE)
  }
  
  # Add Simple Means (avg)
  for(cc in count_cols) {
    orig_suffix <- sub("n_days", "", cc)
    out_name <- paste0("avg", prefix, "Freq", orig_suffix)
    aggs[[out_name]] <- call("mean", as.name(cc), na.rm=TRUE)
  }
  
  # Execute
  final <- merged[, eval(as.call(c(list(quote(list)), aggs))), 
                  by = .(dms_orig_GEOID, dms_dest_GEOID, year)]
  
  return(final)
}

# Run Calculations
idx_hot  <- calc_index_all(routes, counts_hot,  "HighTemp") 
idx_cold <- calc_index_all(routes, counts_cold, "LowTemp")
idx_rain <- calc_index_all(routes, counts_pr,   "HighPrec") 
idx_wind <- calc_index_all(routes, counts_vs,   "HighVs")   

# --- 5. MERGE & SAVE ---
message("Merging Master Index...")

master_index <- idx_hot %>%
  full_join(idx_cold, by=c("dms_orig_GEOID", "dms_dest_GEOID", "year")) %>%
  full_join(idx_rain, by=c("dms_orig_GEOID", "dms_dest_GEOID", "year")) %>%
  full_join(idx_wind, by=c("dms_orig_GEOID", "dms_dest_GEOID", "year"))

# Zero-fill numeric NAs
num_cols <- names(master_index)[sapply(master_index, is.numeric)]
num_cols <- setdiff(num_cols, "year") # Don't touch year

for (col in num_cols) {
  set(master_index, i=which(is.na(master_index[[col]])), j=col, value=0)
}

# REMOVE INVALID ROWS (Failed Joins)
# year > 0 prevents the "Year 0" artifact
master_index <- master_index[!is.na(year) & year > 0]

saveRDS(master_index, OUTPUT_FILE)
message(paste("DONE! Master Index saved to:", OUTPUT_FILE))
message(paste("Total valid rows:", nrow(master_index)))
message("Columns generated:")
print(names(master_index))
