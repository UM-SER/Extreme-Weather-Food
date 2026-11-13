# ==============================================================================
# 02_process_weather_nc.R (The "Simple Date" Fix)
# Purpose: Extract Daily Weather for ALL US Counties.
#          - Speed: High (Uses 'terra' brick method).
#          - Logic: Force Calendar Dates (Col 1 = Jan 1, Col 365 = Dec 31).
# ==============================================================================

library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(data.table)
library(stringr)
library(purrr)

# --- 1. CONFIGURATION ---
PROJECT_DIR <- "."
DATA_DIR    <- file.path(PROJECT_DIR, "data/Weather/gridmet_raw")
RESULT_DIR  <- file.path(PROJECT_DIR, "data/Weather")

# MAP PATH (Verify this matches your folder!)
MAP_DIR     <- file.path(PROJECT_DIR, "data/Shapefile") 
SHP_PATH    <- file.path(MAP_DIR, "cb_2017_us_county_20m.shp")

if(!dir.exists(RESULT_DIR)) dir.create(RESULT_DIR, recursive = TRUE)

VARS_TO_PROCESS <- list(
  "tmmx" = "tmmx",  # Max Temp
  "tmmn" = "tmmn",  # Min Temp
  "pr"   = "pr",    # Precip
  "vs"   = "vs"     # Wind
)

# --- 2. PREPARE MAP ---
message("Loading County Map...")
counties <- st_read(SHP_PATH, quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(GEOID = str_pad(GEOID, 5, "left", "0")) %>%
  dplyr::select(GEOID) 

message(paste("Loaded", nrow(counties), "counties."))

# --- 3. PROCESSING FUNCTION ---

process_variable <- function(var_name, var_pattern) {
  
  message(paste0("\n=== Processing Variable: ", var_name, " ==="))
  
  files <- list.files(file.path(DATA_DIR, var_pattern), 
                      pattern = "\\.nc$", full.names = TRUE)
  
  all_years_list <- list()
  
  for (f in files) {
    year_str <- str_extract(basename(f), "\\d{4}")
    message(paste("  Processing Year:", year_str))
    
    # 1. Load Raster
    r <- rast(f)
    
    # 2. Extract Data (The Fast Way)
    # result is a Matrix: Rows = Counties, Cols = Days
    vals <- exact_extract(r, counties, 'mean', progress = FALSE)
    
    # 3. GENERATE SIMPLE CALENDAR (The Old Simple Logic)
    # Create a sequence: Jan 1 to Dec 31 for this specific year
    date_seq <- seq(as.Date(paste0(year_str, "-01-01")), 
                    as.Date(paste0(year_str, "-12-31")), by="day")
    
    # Safety: Ensure matrix columns match calendar length
    # (Handles leap years or truncated files automatically)
    if(ncol(vals) != length(date_seq)) {
      # Use valid range only
      valid_len <- min(ncol(vals), length(date_seq))
      vals <- vals[, 1:valid_len]
      date_seq <- date_seq[1:valid_len]
    }
    
    # 4. ASSIGN DATES AS COLUMN NAMES
    # This aligns Col 1 -> "2018-01-01", Col 2 -> "2018-01-02"
    colnames(vals) <- as.character(date_seq)
    
    # 5. Convert to Long Format (What you are used to)
    dt <- as.data.table(vals)
    dt[, GEOID := counties$GEOID]
    
    # Melt: Turns columns "2018-01-01", "2018-01-02" into a 'date' column
    dt_long <- melt(dt, id.vars = "GEOID", 
                    variable.name = "date", 
                    value.name = "value")
    
    # 6. Final Polish
    # Convert date string back to proper Date object if needed, or keep string
    dt_long[, date := as.Date(date)]
    
    # Unit Conversion
    if (var_name %in% c("tmmx", "tmmn")) {
      dt_long[, value := value - 273.15] # Kelvin -> Celsius
    }
    
    all_years_list[[year_str]] <- dt_long
    rm(r, vals, dt, dt_long); gc()
  }
  
  # Combine & Save
  message(paste("  Merging all years for", var_name, "..."))
  final_dt <- rbindlist(all_years_list)
  col_n <- if(var_name %in% c("tmmx", "tmmn")) "temp" else var_name
  setnames(final_dt, "value", col_n)
  
  out_file <- file.path(RESULT_DIR, paste0("county_daily_", var_name, "_2018_2023.rds"))
  saveRDS(final_dt, out_file)
  message(paste("  Saved:", out_file))
}

# --- 4. EXECUTION ---
walk2(names(VARS_TO_PROCESS), VARS_TO_PROCESS, process_variable)
message("PROCESSING COMPLETE.")