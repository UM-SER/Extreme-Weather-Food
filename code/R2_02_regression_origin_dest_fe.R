# ==============================================================================
# 02_regression_origin_dest_fe.R
# Purpose: Comment 2 robustness — add origin×year and destination×year FE
#          to isolate route-level disruption from OD market shocks.
#
# Specifications:
#   Baseline:    total_tons ~ weather | unit + year
#   Robustness A: total_tons ~ weather | unit + dms_orig^year
#   Robustness B: total_tons ~ weather | unit + dms_dest^year
#   Robustness C: total_tons ~ weather | unit + dms_orig^year + dms_dest^year
# ==============================================================================

library(fixest)
library(dplyr)

# --- PATHS ---
PANEL_FILE  <- "./data/panel_od_year_truck_sctg01_07.rds"
OUTPUT_DIR  <- "./tables"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# --- LOAD DATA ---
message("Loading panel data...")
df <- readRDS(PANEL_FILE)
message("Panel dimensions: ", nrow(df), " rows x ", ncol(df), " cols")

# Exclude Alaska (zone 20) and Hawaii (zones 151, 159) — non-contiguous states
ak_hi_zones <- c(20, 151, 159)
# 2026-10-26: AK/HI routes kept so all tables use the full sample (N = 74,238)
# df <- df %>% filter(!dms_orig %in% ak_hi_zones & !dms_dest %in% ak_hi_zones)
message("After removing AK/HI: ", nrow(df), " rows")

# --- CHECK: do dms_orig / dms_dest exist in panel? ---
if (!all(c("dms_orig", "dms_dest") %in% names(df))) {
  stop("dms_orig or dms_dest not found in panel. Need to merge from FAF or route table.")
}

# --- WEATHER VARIABLES (3SD simple average, matches paper main spec) ---
weather_vars <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd",
                  "avgHighPrecFreq",     "avgHighVsFreq")

rhs <- paste(weather_vars, collapse = " + ")

my_signif <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)

my_dict <- c(
  "total_tons"           = "Total Tons",
  "total_tmiles"         = "Total Ton-Miles",
  "avgHighTempFreq_3sd"  = "Extreme Hot Days",
  "avgLowTempFreq_3sd"   = "Extreme Cold Days",
  "avgHighPrecFreq"      = "Heavy Precip Days",
  "avgHighVsFreq"        = "High Wind Days"
)

# ==============================================================================
# TONS
# ==============================================================================
message("Running regressions (Tons)...")

# Baseline
m_base_tons <- feols(
  as.formula(paste("total_tons ~", rhs, "| unit + year")),
  cluster = ~unit, data = df
)

# + Origin × Year FE
m_orig_tons <- feols(
  as.formula(paste("total_tons ~", rhs, "| unit + dms_orig^year")),
  cluster = ~unit, data = df
)

# + Destination × Year FE
m_dest_tons <- feols(
  as.formula(paste("total_tons ~", rhs, "| unit + dms_dest^year")),
  cluster = ~unit, data = df
)

# + Both Origin × Year and Destination × Year FE
m_both_tons <- feols(
  as.formula(paste("total_tons ~", rhs, "| unit + dms_orig^year + dms_dest^year")),
  cluster = ~unit, data = df
)

# ==============================================================================
# TON-MILES
# ==============================================================================
message("Running regressions (Ton-Miles)...")

m_base_tm <- feols(
  as.formula(paste("total_tmiles ~", rhs, "| unit + year")),
  cluster = ~unit, data = df
)

m_orig_tm <- feols(
  as.formula(paste("total_tmiles ~", rhs, "| unit + dms_orig^year")),
  cluster = ~unit, data = df
)

m_dest_tm <- feols(
  as.formula(paste("total_tmiles ~", rhs, "| unit + dms_dest^year")),
  cluster = ~unit, data = df
)

# + Both Origin × Year and Destination × Year FE
m_both_tm <- feols(
  as.formula(paste("total_tmiles ~", rhs, "| unit + dms_orig^year + dms_dest^year")),
  cluster = ~unit, data = df
)

# ==============================================================================
# OUTPUT TABLES
# ==============================================================================
message("Saving tables...")

# Tons table
etable(
  m_base_tons, m_orig_tons, m_dest_tons, m_both_tons,
  signif.code = my_signif,
  dict        = my_dict,
  headers     = list("Fixed Effects" = c("Baseline\n(Route+Year)",
                                         "+Origin×Year",
                                         "+Dest×Year",
                                         "+Both")),
  title  = "Table S3: Robustness checks (Tons)",
  file   = file.path(OUTPUT_DIR, "TableS4_OD_FE_tons.txt"),
  replace = TRUE
)

# Ton-miles table
etable(
  m_base_tm, m_orig_tm, m_dest_tm, m_both_tm,
  signif.code = my_signif,
  dict        = my_dict,
  headers     = list("Fixed Effects" = c("Baseline\n(Route+Year)",
                                         "+Origin×Year",
                                         "+Dest×Year",
                                         "+Both")),
  title  = "Table S3: Robustness checks (Ton-Miles)",
  file   = file.path(OUTPUT_DIR, "TableS4_OD_FE_tmiles.txt"),
  replace = TRUE
)

message("Done! Results saved to: ", OUTPUT_DIR)
message("Files: TableR1_OD_FE_tons.txt, TableR2_OD_FE_tmiles.txt")
