# ==============================================================================
# 06_regression_analysis.R
# Purpose: Run analysis for BOTH '_3sd' and '_5c' specs.
#          Dependent Variables: Total Tons AND Total Ton-Miles .
#          Outputs to: ./tables/main
# ==============================================================================

library(fixest)
setFixest_nthreads(1)   # long multi-threaded sessions crashed (segfault) on R 4.5.2 / fixest 0.13.2
library(utils)
library(dplyr) # Needed for data prep

# ------------------------------------------------------------------------------
# 1. SETUP: PATHS & VARIABLES
# ------------------------------------------------------------------------------

# *** DEFINING THE ROOT OUTPUT DIRECTORY ***
base_dir   <- "./tables/main"
input_data <- "./data/panel_od_year_truck_sctg01_07.rds"

# --- 1.1 LOAD & PREP DATA ---
message("Loading Data...")
df <- readRDS(input_data)

# Ensure variables match your formulas:
# 1. Create 'share_tons_pct' (0-100) from 'truck_share_tons' (0-1)
# 2. Ensure NO NAs in 'total_tons' or 'total_tmiles'
df <- df %>%
  mutate(
    share_tons_pct = truck_share_tons * 100, 
    total_tons     = coalesce(total_tons, 0),
    total_tmiles   = coalesce(total_tmiles, 0)
  )

# --- Specification A: 3 Standard Deviations (_3sd) ---
vars_distw_3sd <- c("distwHighTempFreq_3sd", "distwLowTempFreq_3sd", 
                    "distwHighPrecFreq",     "distwHighVsFreq")

vars_avg_3sd   <- c("avgHighTempFreq_3sd",   "avgLowTempFreq_3sd", 
                    "avgHighPrecFreq",       "avgHighVsFreq")

# --- Specification B: 5 Degrees Celsius (_5c) ---
vars_distw_5c  <- c("distwHighTempFreq_5c",  "distwLowTempFreq_5c", 
                    "distwHighPrecFreq",     "distwHighVsFreq")

vars_avg_5c    <- c("avgHighTempFreq_5c",    "avgLowTempFreq_5c", 
                    "avgHighPrecFreq",       "avgHighVsFreq")

# Common Settings
my_signif   <- c("***"=0.001, "**"=0.01, "*"=0.05)
fe_part     <- "unit + year"
cluster_var <- ~unit

# Label Dictionary
my_dict <- c(
  "total_tons" = "Total Tons",
  "total_tmiles" = "Total Ton-Miles",
  "share_tons_pct" = "Truck Share (%)",
  
  "distwHighTempFreq_3sd" = "Extreme Hot Days",
  "distwLowTempFreq_3sd"  = "Extreme Cold Days",
  "avgHighTempFreq_3sd"   = "Extreme Hot Days",
  "avgLowTempFreq_3sd"    = "Extreme Cold Days",
  
  "distwHighTempFreq_5c"  = "Extreme Hot Days",
  "distwLowTempFreq_5c"   = "Extreme Cold Days",
  "avgHighTempFreq_5c"    = "Extreme Hot Days",
  "avgLowTempFreq_5c"     = "Extreme Cold Days",
  
  "distwHighPrecFreq"     = "Heavy Precip Days",
  "distwHighVsFreq"       = "High Wind Days",
  "avgHighPrecFreq"       = "Heavy Precip Days",
  "avgHighVsFreq"         = "High Wind Days"
)

# ------------------------------------------------------------------------------
# 2. ANALYSIS FUNCTION
# ------------------------------------------------------------------------------

run_analysis_suite <- function(weather_vars, sub_folder_name) {
  
  # Construct full path
  full_output_path <- file.path(base_dir, sub_folder_name)
  
  # Create directory recursively
  if(!dir.exists(full_output_path)) {
    dir.create(full_output_path, recursive = TRUE)
  }
  message(paste0("  -> Saving to: ", full_output_path))
  
  # --- CONSTRUCT FORMULAS ---
  rhs_vars <- paste(weather_vars, collapse = " + ")
  
  fml_tons   <- as.formula(paste("total_tons ~",   rhs_vars, "|", fe_part))
  fml_tmiles <- as.formula(paste("total_tmiles ~", rhs_vars, "|", fe_part))
  fml_share  <- as.formula(paste("share_tons_pct ~", rhs_vars, "|", fe_part))
  
  # --- TABLE 1: FREIGHT VOLUME (TONS & T-MILES) ---
  
  # 1. Full Sample
  t1_tons_full     <- feols(fml_tons,   cluster = cluster_var, data = df)
  t1_tmiles_full   <- feols(fml_tmiles, cluster = cluster_var, data = df)
  
  # 2. 0-500 Miles
  t1_tons_0_500    <- feols(fml_tons,   cluster = cluster_var, data = subset(df, dist_cat == "Short"))
  t1_tmiles_0_500  <- feols(fml_tmiles, cluster = cluster_var, data = subset(df, dist_cat == "Short"))
  
  # 3. 500-1000 Miles
  t1_tons_500_1k   <- feols(fml_tons,   cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
  t1_tmiles_500_1k <- feols(fml_tmiles, cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
  
  # 4. >1000 Miles
  t1_tons_gt1k     <- feols(fml_tons,   cluster = cluster_var, data = subset(df, dist_cat == "Long"))
  t1_tmiles_gt1k   <- feols(fml_tmiles, cluster = cluster_var, data = subset(df, dist_cat == "Long"))
  
  etable(t1_tons_full, t1_tmiles_full, t1_tons_0_500, t1_tmiles_0_500, 
         t1_tons_500_1k, t1_tmiles_500_1k, t1_tons_gt1k, t1_tmiles_gt1k,
         signif.code = my_signif, dict = my_dict,
         headers = list("Sample"=c("Full", "Full", "0-500", "0-500", "500-1000", "500-1000", ">1000", ">1000")),
         file = file.path(full_output_path, "Table1_Volume_by_Distance.txt"), replace = TRUE)
  
  # --- TABLE 2: VOLUME HETEROGENEITY (TONS & T-MILES) ---
  # Only run if SVI variable exists
  if("svi_cat" %in% names(df)){
    t2_tons   <- feols(fml_tons,   cluster = cluster_var, split = ~svi_cat, data = df)
    t2_tmiles <- feols(fml_tmiles, cluster = cluster_var, split = ~svi_cat, data = df)
    
    etable(t2_tons, t2_tmiles, 
           signif.code = my_signif, dict = my_dict,
           file = file.path(full_output_path, "Table2_Volume_SVI.txt"), replace = TRUE)
  }
  
  # --- TABLE 3 & 4: TRUCK SHARE (OPTIONAL) ---
  # Wrapped in try() to skip if share is constant (100%)
  try({
    t3_full   <- feols(fml_share, cluster = cluster_var, data = df)
    t3_0_500  <- feols(fml_share, cluster = cluster_var, data = subset(df, dist_cat == "Short"))
    t3_500_1k <- feols(fml_share, cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
    t3_gt1k   <- feols(fml_share, cluster = cluster_var, data = subset(df, dist_cat == "Long"))
    
    etable(t3_full, t3_0_500, t3_500_1k, t3_gt1k,
           signif.code = my_signif, dict = my_dict,
           headers = list("Distance"=c("Full Sample", "0-500m", "500-1000m", ">1000m")),
           file = file.path(full_output_path, "Table3_Share_by_Distance.txt"), replace = TRUE)
    
    if("svi_cat" %in% names(df)){
      t4_share <- feols(fml_share, cluster = cluster_var, split = ~svi_cat, data = df)
      etable(t4_share, 
             signif.code = my_signif, dict = my_dict,
             file = file.path(full_output_path, "Table4_Share_SVI.txt"), replace = TRUE)
    }
  }, silent = TRUE)
  invisible(gc())   # free memory between specification sets
}

# ------------------------------------------------------------------------------
# 3. EXECUTION
# ------------------------------------------------------------------------------

# --- 1. RUN 3SD ---
message("Running 3SD Specifications...")
run_analysis_suite(vars_distw_3sd, "results_3sd/dist_weighted")
run_analysis_suite(vars_avg_3sd,   "results_3sd/avg_weighted")

# --- 2. RUN 5C ---
message("Running 5C Specifications...")
run_analysis_suite(vars_distw_5c, "results_5c/dist_weighted")
run_analysis_suite(vars_avg_5c,   "results_5c/avg_weighted")

# ------------------------------------------------------------------------------
# 4. TABLE 5: COMPARISON TABLES (TONS & T-MILES)
# ------------------------------------------------------------------------------
message("Generating Comparison Tables (Table 5)...")

# Helper
make_fml <- function(dep_var, vars) {
  as.formula(paste(dep_var, "~", paste(vars, collapse="+"), "|", fe_part))
}

# --- Comparison for 3SD ---
path_3sd <- file.path(base_dir, "results_3sd")
if(!dir.exists(path_3sd)) dir.create(path_3sd, recursive = TRUE)

t5_3sd_tons_d   <- feols(make_fml("total_tons", vars_distw_3sd),   cluster = cluster_var, data = df)
t5_3sd_tm_d     <- feols(make_fml("total_tmiles", vars_distw_3sd), cluster = cluster_var, data = df)
t5_3sd_tons_a   <- feols(make_fml("total_tons", vars_avg_3sd),     cluster = cluster_var, data = df)
t5_3sd_tm_a     <- feols(make_fml("total_tmiles", vars_avg_3sd),   cluster = cluster_var, data = df)

etable(t5_3sd_tons_d, t5_3sd_tm_d, t5_3sd_tons_a, t5_3sd_tm_a,
       signif.code = my_signif, dict = my_dict,
       headers = list("Method" = c("Dist-Weighted", "Dist-Weighted", "Avg-Weighted", "Avg-Weighted")),
       file = file.path(path_3sd, "Table5_Comparison_Method.txt"), replace = TRUE)

# --- Comparison for 5C ---
path_5c <- file.path(base_dir, "results_5c")
if(!dir.exists(path_5c)) dir.create(path_5c, recursive = TRUE)

t5_5c_tons_d    <- feols(make_fml("total_tons", vars_distw_5c),   cluster = cluster_var, data = df)
t5_5c_tm_d      <- feols(make_fml("total_tmiles", vars_distw_5c), cluster = cluster_var, data = df)
t5_5c_tons_a    <- feols(make_fml("total_tons", vars_avg_5c),     cluster = cluster_var, data = df)
t5_5c_tm_a      <- feols(make_fml("total_tmiles", vars_avg_5c),   cluster = cluster_var, data = df)

etable(t5_5c_tons_d, t5_5c_tm_d, t5_5c_tons_a, t5_5c_tm_a,
       signif.code = my_signif, dict = my_dict,
       headers = list("Method" = c("Dist-Weighted", "Dist-Weighted", "Avg-Weighted", "Avg-Weighted")),
       file = file.path(path_5c, "Table5_Comparison_Method.txt"), replace = TRUE)

message("Done! Check directory: ", base_dir)