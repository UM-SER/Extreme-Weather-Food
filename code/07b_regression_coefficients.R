# ==============================================================================
# 06_regression_analysis_final.R
# Purpose: Run Regressions for Truck Volume AND Share (Tons & Ton-Miles).
#          - Saves tables (.txt) AND coefficient datasets (.rds) for plotting.
#          - Handles SVI Heterogeneity for both Volume and Share.
# ==============================================================================

library(fixest)
library(utils)
library(dplyr)
library(broom)
library(tidyr)

# --- 1. CONFIGURATION ---
BASE_DIR   <- "./tables/main"
INPUT_DATA <- "./data/panel_od_year_truck_sctg01_07.rds"

# --- 1.1 LOAD & PREP DATA ---
message("Loading Data...")
df <- readRDS(INPUT_DATA)

df <- df %>%
  mutate(
    # Share: 0-1 -> 0-100
    share_tons_pct   = truck_share_tons * 100,
    share_tmiles_pct = truck_share_tmiles * 100,
    
    # NAs to 0 for Volume
    total_tons     = coalesce(total_tons, 0),
    total_tmiles   = coalesce(total_tmiles, 0),
    
    # Ensure SVI Factor Order
    svi_cat = factor(svi_cat, levels = c("Low", "Low-Medium", "Medium-High", "High"))
  )

# Specs
VARS_DISTW_3SD <- c("distwHighTempFreq_3sd", "distwLowTempFreq_3sd", "distwHighPrecFreq", "distwHighVsFreq")
VARS_AVG_3SD   <- c("avgHighTempFreq_3sd",   "avgLowTempFreq_3sd",   "avgHighPrecFreq",   "avgHighVsFreq")
VARS_DISTW_5C  <- c("distwHighTempFreq_5c",  "distwLowTempFreq_5c",  "distwHighPrecFreq", "distwHighVsFreq")
VARS_AVG_5C    <- c("avgHighTempFreq_5c",    "avgLowTempFreq_5c",    "avgHighPrecFreq",   "avgHighVsFreq")

my_signif   <- c("***"=0.001, "**"=0.01, "*"=0.05)
fe_part     <- "unit + year"
cluster_var <- ~unit

my_dict <- c("total_tons" = "Truck Tons", "total_tmiles" = "Truck Ton-Miles", 
             "share_tons_pct" = "Share (Tons) %", "share_tmiles_pct" = "Share (TMiles) %")

# ------------------------------------------------------------------------------
# 2. ANALYSIS FUNCTION
# ------------------------------------------------------------------------------
run_analysis_suite <- function(weather_vars, sub_folder_name) {
  
  full_output_path <- file.path(BASE_DIR, sub_folder_name)
  if(!dir.exists(full_output_path)) dir.create(full_output_path, recursive = TRUE)
  message(paste0("  -> Output: ", full_output_path))
  
  rhs <- paste(weather_vars, collapse = " + ")
  fml_tons     <- as.formula(paste("total_tons ~",   rhs, "|", fe_part))
  fml_tmiles   <- as.formula(paste("total_tmiles ~", rhs, "|", fe_part))
  fml_sh_tons  <- as.formula(paste("share_tons_pct ~",   rhs, "|", fe_part))
  fml_sh_tm    <- as.formula(paste("share_tmiles_pct ~", rhs, "|", fe_part))
  
  # --- A. VOLUME REGRESSIONS (Table 1) ---
  message("     Running Volume Regressions (Distance)...")
  
  # 1. Tons
  m_t_full <- feols(fml_tons, cluster = cluster_var, data = df)
  m_t_005  <- feols(fml_tons, cluster = cluster_var, data = subset(df, dist_cat == "Short"))
  m_t_510  <- feols(fml_tons, cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
  m_t_gt1  <- feols(fml_tons, cluster = cluster_var, data = subset(df, dist_cat == "Long"))
  
  # 2. Ton-Miles
  m_m_full <- feols(fml_tmiles, cluster = cluster_var, data = df)
  m_m_005  <- feols(fml_tmiles, cluster = cluster_var, data = subset(df, dist_cat == "Short"))
  m_m_510  <- feols(fml_tmiles, cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
  m_m_gt1  <- feols(fml_tmiles, cluster = cluster_var, data = subset(df, dist_cat == "Long"))
  
  # Save Table 1
  etable(m_t_full, m_t_005, m_t_510, m_t_gt1, m_m_full, m_m_005, m_m_510, m_m_gt1,
         signif.code = my_signif, dict = my_dict,
         file = file.path(full_output_path, "Table1_Volume_Distance.txt"), replace = TRUE)
  
  # Save Volume Coeffs
  extract_coeffs <- function(model, name, outcome) {
    tidy(model, conf.int = TRUE) %>% 
      filter(term %in% weather_vars) %>% 
      mutate(model = name, outcome = outcome)
  }
  
  coeffs_dist <- bind_rows(
    extract_coeffs(m_t_full, "Full Sample", "total_tons"),
    extract_coeffs(m_t_005,  "0–500 miles", "total_tons"),
    extract_coeffs(m_t_510,  "500–1000 miles", "total_tons"),
    extract_coeffs(m_t_gt1,  ">1000 miles", "total_tons"),
    extract_coeffs(m_m_full, "Full Sample", "total_tmiles"),
    extract_coeffs(m_m_005,  "0–500 miles", "total_tmiles"),
    extract_coeffs(m_m_510,  "500–1000 miles", "total_tmiles"),
    extract_coeffs(m_m_gt1,  ">1000 miles", "total_tmiles")
  )
  saveRDS(coeffs_dist, file.path(full_output_path, "coeffs_distance.rds"))
  
  # --- B. SHARE REGRESSIONS (Table 3) ---
  message("     Running Share Regressions (Distance)...")
  
  # 1. Share Tons
  st_full <- feols(fml_sh_tons, cluster = cluster_var, data = df)
  st_005  <- feols(fml_sh_tons, cluster = cluster_var, data = subset(df, dist_cat == "Short"))
  st_510  <- feols(fml_sh_tons, cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
  st_gt1  <- feols(fml_sh_tons, cluster = cluster_var, data = subset(df, dist_cat == "Long"))
  
  etable(st_full, st_005, st_510, st_gt1, dict = my_dict, signif.code = my_signif,
         headers = list("Dist" = c("Full", "<500", "500-1k", ">1k")),
         file = file.path(full_output_path, "Table3a_Share_Tons.txt"), replace = TRUE)
  
  # 2. Share Ton-Miles
  stm_full <- feols(fml_sh_tm, cluster = cluster_var, data = df)
  stm_005  <- feols(fml_sh_tm, cluster = cluster_var, data = subset(df, dist_cat == "Short"))
  stm_510  <- feols(fml_sh_tm, cluster = cluster_var, data = subset(df, dist_cat == "Medium"))
  stm_gt1  <- feols(fml_sh_tm, cluster = cluster_var, data = subset(df, dist_cat == "Long"))
  
  etable(stm_full, stm_005, stm_510, stm_gt1, dict = my_dict, signif.code = my_signif,
         headers = list("Dist" = c("Full", "<500", "500-1k", ">1k")),
         file = file.path(full_output_path, "Table3b_Share_TMiles.txt"), replace = TRUE)
  
  # Save Share Coeffs
  coeffs_share <- bind_rows(
    extract_coeffs(st_full, "Full Sample", "share_tons"),
    extract_coeffs(st_005,  "0–500 miles", "share_tons"),
    extract_coeffs(st_510,  "500–1000 miles", "share_tons"),
    extract_coeffs(st_gt1,  ">1000 miles", "share_tons"),
    extract_coeffs(stm_full, "Full Sample", "share_tmiles"),
    extract_coeffs(stm_005,  "0–500 miles", "share_tmiles"),
    extract_coeffs(stm_510,  "500–1000 miles", "share_tmiles"),
    extract_coeffs(stm_gt1,  ">1000 miles", "share_tmiles")
  )
  saveRDS(coeffs_share, file.path(full_output_path, "coeffs_share.rds"))
  
  # --- C. SVI HETEROGENEITY (Table 2 & 4) ---
  if("svi_cat" %in% names(df) && nlevels(df$svi_cat) > 1) {
    message("     Running SVI Heterogeneity...")
    
    # helper to extract from fixest_multi
    get_split_coeffs <- function(model_multi, outcome_name) {
      model_list <- as.list(model_multi) # <--- FIX: Convert to list for indexing
      bind_rows(lapply(names(model_list), function(cat) {
        tidy(model_list[[cat]], conf.int = TRUE) %>%
          filter(term %in% weather_vars) %>%
          mutate(model = paste(cat, "SVI"), outcome = outcome_name)
      }))
    }
    
    # 1. Volume SVI (Table 2)
    m_svi_tons   <- feols(fml_tons, cluster = cluster_var, split = ~svi_cat, data = df)
    m_svi_tmiles <- feols(fml_tmiles, cluster = cluster_var, split = ~svi_cat, data = df)
    
    etable(m_svi_tons, m_svi_tmiles, dict = my_dict, signif.code = my_signif,
           file = file.path(full_output_path, "Table2b_Volume_SVI_coefficients.txt"), replace = TRUE)
    
    coeffs_svi <- bind_rows(
      get_split_coeffs(m_svi_tons, "total_tons"),
      get_split_coeffs(m_svi_tmiles, "total_tmiles")
    )
    saveRDS(coeffs_svi, file.path(full_output_path, "coeffs_svi.rds"))
    
    # 2. Share SVI (Table 4)
    s_svi_tons   <- feols(fml_sh_tons, cluster = cluster_var, split = ~svi_cat, data = df)
    s_svi_tmiles <- feols(fml_sh_tm,   cluster = cluster_var, split = ~svi_cat, data = df)
    
    etable(s_svi_tons, dict = my_dict, signif.code = my_signif, file = file.path(full_output_path, "Table4a_Share_SVI_Tons.txt"), replace = TRUE)
    etable(s_svi_tmiles, dict = my_dict, signif.code = my_signif, file = file.path(full_output_path, "Table4b_Share_SVI_TMiles.txt"), replace = TRUE)
    
    coeffs_share_svi <- bind_rows(
      get_split_coeffs(s_svi_tons, "share_tons"),
      get_split_coeffs(s_svi_tmiles, "share_tmiles")
    )
    saveRDS(coeffs_share_svi, file.path(full_output_path, "coeffs_share_svi.rds"))
  }
}

# --- 3. EXECUTION ---
message("Running 3SD Specifications...")
run_analysis_suite(VARS_DISTW_3SD, "results_3sd/dist_weighted")
run_analysis_suite(VARS_AVG_3SD,   "results_3sd/avg_weighted")

message("Running 5C Specifications...")
run_analysis_suite(VARS_DISTW_5C, "results_5c/dist_weighted")
run_analysis_suite(VARS_AVG_5C,   "results_5c/avg_weighted")

message("Done.")

# ==============================================================================
# 4. MAGNITUDE COMPARISON (Percent of Mean)
# ==============================================================================
run_magnitude_comparison <- function(weather_vars, sub_folder_name) {
  
  full_output_path <- file.path(BASE_DIR, sub_folder_name)
  message(paste0("  -> Calculating Magnitudes in: ", full_output_path))
  
  rhs <- paste(weather_vars, collapse = " + ")
  fml_tons   <- as.formula(paste("total_tons ~",   rhs, "|", fe_part))
  fml_tmiles <- as.formula(paste("total_tmiles ~", rhs, "|", fe_part))
  
  # Define Subsets
  df_full <- df
  df_005  <- subset(df, dist_cat == "Short")
  df_510  <- subset(df, dist_cat == "Medium")
  df_gt1  <- subset(df, dist_cat == "Long")
  
  # Helper to run model AND calc % effect
  get_magnitude <- function(data_sub, fml, name, outcome_name) {
    # 1. Run Model
    m <- feols(fml, cluster = cluster_var, data = data_sub)
    
    # 2. Get Mean of Y
    mean_y <- mean(data_sub[[outcome_name]], na.rm = TRUE)
    
    # 3. Extract & Calculate %
    tidy(m, conf.int = TRUE) %>%
      filter(term %in% weather_vars) %>%
      mutate(
        model = name,
        outcome = outcome_name,
        mean_dep_var = mean_y,
        pct_effect = (estimate / mean_y) * 100, # <--- THE KEY METRIC
        stars = case_when(p.value<0.001~"***", p.value<0.01~"**", p.value<0.05~"*", TRUE~"")
      )
  }
  
  # Run for Tons vs Ton-Miles across distances
  res <- bind_rows(
    get_magnitude(df_full, fml_tons, "Full Sample", "total_tons"),
    get_magnitude(df_full, fml_tmiles, "Full Sample", "total_tmiles"),
    
    get_magnitude(df_005, fml_tons, "0-500 miles", "total_tons"),
    get_magnitude(df_005, fml_tmiles, "0-500 miles", "total_tmiles"),
    
    get_magnitude(df_510, fml_tons, "500-1000 miles", "total_tons"),
    get_magnitude(df_510, fml_tmiles, "500-1000 miles", "total_tmiles"),
    
    get_magnitude(df_gt1, fml_tons, ">1000 miles", "total_tons"),
    get_magnitude(df_gt1, fml_tmiles, ">1000 miles", "total_tmiles")
  )
  
  # Pivot for nice "Tons vs Ton-Miles" side-by-side view
  table_out <- res %>%
    select(model, term, outcome, estimate, pct_effect, stars) %>%
    pivot_wider(
      id_cols = c(model, term),
      names_from = outcome,
      values_from = c(estimate, pct_effect, stars)
    ) %>%
    select(model, term, 
           est_Tons=estimate_total_tons, pct_Tons=pct_effect_total_tons, star_Tons=stars_total_tons,
           est_TM=estimate_total_tmiles, pct_TM=pct_effect_total_tmiles, star_TM=stars_total_tmiles)
  
  # Save
  write.csv(table_out, file.path(full_output_path, "Table5_Magnitude_Comparison.csv"), row.names = FALSE)
  message("     Table 5 Saved.")
}

# --- 4. EXECUTION ---
message("Running Magnitude Comparison (3SD Avg)...")
run_magnitude_comparison(VARS_AVG_3SD, "results_3sd/avg_weighted")

message("Done.")
