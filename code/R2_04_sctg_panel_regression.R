# ==============================================================================
# 04_sctg_panel_regression.R
# Purpose: Comment 1c — rebuild panel separately for raw ag (SCTG 1-4)
#          and processed food (SCTG 5-7), then run main weather regression
#          on each subsample to test commodity-type heterogeneity.
#
# Raw ag:        SCTG 1 (live animals/fish), 2 (cereal grains),
#                     3 (other ag), 4 (animal feed)
# Processed food: SCTG 5 (meat/seafood), 6 (milled grain/bakery),
#                     7 (other prepared foodstuffs/fats)
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(fixest)

BASE    <- "."
OUT_DIR <- file.path(BASE, "tables")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# --- Inputs (same as 05_faf_merge_analysis.R) ---
FAF_CSV    <- Sys.getenv("FAF_CSV", file.path(BASE, "data/FAF/FAF5.6.1_2018-2023.csv"))
MAIN_PANEL <- file.path(BASE, "data/panel_od_year_truck_sctg01_07.rds")  # run 06_build_panel.R first
WEATHER    <- file.path(BASE, "data/Weather/od_route_weather_exposure.rds")
SVI        <- file.path(BASE, "data/SVI/od_route_svi.rds")
LOOKUP     <- file.path(BASE, "data/Route/faf_zone_lookup.rds")
ROUTE_FILE <- file.path(BASE, "data/Route/od_route_county_segments_fullgeom.csv")

# Steps 1-3 rebuild the SCTG panels from the raw FAF5 CSV. If the CSV is not
# available, the stored panels in data/ are used for the regressions in step 4.
if (file.exists(FAF_CSV)) {
# ==============================================================================
# 1. Load and reshape FAF5 — keep sctg2
# ==============================================================================
message("Loading FAF5 CSV...")
faf <- read.csv(FAF_CSV)

faf_food <- faf %>%
  filter(trade_type == 1, sctg2 %in% 1:7) %>%
  pivot_longer(
    cols         = matches("^(tons_|tmiles_)"),
    names_to     = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(year = as.integer(year))

# Aggregate to dms_orig × dms_dest × sctg2 × year  (truck only)
message("Aggregating by SCTG...")
faf_agg <- faf_food %>%
  group_by(dms_orig, dms_dest, sctg2, year) %>%
  summarise(
    dist_band    = max(dist_band, na.rm = TRUE),
    total_tons   = sum(tons[dms_mode == 1],   na.rm = TRUE),
    total_tmiles = sum(tmiles[dms_mode == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_tons > 0)

cat("Rows after SCTG aggregation:", nrow(faf_agg), "\n")
cat("SCTG distribution:\n"); print(table(faf_agg$sctg2))

# ==============================================================================
# 2. Merge weather, SVI, GEOID lookup, distance
# ==============================================================================
message("Merging context data...")

lookup <- readRDS(LOOKUP) %>%
  select(FAF_ID, final_geoid) %>%
  distinct() %>%
  mutate(final_geoid = str_pad(final_geoid, 5, "left", "0"))

weather <- readRDS(WEATHER) %>%
  mutate(
    year         = as.integer(year),
    dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"),
    dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0")
  )

svi <- readRDS(SVI) %>%
  mutate(
    dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"),
    dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0")
  )

dist_lookup <- read.csv(ROUTE_FILE,
                         colClasses = c(dms_orig_GEOID = "character",
                                        dms_dest_GEOID = "character")) %>%
  group_by(dms_orig_GEOID, dms_dest_GEOID) %>%
  summarise(total_distance = mean(total_distance, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    dms_orig_GEOID = str_pad(dms_orig_GEOID, 5, "left", "0"),
    dms_dest_GEOID = str_pad(dms_dest_GEOID, 5, "left", "0")
  )

panel <- faf_agg %>%
  mutate(orig_3d = sprintf("%03d", dms_orig),
         dest_3d = sprintf("%03d", dms_dest)) %>%
  left_join(lookup, by = c("orig_3d" = "FAF_ID")) %>%
  rename(dms_orig_GEOID = final_geoid) %>%
  left_join(lookup, by = c("dest_3d" = "FAF_ID")) %>%
  rename(dms_dest_GEOID = final_geoid) %>%
  filter(!is.na(dms_orig_GEOID) & !is.na(dms_dest_GEOID)) %>%
  left_join(weather,      by = c("dms_orig_GEOID", "dms_dest_GEOID", "year")) %>%
  left_join(svi,          by = c("dms_orig_GEOID", "dms_dest_GEOID")) %>%
  left_join(dist_lookup,  by = c("dms_orig_GEOID", "dms_dest_GEOID")) %>%
  mutate(
    unit = paste0(dms_orig_GEOID, dms_dest_GEOID, "_", sctg2),
    across(matches("^(distw|avg)"), ~replace_na(., 0)),
    total_distance = replace_na(total_distance, 0),
    dist_cat = case_when(
      dist_band %in% c(1, 2, 3) ~ "Short",
      dist_band %in% c(4, 5)    ~ "Medium",
      dist_band %in% c(6, 7, 8) ~ "Long",
      TRUE ~ "Unknown"
    )
  )

# Exclude Alaska (zone 20) and Hawaii (zones 151, 159) — non-contiguous states
ak_hi_zones <- c(20, 151, 159)
# 2026-10-26: AK/HI routes kept (N = 74,238). Superseded 2026-11-11: the main panel now holds the 12,173 routes whose full road line lies within the contiguous U.S. (N = 73,038; see 06_build_panel.R)
# panel <- panel %>% filter(!dms_orig %in% ak_hi_zones & !dms_dest %in% ak_hi_zones)

# 2026-11-11 (R2, Comment 21): same routes as the main panel, i.e. the 12,173
# routes that lie entirely within the contiguous U.S. (see 06_build_panel.R).
main_units <- unique(readRDS(MAIN_PANEL)$unit)
panel <- panel %>% filter(paste0(dms_orig_GEOID, dms_dest_GEOID) %in% main_units)

cat("Full SCTG panel rows (contiguous-U.S. routes):", nrow(panel), "\n")

# ==============================================================================
# 3. Aggregate to route x year level within each commodity group
#    Weather variables are route-year level (same across SCTG on same route),
#    so they are valid group keys. Tons/tmiles are summed across SCTG codes.
# ==============================================================================
agg_to_route <- function(df) {
  df %>%
    group_by(dms_orig_GEOID, dms_dest_GEOID, year,
             avgHighTempFreq_3sd, avgLowTempFreq_3sd,
             avgHighPrecFreq, avgHighVsFreq) %>%
    summarise(
      total_tons   = sum(total_tons,   na.rm = TRUE),
      total_tmiles = sum(total_tmiles, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(unit = paste0(dms_orig_GEOID, dms_dest_GEOID))
}

panel_rawag     <- panel %>% filter(sctg2 %in% 1:4) %>% agg_to_route()
panel_processed <- panel %>% filter(sctg2 %in% 5:7) %>% agg_to_route()

cat("Raw ag rows (SCTG 1-4, route x year):", nrow(panel_rawag), "\n")
cat("Processed rows (SCTG 5-7, route x year):", nrow(panel_processed), "\n")

saveRDS(panel_rawag,     file.path(BASE, "data/panel_od_year_truck_sctg01_04.rds"))
saveRDS(panel_processed, file.path(BASE, "data/panel_od_year_truck_sctg05_07.rds"))
message("Panels saved.")
} else {
  message("FAF5 CSV not found: using stored SCTG panels in data/.")
  panel_rawag     <- readRDS(file.path(BASE, "data/panel_od_year_truck_sctg01_04.rds"))
  panel_processed <- readRDS(file.path(BASE, "data/panel_od_year_truck_sctg05_07.rds"))
}

# ==============================================================================
# 4. Regressions — same spec as main paper
# ==============================================================================
weather_vars <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd",
                  "avgHighPrecFreq",     "avgHighVsFreq")
rhs <- paste(weather_vars, collapse = " + ")

my_dict <- c(
  "total_tons"          = "Total Tons",
  "total_tmiles"        = "Total Ton-Miles",
  "avgHighTempFreq_3sd" = "Extreme Hot Days",
  "avgLowTempFreq_3sd"  = "Extreme Cold Days",
  "avgHighPrecFreq"     = "Heavy Precip Days",
  "avgHighVsFreq"       = "High Wind Days"
)
my_signif <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)

message("Running regressions...")

run_reg <- function(df, depvar) {
  feols(as.formula(paste(depvar, "~", rhs, "| unit + year")),
        cluster = ~unit, data = df)
}

# Full sample (panel_od_year_truck_sctg01_07.rds — same spec as paper Table 1)
df_full <- readRDS(file.path(BASE, "data/panel_od_year_truck_sctg01_07.rds"))
m_full_tons <- run_reg(df_full, "total_tons")
m_full_tm   <- run_reg(df_full, "total_tmiles")

# Raw ag
m_rawag_tons   <- run_reg(panel_rawag,     "total_tons")
m_rawag_tm     <- run_reg(panel_rawag,     "total_tmiles")

# Processed food
m_proc_tons    <- run_reg(panel_processed, "total_tons")
m_proc_tm      <- run_reg(panel_processed, "total_tmiles")

# ==============================================================================
# 5. Output tables
# ==============================================================================
message("Saving tables...")

# TableR3 — original layout (unchanged)
etable(
  m_rawag_tons, m_rawag_tm, m_proc_tons, m_proc_tm,
  signif.code = my_signif,
  dict        = my_dict,
  headers     = list("Commodity Group" = c("Raw Ag (SCTG 1-4)",
                                            "Raw Ag (SCTG 1-4)",
                                            "Processed (SCTG 5-7)",
                                            "Processed (SCTG 5-7)"),
                     "Outcome"         = c("Tons", "Ton-Miles",
                                            "Tons", "Ton-Miles")),
  title   = "Table R3: Weather Effects by Commodity Type (Raw Ag vs Processed Food)",
  file    = file.path(OUT_DIR, "TableR3_SCTG_tons_tmiles.txt"),
  replace = TRUE
)

# Table S5 — Tons panel (3 columns: Full, Raw Ag, Processed)
etable(
  m_full_tons, m_rawag_tons, m_proc_tons,
  signif.code = my_signif,
  dict        = my_dict,
  headers     = list("Sample" = c("Full Sample",
                                   "Raw Ag (SCTG 1-4)",
                                   "Processed Food (SCTG 5-7)")),
  title   = "Table S5: Weather Effects by Food Product (Tons)",
  file    = file.path(OUT_DIR, "TableS5_SCTG_tons.txt"),
  replace = TRUE
)

# Table S5 — Ton-Miles panel (3 columns: Full, Raw Ag, Processed)
etable(
  m_full_tm, m_rawag_tm, m_proc_tm,
  signif.code = my_signif,
  dict        = my_dict,
  headers     = list("Sample" = c("Full Sample",
                                   "Raw Ag (SCTG 1-4)",
                                   "Processed Food (SCTG 5-7)")),
  title   = "Table S5: Weather Effects by Food Product (Ton-Miles)",
  file    = file.path(OUT_DIR, "TableS5_SCTG_tmiles.txt"),
  replace = TRUE
)

message("Done! Check tables/ for panels, TableR3, and TableS5.")

# Quick summary of key coefficients
cat("\n=== Key coefficients (Tons) ===\n")
cat("--- Raw Ag (SCTG 1-4) ---\n")
print(coef(m_rawag_tons))
cat("\n--- Processed Food (SCTG 5-7) ---\n")
print(coef(m_proc_tons))
