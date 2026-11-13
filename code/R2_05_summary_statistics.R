# ==============================================================================
# 05_summary_statistics.R
# Purpose: Compute summary statistics for Table S2
#          Full sample + SCTG 1-4 (raw ag) + SCTG 5-7 (processed food)
# ==============================================================================

library(dplyr)

PANEL_FILE <- "./data/panel_od_year_truck_sctg01_07.rds"
OUT_DIR    <- "./tables"

# --- Load panels ---
df_full   <- readRDS(PANEL_FILE)
panel_rawag     <- readRDS("./data/panel_od_year_truck_sctg01_04.rds")
panel_processed <- readRDS("./data/panel_od_year_truck_sctg05_07.rds")

# Exclude Alaska and Hawaii from full sample
ak_hi_zones <- c(20, 151, 159)
# 2026-10-26: AK/HI routes kept (N = 74,238). Superseded 2026-11-11: the main panel now holds the 12,173 routes whose full road line lies within the contiguous U.S. (N = 73,038; see 06_build_panel.R)
# df_full <- df_full %>% filter(!dms_orig %in% ak_hi_zones & !dms_dest %in% ak_hi_zones)

cat("Full sample N:  ", nrow(df_full), "\n")
cat("SCTG 1-4 N:     ", nrow(panel_rawag), "\n")
cat("SCTG 5-7 N:     ", nrow(panel_processed), "\n\n")

# --- Variables for freight outcomes and weather ---
vars_common <- c("total_tons", "total_tmiles",
                 "avgHighTempFreq_3sd", "avgLowTempFreq_3sd",
                 "avgHighPrecFreq", "avgHighVsFreq")

compute_stats <- function(df, sample_label) {
  df %>%
    select(all_of(intersect(vars_common, names(df)))) %>%
    summarise(across(everything(), list(
      Mean = ~round(mean(., na.rm = TRUE), 2),
      SD   = ~round(sd(.,   na.rm = TRUE), 2)
    ))) %>%
    tidyr::pivot_longer(everything(),
      names_to  = c("Variable", ".value"),
      names_sep = "_(?=[^_]+$)"
    ) %>%
    mutate(Sample = sample_label)
}

stats_full <- compute_stats(df_full,         "Full sample")
stats_raw  <- compute_stats(panel_rawag,     "SCTG 1-4")
stats_proc <- compute_stats(panel_processed, "SCTG 5-7")

# --- SVI: route-level average from full panel, matched to each subsample ---
routes_rawag <- unique(panel_rawag$unit)
routes_proc  <- unique(panel_processed$unit)

svi_full <- df_full %>%
  summarise(Mean = round(mean(avgSVI, na.rm = TRUE), 2),
            SD   = round(sd(avgSVI,   na.rm = TRUE), 2)) %>%
  mutate(Variable = "avgSVI", Sample = "Full sample")

svi_raw <- df_full %>%
  filter(unit %in% routes_rawag) %>%
  summarise(Mean = round(mean(avgSVI, na.rm = TRUE), 2),
            SD   = round(sd(avgSVI,   na.rm = TRUE), 2)) %>%
  mutate(Variable = "avgSVI", Sample = "SCTG 1-4")

svi_proc <- df_full %>%
  filter(unit %in% routes_proc) %>%
  summarise(Mean = round(mean(avgSVI, na.rm = TRUE), 2),
            SD   = round(sd(avgSVI,   na.rm = TRUE), 2)) %>%
  mutate(Variable = "avgSVI", Sample = "SCTG 5-7")

# --- Combine all ---
all_stats <- bind_rows(
  stats_full, stats_raw, stats_proc,
  svi_full,   svi_raw,   svi_proc
) %>%
  select(Sample, Variable, Mean, SD)

cat("=== Summary Statistics ===\n")
print(all_stats, n = 30)

# --- Save ---
labels <- c(
  "total_tons"          = "Tons (thousand tons)",
  "total_tmiles"        = "Ton-miles (million ton-miles)",
  "avgHighTempFreq_3sd" = "Extreme hot days",
  "avgLowTempFreq_3sd"  = "Extreme cold days",
  "avgHighPrecFreq"     = "Heavy precipitation days",
  "avgHighVsFreq"       = "High wind speed days",
  "avgSVI"              = "Social Vulnerability Index"
)

# --- Format as wide table for Excel ---
var_order <- c("total_tons", "total_tmiles",
               "avgHighTempFreq_3sd", "avgLowTempFreq_3sd",
               "avgHighPrecFreq", "avgHighVsFreq", "avgSVI")

out <- all_stats %>%
  mutate(
    Variable = ifelse(Variable %in% names(labels), labels[Variable], Variable),
    Variable = factor(Variable, levels = labels[var_order])
  ) %>%
  arrange(Variable)

wide <- out %>%
  tidyr::pivot_wider(
    names_from  = Sample,
    values_from = c(Mean, SD),
    names_glue  = "{Sample} {.value}"
  ) %>%
  select(Variable,
         `Full sample Mean`, `Full sample SD`,
         `SCTG 1-4 Mean`,    `SCTG 1-4 SD`,
         `SCTG 5-7 Mean`,    `SCTG 5-7 SD`)

write.csv(wide, file.path(OUT_DIR, "TableS2_summary_stats.csv"), row.names = FALSE)
message("Saved: TableS2_summary_stats.csv")
