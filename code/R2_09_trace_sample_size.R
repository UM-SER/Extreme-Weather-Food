# ==============================================================================
# 09_trace_sample_size.R
# Purpose: Step-by-step row counts showing how 74,238 and 74,220 arise,
#          following the logic of 05_faf_merge_analysis.R (read-only).
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
ROOT <- "."
OUT  <- file.path(ROOT, "tables/diagnostics")
sink(file.path(OUT, "09_log.txt"), split = TRUE)

faf <- read_csv(file.path(ROOT, "data/FAF/FAF5.6.1_2018-2023.csv"),
                col_types = cols(.default = "n"))
cat("Step 0  FAF5.6.1 raw records:", nrow(faf), "\n")
cat("        domestic zones in data:", n_distinct(c(faf$dms_orig, faf$dms_dest), na.rm = TRUE), "\n")

s1 <- faf %>% filter(trade_type == 1)
cat("Step 1  domestic (trade_type == 1):", nrow(s1), "\n")
s2 <- s1 %>% filter(sctg2 %in% 1:7)
cat("Step 2  SCTG 01-07:", nrow(s2), " records;", nrow(distinct(s2, dms_orig, dms_dest)), "OD pairs (any mode)\n")
s2t <- s2 %>% filter(dms_mode == 1)
cat("        of which truck records:", nrow(s2t), ";", nrow(distinct(s2t, dms_orig, dms_dest)), "OD pairs with any truck record\n")

long <- s2 %>% pivot_longer(matches("^(tons_|value_|tmiles_)"), names_to = c(".value", "year"),
                            names_pattern = "(.*)_(\\d+)") %>% mutate(year = as.integer(year))
cat("        years in data:", paste(sort(unique(long$year)), collapse = ", "), "\n")
long <- long %>% filter(year %in% 2018:2023)
agg <- long %>% group_by(dms_orig, dms_dest, year) %>%
  summarise(truck_tons = sum(tons[dms_mode == 1], na.rm = TRUE), .groups = "drop")
cat("Step 3  OD x year (all modes summed):", nrow(agg), " =", n_distinct(paste(agg$dms_orig, agg$dms_dest)), "OD x",
    n_distinct(agg$year), "years\n")
s4 <- agg %>% filter(truck_tons > 0)
cat("Step 4  keep truck_tons > 0:", nrow(s4), " rows;", nrow(distinct(s4, dms_orig, dms_dest)), "OD pairs\n")
cat("        years per OD:\n"); print(table(table(paste(s4$dms_orig, s4$dms_dest))))

lk <- readRDS(file.path(ROOT, "data/Route/faf_zone_lookup.rds")) %>% distinct(FAF_ID, final_geoid)
s5 <- s4 %>% mutate(o = sprintf("%03d", dms_orig), d = sprintf("%03d", dms_dest)) %>%
  left_join(lk, by = c("o" = "FAF_ID")) %>% rename(og = final_geoid) %>%
  left_join(lk, by = c("d" = "FAF_ID")) %>% rename(dg = final_geoid) %>%
  filter(!is.na(og), !is.na(dg))
cat("Step 5  zone -> GEOID matched:", nrow(s5), " rows;", nrow(distinct(s5, dms_orig, dms_dest)), "OD pairs\n")
cat("        => panel_od_year_truck_sctg01_07.rds has", nrow(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07.rds"))), "rows\n")

akhi <- c(20, 151, 159)
ak <- s5 %>% filter(dms_orig %in% akhi | dms_dest %in% akhi)
cat("\nStep 6  routes touching Alaska (20) / Hawaii (151, 159):\n")
print(ak %>% count(dms_orig, dms_dest, name = "years"))
cat("        rows removed:", nrow(ak), " -> remaining", nrow(s5) - nrow(ak), "rows,",
    nrow(distinct(s5, dms_orig, dms_dest)) - nrow(distinct(ak, dms_orig, dms_dest)), "OD pairs\n")
ak_any <- s2t %>% filter(dms_orig %in% akhi | dms_dest %in% akhi) %>% distinct(dms_orig, dms_dest)
cat("        (truck SCTG 01-07 records involving AK/HI in raw FAF:", nrow(ak_any), "OD pairs)\n")
sink()
