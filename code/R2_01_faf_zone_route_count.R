# Verify unique FAF zones and route count from raw FAF5 data
library(dplyr)
library(readr)

# Raw FAF5.6.1 file (not in the repository; download from https://www.bts.gov/faf).
# Another location can be given with the FAF_CSV environment variable.
FAF_CSV <- Sys.getenv("FAF_CSV", "./data/FAF/FAF5.6.1_2018-2023.csv")
faf <- read_csv(FAF_CSV, col_types = cols(.default = "n"))

# --- All data: unique zones ---
all_zones <- union(unique(faf$dms_orig), unique(faf$dms_dest))
cat("Total unique FAF zones in raw data:", length(all_zones), "\n")

# --- trade_type distribution (Comment 1d) ---
cat("\n=== trade_type distribution (ALL commodities) ===\n")
faf %>%
  mutate(tons_total = rowSums(select(., starts_with("tons_")), na.rm=TRUE)) %>%
  group_by(trade_type) %>%
  summarise(n_rows = n(), total_tons = sum(tons_total)) %>%
  mutate(
    trade_label = case_when(
      trade_type == 1 ~ "Domestic",
      trade_type == 2 ~ "Import",
      trade_type == 3 ~ "Export",
      trade_type == 4 ~ "Intra-regional",
      TRUE ~ "Other"
    ),
    pct_rows = round(n_rows / sum(n_rows) * 100, 1),
    pct_tons = round(total_tons / sum(total_tons) * 100, 1)
  ) %>%
  select(trade_type, trade_label, n_rows, pct_rows, total_tons, pct_tons) %>%
  print()

# --- Food freight only: SCTG 1-7 ---
cat("\n=== trade_type distribution (Food freight: SCTG 1-7) ===\n")
faf_food <- faf %>% filter(sctg2 %in% 1:7)
faf_food %>%
  mutate(tons_total = rowSums(select(., starts_with("tons_")), na.rm=TRUE)) %>%
  group_by(trade_type) %>%
  summarise(n_rows = n(), total_tons = sum(tons_total)) %>%
  mutate(
    trade_label = case_when(
      trade_type == 1 ~ "Domestic",
      trade_type == 2 ~ "Import",
      trade_type == 3 ~ "Export",
      trade_type == 4 ~ "Intra-regional",
      TRUE ~ "Other"
    ),
    pct_rows = round(n_rows / sum(n_rows) * 100, 1),
    pct_tons = round(total_tons / sum(total_tons) * 100, 1)
  ) %>%
  select(trade_type, trade_label, n_rows, pct_rows, total_tons, pct_tons) %>%
  print()

# --- SCTG composition of domestic food freight (Comment 1c) ---
cat("\n=== SCTG composition (Domestic truck, SCTG 1-7) ===\n")
faf_domestic_truck <- faf %>% filter(trade_type == 1, sctg2 %in% 1:7, dms_mode == 1)
faf_domestic_truck %>%
  mutate(tons_total = rowSums(select(., starts_with("tons_")), na.rm=TRUE)) %>%
  group_by(sctg2) %>%
  summarise(total_tons = sum(tons_total)) %>%
  mutate(pct = round(total_tons / sum(total_tons) * 100, 1)) %>%
  print()

# --- Unique routes in domestic truck food freight ---
cat("\n=== Unique OD routes (domestic truck, SCTG 1-7) ===\n")
routes <- faf_domestic_truck %>%
  distinct(dms_orig, dms_dest)
cat("Unique routes:", nrow(routes), "\n")
cat("Unique origins:", n_distinct(faf_domestic_truck$dms_orig), "\n")
cat("Unique destinations:", n_distinct(faf_domestic_truck$dms_dest), "\n")

# Save results
save_dir <- "./tables"

# Trade type table
trade_table <- faf_food %>%
  mutate(tons_total = rowSums(select(., starts_with("tons_")), na.rm=TRUE)) %>%
  group_by(trade_type) %>%
  summarise(n_rows = n(), total_tons = sum(tons_total)) %>%
  mutate(
    trade_label = case_when(
      trade_type == 1 ~ "Domestic", trade_type == 2 ~ "Import",
      trade_type == 3 ~ "Export",   trade_type == 4 ~ "Intra-regional"
    ),
    pct_tons = round(total_tons / sum(total_tons) * 100, 1)
  )
write.csv(trade_table, file.path(save_dir, "trade_type_distribution.csv"), row.names=FALSE)

# SCTG table
sctg_table <- faf_domestic_truck %>%
  mutate(tons_total = rowSums(select(., starts_with("tons_")), na.rm=TRUE)) %>%
  group_by(sctg2) %>%
  summarise(total_tons = sum(tons_total)) %>%
  mutate(pct = round(total_tons / sum(total_tons) * 100, 1))
write.csv(sctg_table, file.path(save_dir, "sctg_composition.csv"), row.names=FALSE)

# Table S1 of the Supplementary Information (SCTG composition, cumulative 2018-2023)
sctg_desc <- c("Live animals and fish", "Cereal grains", "Other agricultural products",
               "Animal feed and products of animal origin", "Meat, poultry, fish, seafood, and preparations",
               "Milled grain products, bakery products, pasta", "Other prepared foodstuffs and fats and oils")
s1 <- sctg_table %>% mutate(Description = sctg_desc[sctg2], Share = sprintf("%.1f%%", pct)) %>%
  select(SCTG = sctg2, Description, Share, total_tons)
if (!dir.exists(file.path(save_dir, "supplementary"))) dir.create(file.path(save_dir, "supplementary"))
write.csv(s1, file.path(save_dir, "supplementary", "TableS1_sctg_composition.csv"), row.names = FALSE)

cat("\nDone. Results saved to tables/\n")
