# ==============================================================================
# 08_main_figures.R
# Purpose: Generate Figures 1-5 and Supplementary Figures S5-S7 using AVG-WEIGHTED (3SD) Data.
#          - Uses 'avg*' variables for consistency with regressions.
#          - Uses pre-calculated 'dist_cat' from Step 05.
# ==============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidyr)
library(fixest)
library(broom)

# --- 1. CONFIGURATION ---
BASE_DIR    <- "./figures"
DATA_DIR    <- "./data"
MAP_DIR     <- "./data/Shapefile"
ROUTE_DIR   <- "./data/Route"
REG_DIR     <- "./tables/main"

# Inputs
INPUT_DATA  <- file.path(DATA_DIR, "panel_od_year_truck_sctg01_07.rds")
ROUTES_FILE <- file.path(ROUTE_DIR, "od_route_geometries_conus.gpkg") 
STATE_SHP   <- file.path(MAP_DIR, "cb_2018_us_state_5m.shp")

# *** CHANGE 1: Point to 'avg_weighted' folder ***
# Coefficient Sources (For reference or direct plotting if needed)
DIST_COEFFS      <- file.path(REG_DIR, "results_3sd/avg_weighted/coeffs_distance.rds")
SVI_COEFFS       <- file.path(REG_DIR, "results_3sd/avg_weighted/coeffs_svi.rds")
SHARE_COEFFS     <- file.path(REG_DIR, "results_3sd/avg_weighted/coeffs_share.rds")
SHARE_SVI_COEFFS <- file.path(REG_DIR, "results_3sd/avg_weighted/coeffs_share_svi.rds")

# Output Dirs
if(!dir.exists(file.path(BASE_DIR, "figure1"))) dir.create(file.path(BASE_DIR, "figure1"), recursive = TRUE)
if(!dir.exists(file.path(BASE_DIR, "figure2"))) dir.create(file.path(BASE_DIR, "figure2"), recursive = TRUE)
for (d in c("figure3", "figure4", "figure5")) if(!dir.exists(file.path(BASE_DIR, d))) dir.create(file.path(BASE_DIR, d), recursive = TRUE)
if(!dir.exists(file.path(BASE_DIR, "supplementary"))) dir.create(file.path(BASE_DIR, "supplementary"), recursive = TRUE)

# Palette
svi_route_colors <- c("Low"="#008f91", "Low-Medium"="#4489c8", "Medium-High"="#ffcd44", "High"="#ed7e7a")

# ==============================================================================
# PART A: FIGURE 1 (MAP + BAR CHARTS)
# ==============================================================================
message("Generating Figure 1 (Using Avg Weighted Vars)...")

# --- A1. LOAD MAP ---
if (file.exists(ROUTES_FILE)) {
  routes_within_joined <- st_read(ROUTES_FILE, quiet = TRUE)
  if("svi_group" %in% names(routes_within_joined)) {
    routes_within_joined$svi_group[routes_within_joined$svi_group == "Medium-Low"] <- "Low-Medium"
    routes_within_joined$svi_group <- factor(routes_within_joined$svi_group, levels = c("Low", "Low-Medium", "Medium-High", "High"))
  }
} else {
  stop("FATAL: 'od_route_geometries_conus.gpkg' not found.")
}

us_states <- st_read(STATE_SHP, quiet = TRUE) %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

# --- A2. WEATHER SUMMARY ---
# *** CHANGE 2: Use 'avg' variables for consistency ***
df <- readRDS(INPUT_DATA)

weather_summary <- df %>%
  rename(SviGroup = svi_cat) %>%
  filter(!is.na(SviGroup)) %>%
  group_by(SviGroup) %>%
  summarise(
    HighTemp = mean(avgHighTempFreq_3sd, na.rm = TRUE),
    LowTemp  = mean(avgLowTempFreq_3sd,  na.rm = TRUE),
    HighPrec = mean(avgHighPrecFreq,     na.rm = TRUE), 
    HighWind = mean(avgHighVsFreq,       na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(SviGroup = recode(SviGroup, "Low SVI" = "Low", "Medium-Low" = "Low-Medium"))

# --- A3. PLOTTING ---
# 1. Map
map_plot <- ggplot() +
  geom_sf(data = us_states, fill = "white", color = "black", size = 0.6) + 
  geom_sf(data = routes_within_joined, aes(color = svi_group), size = 0.6, alpha = 0.9, key_glyph = "path") +
  scale_color_manual(values = svi_route_colors, name = "Route by SVI", drop = FALSE, na.value = "grey85") +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1, linetype = "solid", fill = NA))) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm"), 
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5), 
        panel.grid = element_blank(), axis.text = element_blank())

# 2. Bars
bar_w <- 0.6
lock_axis <- function(breaks) {
  scale_y_continuous(breaks = breaks, limits = range(breaks), expand = expansion(mult = c(0, 0), add = 0))
}

plot_bar <- function(data, y_var, title) {
  ggplot(data, aes(x = SviGroup, y = .data[[y_var]], fill = SviGroup)) +
    geom_col(width = bar_w) + 
    geom_text(aes(label = sprintf("%.2f", .data[[y_var]])), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = "bold") +
    scale_fill_manual(values = svi_route_colors) +
    labs(y = title, x = NULL) + theme_minimal() + 
    theme(legend.position = "none", axis.text.x = element_text(size=8), axis.title.y = element_text(size=8, face="bold"))
}

p1 <- plot_bar(weather_summary, "HighTemp", "Mean Extreme Hot Days")
p2 <- plot_bar(weather_summary, "LowTemp", "Mean Extreme Cold Days")
p3 <- plot_bar(weather_summary, "HighPrec", "Mean Heavy Precip Days") + lock_axis(c(0, 1, 2, 3))
p4 <- plot_bar(weather_summary, "HighWind", "Mean High Wind Days") + lock_axis(c(0, 1, 2, 3))

final_map_hist <- (map_plot / (p1 | p2) / (p3 | p4)) + 
  plot_layout(heights = c(1.6, 1, 1)) + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12, face = "bold"), plot.tag.position = c(0.01, 0.99))

ggsave(file.path(BASE_DIR, "figure1/figure1.png"), final_map_hist, width = 7.2, height = 8.8, dpi = 300)
ggsave(file.path(BASE_DIR, "figure1/figure1.pdf"), final_map_hist, width = 7.2, height = 8.8)

# ==============================================================================
# PART B: FIGURE 2 (COEFFICIENT PLOTS)
# ==============================================================================
message("Generating Figure 2...")

VARS_USED <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd", "avgHighPrecFreq", "avgHighVsFreq")
VAR_LABELS <- c("avgHighTempFreq_3sd" = "High Temp", "avgLowTempFreq_3sd" = "Low Temp", "avgHighPrecFreq" = "Heavy Precip", "avgHighVsFreq" = "High Wind")

get_plot_data <- function(data, label, outcome) {
  data_reg <- data %>% filter(!is.na(total_distance))
  fml <- as.formula(paste(outcome, "~", paste(VARS_USED, collapse="+"), "| unit + year"))
  m <- feols(fml, cluster = ~unit, data = data_reg)
  tidy(m, conf.int = TRUE) %>%
    filter(term %in% VARS_USED) %>%
    mutate(model = label, term = str_replace_all(term, VAR_LABELS),
           conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error,
           stars = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ ""),
           label_text = paste0(sprintf("%.3f", estimate), stars))
}

# A. Distance (Using new labels to match Fig 4)
df_tons_dist <- bind_rows(
  get_plot_data(df, "Full Sample", "total_tons"),
  get_plot_data(df %>% filter(dist_cat == "Short"), "0-500 miles", "total_tons"),
  get_plot_data(df %>% filter(dist_cat == "Medium"), "500-1000 miles", "total_tons"),
  get_plot_data(df %>% filter(dist_cat == "Long"), ">1000 miles", "total_tons")
)
df_tmiles_dist <- bind_rows(
  get_plot_data(df, "Full Sample", "total_tmiles"),
  get_plot_data(df %>% filter(dist_cat == "Short"), "0-500 miles", "total_tmiles"),
  get_plot_data(df %>% filter(dist_cat == "Medium"), "500-1000 miles", "total_tmiles"),
  get_plot_data(df %>% filter(dist_cat == "Long"), ">1000 miles", "total_tmiles")
)

# B. SVI
df$svi_cat <- as.character(df$svi_cat)
df$svi_cat[df$svi_cat == "Medium-Low"] <- "Low-Medium"
svi_levels_present <- c("Low", "Low-Medium", "Medium-High", "High")
df_tons_svi <- bind_rows(lapply(svi_levels_present, function(cat) get_plot_data(df %>% filter(svi_cat == cat), paste(cat, "SVI"), "total_tons")))
df_tmiles_svi <- bind_rows(lapply(svi_levels_present, function(cat) get_plot_data(df %>% filter(svi_cat == cat), paste(cat, "SVI"), "total_tmiles")))

# Order & Plot
term_order <- rev(c("High Temp", "Low Temp", "Heavy Precip", "High Wind"))
dist_order <- c("Full Sample", "0-500 miles", "500-1000 miles", ">1000 miles")
svi_order <- c("Low SVI", "Low-Medium SVI", "Medium-High SVI", "High SVI")

# Apply Factors
# *** FIX: rev(svi_order) ensures 'Low SVI' is at the TOP of the Y-axis ***
df_tons_dist   <- df_tons_dist %>% mutate(term=factor(term, levels=term_order), model=factor(model, levels=rev(dist_order)))
df_tmiles_dist <- df_tmiles_dist %>% mutate(term=factor(term, levels=term_order), model=factor(model, levels=rev(dist_order)))
df_tons_svi    <- df_tons_svi %>% mutate(term=factor(term, levels=term_order), model=factor(model, levels=rev(svi_order)))
df_tmiles_svi  <- df_tmiles_svi %>% mutate(term=factor(term, levels=term_order), model=factor(model, levels=rev(svi_order)))

cols <- c("#4B8FC2", "#D81B60", "#FFC107", "#43A047") 
common_theme <- theme_minimal(base_size = 11) + theme(plot.title = element_text(hjust=0.5, size=12), plot.tag = element_text(size=14, face="bold"), panel.border = element_rect(color="black", fill=NA, linewidth=0.5), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(color = "grey92"), axis.text.y = element_text(size=10, color="black"), axis.title.x = element_text(margin = margin(t=8)))

make_coefplot <- function(df, model_label, color, tag_letter) {
  ggplot(df %>% filter(model == model_label), aes(x = estimate, y = term)) + geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") + geom_segment(aes(x = conf.low, xend = conf.high, yend = term), color = color, linewidth = 1.2) + geom_point(color = color, fill = "white", shape = 21, size = 2.6, stroke = 1) + geom_text(aes(label = label_text, x = Inf), hjust = 1.1, size = 3.5, fontface = "plain") + scale_x_continuous(expand = expansion(mult = c(0.1, 0.45))) + labs(x = "Coefficient", y = NULL, title = model_label, tag = tag_letter) + common_theme
}
make_var_panel <- function(df, term_key, panel_title, color, tag_letter) {
  ggplot(df %>% filter(term == term_key), aes(x = estimate, y = model)) + geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") + geom_segment(aes(x = conf.low, xend = conf.high, yend = model), linewidth = 1.2, color = color) + geom_point(shape = 21, fill = "white", color = color, size = 2.6, stroke = 1) + geom_text(aes(label = label_text, x = Inf), hjust = 1.1, size = 3.5, fontface = "plain") + scale_x_continuous(expand = expansion(mult = c(0.1, 0.45))) + labs(x = "Coefficient", y = NULL, title = panel_title, tag = tag_letter) + common_theme
}

# Save Fig 2
ggsave(file.path(BASE_DIR, "figure2/coef_plot_tons.png"), (make_coefplot(df_tons_dist, "Full Sample", cols[1], "a") | make_coefplot(df_tons_dist, "0-500 miles", cols[2], "b")) / (make_coefplot(df_tons_dist, "500-1000 miles", cols[3], "c") | make_coefplot(df_tons_dist, ">1000 miles", cols[4], "d")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure2/coef_plot_tmiles.png"), (make_coefplot(df_tmiles_dist, "Full Sample", cols[1], "e") | make_coefplot(df_tmiles_dist, "0-500 miles", cols[2], "f")) / (make_coefplot(df_tmiles_dist, "500-1000 miles", cols[3], "g") | make_coefplot(df_tmiles_dist, ">1000 miles", cols[4], "h")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure3/coef_svi_tons.png"), (make_coefplot(df_tons_svi, "Low SVI", cols[1], "a") | make_coefplot(df_tons_svi, "Low-Medium SVI", cols[2], "b")) / (make_coefplot(df_tons_svi, "Medium-High SVI", cols[3], "c") | make_coefplot(df_tons_svi, "High SVI", cols[4], "d")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure3/coef_svi_tonmiles.png"), (make_coefplot(df_tmiles_svi, "Low SVI", cols[1], "e") | make_coefplot(df_tmiles_svi, "Low-Medium SVI", cols[2], "f")) / (make_coefplot(df_tmiles_svi, "Medium-High SVI", cols[3], "g") | make_coefplot(df_tmiles_svi, "High SVI", cols[4], "h")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure2/coef_plot_tons_byVAR.png"), (make_var_panel(df_tons_dist, "High Temp", "Extreme hot days", cols[1], "a") | make_var_panel(df_tons_dist, "Low Temp", "Extreme cold days", cols[2], "b")) / (make_var_panel(df_tons_dist, "Heavy Precip", "Heavy precipitation days", cols[3], "c") | make_var_panel(df_tons_dist, "High Wind", "High wind days", cols[4], "d")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure2/coef_plot_tmiles_byVAR.png"), (make_var_panel(df_tmiles_dist, "High Temp", "Extreme hot days", cols[1], "e") | make_var_panel(df_tmiles_dist, "Low Temp", "Extreme cold days", cols[2], "f")) / (make_var_panel(df_tmiles_dist, "Heavy Precip", "Heavy precipitation days", cols[3], "g") | make_var_panel(df_tmiles_dist, "High Wind", "High wind days", cols[4], "h")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure3/coef_svi_tons_byVAR.png"), (make_var_panel(df_tons_svi, "High Temp", "Extreme hot days", cols[1], "a") | make_var_panel(df_tons_svi, "Low Temp", "Extreme cold days", cols[2], "b")) / (make_var_panel(df_tons_svi, "Heavy Precip", "Heavy precipitation days", cols[3], "c") | make_var_panel(df_tons_svi, "High Wind", "High wind days", cols[4], "d")), width=10, height=8, dpi=300)
ggsave(file.path(BASE_DIR, "figure3/coef_svi_tonmiles_byVAR.png"), (make_var_panel(df_tmiles_svi, "High Temp", "Extreme hot days", cols[1], "e") | make_var_panel(df_tmiles_svi, "Low Temp", "Extreme cold days", cols[2], "f")) / (make_var_panel(df_tmiles_svi, "Heavy Precip", "Heavy precipitation days", cols[3], "g") | make_var_panel(df_tmiles_svi, "High Wind", "High wind days", cols[4], "h")), width=10, height=8, dpi=300)

# ==============================================================================
# PART C: FIGURES 4-5 (TRUCK SHARE BY DISTANCE AND BY SVI)
# ==============================================================================
# Coefficients come from code/07b_regression_coefficients.R (run it first).
# Distance groups are the FAF truck distance band (dist_cat, see 06_build_panel.R).
# Stars follow the manuscript captions: *** p<0.001, ** p<0.01, * p<0.05.
message("Generating Figures 4-5 from coefficient files...")
FIG4_DIR <- file.path(BASE_DIR, "figure4")
if(!dir.exists(FIG4_DIR)) dir.create(FIG4_DIR, recursive = TRUE)

read_share <- function(path, outcome_name) {
  readRDS(path) %>%
    filter(outcome == outcome_name) %>%
    mutate(
      term  = unname(VAR_LABELS[term]),
      model = gsub("–", "-", sub("^sample\\.var: svi_cat; sample: ", "", model)),
      stars = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "")
    ) %>%
    select(term, model, estimate, std.error, stars) %>%
    as.data.frame()
}
df_tons_dist_man   <- read_share(SHARE_COEFFS,     "share_tons")
df_tmiles_dist_man <- read_share(SHARE_COEFFS,     "share_tmiles")
df_tons_svi_man    <- read_share(SHARE_SVI_COEFFS, "share_tons")
df_tmiles_svi_man  <- read_share(SHARE_SVI_COEFFS, "share_tmiles")

# 2. Prep Function
prep_man <- function(df, model_order) {
  df %>% mutate(conf.low = estimate - 1.96 * std.error, conf.high = estimate + 1.96 * std.error, label_text = paste0(sprintf("%.4f", estimate), stars), term = factor(term, levels = rev(c("High Temp", "Low Temp", "Heavy Precip", "High Wind"))), model = factor(model, levels = model_order))
}
# *** FIX: Updated Labels here too ***
dist_rev <- rev(c("Full Sample", "0-500 miles", "500-1000 miles", ">1000 miles"))
svi_rev  <- rev(c("Low SVI", "Low-Medium SVI", "Medium-High SVI", "High SVI"))

df_tons_dist_man   <- prep_man(df_tons_dist_man, dist_rev)
df_tmiles_dist_man <- prep_man(df_tmiles_dist_man, dist_rev)
df_tons_svi_man    <- prep_man(df_tons_svi_man, svi_rev)
df_tmiles_svi_man  <- prep_man(df_tmiles_svi_man, svi_rev)

# 3. Plotting
# Using locally defined make_var_panel from above
p1 <- (make_var_panel(df_tons_dist_man, "High Temp", "Extreme hot days", cols[1], "a") | make_var_panel(df_tons_dist_man, "Low Temp", "Extreme cold days", cols[2], "b")) / (make_var_panel(df_tons_dist_man, "Heavy Precip", "Heavy precipitation days", cols[3], "c") | make_var_panel(df_tons_dist_man, "High Wind", "High wind days", cols[4], "d"))
ggsave(file.path(FIG4_DIR, "Share_Tons_Dist_byVAR.png"), p1, width=10, height=8, dpi=300)

p2 <- (make_var_panel(df_tmiles_dist_man, "High Temp", "Extreme hot days", cols[1], "e") | make_var_panel(df_tmiles_dist_man, "Low Temp", "Extreme cold days", cols[2], "f")) / (make_var_panel(df_tmiles_dist_man, "Heavy Precip", "Heavy precipitation days", cols[3], "g") | make_var_panel(df_tmiles_dist_man, "High Wind", "High wind days", cols[4], "h"))
ggsave(file.path(FIG4_DIR, "Share_TMiles_Dist_byVAR.png"), p2, width=10, height=8, dpi=300)

p3 <- (make_var_panel(df_tons_svi_man, "High Temp", "Extreme hot days", cols[1], "a") | make_var_panel(df_tons_svi_man, "Low Temp", "Extreme cold days", cols[2], "b")) / (make_var_panel(df_tons_svi_man, "Heavy Precip", "Heavy precipitation days", cols[3], "c") | make_var_panel(df_tons_svi_man, "High Wind", "High wind days", cols[4], "d"))
ggsave(file.path(BASE_DIR, "figure5", "Share_Tons_SVI_byVAR.png"), p3, width=10, height=8, dpi=300)

p4 <- (make_var_panel(df_tmiles_svi_man, "High Temp", "Extreme hot days", cols[1], "e") | make_var_panel(df_tmiles_svi_man, "Low Temp", "Extreme cold days", cols[2], "f")) / (make_var_panel(df_tmiles_svi_man, "Heavy Precip", "Heavy precipitation days", cols[3], "g") | make_var_panel(df_tmiles_svi_man, "High Wind", "High wind days", cols[4], "h"))
ggsave(file.path(BASE_DIR, "figure5", "Share_TMiles_SVI_byVAR.png"), p4, width=10, height=8, dpi=300)

# ==============================================================================
# SUPPLEMENTARY FIGURE S5 (ANNUAL TRENDS, FULL SAMPLE)
# ==============================================================================
message("Generating Figure S5 (Using Avg Vars)...")
OUT_DIR <- file.path(BASE_DIR, "supplementary")

df_summary <- df %>%
  group_by(year) %>%
  summarise(
    mean_tons   = mean(total_tons, na.rm = TRUE) ,    
    mean_tmiles = mean(total_tmiles, na.rm = TRUE) ,   
    # *** CHANGE 4: Use 'avg' variables ***
    mean_hot    = mean(avgHighTempFreq_3sd, na.rm = TRUE),
    mean_cold   = mean(avgLowTempFreq_3sd, na.rm = TRUE),
    mean_precip = mean(avgHighPrecFreq,     na.rm = TRUE),
    mean_wind   = mean(avgHighVsFreq,       na.rm = TRUE)
  ) %>% ungroup()

# Recalculate VJUST
calc_vjust <- function(data, col_name) {
  val <- data[[col_name]]
  n <- length(val)
  vjust_vec <- rep(-1.2, n)
  for(i in 2:(n-1)) { if(val[i] < val[i-1] & val[i] < val[i+1]) vjust_vec[i] <- 1.8 }
  if(val[1] < val[2]) vjust_vec[1] <- 1.8
  if(val[n] < val[n-1]) vjust_vec[n] <- 1.8
  return(vjust_vec)
}
df_summary$v_tons   <- calc_vjust(df_summary, "mean_tons")
df_summary$v_tmiles <- calc_vjust(df_summary, "mean_tmiles")
df_summary$v_hot    <- calc_vjust(df_summary, "mean_hot")
df_summary$v_cold   <- calc_vjust(df_summary, "mean_cold")
df_summary$v_precip <- calc_vjust(df_summary, "mean_precip")
df_summary$v_wind   <- calc_vjust(df_summary, "mean_wind")

NPG_RED    <- "#E64B35"; NPG_BLUE <- "#4DBBD5"; NPG_GREEN <- "#00A087"; NPG_ORANGE <- "#E6A800"; NPG_PURPLE <- "#8491B4"; NPG_SLATE <- "#3C5488"
common_theme <- theme_minimal(base_size = 11) + theme(plot.title = element_text(hjust=0.5, size=11, face="bold", color="black"), plot.tag = element_text(size=14, face="bold", color="black"), plot.tag.position = c(0.01, 0.98), panel.border = element_rect(color="black", fill=NA, linewidth=0.5), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(color = "grey92"), axis.text = element_text(size=9, color="black"), axis.title.x = element_blank())

make_trend_plot <- function(data, y_var, vjust_var, title, color, tag_letter, y_suffix="") {
  ggplot(data, aes(x = year, y = .data[[y_var]])) +
    geom_line(color = color, linewidth = 1.0) +
    geom_point(color = color, fill = "white", shape = 21, size = 3, stroke = 1.5) +
    geom_text(aes(label = sprintf("%.2f", .data[[y_var]]), vjust = .data[[vjust_var]]), size = 3, fontface = "bold", color = "white", stroke = 2) +
    geom_text(aes(label = sprintf("%.2f", .data[[y_var]]), vjust = .data[[vjust_var]]), size = 3, fontface = "bold", color = "black") + 
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) + scale_x_continuous(breaks = unique(data$year)) +
    labs(x = NULL, y = y_suffix, title = title, tag = tag_letter) + common_theme
}

p_a <- make_trend_plot(df_summary, "mean_tons", "v_tons", "Mean Tons Shipped", NPG_PURPLE, "a", "(Thousands)")
p_b <- make_trend_plot(df_summary, "mean_tmiles", "v_tmiles", "Mean Ton-Miles", NPG_SLATE, "b", "(Million)")
p_c <- make_trend_plot(df_summary, "mean_hot", "v_hot", "Mean Extreme Hot Days", NPG_RED, "c")
p_d <- make_trend_plot(df_summary, "mean_cold", "v_cold", "Mean Extreme Cold Days", NPG_BLUE, "d")
p_e <- make_trend_plot(df_summary, "mean_precip", "v_precip", "Mean Heavy Precip Days", NPG_ORANGE, "e")
p_f <- make_trend_plot(df_summary, "mean_wind", "v_wind", "Mean High Wind Days", NPG_GREEN, "f")

final_figure <- (p_a | p_b) / (p_c | p_d) / (p_e | p_f) + plot_layout(heights = c(1, 1, 1))
ggsave(file.path(OUT_DIR, "FigureS5_Trends_Overall.png"), final_figure, width = 8.5, height = 10, dpi = 300)
ggsave(file.path(OUT_DIR, "FigureS5_Trends_Overall.pdf"), final_figure, width = 8.5, height = 10)

# ==============================================================================
# SUPPLEMENTARY FIGURES (TRENDS BY SUBGROUP)
# ==============================================================================
# --- 1. CONFIGURATION ---
OUT_DIR <- file.path(BASE_DIR, "supplementary")

# --- 2. DATA PREPARATION (Using existing dist_cat from Step 05) ---
df <- readRDS(INPUT_DATA)
df <- df %>%
  mutate(
    # Clean SVI
    svi_cat = as.character(svi_cat),
    svi_cat = recode(svi_cat, "Medium-Low" = "Low-Medium"), 
    svi_cat = factor(svi_cat, levels = c("Low", "Low-Medium", "Medium-High", "High"))
  ) %>%
  # *** FIX: No need to merge dist_band; use existing dist_cat if present, or create from dist_band ***
  # Ensuring dist_cat factor order
  mutate(dist_cat = factor(dist_cat, levels = c("Short", "Medium", "Long"))) %>%
  filter(!is.na(dist_cat), !is.na(svi_cat))

# --- 3. AGGREGATION (Using Avg Vars) ---
vars_expr <- list(
  "mean_tons"   = expression(mean(total_tons, na.rm = TRUE) ),   
  "mean_tmiles" = expression(mean(total_tmiles, na.rm = TRUE) ), 
  # *** CHANGE 5: Use 'avg' variables ***
  "mean_hot"    = expression(mean(avgHighTempFreq_3sd, na.rm = TRUE)),
  "mean_cold"   = expression(mean(avgLowTempFreq_3sd, na.rm = TRUE)),
  "mean_precip" = expression(mean(avgHighPrecFreq, na.rm = TRUE)),
  "mean_wind"   = expression(mean(avgHighVsFreq, na.rm = TRUE))
)

get_grouped_summary <- function(group_var) {
  df %>%
    group_by(year, across(all_of(group_var))) %>%
    summarise(
      mean_tons   = eval(vars_expr$mean_tons),
      mean_tmiles = eval(vars_expr$mean_tmiles),
      mean_hot    = eval(vars_expr$mean_hot),
      mean_cold   = eval(vars_expr$mean_cold),
      mean_precip = eval(vars_expr$mean_precip),
      mean_wind   = eval(vars_expr$mean_wind),
      .groups = "drop"
    ) %>%
    rename(Group = !!group_var)
}

# --- 4. PLOTTING SETUP ---
common_theme <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust=0.5, size=10, face="bold", color="black"), plot.tag = element_text(size=12, face="bold", color="black"), panel.border = element_rect(color="black", fill=NA, linewidth=0.5), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size=9, face="bold"), legend.key.width = unit(1.0, "cm"), axis.title.x = element_blank(), axis.text = element_text(color="black"))

make_panel <- function(data, y_var, title, colors, tag_letter, y_suffix="") {
  ggplot(data, aes(x = year, y = .data[[y_var]], color = Group, group = Group)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2, shape = 19, stroke = 0.5, fill = "white") + 
    scale_color_manual(values = colors) + scale_x_continuous(breaks = unique(data$year)) +
    labs(x = NULL, y = y_suffix, title = title, tag = tag_letter) + common_theme
}

# --- 5. FIGURE A: BY DISTANCE ---
df_dist <- get_grouped_summary("dist_cat")
cols_dist <- c("Short" = "#00A087", "Medium" = "#4DBBD5", "Long" = "#E64B35")

p1_a <- make_panel(df_dist, "mean_tons",   "Mean Tons Shipped",       cols_dist, "a", "(Thousands)")
p1_b <- make_panel(df_dist, "mean_tmiles", "Mean Ton-Miles",          cols_dist, "b", "(Millions)")
p1_c <- make_panel(df_dist, "mean_hot",    "Mean Extreme Hot Days",   cols_dist, "c")
p1_d <- make_panel(df_dist, "mean_cold",   "Mean Extreme Cold Days",  cols_dist, "d")
p1_e <- make_panel(df_dist, "mean_precip", "Mean Heavy Precip Days",  cols_dist, "e")
p1_f <- make_panel(df_dist, "mean_wind",   "Mean High Wind Days",     cols_dist, "f")

fig_dist <- (p1_a | p1_b) / (p1_c | p1_d) / (p1_e | p1_f) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "FigureS6_Trends_Distance.png"), fig_dist, width = 8.5, height = 10, dpi = 300)

# --- 6. FIGURE B: BY SVI ---
df_svi <- get_grouped_summary("svi_cat")
cols_svi <- c("Low" = "#008f91", "Low-Medium" = "#4489c8", "Medium-High" = "#ffcd44", "High" = "#ed7e7a")

p2_a <- make_panel(df_svi, "mean_tons",   "Mean Tons Shipped",       cols_svi, "a", "(Thousands)")
p2_b <- make_panel(df_svi, "mean_tmiles", "Mean Ton-Miles",          cols_svi, "b", "(Millions)")
p2_c <- make_panel(df_svi, "mean_hot",    "Mean Extreme Hot Days",   cols_svi, "c")
p2_d <- make_panel(df_svi, "mean_cold",   "Mean Extreme Cold Days",  cols_svi, "d")
p2_e <- make_panel(df_svi, "mean_precip", "Mean Heavy Precip Days",  cols_svi, "e")
p2_f <- make_panel(df_svi, "mean_wind",   "Mean High Wind Days",     cols_svi, "f")

fig_svi <- (p2_a | p2_b) / (p2_c | p2_d) / (p2_e | p2_f) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "FigureS7_Trends_SVI.png"), fig_svi, width = 8.5, height = 10, dpi = 300)

message("DONE! Figures 1-5 and S5-S7 generated.")