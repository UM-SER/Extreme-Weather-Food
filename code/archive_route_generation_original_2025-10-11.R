install.packages(c("osrm", "tidygeocoder"))
install.packages('units')
library(osrm)
library(sf)
library(tidygeocoder)
library(ggplot2)
library(dplyr)
library(tigris)
library(purrr)
library(progressr)
library(readr)
library(units)
library(plm)
library(modelsummary)
library(sandwich)
library(ggplot2)
library(patchwork) 
library(tidyr)
library(tidyverse)

# load map######################################################################
'''
counties_sf <- counties(cb = TRUE, resolution = "20m") %>% 
  st_transform(4269) %>% 
  mutate(GEOID = paste0(STATEFP, COUNTYFP))
'''
counties_sf2 <- st_read("./data/Shapefile/tl_2017_us_county.shp")

#counties_sf_5m<- st_read("./data/map/cb_2017_us_county_5m/cb_2017_us_county_5m.shp")
counties_sf<- st_read("./data/Shapefile/cb_2017_us_county_20m.shp")
remainder_state<- st_read("./data/Shapefile/2017_Remainder_of_States.shp")
CFS_Metro_Areas<- st_read("./data/Shapefile/2017_CFS_Metro_Areas.shp")
states_sf <- states(cb = TRUE) %>%
  st_transform(4269)

# route count###################################################################
route_counts <- filtered_data_ton_year %>%
  count(unit, name = "count") 

hist(route_counts$count)

# Example & plot ###############################################################
mainland_counties <- counties_sf2 %>% 
  filter(!(STATEFP %in% c("02", "15", "72")))

start_geoid <- "53073"  #
end_geoid <- "25025"    #

start_county <- counties_sf2 %>% filter(GEOID == start_geoid)
end_county   <- counties_sf2 %>% filter(GEOID == end_geoid)

start_point <- st_point_on_surface(start_county)
end_point   <- st_point_on_surface(end_county)

print(st_crs(start_point))
print(st_crs(end_point))

# Get route using osrmRoute
route <- osrmRoute(
  src = start_point,  # Start point
  dst = end_point    # End point
)

# Get route summary (distance in meters, duration in minutes)
route_summary <- osrmTable(src = start_point, dst = end_point)
cat(
  "Distance:", round(route_summary$distances / 1000, 2), "km",
  "\nDuration:", round(route_summary$durations, 2), "minutes\n"
)

ggplot() +
  geom_sf(data = route, color = "blue", linewidth = 1) +  # Driving route
  geom_sf(data = start_point, color = "green", size = 3) + # Start point
  geom_sf(data = end_point, color = "red", size = 3) +     # End point
  theme_void() +
  labs(title = "Driving Route Between Counties")

intersection <- st_intersects(route, counties_sf, sparse = FALSE)
county_indices <- which(intersection[1, ])  # Get the indices of intersecting counties

# Extract the intersecting counties
counties_passed <- counties_sf[county_indices, ]
passed_info <- counties_passed %>%
  st_drop_geometry() %>%
  select(NAME, STATE = STATEFP) %>%
  distinct()
print(passed_info)


p <- ggplot() +
  geom_sf(data = mainland_counties, fill = "grey95", color = "white", size = 0.3) +
  geom_sf(data = counties_passed, fill = "red", color = NA, alpha = 0.4) +
  geom_sf(data = route, color = "blue", size = 1.5) +
  geom_sf(data = start_point, color = "green", size = 3) +
  geom_sf(data = end_point, color = "red", size = 3) +
  theme_void() +
  labs(
    title = "Driving Route and Intersecting Counties (Contiguous  U.S.)",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none" 
  )


# Print the map
print(p)

plot_filename <- paste0(start_geoid, "_", end_geoid, ".png")
ggsave(filename = file.path("./data/result/route", plot_filename), 
       plot = p, width = 10, height = 6, dpi = 300)

# testing route#################################################
start_geoid <- "12011"  # dms_orig_GEOID
end_geoid <- "53069"    # dms_dest_GEOID

start_county <- counties_sf %>% filter(GEOID == start_geoid)
end_county   <- counties_sf %>% filter(GEOID == end_geoid)

start_point <- st_point_on_surface(start_county)
end_point   <- st_point_on_surface(end_county)

print(st_crs(start_point))
print(st_crs(end_point))

# Get centroids using geocoding
route <- osrmRoute(
  src = start_point,  # Start point
  dst = end_point    # End point
)

# Get route summary (distance in meters, duration in minutes)
route_summary <- osrmTable(src = start_point, dst = end_point)
cat(
  "Distance:", round(route_summary$distances / 1000, 2), "km",
  "\nDuration:", round(route_summary$durations, 2), "minutes\n"
)

intersection <- st_intersects(route, counties_sf, sparse = FALSE)
county_indices <- which(intersection[1, ])  # Get the indices of intersecting counties


# Extract the intersecting counties
counties_passed <- counties_sf[county_indices, ]
passed_info <- counties_passed %>%
  st_drop_geometry() %>%
  select(NAME, STATE = STATEFP, geoid= GEOID) %>%
  distinct()

years <- 2018:2023
passed_info <- passed_info %>%
  crossing(year = years)

passed_info_joined <- passed_info %>%
  left_join(tmmx_above_35_days, by = c("geoid" = "GEOID", "year" = "year"))
passed_info_joined <- passed_info_joined %>%
  mutate(tmmx_above_35_days = replace_na(tmmx_above_35_days, 0))
passed_info_summary <- passed_info_joined %>%
  group_by(year) %>%
  summarise(OverallHighTempFreq = mean(tmmx_above_35_days, na.rm = TRUE))

passed_info_summary <- passed_info_summary %>%
  mutate(start_geoid = start_geoid,
         end_geoid = end_geoid)

# process_freight_record  #############################
process_freight_record <- function(orig_id, dest_id, year) {
  year <- as.integer(year)
  
  # --- Determine the start point ---
  if (nchar(orig_id) == 5) {
    # County: use st_point_on_surface() 
    start_region <- counties_sf %>% filter(GEOID == orig_id)
    start_point <- st_point_on_surface(start_region)
  } else if (nchar(orig_id) == 2) {
    # State: use st_centroid()
    start_region <- states_sf %>% filter(STATEFP == orig_id)
    start_point <- st_centroid(start_region)
  } else {
    warning("Origin id (", orig_id, ") is not recognized. It should be 5 (county) or 2 (state) characters.")
    return(tibble(
      dms_orig_GEOID = orig_id,
      dms_dest_GEOID = dest_id,
      year = year,
      OverallHighTempFreq = NA_real_,
      OverallLowTempFreq = NA_real_,
      OverallHighPrecFreq = NA_real_,
      OverallHighVsFreq = NA_real_,
      OverallSvi = NA_real_,
      OverallSvi1 = NA_real_,
      OverallSvi2 = NA_real_,
      OverallSvi3 = NA_real_,
      OverallSvi4 = NA_real_,
      
    ))
  }
  
  # --- Determine the end point ---
  if (nchar(dest_id) == 5) {
    # County: use st_point_on_surface()
    end_region <- counties_sf %>% filter(GEOID == dest_id)
    end_point <- st_point_on_surface(end_region)
  } else if (nchar(dest_id) == 2) {
    # State: use st_centroid() 
    end_region <- states_sf %>% filter(STATEFP == dest_id)
    end_point <- st_centroid(end_region)
  } else {
    warning("Destination id (", dest_id, ") is not recognized. It should be 5 (county) or 2 (state) characters.")
    return(tibble(
      dms_orig_GEOID = orig_id,
      dms_dest_GEOID = dest_id,
      year = year,
      OverallHighTempFreq = NA_real_,
      OverallLowTempFreq = NA_real_,
      OverallHighPrecFreq = NA_real_,
      OverallHighVsFreq = NA_real_,
      OverallSvi = NA_real_,
      OverallSvi1 = NA_real_,
      OverallSvi2 = NA_real_,
      OverallSvi3 = NA_real_,
      OverallSvi4 = NA_real_
    ))
  }
  
  # --- Compute the route using OSRM ---
  route <- tryCatch({
    osrmRoute(src = start_point, dst = end_point)
  }, error = function(e) {
    warning("OSRM route error for (", orig_id, " to ", dest_id, "): ", e)
    return(NULL)
  })
  
  # If no route could be computed, return NA
  if (is.null(route)) {
    return(tibble(
      dms_orig_GEOID = orig_id,
      dms_dest_GEOID = dest_id,
      year = year,
      OverallHighTempFreq = NA_real_,
      OverallLowTempFreq = NA_real_,
      OverallHighPrecFreq = NA_real_,
      OverallHighVsFreq = NA_real_,
      OverallSvi = NA_real_,
      OverallSvi1 = NA_real_,
      OverallSvi2 = NA_real_,
      OverallSvi3 = NA_real_,
      OverallSvi4 = NA_real_,
      distwHighTempFreq = NA_real_,
      distwLowTempFreq = NA_real_,
      distwHighPrecFreq = NA_real_,
      distwHighVsFreq = NA_real_
    ))
  }
  
  # --- Determine the counties that the route passes through ---
  intersection <- st_intersects(route, counties_sf, sparse = FALSE)
  county_indices <- which(intersection[1, ])
  
  if (length(county_indices) == 0) {
    OverallHighTempFreq <- NA_real_
    OverallLowTempFreq <- NA_real_
    OverallHighPrecFreq <- NA_real_
    OverallHighVsFreq <- NA_real_
    OverallSvi <- NA_real_
    OverallSvi1 <- NA_real_
    OverallSvi2 <- NA_real_
    OverallSvi3 <- NA_real_
    OverallSvi4 <- NA_real_
    distwHighTempFreq <- NA_real_
    distwLowTempFreq <- NA_real_
    distwHighPrecFreq <- NA_real_
    distwHighVsFreq <- NA_real_
    
  } else {
    counties_passed <- counties_sf[county_indices, ]
    passed_info <- counties_passed %>%
      st_drop_geometry() %>%
      select(NAME, STATE = STATEFP, geoid = GEOID) %>%
      distinct() %>%
      mutate(year = year, # Add the year from the freight record
             dms_orig_GEOID = orig_id,
             dms_dest_GEOID = dest_id)  
    # --- Join with the climate data for that year ---
    joined_info <- passed_info %>%
      left_join(tmmx_above_35_days %>% filter(year == !!year),
                by = c("geoid" = "GEOID", "year" = "year")) %>%
      left_join(tmmn_below_0_days %>% filter(year == !!year),
                by = c("geoid" = "GEOID", "year" = "year")) %>%
      left_join(pr_above_50_days %>% filter(year == !!year),
                by = c("geoid" = "GEOID", "year" = "year")) %>%
      left_join(vs_above_8_days %>% filter(year == !!year),
                by = c("geoid" = "GEOID", "year" = "year")) %>%
      left_join(svi_2022,
                by = c("geoid" = "FIPS")) %>%
      left_join(result_route_table,
                by = c("dms_orig_GEOID" = "dms_orig_GEOID", "dms_dest_GEOID" = "dms_dest_GEOID",
                       "geoid" = "geoid"))%>%
      mutate(
        tmmx_above_35_days = ifelse(is.na(tmmx_above_35_days), 0, tmmx_above_35_days),
        days_tmmn_below_0 = ifelse(is.na(days_tmmn_below_0), 0, days_tmmn_below_0),
        pr_above_50_days = ifelse(is.na(pr_above_50_days), 0, pr_above_50_days),
        vs_above_8_days = ifelse(is.na(vs_above_8_days), 0, vs_above_8_days),
        svi = ifelse(is.na(RPL_THEMES), 0, RPL_THEMES),
        svi1 = ifelse(is.na(RPL_THEME1), 0, RPL_THEME1),
        svi2 = ifelse(is.na(RPL_THEME2), 0, RPL_THEME2),
        svi3 = ifelse(is.na(RPL_THEME3), 0, RPL_THEME3),
        svi4 = ifelse(is.na(RPL_THEME4), 0, RPL_THEME4),
        distance = ifelse(is.na(distance), 0, distance),
        total_distance = ifelse(is.na(total_distance), 0, total_distance)
      )
    # Compute overall frequencies
    OverallHighTempFreq <- mean(joined_info$tmmx_above_35_days, na.rm = TRUE)
    OverallLowTempFreq  <- mean(joined_info$days_tmmn_below_0, na.rm = TRUE)
    OverallHighPrecFreq <- mean(joined_info$pr_above_50_days, na.rm = TRUE)
    OverallHighVsFreq <- mean(joined_info$vs_above_8_days, na.rm = TRUE)
    OverallSvi <- mean(joined_info$svi, na.rm = TRUE)
    OverallSvi1 <- mean(joined_info$svi1, na.rm = TRUE)
    OverallSvi2 <- mean(joined_info$svi2, na.rm = TRUE)
    OverallSvi3 <- mean(joined_info$svi3, na.rm = TRUE)
    OverallSvi4 <- mean(joined_info$svi4, na.rm = TRUE)
    distwHighTempFreq <- sum((joined_info$distance * joined_info$tmmx_above_35_days)/joined_info$total_distance)
    distwLowTempFreq <- sum((joined_info$distance * joined_info$days_tmmn_below_0)/joined_info$total_distance)
    distwHighPrecFreq <- sum((joined_info$distance * joined_info$pr_above_50_days)/joined_info$total_distance)
    distwHighVsFreq <- sum((joined_info$distance * joined_info$vs_above_8_days)/joined_info$total_distance)
  }
  
  # Return the results for this freight record
  tibble(
    dms_orig_GEOID = orig_id,
    dms_dest_GEOID = dest_id,
    year = year,
    OverallHighTempFreq = OverallHighTempFreq,
    OverallLowTempFreq = OverallLowTempFreq,
    OverallHighPrecFreq = OverallHighPrecFreq,
    OverallHighVsFreq = OverallHighVsFreq,
    OverallSvi = OverallSvi,
    OverallSvi1 = OverallSvi1,
    OverallSvi2 = OverallSvi2,
    OverallSvi3 = OverallSvi3,
    OverallSvi4 = OverallSvi4,
    distwHighTempFreq = distwHighTempFreq,
    distwLowTempFreq = distwLowTempFreq,
    distwHighPrecFreq = distwHighPrecFreq,
    distwHighVsFreq = distwHighVsFreq
  )
}

rm(result)
result <- pmap_dfr(
  list(
    Panel_sum$dms_orig_GEOID, 
    Panel_sum$dms_dest_GEOID, 
    Panel_sum$year,
    seq_along(Panel_sum$dms_orig_GEOID)  # Create an iteration counter
  ),
  function(orig_id, dest_id, year, iter) {
    cat("Processing record:", iter, "of", nrow(Panel_sum), "\n")
    process_freight_record(orig_id, dest_id, year)
  }
)

output_dir <- "./data/result/route"
output_file <- file.path(output_dir, "Overall_result_0305.csv")

write_csv(result, output_file)
# test ############################################################
result_path <- "./data/Route/Overall_result_0305.csv"
result <- read.csv(result_path, stringsAsFactors = FALSE)
result <- result %>%
  mutate(
    dms_orig_GEOID = ifelse(nchar(as.character(dms_orig_GEOID)) == 1, 
                            paste0("0", dms_orig_GEOID),
                            ifelse(nchar(as.character(dms_orig_GEOID)) == 4, 
                                   paste0("0", dms_orig_GEOID),
                                   as.character(dms_orig_GEOID))),
    dms_dest_GEOID = ifelse(nchar(as.character(dms_dest_GEOID)) == 1, 
                            paste0("0", dms_dest_GEOID),
                            ifelse(nchar(as.character(dms_dest_GEOID)) == 4, 
                                   paste0("0", dms_dest_GEOID),
                                   as.character(dms_dest_GEOID)))
  )

result_route_table_path <- "./data/Route/result_route_table.csv"
result_route_table <- read.csv(result_route_table_path, stringsAsFactors = FALSE)

# SviGroup ###############
colnames(Panel_sum)
rm(joined_panel)
joined_panel <- Panel_sum %>%
  left_join(result, by = c("dms_orig_GEOID", "dms_dest_GEOID", "year"))
colnames(joined_panel)

hist(joined_panel$OverallSvi,
     main = "Distribution of Overall Social Vulnerability Index along route",
     xlab = "Overall Social Vulnerability Index")

joined_panel$SviGroup <- cut(
  joined_panel$OverallSvi,
  breaks = quantile(joined_panel$OverallSvi, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Low SVI", "Medium-Low SVI", "Medium-High SVI", "High SVI")
)



# summary########
colnames(joined_panel)
route_avg_tons_per_year <- joined_panel %>%
  group_by(unit, year) %>%
  summarise(avg_tons = mean(tons, na.rm = TRUE), .groups = "drop")
head(route_avg_tons_per_year)

svi_avg_tons <- joined_panel %>%
  group_by(SviGroup) %>%uiuio;uuoii;o
  summarise(avg_tons = mean(tons, na.rm = TRUE), .groups = "drop")
print(svi_avg_tons)

svi_distance_summary <- joined_panel %>%
  group_by(SviGroup) %>%
  summarise(
    avg_distance = mean(distance, na.rm = TRUE),
    median_distance = median(distance, na.rm = TRUE),
    sd_distance = sd(distance, na.rm = TRUE),
    min_distance = min(distance, na.rm = TRUE),
    max_distance = max(distance, na.rm = TRUE),
    .groups = "drop"
  )
print(svi_distance_summary)
# svi 
summary_table <- joined_panel %>%
  group_by(year, SviGroup) %>%
  summarise(
    Mean_Tons = mean(tons, na.rm = TRUE),
    SD_Tons = sd(tons, na.rm = TRUE),
    Mean_Distance = mean(distance, na.rm = TRUE),
    SD_Distance = sd(distance, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(year, SviGroup)

# weather_summary
print(summary_table)
weather_summary <- joined_panel %>%
  group_by(SviGroup) %>%
  summarise(
    HighTemp = mean(OverallHighTempFreq, na.rm = TRUE),
    LowTemp = mean(OverallLowTempFreq, na.rm = TRUE),
    HighPrec = mean(OverallHighPrecFreq, na.rm = TRUE),
    HighWind = mean(OverallHighVsFreq, na.rm = TRUE)
  )
print(weather_summary)

# svi to tons
summary_table_tons <- joined_panel %>%
  group_by(year, SviGroup) %>%
  summarise(
    Mean_Tons = mean(tons, na.rm = TRUE),
    SD_Tons = sd(tons, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(year, SviGroup)

# svi to distance
distance_summary <- joined_panel %>%
  group_by(SviGroup) %>%
  summarise(
    Mean_Distance = mean(distance, na.rm = TRUE),
    SD_Distance = sd(distance, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(SviGroup)
print(distance_summary)

# svi tmiles
summary_table_tmiles <- joined_panel %>%
  group_by(year, SviGroup) %>%
  summarise(
    Mean_Tmiles = mean(tmiles, na.rm = TRUE),
    SD_Tmiles = sd(tmiles, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(year, SviGroup)

# plot ########
# a.
ggplot(joined_panel, aes(x = OverallSvi)) +
  geom_histogram(bins = 30, fill = "grey60", color = "black") +
  labs(
    title = "Distribution of Overall Social Vulnerability Index Along Routes",
    x = "Overall Social Vulnerability Index",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank()
  )

ggsave("Overall_SVI_Distribution.pdf", width = 6, height = 4, dpi = 600)
ggsave(
  filename = "Overall_SVI_Distribution.pdf",
  plot = p,
  path = "./conference/aere",
  width = 6, height = 4, dpi = 600
)
# b.

weather_summary <- weather_summary %>%
  mutate(SviGroup = recode(SviGroup,
                           "Low SVI" = "Low",
                           "Medium-Low SVI" = "Low-Medium",
                           "Medium-High SVI" = "Medium-High",
                           "High SVI" = "High"))

weather_summary <- weather_summary %>%
  mutate(SviGroup = recode(SviGroup,
                           "Medium-Low" = "Low-Medium"))

nature_palette <- c(
  "Low" = "#008f91",         
  "Medium-Low" = "#4489c8",  
  "Medium-High" = "#ffcd44", 
  "High" = "#ed7e7a"        
)


p1 <- ggplot(weather_summary, aes(x = SviGroup, y = HighTemp, fill = SviGroup)) +
  geom_col() +
  scale_fill_manual(values = nature_palette) +
  labs(y = "Average High Temperature Frequency", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)
  )

p2 <- ggplot(weather_summary, aes(x = SviGroup, y = LowTemp, fill = SviGroup)) +
  geom_col() +
  scale_fill_manual(values = nature_palette) +
  labs(y = "Average Low Temperature Frequency", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)
  )

p3 <- ggplot(weather_summary, aes(x = SviGroup, y = HighPrec, fill = SviGroup)) +
  geom_col() +
  scale_fill_manual(values = nature_palette) +
  labs(y = "Average High Precipitation Frequency", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)
  )

p4 <- ggplot(weather_summary, aes(x = SviGroup, y = HighWind, fill = SviGroup)) +
  geom_col() +
  scale_fill_manual(values = nature_palette) +
  labs(y = "Average High Wind Frequency", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)
        )

combined_plot <- (p1 | p2) / (p3 | p4)
print(combined_plot)

ggsave("./figures/weather_bar_by_svi.png",
       plot = combined_plot, width = 10, height = 6, dpi = 600)

ggsave("./figures/figure1/p1_high_temp.png", plot = p1, width = 6, height = 4, dpi = 300)
ggsave("./figures/figure1/p2_low_temp.png", plot = p2, width = 6, height = 4, dpi = 300)
ggsave("./figures/figure1/p3_high_precip.png", plot = p3, width = 6, height = 4, dpi = 300)
ggsave("./figures/figure1/p4_high_wind.png", plot = p4, width = 6, height = 4, dpi = 300)
# c.svi tons
ggplot(summary_table_tons, aes(x = year, y = Mean_Tons, color = SviGroup, group = SviGroup)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = nature_palette) +
  labs(
    title = "Freight Volume (Tons) by SVI Group Over Time",
    x = "Year", y = "Mean Tons",
    color = "SVI Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("./figures/tons_by_svigroup.png",
       width = 7, height = 4.5, dpi = 600)

# d. SVI Ton-Miles
ggplot(summary_table_tmiles, aes(x = year, y = Mean_Tmiles, color = SviGroup, group = SviGroup)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = nature_palette) +
  labs(
    title = "Ton-Miles by SVI Group Over Time",
    x = "Year", y = "Mean Ton-Miles",
    color = "SVI Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("./figures/tmiles_by_svigroup.png",
       width = 7, height = 4.5, dpi = 600)

# svi distance
ggplot(summary_table, aes(x = year, y = Mean_Distance, color = SviGroup, group = SviGroup)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Freight Distance by SVI Group Over Time",
    x = "Year", y = "Mean Distance (miles or km)",
    color = "SVI Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("./figures/distance_by_svigroup.png",
       width = 7, height = 4.5, dpi = 600)
# regression all ###############################################################
# formular tons ~ distwHighTempFreq
custom_stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
formula1 <- tons ~   distwHighTempFreq +
  distwLowTempFreq + distwHighPrecFreq + distwHighVsFreq
formula2 <- tmiles ~ distwHighTempFreq +
  distwLowTempFreq + distwHighPrecFreq + distwHighVsFreq

# formula tons ~   OverallHighTempFreq

custom_stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
formula1 <- tons ~   OverallHighTempFreq +
  OverallLowTempFreq + OverallHighPrecFreq + OverallHighVsFreq
formula2 <- tmiles ~ OverallHighTempFreq +
  OverallLowTempFreq + OverallHighPrecFreq + OverallHighVsFreq

#Panel_sum_subset  <- joined_panel %>%
#  filter(dist_band%in% 4:5)
Panel_sum_subset  <- joined_panel

hist(Panel_sum_subset$dms_orig_svi_combined)

Panel_sum_subset_df <- as.data.frame(Panel_sum_subset)
table(Panel_sum_subset_df$unit, useNA = "ifany")  # 检查 unit 是否有 NA
table(Panel_sum_subset_df$year, useNA = "ifany")  # 检查 year 是否有 NA
Panel_sum_subset_cleaned <- Panel_sum_subset_df %>%
  filter(!is.na(unit) & !is.na(year))
#  #

formula1 <- tons ~   OverallHighTempFreq +
  OverallLowTempFreq + OverallHighPrecFreq + OverallHighVsFreq
formula2 <- tmiles ~ OverallHighTempFreq +
  OverallLowTempFreq + OverallHighPrecFreq + OverallHighVsFreq

m_full_tons   <- plm(formula1, data = Panel_sum_subset_cleaned, effect = "twoways", model = "within")
m_full_tmiles <- plm(formula2, data = Panel_sum_subset_cleaned, effect = "twoways", model = "within")

## Sub-samples by distance band
sub_1_3 <- filter(Panel_sum_subset_cleaned, dist_band %in% 1:3)
sub_4_5 <- filter(Panel_sum_subset_cleaned, dist_band %in% 4:5)
sub_6_8 <- filter(Panel_sum_subset_cleaned, dist_band %in% 6:8)

m_1_3_tons   <- plm(formula1, data = sub_1_3, effect = "twoways", model = "within")
m_1_3_tmiles <- plm(formula2, data = sub_1_3, effect = "twoways", model = "within")

m_4_5_tons   <- plm(formula1, data = sub_4_5, effect = "twoways", model = "within")
m_4_5_tmiles <- plm(formula2, data = sub_4_5, effect = "twoways", model = "within")

m_6_8_tons   <- plm(formula1, data = sub_6_8, effect = "twoways", model = "within")
m_6_8_tmiles <- plm(formula2, data = sub_6_8, effect = "twoways", model = "within")

# --- 4. Clustered standard errors (cluster at "unit") ---
clustered_se <- list(
  vcovHC(m_full_tons,   type = "HC1", cluster = "group"),
  vcovHC(m_full_tmiles, type = "HC1", cluster = "group"),
  vcovHC(m_1_3_tons,    type = "HC1", cluster = "group"),
  vcovHC(m_1_3_tmiles,  type = "HC1", cluster = "group"),
  vcovHC(m_4_5_tons,    type = "HC1", cluster = "group"),
  vcovHC(m_4_5_tmiles,  type = "HC1", cluster = "group"),
  vcovHC(m_6_8_tons,    type = "HC1", cluster = "group"),
  vcovHC(m_6_8_tmiles,  type = "HC1", cluster = "group")
)

# --- 5. Combine into one table ---
custom_stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

model_list <- list(
  "Full (Tons)"            = m_full_tons,
  "Full (Ton-Miles)"       = m_full_tmiles,
  "<500 mi (Tons)"         = m_1_3_tons,
  "<500 mi (Ton-Miles)"    = m_1_3_tmiles,
  "500–1000 mi (Tons)"     = m_4_5_tons,
  "500–1000 mi (Ton-Miles)"= m_4_5_tmiles,
  ">1000 mi (Tons)"        = m_6_8_tons,
  ">1000 mi (Ton-Miles)"   = m_6_8_tmiles
)

summary_table <- modelsummary(
  model_list,
  stars = custom_stars,
  vcov  = clustered_se,
  output = "markdown"
)
print(summary_table)

modelsummary(
  model_list,
  stars = custom_stars,
  vcov  = clustered_se,
  output = "./tables/main/fe_simpleaverage_8models.html"
)
# regress on logtons and logtmiles
Panel_sum_subset_cleaned <- Panel_sum_subset_cleaned %>%
  mutate(
    logtons   = log1p(tons),     # log(tons + 1)
    logtmiles = log1p(tmiles)    # log(tmiles + 1)
  )
Panel_sum_subset_cleaned <- pdata.frame(Panel_sum_subset_cleaned, index = c("unit", "year"))

formula1 <- logtons ~ OverallHighTempFreq +
  OverallLowTempFreq + OverallHighPrecFreq + OverallHighVsFreq
formula2 <- logtmiles ~ OverallHighTempFreq +
  OverallLowTempFreq + OverallHighPrecFreq + OverallHighVsFreq

m_full_tons   <- plm(formula1, data = Panel_sum_subset_cleaned, effect = "twoways", model = "within")
m_full_tmiles <- plm(formula2, data = Panel_sum_subset_cleaned, effect = "twoways", model = "within")

sub_1_3 <- dplyr::filter(Panel_sum_subset_cleaned, dist_band %in% 1:3)
sub_4_5 <- dplyr::filter(Panel_sum_subset_cleaned, dist_band %in% 4:5)
sub_6_8 <- dplyr::filter(Panel_sum_subset_cleaned, dist_band %in% 6:8)

m_1_3_tons    <- plm(formula1, data = sub_1_3, effect = "twoways", model = "within")
m_4_5_tons    <- plm(formula1, data = sub_4_5, effect = "twoways", model = "within")
m_6_8_tons    <- plm(formula1, data = sub_6_8, effect = "twoways", model = "within")

m_1_3_tmiles  <- plm(formula2, data = sub_1_3, effect = "twoways", model = "within")
m_4_5_tmiles  <- plm(formula2, data = sub_4_5, effect = "twoways", model = "within")
m_6_8_tmiles  <- plm(formula2, data = sub_6_8, effect = "twoways", model = "within")

## 5) Clustered SE (cluster at 'unit' per first index)
clustered_se <- list(
  vcovHC(m_full_tons,   type = "HC1", cluster = "group"),
  vcovHC(m_full_tmiles, type = "HC1", cluster = "group"),
  vcovHC(m_1_3_tons,    type = "HC1", cluster = "group"),
  vcovHC(m_1_3_tmiles,  type = "HC1", cluster = "group"),
  vcovHC(m_4_5_tons,    type = "HC1", cluster = "group"),
  vcovHC(m_4_5_tmiles,  type = "HC1", cluster = "group"),
  vcovHC(m_6_8_tons,    type = "HC1", cluster = "group"),
  vcovHC(m_6_8_tmiles,  type = "HC1", cluster = "group")
)

## 6) Table with all 8 models
custom_stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

model_list <- list(
  "Full (log Tons)"        = m_full_tons,
  "Full (log Ton-Miles)"   = m_full_tmiles,
  "<500 mi (log Tons)"     = m_1_3_tons,
  "<500 mi (log Ton-Miles)"= m_1_3_tmiles,
  "500–1000 (log Tons)"    = m_4_5_tons,
  "500–1000 (log Ton-Miles)"= m_4_5_tmiles,
  ">1000 (log Tons)"       = m_6_8_tons,
  ">1000 (log Ton-Miles)"  = m_6_8_tmiles
)

# On-screen preview
ms_tbl <- modelsummary(
  model_list,
  stars = custom_stars,
  vcov  = clustered_se,
  output = "markdown"
)
print(ms_tbl)
modelsummary(
  model_list,
  stars = custom_stars,
  vcov  = clustered_se,
  output = "./tables/main/fe_log_sa_8models.html"
)

# regression  truck fraction ########################################################
truck_fraction_by_unit <- filtered_data %>%
  group_by(unit) %>%
  summarise(
    truck_tons_fraction = sum(tons[dms_mode == 1], na.rm = TRUE) / sum(tons, na.rm = TRUE)
  )

print(truch_fraction$truck_tons_fraction)

Panel_sum_subset  <- joined_panel %>%
  filter(dist_band%in% 4:5)
Panel_sum_subset  <- joined_panel

hist(Panel_sum_subset$dms_orig_svi_combined)

Panel_sum_subset_df <- as.data.frame(Panel_sum_subset)
table(Panel_sum_subset_df$unit, useNA = "ifany")  # 检查 unit 是否有 NA
table(Panel_sum_subset_df$year, useNA = "ifany")  # 检查 year 是否有 NA
Panel_sum_subset_cleaned <- Panel_sum_subset_df %>%
  filter(!is.na(unit) & !is.na(year))

Panel_sum_subset_cleaned <- pdata.frame(Panel_sum_subset_cleaned, index = c("unit", "year"))

models <- list(
  plm(formula1, data = Panel_sum_subset_cleaned,effect = "twoways", model = "within"),
  plm(formula2, data = Panel_sum_subset_cleaned,effect = "twoways",  model = "within")
)
summary_table <- modelsummary(models, stars = custom_stars)

clustered_se <- lapply(models, function(model) {
  vcovHC(model, type = "HC1", cluster = "group")  # "group" clusters at the first index, i.e., "unit"
})
summary_table <- modelsummary(models,
                              stars = custom_stars,
                              vcov = clustered_se
)

print(summary_table)
mean(Panel_sum_subset_cleaned$tons)

# plot selected routes###########################################################
# unique_routes <- joined_panel %>% 
#   filter(dist_band %in% 1:3) %>% 
#   distinct(dms_orig_GEOID, dms_dest_GEOID)

unique_routes <- joined_panel %>% 
  distinct(dms_orig_GEOID, dms_dest_GEOID)
# Step 2: Define a function that returns an OSRM route for a given pair
get_route <- function(orig_id, dest_id) {
  
  # Determine start point based on the length of the origin ID
  if (nchar(orig_id) == 5) {
    start_region <- counties_sf2 %>% filter(GEOID == orig_id)
    start_point <- st_point_on_surface(start_region)
  } else if (nchar(orig_id) == 2) {
    start_region <- states_sf %>% filter(STATEFP == orig_id)
    start_point <- st_centroid(start_region)
  } else {
    warning("Origin id ", orig_id, " is not recognized. Must be 5 or 2 characters.")
    return(NULL)
  }
  
  # Determine end point based on the length of the destination ID
  if (nchar(dest_id) == 5) {
    end_region <- counties_sf2 %>% filter(GEOID == dest_id)
    end_point <- st_point_on_surface(end_region)
  } else if (nchar(dest_id) == 2) {
    end_region <- states_sf %>% filter(STATEFP == dest_id)
    end_point <- st_centroid(end_region)
  } else {
    warning("Destination id ", dest_id, " is not recognized. Must be 5 or 2 characters.")
    return(NULL)
  }
  
  # Compute route using OSRM
  route <- tryCatch({
    osrmRoute(src = start_point, dst = end_point)
  }, error = function(e) {
    warning("OSRM error for ", orig_id, " -> ", dest_id, ": ", e)
    return(NULL)
  })
  
  if (!is.null(route)) {
    # Attach identifiers for later use if route is successful
    route$dms_orig_GEOID <- orig_id
    route$dms_dest_GEOID <- dest_id
  }
  
  return(route)
}

# Step 3: Compute routes for each unique pair
routes_list <- pmap(
  list(
    unique_routes$dms_orig_GEOID, 
    unique_routes$dms_dest_GEOID, 
    seq_along(unique_routes$dms_orig_GEOID)
  ),
  function(orig_id, dest_id, iter) {
    cat("Processing route", iter, "of", nrow(unique_routes), ":", orig_id, "->", dest_id, "\n")
    get_route(orig_id, dest_id)
  }
)

# Create a fail list: pairs for which get_route returned NULL
fail_list <- unique_routes[ sapply(routes_list, is.null), ]
cat("Number of routes failed:", nrow(fail_list), "\n")
print(fail_list)

# Combine successful routes into one sf object
routes_1_3 <- do.call(rbind, routes_list[!sapply(routes_list, is.null)])
routes_all <-routes_1_3

mainland_union <- st_union(mainland_counties)
routes_within <- routes_all[st_within(routes_all, mainland_union, sparse = FALSE)[,1], ]

# st_write(routes_within,
#          "./data/Route/routes_within.gpkg",
#          layer = "routes_within",
#          delete_layer = TRUE)
# 

# Plot all routes on a map of the mainland U.S.
p <- ggplot() +
  # Background: mainland counties with a light fill
  geom_sf(data = mainland_counties, fill = "grey95", color = "white", size = 0.3) +
  # Draw all routes (cropped) as distinct blue lines
  geom_sf(data = routes_within, color = "blue", size = 1.5) +
  # Remove grid lines using a void theme
  theme_void() +
  labs(
    title = "Driving Routes for dist_band under 500 miles (Contiguous U.S.)",
    caption = "Data: OSRM, US Census Bureau"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )
print(p)
plot_filename <- paste0("", ".png")
ggsave(filename = file.path("./data/result/route", plot_filename), 
       plot = p, width = 10, height = 6, dpi = 300)


# plot svi distribution
result_2022 <- result %>%
  filter(year == 2022) %>%
  mutate(across(c(dms_orig_GEOID, dms_dest_GEOID), ~ str_pad(as.character(.), 5, pad = "0")))
routes_within <- routes_within %>%
  mutate(across(c(dms_orig_GEOID, dms_dest_GEOID), ~ str_pad(as.character(.), 5, pad = "0")))
routes_within_joined <- routes_within %>%
  left_join(
    result_2022 %>% select(dms_orig_GEOID, dms_dest_GEOID, OverallSvi),
    by = c("dms_orig_GEOID", "dms_dest_GEOID")
  )
routes_within_joined <- routes_within_joined %>%
  mutate(
    svi_group = case_when(
      is.na(OverallSvi) ~ "No Data",
      OverallSvi <= quantile(OverallSvi, 0.25, na.rm = TRUE) ~ "Low",
      OverallSvi <= quantile(OverallSvi, 0.5, na.rm = TRUE) ~ "Low-Medium",
      OverallSvi <= quantile(OverallSvi, 0.75, na.rm = TRUE) ~ "Medium-High",
      TRUE ~ "High"
    ),
    svi_group = factor(svi_group, levels = c("Low", "Low-Medium", "Medium-High", "High", "No Data"))
  )

us_states <- st_read("./data/Shapefile/cb_2018_us_state_5m.shp", quiet = TRUE) %>%
  filter(!STATEFP %in% c("02", "15", "72", "78", "60", "66", "69")) %>%
  summarise(geometry = st_union(geometry))

svi_route_colors <- c(
  "Low" = "#008f91",
  "Low-Medium" = "#4489c8",
  "Medium-High" = "#ffcd44",
  "High" = "#ed7e7a"
)

ggplot() +
  geom_sf(data = us_states, fill = "white", color = "black", size = 0.6) + 
  geom_sf(data = routes_within_joined, aes(color = svi_group), size = 0.6, alpha = 0.9) +
  scale_color_manual(
    values = svi_route_colors,
    name = "Route by SVI",
    drop = FALSE
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) +  # bold legend lines
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Freight Routes Colored by Aggregated Route SVI (2022)"
  )

ggsave("./figures/figure1/svi_route_map.png",
       width = 11, height = 8, dpi = 300)

# figure 1 ###################################################################
map_plot <- ggplot() +
  geom_sf(data = us_states, fill = "white", color = "black", size = 0.6) + 
  geom_sf(data = routes_within_joined, aes(color = svi_group), size = 0.6, alpha = 0.9) +
  scale_color_manual(values = svi_route_colors, name = "Route by SVI", drop = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

bar_w <- 0.6

p1 <- ggplot(weather_summary, aes(x = SviGroup, y = HighTemp, fill = SviGroup)) +
  geom_col(width = bar_w) +
  scale_fill_manual(values = svi_route_colors) +
  labs(y = "Mean High Temperature Days", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 8,face = "bold"),
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_text(size = 8)
  )

p2 <- ggplot(weather_summary, aes(x = SviGroup, y = LowTemp, fill = SviGroup)) +
  geom_col(width = bar_w) +
  scale_fill_manual(values = svi_route_colors) +
  labs(y = "Mean Low Temperature Days", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 8,face = "bold"),
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_text(size = 8)
  )

p3 <- ggplot(weather_summary, aes(x = SviGroup, y = HighPrec, fill = SviGroup)) +
  geom_col(width = bar_w) +
  scale_fill_manual(values = svi_route_colors) +
  labs(y = "Mean High Precipitation Days", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 8,face = "bold"),
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_text(size = 8)
  )

p4 <- ggplot(weather_summary, aes(x = SviGroup, y = HighWind, fill = SviGroup)) +
  geom_col(width = bar_w) +
  scale_fill_manual(values = svi_route_colors) +
  labs(y = "Mean High Wind Speed Days", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 8,face = "bold"),
        axis.text.x  = element_text(size = 8),
        axis.text.y  = element_text(size = 8)
  )
lock_axis <- function(breaks) {
  scale_y_continuous(
    breaks = breaks,
    limits = range(breaks),                     # top/bottom at ticks
    expand = expansion(mult = c(0, 0), add = 0) # no padding
  )
}

p3 <- p3 + lock_axis(c(0, 0.1, 0.2, 0.3))
p4 <- p4 + lock_axis(c(0, 0.5, 1, 1.5))



row2 <- p1 | p2
row3 <- p3 | p4

final_map_hist <- (map_plot / row2 / row3) +
  plot_layout(heights = c(1.6, 1, 1)) +     # make the map row taller
  plot_annotation(
    tag_levels = 'a',                        # auto-label panels a–e
  ) &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0.01, 0.99)
  )

out_dir <- "./figures/figure1"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "figure1.pdf"),
       final_map_hist, width = 7.2, height = 8.8)

ggsave(file.path(out_dir, "figure1.png"),
       final_map_hist, width = 7.2, height = 8.8, dpi = 300)

# route table####################################
counties_proj <- st_transform(counties_sf, 5070)
failed_routes <- list()
process_route_distance <- function(orig_id, dest_id) {
  if (orig_id == dest_id) {
    if(nchar(orig_id) == 5) {
      county <- counties_sf %>% filter(GEOID == orig_id)
      county_name <- county$NAME[1]
      return(tibble(
        dms_orig_GEOID = orig_id,
        dms_dest_GEOID = dest_id,
        geoid = orig_id,
        name = county_name,
        distance = 1,         
        total_distance = 1    
      ))
    } else if(nchar(orig_id) == 2) {
      return(tibble(
        dms_orig_GEOID = orig_id,
        dms_dest_GEOID = dest_id,
        geoid = orig_id,
        name = "State Route",
        distance = 1,
        total_distance = 1
      ))
    }
  }
  
  # Determine start point
  if (nchar(orig_id) == 5) {
    start_region <- counties_sf %>% filter(GEOID == orig_id)
    start_point <- st_point_on_surface(start_region)
  } else if (nchar(orig_id) == 2) {
    start_region <- states_sf %>% filter(STATEFP == orig_id)
    start_point <- st_centroid(start_region)
  } else {
    stop("Invalid origin ID: must be 5 (county) or 2 (state) characters.")
  }
  
  # Determine end point
  if (nchar(dest_id) == 5) {
    end_region <- counties_sf %>% filter(GEOID == dest_id)
    end_point <- st_point_on_surface(end_region)
  } else if (nchar(dest_id) == 2) {
    end_region <- states_sf %>% filter(STATEFP == dest_id)
    end_point <- st_centroid(end_region)
  } else {
    stop("Invalid destination ID: must be 5 (county) or 2 (state) characters.")
  }
  
  # Compute the route using OSRM
  route <- tryCatch({
    osrmRoute(src = start_point, dst = end_point)
  }, error = function(e) {
    warning("OSRM route error for ", orig_id, " to ", dest_id, ": ", e)
    
    # Log failure
    failed_routes <- append(failed_routes, list(tibble(
      dms_orig_GEOID = orig_id,
      dms_dest_GEOID = dest_id,
      error_message = as.character(e)
    )))
    
    return(NULL)
  })
  
  if (is.null(route)) {
    return(tibble(
      dms_orig_GEOID = orig_id,
      dms_dest_GEOID = dest_id,
      geoid = NA_character_,
      name = NA_character_,
      distance = NA_real_,
      total_distance = NA_real_
    ))
  }
  
  # Transform route and counties to NAD83 / Conus Albers (EPSG:5070) for distance calculations
  route_proj <- st_transform(route, 5070)
  
  # Get the intersection: portions of the route that lie within each county
  route_intersect <- st_intersection(route_proj, counties_proj)
  
  # Compute the length of each intersected segment (in meters) and convert to miles
  route_intersect <- route_intersect %>% 
    mutate(segment_length_m = st_length(geometry),
           distance = as.numeric(set_units(segment_length_m, "mi")))
  
  # Total distance along the route in miles
  total_distance <- sum(route_intersect$distance, na.rm = TRUE)
  
  # Build a table: one row per passed county, with county GEOID, NAME, segment distance, and overall total distance.
  route_table <- route_intersect %>% 
    st_drop_geometry() %>%
    mutate(dms_orig_GEOID = orig_id,
           dms_dest_GEOID = dest_id,
           total_distance = total_distance) %>%
    select(dms_orig_GEOID, dms_dest_GEOID, geoid = GEOID, name = NAME, distance, total_distance)
  
  return(route_table)
}

# Extract unique route pairs from Panel_sum_subset
unique_routes <- Panel_sum_subset_cleaned %>% 
  distinct(dms_orig_GEOID, dms_dest_GEOID)

# Loop over each unique route and compute county-level distances
result_route_table_temp <- pmap_dfr(
  list(unique_routes$dms_orig_GEOID, unique_routes$dms_dest_GEOID),
  function(orig_id, dest_id) {
    cat("Processing route:", orig_id, "->", dest_id, "\n")
    process_route_distance(orig_id, dest_id)
  }
)

# Convert distances to miles (they are numeric values in miles now)
result_route_table_temp <- result_route_table_temp %>%
  mutate(distance = round(distance, 2),
         total_distance = round(total_distance, 2))

working_check <- result_route_table_temp %>% 
  distinct(dms_orig_GEOID, dms_dest_GEOID)
rm(working_check)
# View the final table
print(result_route_table_temp)
output_dir <- "./data/result/route"
output_file <- file.path(output_dir, "od_route_county_segments.csv")
write_csv(result_route_table_temp, output_file)
