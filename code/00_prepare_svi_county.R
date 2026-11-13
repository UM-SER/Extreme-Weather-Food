library(sf)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(viridis)
library(readr)
library(ggpattern)

# Define file paths
path_2018 <- "./data/SVI/SVI_2018_US_county/SVI2018_US_county.gdb"
path_2020 <- "./data/SVI/SVI_2020_US_county/SVI2020_US_county.gdb"
path_2022 <- "./data/SVI/SVI_2022_US_county/SVI2022_US_county.gdb"

svi_2018 <- st_read(path_2018) %>%
  dplyr::select(ST, STATE, COUNTY, FIPS, E_TOTPOP, E_POV, E_UNEMP, E_NOHSDP) %>%
  mutate(year = 2018)

svi_2020 <- st_read(path_2020) %>%
  dplyr::select(ST, STATE, COUNTY, FIPS, E_TOTPOP, E_POV150, E_UNEMP,E_HBURD, E_NOHSDP, E_UNINSUR) %>%
  mutate(year = 2020)

svi_2022 <- st_read(path_2022) %>%
  dplyr::select(ST, STATE, COUNTY, FIPS, E_TOTPOP, E_POV150, E_UNEMP,E_HBURD, E_NOHSDP, E_UNINSUR,RPL_THEMES,
                RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4) %>%
  mutate(year = 2022)

hist(svi_2022$RPL_THEMES)

state_summary_2018 <- svi_2018 %>%
  st_drop_geometry() %>%  # Drop geometry before aggregation
  filter(E_TOTPOP != -999, 
         E_POV != -999, 
         E_UNEMP != -999, 
         E_NOHSDP != -999) %>%
  group_by(FIPS) %>%
  summarise(
    total_population = sum(E_TOTPOP, na.rm = TRUE),
    total_poverty = sum(E_POV, na.rm = TRUE),
    total_unemployed = sum(E_UNEMP, na.rm = TRUE),
    total_no_highschool = sum(E_NOHSDP, na.rm = TRUE),
    poverty_rate = (total_poverty / total_population) * 100,
    unemployment_rate = (total_unemployed / total_population) * 100,
    no_highschool_rate = (total_no_highschool / total_population) * 100
  ) %>%
  arrange(FIPS)

# For 2020 - drop geometry and filter out -999 values
state_summary_2020 <- svi_2020 %>%
  st_drop_geometry() %>%  # Drop geometry before aggregation
  filter(E_TOTPOP != -999, 
         E_POV150 != -999, 
         E_UNEMP != -999, 
         E_HBURD != -999,
         E_NOHSDP != -999,
         E_UNINSUR != -999) %>%
  group_by(FIPS) %>%
  summarise(
    total_population = sum(E_TOTPOP, na.rm = TRUE),
    total_poverty150 = sum(E_POV150, na.rm = TRUE),
    total_unemployed = sum(E_UNEMP, na.rm = TRUE),
    total_Housing_cost_burdened = sum(E_HBURD, na.rm = TRUE),
    total_no_highschool = sum(E_NOHSDP, na.rm = TRUE),
    total_uninsured = sum(E_UNINSUR, na.rm = TRUE),
    poverty150_rate = (total_poverty150 / total_population) * 100,
    unemployment_rate = (total_unemployed / total_population) * 100,
    hcb_rate = (total_Housing_cost_burdened / total_population) * 100,
    no_highschool_rate = (total_no_highschool / total_population) * 100,
    uninsured_rate = (total_uninsured / total_population) * 100
  ) %>%
  arrange(FIPS)

# For 2022 - drop geometry and filter out -999 values
state_summary_2022 <- svi_2022 %>%
  st_drop_geometry() %>%  # Drop geometry before aggregation
  filter(E_TOTPOP != -999, 
         E_POV150 != -999, 
         E_UNEMP != -999, 
         E_HBURD != -999,
         E_NOHSDP != -999,
         E_UNINSUR != -999,
         RPL_THEMES != -999,
         RPL_THEME1 != -999,
         RPL_THEME2 != -999,
         RPL_THEME3 != -999,
         RPL_THEME4 != -999) %>%
  group_by(FIPS) %>%
  summarise(
    total_population = sum(E_TOTPOP, na.rm = TRUE),
    total_poverty150 = sum(E_POV150, na.rm = TRUE),
    total_unemployed = sum(E_UNEMP, na.rm = TRUE),
    total_Housing_cost_burdened = sum(E_HBURD, na.rm = TRUE),
    total_no_highschool = sum(E_NOHSDP, na.rm = TRUE),
    total_uninsured = sum(E_UNINSUR, na.rm = TRUE),
    poverty150_rate = (total_poverty150 / total_population) * 100,
    unemployment_rate = (total_unemployed / total_population) * 100,
    hcb_rate = (total_Housing_cost_burdened / total_population) * 100,
    no_highschool_rate = (total_no_highschool / total_population) * 100,
    uninsured_rate = (total_uninsured / total_population) * 100,
    svi= mean(RPL_THEMES, na.rm = TRUE),
    svi1= mean(RPL_THEME1, na.rm = TRUE),
    svi2= mean(RPL_THEME2, na.rm = TRUE),
    svi3= mean(RPL_THEME3, na.rm = TRUE),
    svi4= mean(RPL_THEME4, na.rm = TRUE)
  ) %>%
  arrange(FIPS)

state_summary_2018 <- state_summary_2018 %>% mutate(across(where(is.numeric), ~ round(., 2)))
state_summary_2020 <- state_summary_2020 %>% mutate(across(where(is.numeric), ~ round(., 2)))
state_summary_2022 <- state_summary_2022 %>% mutate(across(where(is.numeric), ~ round(., 2)))

###########ADD TO COUNTY MAP####################################################
us_county <- st_read("./data/Shapefile/cb_2018_us_county_5m.shp")
us_county <- us_county[, c("STATEFP", "COUNTYFP", "GEOID")]
state_summary_2018 <- state_summary_2018[, c("FIPS", "total_population", "total_unemployed", "total_no_highschool")]
state_summary_2020 <- state_summary_2020[, c("FIPS", "total_population", "total_unemployed", "total_no_highschool")]
state_summary_2022 <- state_summary_2022[, c("FIPS", "svi","svi1","svi2","svi3","svi4")]


us_county_2018 <- us_county %>%
  left_join(
    state_summary_2018 %>%
      rename(GEOID = FIPS), # Rename FIPS to GEOID for joining
    by = "GEOID"
  ) %>%
  mutate(year = 2018)

# Process for 2020
us_county_2020 <- us_county %>%
  left_join(
    state_summary_2020 %>%
      rename(GEOID = FIPS), # Rename FIPS to GEOID for joining
    by = "GEOID"
  ) %>%
  mutate(year = 2020)

# Process for 2022
us_county_2022 <- us_county %>%
  left_join(
    state_summary_2022 %>%
      rename(GEOID = FIPS), # Rename FIPS to GEOID for joining
    by = "GEOID"
  ) %>%
  mutate(year = 2022)

#
svi_county_combined <- bind_rows(us_county_2022)
svi_county_combined <- svi_county_combined %>%
  filter(!is.na(svi))
svi_state <- svi_county_combined %>%
  group_by(STATEFP, year) %>%
  summarise(
    svi = mean(svi, na.rm = TRUE),
    svi1 = mean(svi1, na.rm = TRUE),
    svi2 = mean(svi2, na.rm = TRUE),
    svi3 = mean(svi3, na.rm = TRUE),
    svi4 = mean(svi4, na.rm = TRUE),
    .groups = "drop"
  )

svi_county_combined <- svi_county_combined %>% st_drop_geometry()
svi_state <- svi_state %>% st_drop_geometry()

write.csv(svi_county_combined, "./data/SVI/county_svi_2022.csv", row.names = FALSE)
write.csv(svi_state, "./data/SVI/svi_state.csv", row.names = FALSE)

###########EXPORT RESULT###############################################
# Create a new workbook
wb <- createWorkbook()

# Add worksheets for each year
addWorksheet(wb, "State Summary 2018")
addWorksheet(wb, "State Summary 2020")
addWorksheet(wb, "State Summary 2022")

# Write data to each worksheet
writeData(wb, "State Summary 2018", state_summary_2018)
writeData(wb, "State Summary 2020", state_summary_2020)
writeData(wb, "State Summary 2022", state_summary_2022)

# Add some basic formatting
# Style for headers
headerStyle <- createStyle(textDecoration = "bold", 
                           fgFill = "#E0E0E0")

# Apply header style to each worksheet
addStyle(wb, "State Summary 2018", headerStyle, rows = 1, cols = 1:ncol(state_summary_2018))
addStyle(wb, "State Summary 2020", headerStyle, rows = 1, cols = 1:ncol(state_summary_2020))
addStyle(wb, "State Summary 2022", headerStyle, rows = 1, cols = 1:ncol(state_summary_2022))

# Set column widths
setColWidths(wb, "State Summary 2018", cols = 1:ncol(state_summary_2018), widths = "auto")
setColWidths(wb, "State Summary 2020", cols = 1:ncol(state_summary_2020), widths = "auto")
setColWidths(wb, "State Summary 2022", cols = 1:ncol(state_summary_2022), widths = "auto")

saveWorkbook(wb, "./data/SVI/state_svi_summaries.xlsx", overwrite = TRUE)

#################### plot #######################################################
us_counties <- st_read("./data/Shapefile/cb_2018_us_county_500k.shp") %>%
  filter(!STATEFP %in% c("02", "15", "72", "78", "60", "66", "69"))  

svi_data <- read_csv("./data/SVI/county_svi_2022.csv") %>%
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0"))  # Ensure GEOID is 5-digit

us_svi_map <- us_counties %>%
  left_join(svi_data, by = c("GEOID" = "GEOID"))

us_svi_map <- us_counties %>%
  left_join(svi_data, by = "GEOID") %>%
  mutate(
    svi_quantile = case_when(
      is.na(svi) ~ "No Data",
      svi <= quantile(svi, 0.25, na.rm = TRUE) ~ "Low",
      svi <= quantile(svi, 0.5, na.rm = TRUE) ~ "Low-Medium",
      svi <= quantile(svi, 0.75, na.rm = TRUE) ~ "Medium-High",
      TRUE ~ "High"
    ),
    svi_quantile = factor(
      svi_quantile,
      levels = c("Low", "Low-Medium", "Medium-High", "High", "No Data")
    )
  )

# 4. Define matching fill colors
svi_colors <- c(
  "Low" = "#fdfdd0",         # pale yellow
  "Low-Medium" = "#c4e3c1",  # green
  "Medium-High" = "#88d0d6", # light teal
  "High" = "#5e91c7",        # blue
  "No Data" = "grey85"
)

# 4. Plot
ggplot(us_svi_map) +
  geom_sf(aes(fill = svi_quantile), color = "black", size = 0.1) +
  scale_fill_manual(
    values = svi_colors,
    name = "Level of Vulnerability",
    drop = FALSE
  ) +
  coord_sf(datum = NA) +  # remove graticules
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    #title = "Social Vulnerability Index (SVI) Across U.S. Counties",
    caption = "Source: CDC/ATSDR SVI & U.S. Census Bureau"
  )
ggsave(
  filename = "./figures/svi_map.png",
  width = 10, height = 7, dpi = 300
)
