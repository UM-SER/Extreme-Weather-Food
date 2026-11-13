# ============================================================
# County-level JJA climatology (mean & sd) from GridMET
# Baseline: 1988-2017
# Variables: tmmx (Tmax) or tmmn (Tmin)
# County shapefile: cb_2023_us_county_500k.shp
# ============================================================

library(terra)
library(data.table)
library(sf)
library(raster)
library(exactextractr)
library(haven)


# ---- 1. Set paths ----
root <- "./data/Weather/gridmet_raw/history"
dir_tmmx <- file.path(root, "tmmx")
dir_tmmn <- file.path(root, "tmmn")
shp_path <- "./data/Shapefile/cb_2023_us_county_500k.shp"
outdir   <- "./data/Weather"

years_base <- 1988:2017  # 30-year baseline

# ---- 2. Load county polygons ----
counties <- vect(shp_path)
counties$GEOID <- as.character(counties$GEOID)  # ensure string ID

# ---- 3. Helper: compute JJA mean raster ----
compute_JJA_mean <- function(file) {
  yr <- as.numeric(gsub(".*_(\\d{4})\\.nc$", "\\1", file))
  r <- rast(file)
  n <- nlyr(r)
  d0 <- as.Date(paste0(yr, "-01-01"))
  d  <- seq.Date(d0, by = "day", length.out = n)
  jja_idx <- which(as.integer(format(d, "%m")) %in% c(6, 7, 8))
  jja <- r[[jja_idx]]
  app(jja, mean, na.rm = TRUE)
}

# ---- 4. Loop through years to extract county means ----
extract_JJA_by_county <- function(dir_nc, varname) {
  res_list <- vector("list", length(years_base))
  
  for (i in seq_along(years_base)) {
    yr <- years_base[i]
    ncfile <- file.path(dir_nc, paste0(varname, "_", yr, ".nc"))
    if (!file.exists(ncfile)) next
    
    message("Processing ", varname, " ", yr, " ...")
    rJ <- compute_JJA_mean(ncfile)
    
    # Extract mean temperature per county (area-weighted mean)
    vals <- terra::extract(rJ, counties, fun = mean, na.rm = TRUE)
    vals$GEOID <- counties$GEOID
    vals$year  <- yr
    vals$tempC <- vals[[2]] - 273.15  # Kelvin → Celsius
    res_list[[i]] <- vals[, c("GEOID", "year", "tempC")]
  }
  
  dt <- rbindlist(res_list)
  # remove any NA values
  dt <- dt[!is.na(tempC)]
  
  # compute baseline mean & sd for each county
  base_stats <- dt[, .(
    mean_JJA = mean(tempC, na.rm = TRUE),
    sd_JJA   = sd(tempC,   na.rm = TRUE)
  ), by = GEOID]
  
  # save results
  fwrite(base_stats, file.path(outdir, paste0("county_baseline_", varname, "_jja_1988_2017.csv")))
  return(base_stats)
}

# ---- 4.1 extract_winter_mean
compute_DJF_mean <- function(year, dir_nc, varname, counties) {
  # Initialize
  layers <- list()
  
  # (1) December of previous year
  file_prev <- file.path(dir_nc, paste0(varname, "_", year - 1, ".nc"))
  if (file.exists(file_prev)) {
    r_prev <- rast(file_prev)
    d_prev <- seq(as.Date(paste0(year - 1, "-01-01")),
                  as.Date(paste0(year - 1, "-12-31")), by = "day")
    dec_idx <- which(format(d_prev, "%m") == "12")
    if (length(dec_idx) > 0) layers <- c(layers, list(r_prev[[dec_idx]]))
    rm(r_prev); gc()
  }
  
  # (2) January–February of current year
  file_curr <- file.path(dir_nc, paste0(varname, "_", year, ".nc"))
  if (file.exists(file_curr)) {
    r_curr <- rast(file_curr)
    d_curr <- seq(as.Date(paste0(year, "-01-01")),
                  as.Date(paste0(year, "-12-31")), by = "day")
    jf_idx <- which(format(d_curr, "%m") %in% c("01", "02"))
    if (length(jf_idx) > 0) layers <- c(layers, list(r_curr[[jf_idx]]))
    rm(r_curr); gc()
  }
  
  # Nothing to process?
  if (length(layers) == 0) return(NULL)
  
  # (3) Combine all DJF layers as one SpatRaster
  r_djf <- do.call(c, layers)
  rm(layers); gc()
  
  # (4) Mean temperature over DJF days
  r_mean <- app(r_djf, fun = mean, na.rm = TRUE)
  rm(r_djf); gc()
  
  # (5) County means (Kelvin → Celsius)
  vals <- terra::extract(r_mean, counties, fun = mean, na.rm = TRUE)
  vals$GEOID <- counties$GEOID
  vals$year  <- year
  vals$tempC <- vals[[2]] - 273.15
  
  rm(r_mean); gc()
  
  data.table(GEOID = vals$GEOID, year = vals$year, tempC = vals$tempC)
}

extract_DJF_by_county <- function(dir_nc, varname, counties, years_base, outdir) {
  res_list <- vector("list", length(years_base))
  
  for (i in seq_along(years_base)) {
    yr <- years_base[i]
    message("Processing ", varname, " DJF ", yr, " ...")
    res <- compute_DJF_mean(yr, dir_nc, varname, counties)
    if (!is.null(res)) res_list[[i]] <- res
  }
  
  dt <- rbindlist(res_list, fill = TRUE)
  dt <- dt[!is.na(tempC)]
  
  base_stats <- dt[, .(
    mean_DJF = mean(tempC, na.rm = TRUE),
    sd_DJF   = sd(tempC,   na.rm = TRUE)
  ), by = GEOID]
  
  fwrite(base_stats, file.path(outdir, paste0("county_baseline_", varname, "_djf_1988_2017.csv")))
  base_stats
}

# ---- 5. Run for  Tmax ----
tmmx_jja_stats <- extract_JJA_by_county(dir_tmmx, "tmmx")
tmmx_jja_stats[, threshold_hot := mean_JJA + 3 * sd_JJA]

# ---- 5.1 Run for  tmin ----
tmmn_djf_stats <- extract_DJF_by_county(
  dir_nc     = dir_tmmn,
  varname    = "tmmn",
  counties   = counties,
  years_base = years_base,
  outdir     = outdir
)
tmmn_djf_stats[, threshold_cold := mean_DJF - 3 * sd_DJF]


# ---- 6.1 calculate tmmx threshold ----
tmmx_jja_stats <- read.csv(
  "./data/Weather/county_baseline_tmmx_jja_1988_2017.csv",
  stringsAsFactors = FALSE
)
setDT(tmmx_jja_stats) 
tmmx_jja_stats$GEOID <- sprintf("%05d", as.integer(tmmx_jja_stats$GEOID))

rm(tmmn_all,tmmx_all)
tmmx_18_23 <- read_dta("./data/Weather/all_tmmx_2018_2023.dta")
tmmx_18_23 <- merge(tmmx_18_23, 
                    tmmx_jja_stats[, .(GEOID, threshold_hot)], 
                    by = "GEOID", 
                    all.x = TRUE)
setDT(tmmx_18_23)
tmmx_extreme <- tmmx_18_23[temp > threshold_hot]
tmmx_extreme[, date := as.IDate(date)]
tmmx_extreme[, year := year(date)]

tmmx_above_threshold <- tmmx_extreme[, .(
  n_days = .N
), by = .(GEOID, year)]
tmmx_above_threshold[, STATEFP := substr(GEOID, 1, 2)]
state_avg <- tmmx_above_threshold[
  , .(n_days = mean(n_days, na.rm = TRUE)),
  by = .(STATEFP, year)
]
state_avg[, GEOID := STATEFP]
state_avg[, STATEFP := NULL]

tmmx_above_threshold <- rbind(tmmx_above_threshold[, .(GEOID, year, n_days)],
                       state_avg[, .(GEOID, year, n_days)],
                       use.names = TRUE, fill = TRUE)
rm(state_avg)
# ---- 6.2 calculate tmmn threshold ----
tmmn_djf_stats <- read.csv(
  "./data/Weather/county_baseline_tmmn_djf_1988_2017.csv",
  stringsAsFactors = FALSE
)
setDT(tmmn_djf_stats) 
tmmn_djf_stats$GEOID <- sprintf("%05d", as.integer(tmmn_djf_stats$GEOID))


tmmn_18_23 <- read_dta("./data/Weather/all_tmmn_2018_2023.dta")
tmmn_18_23 <- merge(tmmn_18_23, 
                    tmmn_djf_stats[, .(GEOID, threshold_cold)], 
                    by = "GEOID", 
                    all.x = TRUE)
setDT(tmmn_18_23)
tmmn_extreme <- tmmn_18_23[temp < threshold_cold]
tmmn_extreme[, date := as.IDate(date)]
tmmn_extreme[, year := year(date)]

tmmn_below_threshold <- tmmn_extreme[, .(
  n_days = .N
), by = .(GEOID, year)]

tmmn_below_threshold[, STATEFP := substr(GEOID, 1, 2)]
state_avg <- tmmn_below_threshold[
  , .(n_days = mean(n_days, na.rm = TRUE)),
  by = .(STATEFP, year)
]
state_avg[, GEOID := STATEFP]
state_avg[, STATEFP := NULL]

tmmn_below_threshold <- rbind(tmmn_below_threshold[, .(GEOID, year, n_days)],
                              state_avg[, .(GEOID, year, n_days)],
                              use.names = TRUE, fill = TRUE)
rm(state_avg)

# ---- 7.1 visulization tmmx ----
county_shp <- st_read(shp_path)
county_shp <- county_shp %>%
  mutate(state_fips = substr(GEOID, 1, 2))

county_shp <- county_shp %>%
  filter(!state_fips %in% c("02", "15", "60", "66", "69", "72", "78"))

county_shp <- county_shp %>%
  left_join(tmmx_jja_stats, by = "GEOID")
county_shp <- county_shp %>%
  left_join(tmmn_djf_stats, by = "GEOID")

p_hot <- ggplot(county_shp) +
  geom_sf(aes(fill = threshold_hot), color = "white", size = 0.05) +
  scale_fill_gradientn(
    colors = c("#FFF7BC", "#FEC44F", "#FE9929", "#D95F0E", "#993404"),
    name = "Hot Threshold (°C)"
  ) +
  labs(
    title = "Extreme Heat Threshold (Jun-Jul-Aug)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
ggsave(
  filename = "./figures/threshold_hot_map.png",
  plot = p_hot,
  width = 10,
  height = 6,
  dpi = 300
)

p_cold <- ggplot(county_shp) +
  geom_sf(aes(fill = threshold_cold), color = "white", size = 0.05) +
  scale_fill_gradientn(
    colors = c("#08306B", "#2171B5", "#6BAED6", "#C6DBEF", "#F7FBFF"),
    name = "Cold Threshold (°C)"
  ) +
  labs(
    title = "Extreme Cold Threshold (Dec-Jan-Feb)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
ggsave(
  filename = "./figures/threshold_cold_map.png",
  plot = p_cold,
  width = 10,
  height = 6,
  dpi = 300
)
# ============================================================