# Associations between extreme weather exposure and U.S. food freight shipments

This repository contains the replication code and data for the manuscript *"Associations between extreme weather exposure and U.S. food freight shipments"*. Please direct questions to **Dr. Haoluan Wang** at haoluan.wang@miami.edu, who may pass them to the appropriate coauthor(s).

## Repository organization

- `code/`: all scripts, in one folder. Run them from the repository root.
  - `00`–`09`: main data construction, analysis, and figures, numbered in run order.
  - `R2_*`: scripts added in the second-round revision (supplementary tables and reproducibility checks).
- `data/`: raw inputs that are small enough to include, all intermediate files, and the analysis panels.
  - `data/FAF/`: Freight Analysis Framework metadata.
  - `data/Shapefile/`: county, state, and CFS area boundaries.
  - `data/Route/`: FAF zone lookup, routing points, the route–county table, and route geometries.
  - `data/Weather/`: daily county weather, temperature baselines, and route-level weather exposure.
  - `data/SVI/`: county and route-level Social Vulnerability Index.
  - `data/panel_*.rds`: the analysis panels (route × year).
- `figures/`: all figures produced by the code.
- `tables/`: regression tables (`tables/main/` and `tables/`) and diagnostics (`tables/diagnostics/`).

## Data

### FAF — Freight Analysis Framework, version 5.6.1 (`data/FAF/`)

- **Source:** U.S. DOT Bureau of Transportation Statistics and Federal Highway Administration, FAF5 regional database 2018–2023, https://www.bts.gov/faf.
- **Not included:** the data file itself (about 555 MB). To rebuild the panels, download it and save it as `data/FAF/FAF5.6.1_2018-2023.csv`.
- **Included:** `FAF5_metadata.xlsx`, the zone and code definitions.
- **Processing:** we keep domestic flows (`trade_type == 1`) of SCTG 01–07 carried by truck (`dms_mode == 1`), sum them to origin zone × destination zone × year, and keep zone pairs with positive truck tons.
- **Distance groups:** each route gets one time-invariant FAF distance band, taken from the truck records. When truck records for a pair report more than one band, we use the band that carries the most truck tons over 2018–2023; ties go to the longer band. Bands 1–3 are short (< 500 miles), 4–5 medium (500–999 miles), and 6–8 long (≥ 1,000 miles). In the sample this gives 17,328 / 22,884 / 32,826 route-years (2,888 / 3,814 / 5,471 routes). All distance-group results (Figures 2 and 4, Figures S2–S4 and S6, and the distance tables) use this definition.

### Shapefile (`data/Shapefile/`)

All files are from the U.S. Census Bureau:

| File | Description |
|---|---|
| `cb_2017_us_county_20m.*` | 2017 cartographic county boundaries |
| `cb_2018_us_state_5m.*` | 2018 cartographic state boundaries |
| `2017_CFS_Metro_Areas.*`, `2017_Remainder_of_States.*` | 2017 Commodity Flow Survey areas |
| `tl_2017_us_state.*` | 2017 TIGER state boundaries |

### Route (`data/Route/`)

| File | Content | Produced by |
|---|---|---|
| `faf_zone_lookup.rds` / `.csv` | FAF zone → representative county (or state) GEOID | `01_build_location_lookup.R` |
| `faf_zone_routing_points.csv` | Coordinates used to route each of the 132 zones. CFS metro and remainder-of-state zones: point-on-surface of the representative county. Whole-state zones: state centroid. | `R2_06_reproducibility_checks.R` |
| `od_route_county_segments_fullgeom.csv` | **Used by steps 4–6.** One row per route × county traversed, with the miles driven in each county (`distance`) and the route length (`total_distance`), measured on the FULL OSRM road line. Same-zone and Alaska/Hawaii routes (not routed) keep their rows from `od_route_county_segments.csv` | `R2_19_full_geometry_check.R`, `R2_20_build_fullgeom_route_files.R` |
| `route_conus_check_fullgeom.csv` | **The sample rule.** One row per panel route (12,373): road miles, miles outside the contiguous U.S. on the full road line, source, and `within_conus` (TRUE for the 12,173 sample routes) | `R2_20_build_fullgeom_route_files.R` |
| `full_geometry/` | Raw output of `R2_19` (per-route results; the `_progress/` checkpoints are not included) | `R2_19_full_geometry_check.R` |
| `od_route_county_segments.csv` | Earlier route table on OSRM's simplified route line (used in the submitted version; kept for the diagnostics) | `archive_route_generation_original_2025-10-11.R`; cleaned version in `02_generate_route_table.R` |
| `od_route_geometries_conus.gpkg` | Route lines from the original route script; only routes that lie entirely within the contiguous U.S. (11,938 routes) | original working scripts |
| `route_conus_check.csv` | Interim check (2026-11-08) on the stored simplified lines plus 432 re-requested routes (12,196 routes passed); superseded by `route_conus_check_fullgeom.csv` | `R2_16_check_routes_within_conus.R` |
| `od_route_geometries_rerouted.gpkg` | Route lines requested again from OSRM for the 432 routes without a line in `od_route_geometries_conus.gpkg` | `R2_16_check_routes_within_conus.R` (interim; not included in the repository, 127 MB; needed only to rerun `R2_18`) |
| `od_route_geometries_sample.gpkg` | Full road lines of the 12,173 sample routes (simplified to 200 m for mapping; the 129 same-zone routes are zero-length lines at the zone's routing point); used by all maps | `R2_17_sample_route_geometries.R` |

**How routes were generated:** Open Source Routing Machine (OSRM), called through `osrm::osrmRoute()` with the package defaults:
- server: `https://routing.openstreetmap.de/`;
- profile: `car`, so each route is the fastest path on the OpenStreetMap road network;
- geometry: the simplified route geometry is returned;
- date: routes were generated in October 2025.

Re-running `02_generate_route_table.R` queries the live server, so its results can differ slightly from the archived table. The script writes to a separate `_rerun` file and does not overwrite the archived one.

### Weather (`data/Weather/`)

- **Source:** gridMET (Abatzoglou 2013), https://www.climatologylab.org/gridmet.html, daily data at about 4 km resolution.
- **Not included:** the raw NetCDF files. To rebuild the weather data, save them under `data/Weather/gridmet_raw/<variable>/` for 2018–2023 and `data/Weather/gridmet_raw/history/<variable>/` for 1988–2017.

| File | Content | Produced by |
|---|---|---|
| `county_daily_{tmmx,tmmn,pr,vs}_2018_2023.rds` | Daily county values: area-weighted mean of grid cells (`exactextractr`); temperatures in °C | `03a_process_gridmet.R` |
| `county_baseline_tmmx_jja_1988_2017.csv`, `county_baseline_tmmn_djf_1988_2017.csv` | County mean and year-to-year SD of the 1988–2017 seasonal means | `03b_temperature_baselines.R` |
| `od_route_weather_exposure.rds` | Route × year counts of extreme-weather days. `avg*` is the simple mean over traversed counties (main specification); `distw*` is the mean weighted by miles driven in each county. | `04_route_weather_index.R` |

Extreme-weather definitions:

| Event | Definition |
|---|---|
| Extreme hot day | daily maximum temperature > baseline June–August mean + 3 SD |
| Extreme cold day | daily minimum temperature < baseline December–February mean − 3 SD |
| Heavy precipitation day | precipitation > 50 mm |
| High-wind day | wind speed > 10 m/s |

### SVI (`data/SVI/`)

- **Source:** CDC/ATSDR Social Vulnerability Index 2022, county level, https://www.atsdr.cdc.gov/place-health/php/svi/.

| File | Content | Produced by |
|---|---|---|
| `county_svi_2022.csv` | County overall percentile ranking (`RPL_THEMES`) | `00_prepare_svi_county.R` |
| `od_route_svi.rds` | Route-level SVI: simple mean (`avgSVI`) and distance-weighted mean (`distwSVI`) over traversed counties | `05_route_svi_index.R` |

### Analysis panels (`data/`)

| File | Content | Produced by |
|---|---|---|
| `panel_od_year_truck_sctg01_07.rds` | Main panel: 12,173 routes × 6 years (2018–2023) = 73,038 observations, balanced. Only routes whose full road line lies within the contiguous U.S. (`within_conus` in `route_conus_check_fullgeom.csv`) | `06_build_panel.R` |
| `panel_od_year_truck_sctg01_07_all_routes.rds` | The same panel before that restriction: 12,373 routes, 74,238 observations. The 200 extra routes start or end in Alaska or Hawaii (3) or run through Canada (197; 185–765 road miles outside the contiguous U.S.; their route length and exposure cover only the U.S. part). Used only for Table S10, column 2, and the sample diagnostics (`R2_06`, `R2_09`, `R2_13`, `R2_14`) | `06_build_panel.R` |
| `panel_od_year_truck_sctg01_04.rds` | Raw agricultural products (SCTG 01–04), route × year, same routes as the main panel | `R2_04_sctg_panel_regression.R` |
| `panel_od_year_truck_sctg05_07.rds` | Processed food (SCTG 05–07), route × year, same routes as the main panel | `R2_04_sctg_panel_regression.R` |

## Code and run order

| Step | Script | Needs raw data? | Output |
|---|---|---|---|
| 0 | `00_prepare_svi_county.R` | SVI geodatabases | `data/SVI/county_svi_2022.csv` |
| 1 | `01_build_location_lookup.R` | — | `data/Route/faf_zone_lookup.rds` |
| 2 | `02_generate_route_table.R` | FAF CSV; internet access | `data/Route/od_route_county_segments_rerun.csv` |
| 3a | `03a_process_gridmet.R` | gridMET NetCDF | `data/Weather/county_daily_*.rds` |
| 3b | `03b_temperature_baselines.R` | gridMET NetCDF for 1988–2017 | `data/Weather/county_baseline_*.csv` |
| 4 | `04_route_weather_index.R` | — | `data/Weather/od_route_weather_exposure.rds` |
| 5 | `05_route_svi_index.R` | — | `data/SVI/od_route_svi.rds` |
| 6 | `06_build_panel.R` | FAF CSV | `data/panel_od_year_truck_sctg01_07.rds` |
| 7a | `07a_main_regression_tables.R` | — | `tables/main/` |
| 7b | `07b_regression_coefficients.R` | — | `tables/main/` (coefficient files for Figures 4–5) |
| 8 | `08_main_figures.R` | — (run after 7b) | `figures/figure1/` to `figure5/`, `figures/supplementary/FigureS5–S7` |
| 9 | `09_supplementary_maps.R` | — | `figures/supplementary/FigureS2–S4` |
| R2 | `R2_02` … `R2_06`, `R2_11`, `R2_12` | — | `tables/`, `tables/diagnostics/`, `figures/supplementary/FigureS1_FAF_Zones.png` |
| R2 | `R2_19_full_geometry_check.R` | internet access (OSRM; about 4 hours) | `data/Route/full_geometry/`; full route lines outside the repository (`FULL_LINES_DIR`) (run before step 4) |
| R2 | `R2_20_build_fullgeom_route_files.R` | — | `data/Route/od_route_county_segments_fullgeom.csv`, `route_conus_check_fullgeom.csv` (run before step 4) |
| R2 | `R2_16_check_routes_within_conus.R` | internet access (OSRM) | `data/Route/route_conus_check.csv`, `od_route_geometries_rerouted.gpkg` (interim check, superseded by R2_19/R2_20) |
| R2 | `R2_17_sample_route_geometries.R` | full route lines from R2_19 | `data/Route/od_route_geometries_sample.gpkg` (run before step 8) |
| R2 | `R2_18_why_258_routes_missing.R` | internet access (OSRM) | `tables/diagnostics/18_why_258_routes_missing.csv`, `_log.txt` (diagnostic only) |
| R2 | `R2_15_supplementary_tables.R` | — | `tables/supplementary/` (Tables S3–S8 in the supplement's layout) |
| R2 | `R2_01_faf_zone_route_count.R` | FAF CSV | `tables/supplementary/TableS1_sctg_composition.csv` |
| R2 | `R2_21_route_sample_summary.R` | — (run after R2_15) | `tables/route_sample_summary.xlsx`: route sample and the routes and observations behind every table and figure |

All intermediate files from steps 0–6 are included in the repository. Steps 7 onward therefore run without the raw FAF and gridMET files or internet access. Running 4, 5, 6, R2_04, 7a, 7b, 8, 9, R2_02, R2_03, R2_05, R2_06, R2_11, R2_12, R2_15 and R2_21 from the repository root regenerates every file in `figures/` and `tables/` except Table S1, which needs the raw FAF file (`R2_01`; the path can be set with the `FAF_CSV` environment variable). Steps 6 and R2_04 also need the FAF CSV; R2_17 needs the full route lines from R2_19, which are not in the repository because of their size. The log of the latest full run is in `tables/diagnostics/run_log_2026-11-11_fullgeom.txt`.

## Figures and tables

| Manuscript item | File | Script | Data |
|---|---|---|---|
| Figure 1 | `figures/figure1/figure1.png` | `08_main_figures.R` | main panel; `od_route_geometries_sample.gpkg` (routes coloured by the regression SVI quartile) |
| Figure 2 (a–d tons, e–h ton-miles) | `figures/figure2/coef_plot_tons_byVAR.png`, `coef_plot_tmiles_byVAR.png` | `08_main_figures.R` | main panel; FAF truck distance band |
| Figure 3 | `figures/figure3/coef_svi_tons_byVAR.png`, `coef_svi_tonmiles_byVAR.png` | `08_main_figures.R` | main panel; SVI quartiles |
| Figure 4 | `figures/figure4/Share_Tons_Dist_byVAR.png`, `Share_TMiles_Dist_byVAR.png` | `07b` → `08` | main panel; FAF truck distance band |
| Figure 5 | `figures/figure5/Share_Tons_SVI_byVAR.png`, `Share_TMiles_SVI_byVAR.png` | `07b` → `08` | main panel; SVI quartiles |
| Figure S1 | `figures/supplementary/FigureS1_FAF_Zones.png` | `R2_03_faf_zone_map.R` | CFS shapefiles; main panel |
| Figures S2–S4 | `figures/supplementary/FigureS2_Short.png`, `FigureS3_Medium.png`, `FigureS4_Long.png` | `09_supplementary_maps.R` | `od_route_geometries_sample.gpkg`; main panel |
| Figure S5 | `figures/supplementary/FigureS5_Trends_Overall.png` | `08_main_figures.R` | main panel |
| Figures S6–S7 | `figures/supplementary/FigureS6_Trends_Distance.png`, `FigureS7_Trends_SVI.png` | `08_main_figures.R` | main panel |
| Table S1 | `tables/supplementary/TableS1_sctg_composition.csv` | `R2_01_faf_zone_route_count.R` | raw FAF CSV |
| Table S2 | `tables/TableS2_summary_stats.csv` | `R2_05_summary_statistics.R` | main and SCTG panels |
| Tables S3–S8 | `tables/supplementary/TableS3_*.csv` … `TableS8_*.csv`; all in `supplementary_tables.md` | `R2_15_supplementary_tables.R` | main and SCTG panels |

Tables S4 and S5 are also written in `fixest` format by `R2_02` (`tables/TableS4_OD_FE_*.txt`, with an extra column using both fixed effects) and `R2_04` (`tables/TableS5_SCTG_*.txt`). Table S9 (data sources) and Table S10 (data procedures) are text only.

Results for the alternative specifications are in sibling folders of `tables/main/results_3sd/avg_weighted/`:
- `dist_weighted/`: distance-weighted exposure;
- `results_5c/`: a +5 °C temperature threshold.

## Software

R 4.5.2 with the following packages: fixest 0.13.2, sf 1.0.23, osrm 4.2.0, exactextractr 0.10.1, terra 1.8.86, data.table 1.17.8, dplyr 1.1.4, tidyr 1.3.1, stringr 1.6.0, readr 2.1.6, readxl 1.4.5, ggplot2 4.0.1, patchwork 1.3.2, broom, tigris 2.2.1, units 1.0.0, purrr 1.2.0.
