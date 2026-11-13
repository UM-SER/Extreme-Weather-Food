# ==============================================================================
# R2_15_supplementary_tables.R
# Purpose: All regression tables of the Supplementary Information in the layout
#          of the supplement (tons and ton-miles side by side, or stacked panels):
#            Table S3  volume by route distance (FAF truck distance band)
#            Table S4  origin-by-year and destination-by-year fixed effects
#            Table S5  by food product (SCTG 1-4 vs SCTG 5-7)
#            Table S6  volume by route-level SVI
#            Table S7  truck share by route distance
#            Table S8  truck share by route-level SVI
#            Table S10 route-construction robustness (new, R2)
#            Table S11 FAF distance class vs OSRM route length (new, R2)
#          Stars: * p<0.05, ** p<0.01, *** p<0.001 (as stated in the table notes).
# Inputs:  data/panel_od_year_truck_sctg01_07.rds, data/panel_od_year_truck_sctg01_04.rds,
#          data/panel_od_year_truck_sctg05_07.rds, data/Route/od_route_geometries_conus.gpkg
# Outputs: tables/supplementary/TableS*.csv and supplementary_tables.md (all tables, with notes)
# Run from the repository root.
# ==============================================================================
suppressMessages({library(dplyr); library(fixest); library(sf); library(stringr)})

OUT <- "./tables/supplementary"
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

panel <- readRDS("./data/panel_od_year_truck_sctg01_07.rds") %>%
  mutate(share_tons_pct = truck_share_tons * 100, share_tmiles_pct = truck_share_tmiles * 100)
rawag <- readRDS("./data/panel_od_year_truck_sctg01_04.rds")
proc  <- readRDS("./data/panel_od_year_truck_sctg05_07.rds")

avg_vars   <- c("avgHighTempFreq_3sd", "avgLowTempFreq_3sd", "avgHighPrecFreq", "avgHighVsFreq")
distw_vars <- c("distwHighTempFreq_3sd", "distwLowTempFreq_3sd", "distwHighPrecFreq", "distwHighVsFreq")
row_labels <- c("Extreme hot days", "Extreme cold days", "Heavy precipitation days", "High wind days")
dist_groups <- c("Full sample" = NA, "0–500 miles" = "Short", "500–1,000 miles" = "Medium", ">1,000 miles" = "Long")
svi_groups  <- c("Low SVI" = "Low", "Low-Medium SVI" = "Low-Medium", "Medium-High SVI" = "Medium-High", "High SVI" = "High")

fit <- function(y, d, x = avg_vars, fe = "unit + year")
  feols(as.formula(paste(y, "~", paste(x, collapse = " + "), "|", fe)), cluster = ~unit, data = d)
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

# One column of a table: estimate (with stars) and SE rows for the four weather variables
column <- function(m, x = avg_vars, digits = 3) {
  ct <- coeftable(m)[x, , drop = FALSE]
  fmt <- function(v) formatC(v, format = "f", digits = digits)
  out <- as.vector(rbind(paste0(fmt(ct[, 1]), stars(ct[, 4])), paste0("(", fmt(ct[, 2]), ")")))
  c(out, format(nobs(m), big.mark = ","))
}
# Assemble a table from a named list of models; fe_rows is a named list of Yes/No vectors
build <- function(models, x_list = NULL, digits = 3, fe_rows = NULL) {
  if (is.null(x_list)) x_list <- rep(list(avg_vars), length(models))
  cols <- mapply(function(m, x) column(m, x, digits), models, x_list)
  if (is.null(dim(cols))) cols <- matrix(cols, ncol = 1)
  n <- nrow(cols)
  var_col <- c(as.vector(rbind(row_labels, "")), "Observations")
  tab <- data.frame(Variable = var_col, cols[seq_len(n), , drop = FALSE], check.names = FALSE)
  names(tab)[-1] <- names(models)
  if (!is.null(fe_rows)) {
    fe <- do.call(rbind, lapply(names(fe_rows), function(r) c(r, fe_rows[[r]])))
    colnames(fe) <- names(tab)
    tab <- rbind(tab[-n, ], as.data.frame(fe, check.names = FALSE), tab[n, ])
  }
  rownames(tab) <- NULL
  tab
}
std_fe <- function(k) list("Route fixed effects" = rep("Yes", k), "Year fixed effects" = rep("Yes", k))
# Stack two outcome panels (A tons, B ton-miles) with the same columns
stack <- function(a, b, label_a = "Panel A. Dependent variable: Tons", label_b = "Panel B. Dependent variable: Ton-miles") {
  blank <- function(lbl, tab) { r <- tab[1, ]; r[] <- ""; r$Variable <- lbl; r }
  core_b <- b[!b$Variable %in% c(names(std_fe(1)), "Origin-by-year fixed effects", "Destination-by-year fixed effects", "Observations"), ]
  fe_b   <- b[ b$Variable %in% c(names(std_fe(1)), "Origin-by-year fixed effects", "Destination-by-year fixed effects", "Observations"), ]
  core_a <- a[!a$Variable %in% fe_b$Variable, ]
  rbind(blank(label_a, a), core_a, blank(label_b, b), core_b, fe_b)
}

md_table <- function(tab) {
  esc <- function(v) gsub("\\*", "\\\\*", v)
  hdr <- paste0("| ", paste(names(tab), collapse = " | "), " |")
  sep <- paste0("|", paste(c(":---", rep(":---:", ncol(tab) - 1)), collapse = "|"), "|")
  body <- apply(tab, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |"))
  c(hdr, sep, body)
}
md <- character()
emit <- function(id, title, tab, note) {
  write.csv(tab, file.path(OUT, paste0(id, ".csv")), row.names = FALSE)
  md <<- c(md, paste0("**", title, "**"), "", md_table(tab), "", paste0("*Note:* ", note), "", "")
  message("Saved ", id)
}
note_se    <- "Standard errors clustered at the route level in parentheses. * p < 0.05, ** p < 0.01, *** p < 0.001."
note_share <- paste("Coefficients represent changes in percentage points.", note_se)
note_dist  <- paste("Distance groups follow the FAF5 distance band of truck shipments (bands 1–3, 4–5 and 6–8;",
                    "the band with the largest truck tonnage when an origin–destination pair has more than one).")

# --- Table S3: volume by distance ---------------------------------------------
sub_dist <- function(g) if (is.na(g)) panel else panel[panel$dist_cat == g, ]
ms <- list()
for (g in names(dist_groups)) {
  ms[[paste(g, "— Tons")]]      <- fit("total_tons",   sub_dist(dist_groups[[g]]))
  ms[[paste(g, "— Ton-miles")]] <- fit("total_tmiles", sub_dist(dist_groups[[g]]))
}
emit("TableS3_volume_by_distance",
     "Table S3. Coefficient estimates of extreme weather on truck-based food freight outcomes by route distance",
     build(ms, fe_rows = std_fe(length(ms))), paste(note_dist, note_se))

# --- Table S4: origin-by-year and destination-by-year fixed effects ------------
od_models <- function(y) list(
  "Model I"   = fit(y, panel),
  "Model II"  = fit(y, panel, fe = "unit + dms_orig^year"),
  "Model III" = fit(y, panel, fe = "unit + dms_dest^year"))
od_fe <- list("Route fixed effects" = c("Yes", "Yes", "Yes"), "Year fixed effects" = c("Yes", "No", "No"),
              "Origin-by-year fixed effects" = c("No", "Yes", "No"),
              "Destination-by-year fixed effects" = c("No", "No", "Yes"))
m_od_tons <- od_models("total_tons")
dropped <- setdiff(seq_len(nrow(panel)), obs(m_od_tons[["Model II"]]))
message("Table S4: observations dropped in Model II: ", length(dropped), "; routes: ",
        paste(unique(panel$unit[dropped]), collapse = ", "))
emit("TableS4_origin_destination_year_FE",
     "Table S4. Robustness checks on coefficient estimates of extreme weather on truck-based food freight outcomes",
     stack(build(m_od_tons, fe_rows = od_fe), build(od_models("total_tmiles"), fe_rows = od_fe)),
     paste("Model II replaces the year fixed effects with origin-by-year fixed effects and Model III with",
           "destination-by-year fixed effects. Models II and III drop", length(dropped), "observations of",
           length(unique(panel$unit[dropped])), "routes whose origin (destination) zone has no other route in a year,",
           "so that the origin-by-year (destination-by-year) fixed effect fits them perfectly.", note_se))

# --- Table S5: by food product -------------------------------------------------
prod_models <- function(y) list("Full sample" = fit(y, panel), "Raw agricultural food (SCTG 1–4)" = fit(y, rawag),
                                "Processed food (SCTG 5–7)" = fit(y, proc))
emit("TableS5_by_food_product",
     "Table S5. Coefficient estimates of extreme weather on truck-based food freight outcomes by food product",
     stack(build(prod_models("total_tons"), fe_rows = std_fe(3)), build(prod_models("total_tmiles"), fe_rows = std_fe(3))),
     note_se)

# --- Table S6: volume by SVI ---------------------------------------------------
ms <- list()
for (g in names(svi_groups)) {
  d <- panel[panel$svi_cat == svi_groups[[g]], ]
  ms[[paste(g, "— Tons")]]      <- fit("total_tons", d)
  ms[[paste(g, "— Ton-miles")]] <- fit("total_tmiles", d)
}
emit("TableS6_volume_by_svi",
     "Table S6. Coefficient estimates of extreme weather on truck-based food freight outcomes by route-level social vulnerability",
     build(ms, fe_rows = std_fe(length(ms))), note_se)

# --- Table S7: truck share by distance -----------------------------------------
ms <- list()
for (g in names(dist_groups)) {
  ms[[paste(g, "— Tons share")]]      <- fit("share_tons_pct",   sub_dist(dist_groups[[g]]))
  ms[[paste(g, "— Ton-miles share")]] <- fit("share_tmiles_pct", sub_dist(dist_groups[[g]]))
}
emit("TableS7_truck_share_by_distance",
     "Table S7. Coefficient estimates of extreme weather on the percentage of food freight via truck by route distance",
     build(ms, digits = 4, fe_rows = std_fe(length(ms))), paste(note_dist, note_share))

# --- Table S8: truck share by SVI ----------------------------------------------
ms <- list()
for (g in names(svi_groups)) {
  d <- panel[panel$svi_cat == svi_groups[[g]], ]
  ms[[paste(g, "— Tons share")]]      <- fit("share_tons_pct", d)
  ms[[paste(g, "— Ton-miles share")]] <- fit("share_tmiles_pct", d)
}
emit("TableS8_truck_share_by_svi",
     "Table S8. Coefficient estimates of extreme weather on the percentage of food freight via truck by route-level social vulnerability",
     build(ms, digits = 4, fe_rows = std_fe(length(ms))), note_share)

# --- Table S10 (new): route-construction robustness ----------------------------
map_units <- st_read("./data/Route/od_route_geometries_conus.gpkg", quiet = TRUE) %>% st_drop_geometry() %>%
  transmute(unit = paste0(str_pad(dms_orig_GEOID, 5, "left", "0"), str_pad(dms_dest_GEOID, 5, "left", "0"))) %>%
  pull(unit)
mapped <- panel[panel$unit %in% map_units, ]
inter  <- panel[panel$dms_orig_GEOID != panel$dms_dest_GEOID, ]
rob_models <- function(y) list("Baseline" = fit(y, panel), "Mapped routes only" = fit(y, mapped),
                               "Excluding intra-zone routes" = fit(y, inter),
                               "Distance-weighted exposure" = fit(y, panel, x = distw_vars))
rob_x <- list(avg_vars, avg_vars, avg_vars, distw_vars)
emit("TableS10_route_construction_robustness",
     "Table S10. Robustness of the main estimates to route construction and exposure aggregation",
     stack(build(rob_models("total_tons"), rob_x, fe_rows = std_fe(4)),
           build(rob_models("total_tmiles"), rob_x, fe_rows = std_fe(4))),
     paste("Column 2 keeps the", format(length(unique(mapped$unit)), big.mark = ","), "routes whose path lies entirely within",
           "the contiguous United States (the routes shown in Figure 1a and Figure S1). Column 3 drops the",
           length(unique(panel$unit[panel$dms_orig_GEOID == panel$dms_dest_GEOID])), "routes whose origin and destination are",
           "the same FAF zone. Column 4 replaces the simple average across traversed counties with the average weighted by",
           "miles traveled in each county.", note_se))

# --- Table S11 (new): FAF distance class vs OSRM route length ------------------
routes <- panel %>% distinct(unit, dms_orig_GEOID, dms_dest_GEOID, dist_cat, total_distance) %>%
  mutate(intra = dms_orig_GEOID == dms_dest_GEOID,
         osrm = cut(total_distance, c(-Inf, 500, 1000, Inf), right = FALSE,
                    labels = c("< 500 miles", "500–999 miles", "≥ 1,000 miles")))
xt <- as.data.frame.matrix(table(factor(routes$dist_cat[!routes$intra], c("Short", "Medium", "Long")),
                                 routes$osrm[!routes$intra]))
agree <- mean(as.character(routes$dist_cat[!routes$intra]) ==
                c("Short", "Medium", "Long")[as.integer(routes$osrm[!routes$intra])])
tab <- data.frame(`FAF distance class` = c("Short (bands 1–3)", "Medium (bands 4–5)", "Long (bands 6–8)"),
                  xt, check.names = FALSE)
tab[] <- lapply(tab, function(v) if (is.numeric(v)) format(v, big.mark = ",") else v)
emit("TableS11_faf_class_vs_osrm_length",
     "Table S11. Route distance class from FAF5 versus OSRM route length (number of inter-zone routes)",
     tab,
     paste0("Inter-zone routes only (", format(sum(!routes$intra), big.mark = ","), "). The FAF class and the OSRM length class agree for ",
            sprintf("%.1f", 100 * agree), "% of routes. All ", sum(routes$intra),
            " intra-zone routes are in the FAF short-distance class."))

writeLines(md, file.path(OUT, "supplementary_tables.md"), useBytes = TRUE)
message("Done: ", OUT)
