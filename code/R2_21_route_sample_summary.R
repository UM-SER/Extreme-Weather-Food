# ==============================================================================
# R2_21_route_sample_summary.R
# Purpose: One file that states the route sample and the number of routes and
#          observations behind every table and figure. Counts are read from the
#          route files, the panels and the saved tables, and checked against each
#          other (the script stops if they disagree).
# Output:  tables/route_sample_summary.xlsx
#   Sheet "Route sample"      how the 12,373 FAF truck routes split into sample / excluded
#   Sheet "Tables & figures"  routes and observations behind each table and figure
#   Sheet "Sample breakdown"  routes and observations by distance group, SVI quartile, year
#   Sheet "Version history"   route counts reported in earlier versions and why they changed
# Optional: COMPARE_DIR = folder with an earlier tables/ copy; adds sheet
#   "Changes vs previous" listing coefficients whose significance stars changed.
# Run after R2_15.
# ==============================================================================
suppressMessages({library(data.table); library(openxlsx)})
setDTthreads(1)
ROOT <- "."
YRS  <- 6

chk  <- fread(file.path(ROOT, "data/Route/route_conus_check_fullgeom.csv"), colClasses = list(character = "unit"))
p    <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07.rds")))
pall <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_07_all_routes.rds")))
p14  <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg01_04.rds")))
p57  <- as.data.table(readRDS(file.path(ROOT, "data/panel_od_year_truck_sctg05_07.rds")))
gl   <- sf::st_read(file.path(ROOT, "data/Route/od_route_geometries_sample.gpkg"), quiet = TRUE)

n_routed <- chk[source == "routed (full road line)" & within_conus == TRUE, .N]
n_intra  <- chk[source == "intra-zone (not routed)" & within_conus == TRUE, .N]
n_canada <- chk[source == "routed (full road line)" & within_conus == FALSE, .N]
n_akhi   <- chk[source == "Alaska or Hawaii", .N]
N        <- uniqueN(p$unit)
out_rng  <- chk[source == "routed (full road line)" & within_conus == FALSE, range(outside_miles)]
stopifnot(N == n_routed + n_intra, nrow(p) == N * YRS, uniqueN(pall$unit) == N + n_canada + n_akhi,
          nrow(gl) == N, setequal(gl$unit, p$unit), all(p[, .N, by = unit]$N == YRS))

# --- Sheet 1: route sample ----------------------------------------------------------
s1 <- data.table(
  Group = c("Routed with OSRM, full road line within the contiguous U.S.",
            "Same-zone routes (origin and destination in one FAF zone; not routed)",
            "MAIN SAMPLE",
            "Excluded: full road line runs through Canada",
            "Excluded: Alaska or Hawaii endpoint",
            "ALL FAF domestic truck routes, SCTG 1-7"),
  Routes = c(n_routed, n_intra, N, n_canada, n_akhi, uniqueN(pall$unit)))
s1[, Observations := Routes * YRS]
s1[, `Road miles outside the contiguous U.S.` := c("0", "-", "0", sprintf("%.0f to %.0f", out_rng[1], out_rng[2]), "not routed", "")]

# --- Sheet 2: tables and figures --------------------------------------------------
obs_row <- function(f) {                       # Observations row of a saved table (csv)
  x <- fread(file.path(ROOT, f), header = TRUE, colClasses = "character")
  v <- unlist(x[x[[1]] == "Observations", -1, with = FALSE])
  as.integer(gsub(",", "", v[nzchar(v)]))
}
dist_n <- p[, .N, by = dist_cat]; svi_n <- p[, .N, by = svi_cat]
nd <- function(g) dist_n[dist_cat == g, N]; ns <- function(g) svi_n[svi_cat == g, N]
row <- function(item, content, sample, obs, note = "") data.table(Item = item, Content = content, Sample = sample,
                                                                   Routes = obs / YRS, Observations = obs, Note = note)
s3 <- obs_row("tables/supplementary/TableS3_volume_by_distance.csv")
s5 <- obs_row("tables/supplementary/TableS5_by_food_product.csv")
s6 <- obs_row("tables/supplementary/TableS6_volume_by_svi.csv")
s10 <- obs_row("tables/supplementary/TableS10_route_construction_robustness.csv")
stopifnot(s3 == c(nrow(p), nrow(p), rep(c(nd("Short"), nd("Medium"), nd("Long")), each = 2)),
          s5 == c(nrow(p), nrow(p14), nrow(p57)),
          s6 == rep(c(ns("Low"), ns("Low-Medium"), ns("Medium-High"), ns("High")), each = 2),
          s10 == c(nrow(p), nrow(pall), p[dms_orig_GEOID != dms_dest_GEOID, .N], nrow(p)),
          obs_row("tables/supplementary/TableS4_origin_destination_year_FE.csv") == nrow(p),
          obs_row("tables/supplementary/TableS7_truck_share_by_distance.csv") == s3,
          obs_row("tables/supplementary/TableS8_truck_share_by_svi.csv") == s6)
s2 <- rbindlist(list(
  row("Figure 1", "Map of sample routes by SVI quartile; weather by SVI", "Main sample", nrow(p),
      sprintf("Map draws %s route lines (%s routed + %s same-zone)", format(nrow(gl), big.mark = ","), format(n_routed, big.mark = ","), n_intra)),
  row("Figure 2 / Table S3", "Freight volume, full sample", "Main sample", s3[1]),
  row("Figure 2 / Table S3", "Freight volume, 0-500 miles", "Short", s3[3]),
  row("Figure 2 / Table S3", "Freight volume, 500-1,000 miles", "Medium", s3[5]),
  row("Figure 2 / Table S3", "Freight volume, > 1,000 miles", "Long", s3[7]),
  row("Figure 3 / Table S6", "Freight volume, SVI Low", "SVI quartile", s6[1]),
  row("Figure 3 / Table S6", "Freight volume, SVI Low-Medium", "SVI quartile", s6[3]),
  row("Figure 3 / Table S6", "Freight volume, SVI Medium-High", "SVI quartile", s6[5]),
  row("Figure 3 / Table S6", "Freight volume, SVI High", "SVI quartile", s6[7]),
  row("Figure 4 / Table S7", "Truck share by distance (full sample)", "Main sample", s3[1], "Distance groups as in Table S3"),
  row("Figure 5 / Table S8", "Truck share by SVI (each quartile)", "SVI quartile", s6[1], "Quartiles as in Table S6"),
  row("Table S1", "SCTG composition of truck food freight", "All FAF domestic truck records", NA_integer_, "Not route-based; unchanged"),
  row("Table S2", "Summary statistics, full sample", "Main sample", nrow(p)),
  row("Table S2", "Summary statistics, SCTG 1-4", "Routes with SCTG 1-4 freight", nrow(p14)),
  row("Table S2", "Summary statistics, SCTG 5-7", "Routes with SCTG 5-7 freight", nrow(p57)),
  row("Table S4", "Origin x year and destination x year fixed effects", "Main sample", nrow(p)),
  row("Table S5", "By food product: SCTG 1-4 / SCTG 5-7", "SCTG panels", nrow(p14), sprintf("SCTG 5-7: %s routes, %s obs", format(nrow(p57) / YRS, big.mark = ","), format(nrow(p57), big.mark = ","))),
  row("Table S10 col. 1", "Baseline", "Main sample", s10[1]),
  row("Table S10 col. 2", "Including routes outside the contiguous U.S.", "All routes", s10[2]),
  row("Table S10 col. 3", "Excluding same-zone routes", "Main sample minus same-zone", s10[3]),
  row("Table S10 col. 4", "Distance-weighted exposure", "Main sample", s10[4]),
  row("Table S11", "FAF distance class vs OSRM route length", "Routed sample routes", n_routed * YRS, "Counts routes, not route-years"),
  row("Figure S1", "FAF zone map", "FAF zones", NA_integer_, "Not route-based"),
  row("Figure S2", "Map and weather, short routes", "Short", nd("Short")),
  row("Figure S3", "Map and weather, medium routes", "Medium", nd("Medium")),
  row("Figure S4", "Map and weather, long routes", "Long", nd("Long")),
  row("Figures S5-S7", "Weather trends (overall, by distance, by SVI)", "Main sample", nrow(p))))
s2[is.na(Observations), Routes := NA]

# --- Sheet 3: breakdown -------------------------------------------------------------
br <- function(d, by, lev, lab) { x <- d[, .(Routes = uniqueN(unit), Observations = .N), by = by]
  x <- x[match(lev, as.character(get(by)))]; data.table(Breakdown = lab, Group = lev, Routes = x$Routes, Observations = x$Observations) }
s3b <- rbind(br(p, "dist_cat", c("Short", "Medium", "Long"), "Distance group (FAF band)"),
             br(p, "svi_cat", c("Low", "Low-Medium", "Medium-High", "High"), "SVI quartile"),
             br(p, "year", as.character(sort(unique(p$year))), "Year"),
             data.table(Breakdown = "Total", Group = "Main sample", Routes = N, Observations = nrow(p)))
s3b[Breakdown == "Distance group (FAF band)", Group := c("Short (0-500 mi)", "Medium (500-1,000 mi)", "Long (> 1,000 mi)")]

# --- Sheet 4: version history -------------------------------------------------------
s4 <- data.table(
  Version = c("Submitted manuscript (text)", "Submitted manuscript (regressions)",
              "Interim, 2026-11-08", "Final, 2026-11-11"),
  Routes = c(11938L, 12373L, 12196L, N),
  Observations = c(71628L, 74238L, 73176L, nrow(p)),
  `How the routes were defined` = c(
    "Routes in the map file: OSRM simplified route lines lying entirely within the contiguous U.S.",
    "All FAF truck routes, including Alaska/Hawaii and routes through Canada",
    "Map-file routes (checked on simplified lines) plus 258 routes re-requested with full lines",
    "All routes re-requested with full road lines; keep those within the contiguous U.S."),
  `Why it changed` = c(
    "Simplified lines cut corners at coasts, the Great Lakes and the border, so 258 domestic routes were wrongly dropped",
    "Text and regressions used different route sets (Reviewer 2, Comment 21)",
    "23 routes kept here run through Canada on their current full road line (Maine, Vermont, New Hampshire, Michigan to and from the Midwest or Northeast)",
    "Every route built with one method in one run; route lines and checks saved, so results no longer depend on new OSRM requests"))

# --- Optional sheet 5: significance changes vs an earlier tables/ copy -------------
s5c <- NULL
cmp_dir <- Sys.getenv("COMPARE_DIR")
if (nzchar(cmp_dir)) {
  stars <- function(x) nchar(gsub("[^*]", "", x))
  s5c <- rbindlist(lapply(list.files(file.path(ROOT, "tables/supplementary"), "^TableS(3|4|5|6|7|8|10)_.*csv$"), function(f) {
    a <- fread(file.path(cmp_dir, "supplementary", f), colClasses = "character"); b <- fread(file.path(ROOT, "tables/supplementary", f), colClasses = "character")
    if (!identical(dim(a), dim(b))) return(NULL)
    var <- b[[1]]; for (i in seq_along(var)) if (var[i] == "" && i > 1) var[i] <- var[i - 1]
    rbindlist(lapply(2:ncol(b), function(j) { k <- which(grepl("^-?[0-9.]+\\**$", b[[j]]) & stars(a[[j]]) != stars(b[[j]]))
      if (!length(k)) return(NULL)
      data.table(Table = sub("_.*", "", f), Column = names(b)[j], Variable = var[k], Previous = a[[j]][k], Final = b[[j]][k]) }))
  }))
}

# --- Write ------------------------------------------------------------------------
wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "Bottom", wrapText = TRUE)
bold <- createStyle(textDecoration = "bold"); num <- createStyle(numFmt = "#,##0"); wrap <- createStyle(wrapText = TRUE, valign = "top")
sheet <- function(name, d, title, widths, bold_rows = integer(), notes = character()) {
  addWorksheet(wb, name)
  writeData(wb, name, title, startRow = 1); addStyle(wb, name, createStyle(textDecoration = "bold", fontSize = 13), 1, 1)
  writeData(wb, name, d, startRow = 3, headerStyle = hs)
  nc <- which(names(d) %in% c("Routes", "Observations"))
  addStyle(wb, name, num, rows = 4:(nrow(d) + 3), cols = nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, name, wrap, rows = 4:(nrow(d) + 3), cols = seq_along(d), gridExpand = TRUE, stack = TRUE)
  if (length(bold_rows)) addStyle(wb, name, bold, rows = bold_rows + 3, cols = seq_along(d), gridExpand = TRUE, stack = TRUE)
  if (length(notes)) writeData(wb, name, notes, startRow = nrow(d) + 5)
  setColWidths(wb, name, seq_along(d), widths)
}
sheet("Route sample", s1, sprintf("Route sample: %s routes, %s observations", format(N, big.mark = ","), format(nrow(p), big.mark = ",")),
      c(64, 10, 14, 34), bold_rows = c(3, 6), notes = c(
  "Rule: a route is in the sample if its full OSRM road line has at most 1 mile outside the contiguous U.S. (48 states + DC, TIGER 2017 state polygons).",
  sprintf("The split is clean: sample routes have 0 miles outside; excluded routes have %.0f-%.0f miles outside. No route passes through Mexico.", out_rng[1], out_rng[2]),
  sprintf("Observations = routes x %d years (2018-2023); the panel is balanced.", YRS),
  "Routes: data/Route/route_conus_check_fullgeom.csv (column within_conus). Panel: data/panel_od_year_truck_sctg01_07.rds.",
  paste("Generated", format(Sys.Date()), "by code/R2_21_route_sample_summary.R; all counts checked against the panels and saved tables.")))
sheet("Tables & figures", s2, "Routes and observations behind each table and figure", c(18, 50, 30, 10, 14, 50))
sheet("Sample breakdown", s3b, "Main sample by distance group, SVI quartile and year", c(26, 24, 10, 14), bold_rows = nrow(s3b))
sheet("Version history", s4, "Route counts in earlier versions", c(34, 10, 14, 60, 70), bold_rows = 4)
if (!is.null(s5c)) sheet("Changes vs previous", s5c, sprintf("Coefficients whose significance stars changed (previous: %s)", basename(dirname(cmp_dir))),
                         c(10, 40, 26, 14, 14))
dir.create(file.path(ROOT, "tables"), showWarnings = FALSE)
saveWorkbook(wb, file.path(ROOT, "tables/route_sample_summary.xlsx"), overwrite = TRUE)
print(s1); print(s2[, .(Item, Content, Routes, Observations)]); if (!is.null(s5c)) print(s5c)
message("Saved tables/route_sample_summary.xlsx")
