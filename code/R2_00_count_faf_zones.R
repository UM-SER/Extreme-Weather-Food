library(sf)

map_dir <- "./data/Shapefile"

metro      <- st_read(file.path(map_dir, "2017_CFS_Metro_Areas.shp"),      quiet = TRUE)
remainder  <- st_read(file.path(map_dir, "2017_Remainder_of_States.shp"),   quiet = TRUE)

cat("CFS Metro Areas:       ", nrow(metro),     "\n")
cat("Remainder of States:   ", nrow(remainder), "\n")
cat("Total FAF zones:       ", nrow(metro) + nrow(remainder), "\n\n")

cat("Metro columns:", paste(names(metro),     collapse=", "), "\n")
cat("Remainder columns:", paste(names(remainder), collapse=", "), "\n\n")

# Show first few rows
print(head(as.data.frame(metro)[, 1:5]))
print(head(as.data.frame(remainder)[, 1:5]))
