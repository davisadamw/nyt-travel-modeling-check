library(tidyverse)
library(sf)
library(stars)

# each point has 4 rows in this data frame (1 for each place type)
# this only matters for 
results_old <- read_rds("Data/model_00s_3000km.rds") %>% 
  pluck("SDF") %>% 
  st_as_sf(crs = 4326) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>%
  st_drop_geometry()
results_new <- read_rds("Data/model_10s_3000km.rds") %>% 
  pluck("SDF") %>% 
  st_as_sf(crs = 4326) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>%
  st_drop_geometry()

# you can run this to see that coefficients are identical at all four rows for each point
# i swear this code works, but it'd definitely have been faster to pivot_longer right at the start
# result_variation <- results_old %>%
#   group_by(X, Y) %>%
#   summarize(across(everything(), sd), .groups = "drop") %>%
#   summarize(across(everything(), list(min_sd = min, max_sd = max))) %>% 
#   pivot_longer(everything(), names_to = c("variable", ".value"), names_pattern = "(.+)_(m[a-z]{2}_sd$)")

# TODO: determine which place type each prediction is from, get the preds as individual columns in the final dataset
# ... add that to results_both
# now, grab only one row per XY and populate a matrix with them
results_both <- left_join(
  distinct(select(results_old, X, Y, `(Intercept)`:log_remote)),
  distinct(select(results_new, X, Y, `(Intercept)`:log_remote)),
  by = c("X", "Y"),
  suffix = c("_00s", "_10s")
) %>% 
  janitor::clean_names() %>% 
  as_tibble()


# function to grab the right columns, make raster, and write result to disk
# rasterFromXYZ is super easy, courtesy f the second answer to this:
#    https://stackoverflow.com/questions/19627344/how-to-create-a-raster-from-a-data-frame-in-r
# feel like there's probably a better way to write this, and it's certainly very data specific as is, but...
# also, writeRaster throws a warning message with our crs that doesn't really matter, so just grab it in a list, handed to map
to_raster <- function(data, field_name) {
  ras_data <- select(data, x, y, {{ field_name }})
  
  ras_data %>% 
    raster::rasterFromXYZ(res = c(0.25, 0.25), crs = 4326) %>% 
    quietly(raster::writeRaster)(glue::glue("Raster_Dump/{names(ras_data)[3]}.img"), overwrite = TRUE)
}

# this makes a raster from each data columns and writes it to disk as an Erdas imagine file
to_raster_results <- map(names(results_both)[3:ncol(results_both)],
                         to_raster, 
                         data = results_both)

# also save the original data to disk
results_both %>% 
  write_rds("Data/gwr_result_coeffs_only_3000km.rds", compress = "gz")
