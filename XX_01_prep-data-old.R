library(tidyverse)
library(sf)
library(stocked)
library(formula.tools)

# OLD VERSION OF DATA PREP CODE 
# CENTERING AND SCALING HAS ALREADY BEEN DONE SO WE CAN SKIP A LOT OF THIS

# load data
places_0009 <- read_rds("Data/model_vars_old_528.rds")
places_1019 <- read_rds("Data/model_vars_new_528.rds")

# load prediction grid
prediction_grid_in <- read_rds("Data/grid_all_place_types.rds")

# model formula ... should probably be an input to this, but w/e
log_model_formula <- adj_nature_mean ~ log_pop + log_elev + avg_temp + temp_range + log_prec + place_type + log_remote
rhs_varnames   <- rhs.vars(log_model_formula)
rhs_varnames_c <- rhs_varnames[rhs_varnames != "place_type"]

# drop NAs in data ... more than I'd like in first time slot, but w/e
# ... also subset columns
places_0009_nona <- places_0009 %>% 
  drop_na(one_of(rhs_varnames)) %>% 
  mutate(model_name = "2000-2009")

places_1019_nona <- places_1019 %>% 
  drop_na(one_of(rhs_varnames)) %>% 
  mutate(model_name = "2010-2019")

prediction_grid_in_nona <- prediction_grid_in %>% 
  mutate(log_prec = if_else(prec < 1, 0, log(prec)),
         log_remote = if_else(remote < 1, 0, log(remote))) %>% 
  # use filter not drop_na here to preserve sf data format
  filter(!if_any(one_of(rhs_varnames), is.na))

# center and scale everything at once
all_model_data <- bind_rows(places_0009_nona,
                            places_1019_nona) %>% 
  select(new_token, latitude, longitude, model_name, adj_nature_mean, all_of(rhs_varnames))

all_model_data_cs <- all_model_data %>% 
  center_and_scale_vars(all_of(rhs_varnames_c))


# also center and scale the grid based on the points in the model and remove Country/Global Region vals
prediction_grid_prepped <- prediction_grid_in_nona %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  st_drop_geometry() %>% 
  center_and_scale_validation(training = all_model_data,
                              all_of(rhs_varnames_c)) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326)


# recode all regions to Country/Global Region to Region, put Region first in the order
# store data
all_model_data_cs %>% 
  write_rds("Data/Intermediates/all_places_prepped_4pts_528.rds", compress = "gz") %>% 
  mutate(place_type = if_else(place_type == "Country/Global Region", "Region", place_type) %>% 
           fct_relevel("Region")) %>% 
  write_rds("Data/Intermediates/all_places_prepped_3pts_528.rds", compress = "gz")

prediction_grid_prepped %>% 
  write_rds("Data/Intermediates/prediction_grid_prepped_4pts_528.rds", compress = "gz") %>% 
  filter(place_type != "Country/Global Region") %>% 
  mutate(place_type = fct_relevel(place_type, "Region")) %>% 
  write_rds("Data/Intermediates/prediction_grid_prepped_3pts_528.rds", compress = "gz")

log_model_formula %>% 
  write_rds("Data/Intermediates/log_model_formula.rds")
