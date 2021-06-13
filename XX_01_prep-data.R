library(tidyverse)
library(sf)
library(stocked)
library(formula.tools)

# OLD VERSION OF DATA PREP CODE 
# CENTERING AND SCALING HAS ALREADY BEEN DONE SO WE CAN SKIP A LOT OF THIS

# load data
places_0009 <- read_rds("Data/model_vars_old.rds")
places_1019 <- read_rds("Data/model_vars_new.rds")

# load prediction grid
prediction_grid_in <- read_rds("Data/grid_all_place_types.rds")

# model formula ... should probably be an input to this, but w/e
log_model_formula <- adj_nature_mean ~ log_pop + log_elev + avg_temp + temp_range + log_prec + place_type + log_remote
rhs_varnames   <- rhs.vars(log_model_formula)
rhs_varnames_c <- rhs_varnames[rhs_varnames != "place_type"]

# drop NAs in data ... none in this data version
# ... also subset columns
places_0009_nona <- places_0009 %>% 
  drop_na(one_of(rhs_varnames)) %>% 
  mutate(model_name = "2000-2009")

places_1019_nona <- places_1019 %>% 
  drop_na(one_of(rhs_varnames)) %>% 
  mutate(model_name = "2010-2019")

# combine the data into a single set
all_model_data <- bind_rows(places_0009_nona,
                            places_1019_nona) %>% 
  select(new_token, latitude, longitude, model_name, adj_nature_mean, all_of(rhs_varnames))

# prediction grid needs to be centered and scaled ... also replace all NAs with 0s
prediction_grid_in_nona <- prediction_grid_in %>% 
  mutate(log_prec = if_else(prec < 1, 0, log(prec)),
         log_remote = if_else(remote < 1, 0, log(remote)),
         across(all_of(rhs_varnames_c), replace_na, 0)) %>% 
  # use filter not drop_na here to preserve sf data format
  filter(!if_any(one_of(rhs_varnames), is.na))

prediction_grid_cs <- prediction_grid_in_nona %>% 
  mutate(across(all_of(rhs_varnames_c), center_and_scale))


# recode all regions to Country/Global Region to Region, put Region first in the order
# store data
all_model_data %>% 
  write_rds("Data/all_places_prepped_4pts_528.rds", compress = "gz") %>% 
  mutate(place_type = if_else(place_type == "Country/Global Region", "Region", place_type) %>% 
           fct_relevel("Region")) %>% 
  write_rds("Data/all_places_prepped_3pts_528.rds", compress = "gz")

prediction_grid_cs %>% 
  write_rds("Data/prediction_grid_prepped_4pts_528.rds", compress = "gz") %>% 
  filter(place_type != "Country/Global Region") %>% 
  mutate(place_type = fct_relevel(place_type, "Region")) %>% 
  write_rds("Data/prediction_grid_prepped_3pts_528.rds", compress = "gz")

log_model_formula %>% 
  write_rds("Data/log_model_formula.rds")
