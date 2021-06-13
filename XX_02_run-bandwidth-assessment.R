library(tidyverse)
library(sf)
library(spgwr)
source("XX_99_bandwidth-selection-workarounds.R")

# load data
all_model_data <- read_rds("Data/Intermediates/all_places_prepped_4pts_528.rds")

prediction_grid_prepped <- read_rds("Data/Intermediates/prediction_grid_prepped_4pts_528.rds") %>% 
  as_Spatial()

log_model_formula <- read_rds("Data/Intermediates/log_model_formula.rds")


# create the model matrix and coords dataset for each model period
model_frames <- all_model_data %>% 
  nest(data = c(new_token, adj_nature_mean:log_remote), coords = c(longitude, latitude)) %>% 
  mutate(coords = map(coords, as.matrix),
         mf = map(data, gwr_sel_mm_create, formula = log_model_formula)) %>% 
  unnest_wider(mf)
 
# run correlation matrix at a bunch of bandwidths
# function to run correlation matrix of coefficient estimates and return the values of one triangle
make_cormat <- function(gwr_obj) {
  cormat <- gwr_obj$SDF %>% 
    st_as_sf() %>% 
    st_drop_geometry() %>% 
    select(-sum.w, -gwr.e, -pred, -localR2) %>% 
    cor() 
  
  tibble(var1 = rownames(cormat)[row(cormat)],
         var2 = colnames(cormat)[col(cormat)],
         vals = as.vector(cormat),
         ut   = as.vector(upper.tri(cormat))) %>% 
    filter(ut, var1 != var2) %>% 
    select(-ut)
}

model_cors_and_cvs <- model_frames %>% 
  crossing(bandwidth = seq(1000, 6000, 200)) %>% 
  mutate(model_ests = pmap(list(data = data, coords = coords, bandwidth = bandwidth),
                           gwr,
                           formula = log_model_formula, longlat = TRUE),
         cormats = map(model_ests, make_cormat),
         cv_score = pmap_dbl(list(bandwidth = bandwidth, y = y, x = x, coords = coords, weights = weights),
                             compute_cv_score))

model_cors <- model_cors_and_cvs %>% 
  select(model_name, bandwidth, cormats) %>% 
  unnest(cormats)

model_cors %>% 
  # filter(var1 != "(Intercept)",
  #        var2 != "(Intercept)",
  #        !str_starts(var1, "place_type"),
  #        !str_starts(var2, "place_type")) %>% 
  mutate(var_pair = paste(model_name, var1, var2)) %>% 
  ggplot(aes(bandwidth, vals, group = var_pair, color = model_name)) +
  geom_line(alpha = 0.5) +
  geom_point(data = ~ with_groups(.x, bandwidth, slice_min, order_by = vals, n = 1)) +
  geom_point(data = ~ with_groups(.x, bandwidth, slice_max, order_by = vals, n = 1)) +
  scale_color_manual("Model", values = c("purple", "darkorange")) +
  scale_x_continuous("Bandwidth, km", breaks = seq(1000, 6000, 500), labels = scales::comma) +
  scale_y_continuous("Correlation of GWR Model Coefficients",
                     breaks = seq(-1, 1, 0.2)) +
  theme_bw()

ggsave("Cors_allpaths.png", width = 6, height = 4, dpi = 450)

# store the modeling results 
bandwidths_mods %>% 
  select(model_name, bandwidth, cv_score:worst_cor_pair) %>% 
  write_rds("Data/Intermediates/bandwidth_ranges.rds", compress = "gz")

adapt_frac_mods %>% 
  select(model_name, q, cv_score:worst_cor_pair) %>% 
  write_rds("Data/Intermediates/adapt_frac_ranges.rds", compress = "gz")

# run the models on the grid
model_00s_3000 <- gwr(log_model_formula, 
                      data = model_frames$data[[1]],
                      coords = model_frames$coords[[1]], 
                      bandwidth = 3500,
                      # predict = TRUE, 
                      # fit.points = prediction_grid_prepped, 
                      longlat = TRUE)
model_10s_3000 <- gwr(log_model_formula, 
                      data = model_frames$data[[2]],
                      coords = model_frames$coords[[2]], 
                      bandwidth = 3500,
                      # predict = TRUE, 
                      # fit.points = prediction_grid_prepped, 
                      longlat = TRUE)


model_00s_3000 %>% 
  write_rds("Data/model_00s_3000km_Feb11.rds", compress = "gz")

model_10s_3000 %>% 
  write_rds("Data/model_10s_3000km_Feb11.rds", compress = "gz")
