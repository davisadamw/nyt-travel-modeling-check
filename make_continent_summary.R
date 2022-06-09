library(tidyverse)
library(sf)

# load cooeffs
coeffs <- read_rds("Data/gwr_result_coeffs_only_3000km.rds")

# load countries, make continents
countries <- read_sf("Data/ne_10m_admin_0_countries_lakes.shp") %>% 
  select(ADMIN, CONTINENT) %>% 
  st_cast("MULTILINESTRING")

# join coefficients to nearest boundaries (workaround for spherical geometry error lmao)
coeffs_with_continent <- coeffs %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(countries), remove = FALSE) %>% 
  st_join(countries, join = st_nearest_feature) %>% 
  st_drop_geometry()

# summarize by continent (first fixing for asian Russia)
continent_summaries <- coeffs_with_continent %>% 
  mutate(continent = case_when(ADMIN != "Russia" ~ CONTINENT,
                               between(x, -10, 60) ~ "Europe",
                               TRUE ~ "Asia")) %>% 
  select(continent, ends_with("0s")) %>% 
  pivot_longer(-continent, 
               names_to = c("variable", ".value"), 
               names_pattern = "([a-z_]+)_([01]0s$)") %>% 
  with_groups(c(continent, variable), summarize,
              across(everything(), 
                     list(mean = mean,
                          min = min,
                          max = max,
                          q25 = ~ quantile(., probs = 0.25),
                          q75 = ~ quantile(., probs = 0.75))))

continent_summaries %>% 
  write_csv("Data/continent_summaries.csv")

