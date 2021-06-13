library(tidyverse)
library(sf)
library(stars)
library(tmap)
traceback(data("World"))

# stretch strength
stretch_strength = 4

# gonna use robinson for now
target_projection <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
#target_projection <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# reproject world polygons into robinson
World <- st_transform(World, target_projection)

# identify, load, and clean rasters ####

# identify all the (paired) rasters (only want the img files, not the xmls that go with them)
# all processing on filed names will be done here and joined to data later in case we want to change naming system
rasters <- tibble(filename = list.files("Raster_Dump", pattern = "img$"),
                  varname  = str_remove(filename, ".img"),
                  variable = str_extract(varname, ".+(?=_[01]0s$)"),
                  decade   = paste0("20", str_extract(varname, "[01]0s")),
                  filepath = file.path("Raster_Dump", filename))

# file to variable
# the default field names will be just the filename ... drop the ".img" from those
filename_to_var <- rasters %>% select(filename, varname) %>% deframe()

# figure titles
map_titles <- tribble(
  ~variable,                     ~title_component,
  "avg_temp",                    "Average Temperature",
  "intercept",                   "Place is Country",
  "log_elev",                    "Elevation",
  "log_pop",                     "Population Density",
  "log_remote",                  "Distance from a Major City",
  "place_type_natural_location", "Place is Natural Location",
  "place_type_region",           "Place is Region",
  "place_type_urban",            "Place is Urban",
  "log_prec",                    "Precipitation",
  "temp_range",                  "Temperature Range"
)

# identify columns corresponding to each variable
variable_pairs <- rasters %>% 
  select(varname, variable) %>%
  left_join(map_titles, by = "variable") %>% 
  nest(var_cols = varname) %>% 
  mutate(var_cols = map(var_cols, pull, 1)) %>% 
  # rearrange columns ... not strictly necessary, but this ensures pmap will work as intended
  select(variable, title_component, var_cols)

# info to attach to columns
col_info <- rasters %>% 
  select(varname, variable, decade)

# load all rasters into a single dataset and convert to sf
all_rasters_sf <- read_stars(rasters$filepath) %>% 
  st_as_sf() %>% 
  rename_with(~ filename_to_var[.], -geometry)

# in ggplot, the easiest way to plot reshaped rasters is by first converting them to sf polygons
# ... this creates a problem with points that touch 180 degs longitude ... for now let's just drop those points i guess
all_rasters_sf_cleaned <- all_rasters_sf %>% 
  mutate(max_abs_lon = map_dbl(geometry, ~ max(abs(st_bbox(.)[c(1,3)])))) %>% 
  filter(max_abs_lon <= 180) %>% 
  select(-max_abs_lon) %>% 
  st_transform(crs = target_projection)

# function to prep each variable for plotting ####

# it works with pivot_longer but throws an error because pivot_* functions aren't generic yet
# gather is generic and gather.sf handles geometry column without throwing error
pull_vals <- function(source_data, var_cols, col_info = NULL) {
  
  # grab only the relevant columns, convert to long-format, and attach relevant info (decade, variable name, etc)
  data_long <- source_data %>% 
    select(one_of(var_cols)) %>% 
    gather(key = "varname",
           value = "value",
           one_of(var_cols))
    # pivot_longer(one_of(var_cols),
    #              names_to  = "varname",
    #              values_to = "value") %>% 
    # st_as_sf()
  
  if (is.null(col_info)) return(data_long)
  
  data_long %>% 
    left_join(col_info, by = "varname")
  
}

# function to do a sigmoidal stretch maintaining 0 as the middle ... should work the same as the ArcGIS version
sigmoidal_stretch <- function(x, strength = 5, scale_to_01 = TRUE) {
  # first do a linear transformation making the extreme value of x equal to +- strength
  x_lt <- x / max(abs(x)) * strength
  
  # then sigmoid curve transformation onto (0, 1), note that this will get closer to 0/1 if strength higher
  x_sig <- 1 / (1 + exp(-x_lt))
  
  if (!scale_to_01) return(x_sig)
  
  # finally scale it linearly so that it will reach either 0 or 1, midpoint remains at 0.5
  # this is just to it easy to have most extreme value hit full score on color bar
  x_sig_a <- x_sig - 0.5
  (x_sig_a / max(abs(x_sig_a)) + 1) / 2
}

# function to run plotting process for each variable, this function is extremely specific to this script / not really generalized at all
make_map <- function(variable, title_component, var_cols, palette, direction, data_source, col_info, strength = 5) {
  
  data <- pull_vals(data_source, var_cols, col_info = col_info)
  
  map_title <- glue::glue("Coefficients for {title_component}\nin Geographically Weighted Regression Model")
  
  # apply sigmoid stretch for plotting
  data <- data %>% 
    mutate(value_stretch = sigmoidal_stretch(value, strength = strength))
  
  figure_filepath <- file.path("Figures/Maps",
                               glue::glue("{variable}_comparison.png"))
  
  # values corresponding to 0, 0.5, 1 on the stretch
  scale_labels <- scales::number(c(-1, 0, 1) * max(abs(data$value)), accuracy = 0.2)
  
  data %>% 
    ggplot() +
    geom_sf(data = World, fill = "grey80", color = NA) +
    geom_sf(aes(fill = value_stretch), color = NA) +
    geom_sf(data = World, fill = NA, size = 0.1) +
    facet_wrap(vars(decade), ncol = 1) +
    scale_fill_distiller(glue::glue("Local Coefficient for\n{title_component}"), 
                         type = "div", palette = palette, direction = direction,
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1), 
                         labels = scale_labels) +
    coord_sf(label_axes = "") +
    ggtitle(map_title) +
    theme_bw() +
    theme(panel.grid.major = element_line(color = "grey60"),
          panel.background = element_rect(fill = "grey20"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  ggsave(figure_filepath, height = 9, width = 6)
  
}

# colorbrewer diverging palettes, conveniently, there is one per variable
palettes <- tribble(
  ~variable,                     ~palette, ~direction,
  "avg_temp",                    "RdBu",   -1,
  "intercept",                   "",       1,
  "log_elev",                    "",       1,
  "log_pop",                     "PRGn",   -1,
  "log_remote",                  "RdYlBu", 1,
  "place_type_natural_location", "",       1,
  "place_type_region",           "",       1,
  "place_type_urban",            "",       1,
  "log_prec",                    "BrBG",   1,
  "temp_range",                  "PuOr",   1
) 

all_palettes <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")

# randomly assign other palettes
palettes <- palettes %>% 
  mutate(palette = if_else(palette == "",
                           sample(all_palettes[! all_palettes %in% palettes$palette], n(), replace = TRUE),
                           palette))

variable_pairs %>% 
  left_join(palettes, by = "variable") %>% 
  #slice(1) %>% 
  pwalk(make_map, data_source = all_rasters_sf_cleaned, col_info = col_info, strength = stretch_strength)





