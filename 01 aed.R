

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, imputeTS, rnaturalearthdata, stringr, sf, geodata, tidyverse, gtools, gstat)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
data <- read_csv('./tbl/pseudo_values_all_tmax.csv')
cntr <- rnaturalearthdata::map_units50
cntr <- st_as_sf(cntr)
cntr <- cntr[st_is_valid(cntr),] 
cntr <- cntr[,'admin']

# Checking the location of the points -------------------------------------
pnts <- st_as_sf(x = data, coords = c('lon', 'lat'), crs = st_crs(4326))
pnts <- st_intersection(pnts, cntr)
name <- unique(pnts$admin)

# Filtering  --------------------------------------------------------------
subc <- filter(cntr, admin %in% pnts$admin)
plot(st_geometry(subc))
plot(st_geometry(pnts), add = TRUE, col = 'red')

# Add NAs -----------------------------------------------------------------
sttn <- unique(data$station)
data <- mutate(data, value = base)
data <- map(.x = 1:length(sttn), .f = function(i){
  cat(i, '\n')
  tb <- filter(data, station == sttn[i])
  tb[sample(x = 1:12, size = 2, replace = FALSE),'value'] <- NA
  return(tb)
}) %>% 
  bind_rows()

# Impute Time Series Data -------------------------------------------------

# Linear - Spline - Stine
data <- map(.x = 1:length(sttn), .f = function(i){
  cat(sttn[i], '\t')
  subd <- data %>% filter(station == sttn[i]) 
  tsrs <- subd %>% pull(value) %>% ts()
  opts <- c('linear', 'spline', 'stine')
  rslt <- map(.x = 1:length(opts), .f = function(k){
    imputeTS::na_interpolation(x = tsrs, option = opts[k]) %>% 
      as.numeric()
  })
  subd <- subd %>% mutate(lnear = rslt[[1]], splne = rslt[[2]], stine = rslt[[3]])
  cat('Done!\n')
  return(subd)
}) %>% 
  bind_rows()

# Add the average in the NA 
data <- map(.x = 1:length(sttn), .f = function(i){
  cat(sttn[i], '\t')
  subd <- data %>% filter(station == sttn[i]) 
  avrg <- mean(subd$value, na.rm = T)
  subd <- subd %>% mutate(mean = value)
  subd[is.na(subd$mean),'mean'] <- avrg
  return(subd)
}) %>% 
  bind_rows()



