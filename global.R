# two panel interactive plot:
# - left: tradeoff chart to click on tradeoff
# - right: 
#
# TODO:
# - table: highlight row with table
# - select alternate points to route (need time progressed), see
#   https://rstudio.github.io/leaflet/shiny.html#inputsevents
#   http://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
# - try cells as polygons
# - add offshore wind raster. see marine cadastre w/ leases (active?), http://atlanticwindconnection.com/
# - consider click/hover on spp weights to show species distribution map on right
# - add progress indicator for: 
#   a. routing b/n new pts by transform, 
#   b. updating risk surface based on new industry profile / species weights.
#   See:
#   - http://shiny.rstudio.com/articles/progress.html
#   - http://shiny.rstudio.com/gallery/progress-bar-example.html
#   - http://shiny.rstudio.com/reference/shiny/latest/Progress.html
#   - http://shiny.rstudio.com/reference/shiny/latest/withProgress.html
# - hide / show details of conservation risk surface by species / weights / industry with:
#   - [shinyBS](https://ebailey78.github.io/shinyBS/examples.html) OR
#   - [shinyjs](http://cran.r-project.org/web/packages/shinyjs/)

library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(sp)
library(stringr)
library(rgdal)
library(raster)
select = dplyr::select
library(gdistance)
library(scales)
library(rasterfaster) # devtools::install_github("jcheng5/rasterfaster")
library(leaflet)      # devtools::install_github("rstudio/leaflet@joe/feature/raster-image") # devtools::install_github("rstudio/leaflet@joe/feature/raster-image")
library(htmltools)
library(shiny)
library(shinyjs)      # devtools::install_github("daattali/shinyjs")
show = sp::show
#library(DT)          # devtools::install_github("rstudio/DT")
library(ggvis)        # devtools::install_github("rstudio/ggvis") # https://github.com/rstudio/ggvis/pull/381
library(markdown)
library(ggplot2)

# Immediately enter the browser when an error occurs
options(error = NULL) # error = NULL|browser|utils::recover
#options(shiny.trace=TRUE)

# params
epsg4326 <- "+proj=longlat +datum=WGS84 +no_defs"
epsg3857 <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
spp_list = list(
  'Delphinids' = c(
    'Harbour porpoise'            = 'HP',
    "Dall's porpoise"             = 'DP',
    'Pacific white-sided dolphin' = 'PW',
    'Killer whale'                = 'KW'),
  'Whales' = c(
    'Humpback whale'              = 'HW',
    'Fin whale'                   = 'FW',
    'Minke whale'                 = 'MW'),
  'Pinnipeds' = c(
    'Harbour seal'                = 'HS',
    'Steller sea lion'            = 'SSL',
    'Elephant seal'               = 'ES'))
v = utils::stack(spp_list)
spp_names = setNames(object=row.names(v), nm=v$values)

# default transform for d data.frame
default_transform = 'x * 10'
default_beg = 'SHG'
default_end = 'KTM'
default_study = 'BC'

# paths
if (Sys.info()[['sysname']] == 'Darwin'){
  # on bbest's Mac
  app_dir = '~/github/consmap'
} else if (Sys.info()[['sysname']] == 'Linux'){
  # on shiny.env Linux
  app_dir = '/shiny/bbest/consmap'
}

addResourcePath('img','img')

data = c(
  rdata           = sprintf('routes/routes_%s_to_%s.Rdata', default_beg, default_end), # '~/Google Drive/dissertation/data/routing/demo.Rdata'
  grd             = 'v72zw_epsg3857.grd', # '~/Google Drive/dissertation/data/bc/v72zw_epsg3857.grd'
  extents_csv     = 'extents.csv',
  points_csv      = 'points.csv',
  ports_csv       = 'ports_bc.csv',
  spp_shp         = 'bc_spp_gcs.shp',
  spp_csv         = 'spp.csv',
  spp_weights_csv = 'spp_weights.csv') 
data = setNames(sprintf('data/%s', data), names(data))

# read data ----

# change dir if not running in Shiny server mode
if (!file.exists('data')) setwd(app_dir) 

# check all files exist
#stopifnot(all(file.exists(data))) # DEBUG

# composite risk raster
r = raster(data[['grd']])

# route beg/end points
pts = read_csv(data[['points_csv']])

# read ports
ports = read_csv(data[['ports_csv']]) %>%
  mutate(pt_radius = log10(sum_ktons) / max(log10(sum_ktons))) %>%
  arrange(name)

# merge oceanic pts with ports
nodes = bind_rows(
  data.frame(
    group = 'New Point', 
    name  = 'Click on map...', 
    code  = 'NEW'),
  ports %>%
    mutate(group = 'Ports') %>%
    arrange(name),
  pts %>%
    filter(name=='S of Haida Gwaii') %>%
    select(name, lon, lat) %>%
    mutate(
      group     = 'Oceanic Access',
      code      = 'SHG'))

# species polygons
spp_ply = readOGR(
  dirname(data[['spp_shp']]),
  tools::file_path_sans_ext(basename(data[['spp_shp']])))

# extents
extents = read_csv(data[['extents_csv']]) %>%
  arrange(country, name, code)

# species weights ----
spp = read_csv(data[['spp_csv']]) %>%
  arrange(group, name)
spp_weights = read_csv(data[['spp_weights_csv']])
spp_labels = spp %>%
  group_by(srank) %>%
  summarize(
    label = paste(code, collapse=',')) %>%
  left_join(spp_weights, by='srank')


# species composite risk ----

# normalize all species densities
for (sp in spp$code){ # sp = spp$code[1] # names(spp_ply@data)
  d = spp_ply@data[,sprintf('%s_d', sp)]
  w = spp %>%
    left_join(spp_weights, by='srank') %>%
    filter(code == sp) %>%
    .$weight_logit
  # z-score, aka "standard score" in https://en.wikipedia.org/wiki/Normalization_(statistics)
  spp_ply@data[, sprintf('%s_z', sp)] = (d - mean(d, na.rm=T)) / sd(d, na.rm=T) * w
}

# sum across species
spp_ply@data$ALL_z = apply(spp_ply@data[, sprintf('%s_z', spp$code)], 1, function(x) sum(x, na.rm = T))


# TODO: add industry weights
w = spp %>%
  left_join(spp_weights, by='srank') %>%
  filter(code == sp) %>%
  .$weight_logit

# create popup for ALL
# TODO: consider outputting all values: x_i - \mu_s / sd_s * w_s
x = spp_ply@data[,c('CellID', 'ALL_z', sprintf('%s_z', spp$code))] %>%
  gather('SP_z', 'value', -CellID, -ALL_z) %>% 
  arrange(CellID, desc(value)) %>% 
  group_by(CellID, ALL_z) %>%
  summarize(
    ALL_popup = paste(
      sprintf('<strong>%s</strong>: %0.3g', SP_z, value),
      collapse='<br>\n')) %>%
  mutate(
    ALL_popup = sprintf('<strong>ALL_z</strong>: %0.3g<br>\nContributing species normalized scores:<br>\n%s', ALL_z, ALL_popup)) %>%
  as.data.frame()
spp_ply@data = data.frame(spp_ply@data, x[match(spp_ply@data[,'CellID'], x[,'CellID']),])

# shift to all positive for use as cost surface
spp_ply@data = spp_ply@data %>%
  mutate(ALL_c = ALL_z - min(ALL_z, na.rm=T))

# transformations to apply to species cost resistance raster
# transforms = c(
#   'x * 0', 'x * 0.1', 'x * 0.5', 'x * 1', 
#   'x * 10', 'x * 100', 'x * 1000', 'x * 10000', 
#   'x^2', '(x*10)^2', '(x*100)^2', 'x^3')
transforms = c(
  'x * 0', 'x * 1', 'x * 10', 'x * 100', 'x * 1000', '(x*100)^2')
  #'x * 0', 'x * 10', 'x * 100')

# normalize cost surface
x = (r / cellStats(r,'max'))

# ones map for getting linear path
r1 = r
r1[!is.na(r)] = 1


# Cetmap Test ----
#img_ec = '~/Downloads/CetMap_2015-09_public-release/Duke_CetMap_Models_EC_20150602/Model_Predictions/North_Atlantic_right_whale_month07_abundance.img'
img_ec = 'data/test_siting/Atlantic_spotted_dolphin_abundance.img'
r_ec_aea = raster(img_ec)
r_ec_mer = projectRaster(r_ec_aea, crs=CRS(epsg3857), res=10000, method='bilinear')
#plot(r_ec)

# run standalone ----

run_routing_standalone = function(lonlat1, lonlat2){
  
  # project original points to web mercator
  xy = c(coordinates(lonlat1), coordinates(lonlat2)) %>%
    matrix(ncol=2, byrow=T) %>%
    SpatialPoints(crs(epsg4326)) %>%
    spTransform(crs(epsg3857))
  
  # update to nearest non-NA points on raster for shortestPath to work
  xy = c(
    coordinates(r)[which.min(mask(distanceFromPoints(r, xy[1]), r)),],
    coordinates(r)[which.min(mask(distanceFromPoints(r, xy[2]), r)),]) %>%
    matrix(ncol=2, byrow=T) %>%
    SpatialPoints(crs(epsg3857))
  
  routes = list()
  for (i in 1:length(transforms)){ # i=8
    
    # progress bar
    cat(sprintf('  routing transform %d (of %d): %s\n', i, length(transforms), transforms[i]))
    
    # apply transform to raster
    xt = eval(parse(text=transforms[i]))
    
    # calculate shortest path
    rt = shortestPath(
      geoCorrection(transition(1 / (r1 + xt), mean, directions=8), type="c"), 
      xy[1],
      xy[2],
      output='SpatialLines')
    
    # input to list with gcs projection, cost and distance
    routes = append(routes, list(list(
      transform = transforms[i],
      route_gcs = spTransform(rt, crs(epsg4326)),
      cost_x    = sum(unlist(extract(x, rt)), na.rm=T), # sum(extract(x, rt)),
      dist_km   = SpatialLinesLengths(rt) / 1000,
      place     = 'British Columbia, Canada')))
  }
  
  # attribute begin/end points, original and on valid raster
  lonlat = spTransform(xy, crs(epsg4326))
  attr(routes, 'pts') = lonlat
  attr(routes, 'pt_codes') = c(beg,end)
  
  # attribute data.frame of route values
  d = data.frame(
    transform = sapply(routes, function(z) z$transform),
    dist_km   = sapply(routes, function(z) z$dist_km),
    cost_x    = sapply(routes, function(z) z$cost_x)) %>%
    mutate(
      industry     = dist_km - min(dist_km),
      conservation = cost_x  - min(cost_x)) %>%
    arrange(industry, desc(conservation))
  attr(routes, 'd') = d
  
  # save if doesn't exist (eg redoing)
  rdata = sprintf('%s/data/routes/routes_%s_to_%s.Rdata', app_dir, beg, end)
  if (!file.exists(rdata)){
    save(routes, file = rdata)
  }
  
  return(routes)
}

# route between all ports / oceanic access points
if (F){
  codes = sort(setdiff(nodes$code, 'NEW'))
  n = length(codes)
  m = matrix(NA, nrow=n, ncol=n, dimnames=list(beg=codes, end=codes))
  m[upper.tri(m)] = 1
  n_k = sum(!is.na(m))
  k = 0
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (!is.na(m[i,j])){
        k = k + 1
        beg = rownames(m)[i]
        end = colnames(m)[j]
        cat(sprintf('%02d/%d: %s -> %s\n', k, n_k, beg, end))
  
        rdata     = sprintf('%s/data/routes/routes_%s_to_%s.Rdata', app_dir, beg, end)
        rdata_rev = sprintf('%s/data/routes/routes_%s_to_%s.Rdata', app_dir, end, beg)
        if (file.exists(rdata_rev) | file.exists(rdata)){
          cat('  Rdata exists\n')
        } else {

          lonlat1 = SpatialPoints(
            nodes %>%
              filter(code==beg) %>%
              select(lon, lat) %>%
              as.data.frame(), crs(epsg4326))
          
          lonlat2 = SpatialPoints(
            nodes %>%
              filter(code==end) %>%
              select(lon, lat) %>%
              as.data.frame(), crs(epsg4326))
          
          run_routing_standalone(lonlat1, lonlat2)
        }
      }
    }
  }
}
