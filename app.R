# two panel interactive plot:
# - left: tradeoff chart to click on tradeoff
# - right: 
#
# TODO:
# - cleanup, checkin, upload to server, email
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

# params
epsg4326 <- "+proj=longlat +datum=WGS84 +no_defs"
epsg3857 <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
load_rdata = T
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

# paths
app_dir        = '~/github/consmap'
data = c(
  rdata           = 'routes.Rdata',       # '~/Google Drive/dissertation/data/routing/demo.Rdata'
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
stopifnot(all(file.exists(data)))

# composite risk raster
r = raster(data[['grd']])

# route beg/end points
pts = read_csv(data[['points_csv']])

# read ports
ports = read_csv(data[['ports_csv']]) %>%
  mutate(pt_radius = log10(sum_ktons) / max(log10(sum_ktons)))

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

# default transform for d data.frame
d_transform = 'x * 10'
  
# routes: load or get shortestPath
if (load_rdata){
  stopifnot(file.exists(data[['rdata']]))
  load(data[['rdata']])
  stopifnot(exists(c('routes')))
} else {
  
  # normalize cost surface
  x = (r / cellStats(r,'max'))

  # ones map for getting linear path
  r1 = r
  r1[!is.na(r)] = 1

  # transformations to apply to species cost resistance raster
  transforms = c(
    'x * 0', 'x * 0.1', 'x * 0.5', 'x * 1', 
    'x * 10', 'x * 100', 'x * 1000', 'x * 10000', 
    'x^2', '(x*10)^2', '(x*100)^2', 'x^3')
  
  routes = list()
  for (i in 1:length(transforms)){ # i=8

    # apply transform to raster
    xt = eval(parse(text=transforms[i]))
    
    # calculate shortest path
    rt = shortestPath(
      # TODO: think of m_vals as transforms: x * 100, x^2, s
      geoCorrection(transition(1 / (r1 + xt), mean, directions=8), type="c"), 
      coordinates(pts_mer)[1,],
      coordinates(pts_mer)[2,],
      output='SpatialLines')
    
    # input to list with gcs projection, cost and distance
    routes = append(routes, list(list(
      transform = transforms[i],
      route_gcs = spTransform(rt, crs(epsg4326)),
      cost_x    = sum(unlist(extract(x, rt)), na.rm=T), # sum(extract(x, rt)),
      dist_km   = SpatialLinesLengths(rt) / 1000,
      place     = 'British Columbia, Canada')))
  }
  
  # save
  save(routes, file = data[['rdata']])
}

# # quick routes update #routes0 = routes
# for (i in 1:length(routes)){ # i=1
#   #routes[[i]]$dist_km = routes[[i]]$dist_m / 1000
#   #routes[[i]]$dist_m <- NULL
#   #routes[[i]]$place = 'British Columbia, Canada'
#   routes[[i]]$extent = 'British Columbia, Canada'
#   routes[[i]]$place = NULL
# }
# save(routes, file = data[['rdata']])

# extract data from routes
d = data.frame(
  extent    = sapply(routes, function(z) z$extent),
  transform = sapply(routes, function(z) z$transform),
  dist_km = sapply(routes, function(z) z$dist_km),
  cost_x = sapply(routes, function(z) z$cost_x)) %>%
  mutate(
    industry = dist_km - min(dist_km),
    conservation = cost_x - min(cost_x)) %>%
  arrange(industry, desc(conservation))

cat_txt_tradeoff = function(transform){
  txt = with(
    d[d$transform==transform,],
    sprintf(paste(
      '- transformation: %s',
      '- dist _(km)_: %0.2f',
      '- cost: %0.2f',
      '- **industry** _(dist - min(dist))_: %0.2f',
      '- **conservation** _(cost - min(cost))_: %0.2f',
      sep='\n'),
      transform,
      dist_km,
      cost_x,
      industry,
      conservation)) %>%
    renderMarkdown(text = .)
  return(txt)
}

# # plot transforms
# f = '~/github/dissertation/fig/routing/shortestPath_transforms.pdf'
# pdf(f)
# plot(x, col=fields::two.colors(start='darkblue', end='red', middle='white', alpha=.5))
# cols = fields::tim.colors(length(transforms))
# for (i in 1:length(transforms)){ # i=16
#   lines(spTransform(routes[[i]]$route, crs(epsg3857)), col=cols[i], lwd=2)
# }
# legend('bottomleft', as.character(transforms), col=cols, lwd=2)
# dev.off()
# system(sprintf('open %s', f))

# ui ----
ui <- fluidPage(
  useShinyjs(),

  navbarPage(
    "Conservation",
    tabPanel(
      "Routing", icon = icon('road'),
      
      fluidRow(
        helpText(HTML(renderMarkdown(text="**Instructions.** Click on a point in the tradeoff chart below to display the mapped route to the right and values below. 
                 Map is zoomable/pannable and start/end points clickable.
                 Eventually you'll be able to create arbitrary start/end points for tradeoff analysis of conservation routing.")))),
      
      hr(),
      
      fluidRow(
        column(
          6,
          selectInput(
            'sel_industry', 'Industry Profile:',
            c('Oil Tanker'='rt_oil','Shipping Tanker'='rt_ship','Cruise Ship'='rt_cruise')),
          hidden(helpText(
            id='hlp_industry', 
            'Eventually these industry profiles will enable customized species responses depending on types of impact.'))),
        column(
          6, 
          selectInput(
            'sel_extent', 'Study Area:',
            extents %>% 
              filter(routing==T) %$% 
              split(.[,c('name','code')], country) %>%
              lapply(function(x) setNames(x$code, x$name))),
          
          selectInput(
            'sel_spp', 'Species:',
            data_frame(
              group='All', name='Composite species risk', code='ALL') %>%
              rbind(
                spp %>%
                  arrange(group, name) %>%
                  select(group, name, code)) %$% 
              split(.[,c('name','code')], group) %>%
              lapply(function(x) setNames(x$code, x$name))))),
      
      fluidRow(
        column(
          4,  # 
          hidden(textInput('txt_transform', 'selected transform', value = d_transform)),
          div(
            style = "height:400px; background-color:#f5f5f5",
            ggvisOutput("ggvis"))),
      
        column(
          8,
          leafletOutput("mymap", height='400px') #,
          #p(),
          #actionButton("recalc", "Reroute"))
          )),
      
      hr(),
      
      fluidRow(
        column(
          4,
          #DT::dataTableOutput('dt_tbl'))
          p(
            "Tradeoff selected: ",
            div(id = 'txt_tradeoff', HTML(cat_txt_tradeoff(d_transform)))
            )),
        column(
          8,
          helpText(HTML(renderMarkdown(
            text='**Background.** Welcome to Conservation Routing! Least cost routes are calculated based on different cost surfaces. 
            The initial cost surface applies a constant value for all cells resulting in a Euclidean path
            with the minimum distance. This path would be the least costly to industry, making it 
            the reference point (_min(dist)_) to which other routes are compared. Other paths are calculated
            by applying transformations to the conservation risk surface, which is calculated as the cumulative species score 
            weighted by extinction risk. The summation of conservation risk values traversed by the path determines
            the conservation score. The reference point (_min(cost)_) is subtracted from all values.'))))),
      
      hr(),
      
      fluidRow(
        column(
          4,
          # speceis weight plot
          plotOutput('plt_spp_weights')),
        column(
          8,
          helpText(HTML(renderMarkdown(
            text="**Conservation Risk**. The conservation risk surface is a cumulative species hotspot map that
            provides the cost against which the least-cost path is routed. This surface is constructed from the
            individual species distribution maps which are weighted by the species' extinction risk before each
            pixel is summed. Finally the resistance surface is divided by the maximum value to normalize it to 
            a maximum of 1."))),
          withMathJax(helpText(
            "More formally, each pixel (\\(i\\)) across \\(n\\) species (\\(s\\)) is summed by its 
            relative density (\\(z_i\\)), which is based on the pixel values' (\\(x_i\\)) deviation (\\(\\sigma_s\\)) from 
            mean density (\\(\\mu_s\\)), and multiplying by the species' extinction risk weight (\\(w_s\\)):
            $$
            z_{i,s} = \\frac{ x_{i,s} - \\mu_s }{ \\sigma_s } \\\\
            Z_i = \\frac{ \\sum_{s=1}^{n} z_{i,s} * w_s }{ n }
            $$"))))),
    
    tabPanel(
      "Siting", icon = icon('map-marker'),
      helpText('Coming soon...'))))

# server ----
server <- function(input, output, session) {

#   points <- eventReactive(input$recalc, {
#     # lr: 48.282911, -121.955969 (lon, lng)
#     # ul: 54.552710, -134.326578
#     # lon: 55 - 48 = 7
#     # lng: -121 - -134 = 13
#     x = c(-134, -121)
#     y = c(48, 55)
#     n = 10
#     cbind(rnorm(n) * diff(x)/2 + mean(x), rnorm(n) * diff(y)/2 + mean(y))
#   }, ignoreNULL = FALSE)
#   lns_gcs <- eventReactive(input$recalc, {
#     get_route(input$bw_adjust)
#   }, ignoreNULL = FALSE)
   
#   get_route <- eventReactive(input$transform, {
#     routes[[input$transform]][['route_gcs']]
#   })
  
  # chart ----
  d_hover <- function(x) {
    if(is.null(x)) return(NULL)
    row <- d[d$transform == x$transform, ]
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }
  
  d_click <- function(x) {
    if(is.null(x)) return(NULL)
    
    # update hidden text to update map, visible text to show details of selected point
    updateTextInput(session, 'txt_transform', value = x$transform)
    shinyjs::text(id = "txt_tradeoff", text = cat_txt_tradeoff(x$transform))
    
    # highlight selected point
    i <- which(d$transform == x$transform)
    isolate({
      values$stroke <- rep(pt_cols[['off']], nrow(d))
      values$stroke[i] <- pt_cols[['on']]})
    
    # return popup of values in HTML
    row <- d[d$transform == x$transform, ]
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }
  
  # initialize points
  #pt_cols = c(on='blue', off='slategray')
  pt_cols = c(on='blue', off='slategray')
  d_stroke = rep(pt_cols[['off']], nrow(d))
  d_stroke[d$transform == d_transform] = pt_cols[['on']]
  values <- reactiveValues(stroke=d_stroke) # values = data.frame(stroke=d_stroke)
  
  d_vis <- reactive({
    d %>%
      ggvis(~conservation, ~industry, key := ~transform) %>% # 
      layer_paths(stroke := 'slategray') %>% 
      layer_points(stroke := ~values$stroke, strokeWidth := 3) %>% # 
      scale_numeric('x', reverse=T) %>%
      scale_numeric('y', reverse=T) %>% 
      add_tooltip(d_hover, 'hover') %>% 
      add_tooltip(d_click, 'click') %>%
      add_axis('x', title = 'risk to species (risk)') %>% 
      add_axis('y', title = 'cost to industry (km)') %>% 
      hide_legend('stroke') %>%
      #set_options(width = 300, height = 400) %>%
      set_options(width = "auto", height = "auto", resizable=FALSE)
  })
  d_vis %>% bind_shiny("ggvis") 

  # TODO: add shinyjs::text
  
#   # table ----
#   output$dt_tbl = DT::renderDataTable({
#     datatable(d, selection='single', filter='none', options = list(
#       pageLength = nrow(d),
#       #initComplete = JS('function(setting, json) { alert("done"); }')
#       initComplete = JS(
#         'function(setting, json) { 
#         var trs = $("#dt_tbl tbody tr");
#         $(trs[1]).addClass("selected");
#         alert("done"); }')
#       ))
#   }, server = F)
#   # TODO: add shinyjs Click behavior to table to deselect/select
#   i = 
#   $(".selected").removeClass('selected')
    
  # http://rstudio.github.io/DT/shiny.html#row-selection
  # http://datatables.net/examples/api/select_single_row.html
  # var trs = $('#example tbody tr')
  # $(trs[1]).addClass('selected')
  # http://stackoverflow.com/questions/24750623/select-a-row-from-html-table-and-send-values-onclick-of-a-button
  # http://shiny.rstudio.com/articles/selecting-rows-of-data.html

  # map ----
  get_bbox <- reactive({
    # if have 2 or more points for selected extent
    if (nrow(filter(pts, extent == input$sel_extent)) >= 2){
      # return bbox of points
      pts %>% 
        as.data.frame() %>%
        filter(extent == input$sel_extent) %>%
        #filter(extent == 'British Columbia, Canada') %>%
        SpatialPointsDataFrame(
          data=., coords = .[,c('lon','lat')], proj4string = CRS(epsg4326)) %>%
        extent() %>%
        c(.@xmin, .@ymin, .@xmax, .@ymax) %>%
        .[-1] %>% unlist() %>%
        return()
    } else {
      # return bbox of extent
      extents %>%
        filter(code == input$sel_extent) %>%
        select(lon_min, lat_min, lon_max, lat_max) %>%
        as.numeric() %>%
        return()
    }
  })

  x = (r / cellStats(r,'max'))
  x_rng = c(cellStats(x,'min'), cellStats(x,'max'))
  
  output$mymap <- renderLeaflet({
    b = get_bbox()
    sp_code = input$sel_spp #sp_code = 'HP'
    sp_fld  = ifelse(
      sp_code == 'ALL',
      'ALL_z',
      sprintf('%s_d', sp_code))
    sp_vals = spp_ply@data[,sp_fld]
    sp_rng  = range(sp_vals)
    sp_pal  = colorNumeric(
      palette = 'Reds',
      domain = sp_rng)
    if (sp_code == 'ALL'){
      sp_popup = spp_ply@data[,'ALL_popup']  
    } else {
      sp_popup = sprintf('<strong>%s: </strong> %g', sp_fld, sp_vals)
    }
    sp_title = ifelse(
      sp_code == 'ALL',
      sprintf('Species risk<br> score'),
      sprintf('%s<br> density (#/km<sup>2</sup>)', spp_names[sp_code]))
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% 
#       addRasterImage(
#         x, opacity = 0.8, project = F, group='c',
#         colors = colorNumeric(palette = 'Reds', domain = x_rng, na.color = "#00000000", alpha = TRUE)) %>%
      # TODO: add popup, xfer raster vals to spp_ply, on fly composite to tmp_val
      addPolygons(
        data = spp_ply, group='Species',
        stroke = FALSE, smoothFactor = 0.9, fillOpacity = 0.9,
        color = sp_pal(sp_vals),
        popup = sp_popup) %>%
      addCircleMarkers(
        ~lon, ~lat, radius=6, color='blue', data=pts, layerId=~name, group='Points',
        popup = ~sprintf('<b>%s</b><br>%0.2f, %0.2f', name, lon, lat)) %>%
      addCircleMarkers(
        lng=~lon, lat=~lat, data=ports, layerId=~port, group='Ports',
        radius=~pt_radius*10, color='purple', stroke=F, fillOpacity = 0.5,
        popup = ~sprintf(
          '<b>%s</b><br>
          location<br>
          - longitude: %0.2f<br>
          - latitude: %0.2f<br>
          cargo (x1000 tons) handled, 2011<br>
          - domestic: %s<br>
          - international: %s<br>
          - total: %s', 
          port, lon, lat, 
          formatC(int_ktons, 1, format='f', big.mark = ','), 
          formatC(dom_ktons, 1, format='f', big.mark = ','), 
          formatC(sum_ktons, 1, format='f', big.mark = ','))) %>%
       addLegend(
         'bottomleft', 
         pal = sp_pal,values = sp_rng, title = sp_title) %>%
      # Layers control
      addLayersControl(
        #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c('Species', 'Points', 'Ports'),
        options = layersControlOptions(collapsed=T)
      ) %>%
      fitBounds(b[1], b[2], b[3], b[4])
  })
  
  #observeEvent(input$map1_marker_click, {
#   observeEvent(input$txt_transform, {
#     # add least cost path
#     i = which(sapply(routes, function(z) z$transform) == input$txt_transform)
#     leafletProxy('mymap') %>%
#       removeShape(c('routes')) %>% 
#       addPolylines(data = routes[[i]][['route_gcs']], layerId='routes', color='blue') # , color='purple', weight=3)
#   })
    
  observeEvent(input$sel_industry, {
    if (input$sel_industry != 'rt_oil'){
      shinyjs::show('hlp_industry')
    } else {
      shinyjs::hide('hlp_industry')
    }
  })

  output$plt_spp_weights = renderPlot({
    
    g = ggplot(aes(x=srank, y=weight_logit, group=1), data=spp_weights) +
      geom_point() + geom_line(col='slategray') +
      annotate('text', x=spp_labels$srank, y=spp_labels$weight_logit+0.01, label=spp_labels$label)
    print(g)
    
  })
  
}

shinyApp(ui, server)

# deploy by copying over ssh to the MGEL server
# system(sprintf('rsync -r --delete %s bbest@shiny.env.duke.edu:/shiny/bbest/', app_dir))
# system(sprintf("ssh bbest@shiny.env.duke.edu 'chmod g+w -R /shiny/bbest/%s'", basename(app_dir)))
# app_dir=~/github/consmap; rsync -r --delete $app_dir bbest@shiny.env.duke.edu:/shiny/bbest/

# deploy by copying over ssh to the NCEAS server
# system(sprintf('rsync -r --delete %s bbest@fitz.nceas.ucsb.edu:/srv/shiny-server/', app_dir))
# system(sprintf("ssh bbest@fitz.nceas.ucsb.edu 'chmod g+w -R /srv/shiny-server/%s'", basename(app_dir)))

# deploy to shinyapps
# shinyapps::appDependencies()
# p = devtools::session_info()
# cat(as.character(as.list(i)[[1]]), file='prep/R_environment.txt', sep='\n')
# p = p %>%
#   as.list() %>% 
#   .[[2]] %>%
#   as.data.frame() %>%
#   mutate(
#     github = str_match(source, "Github \\((.*)\\)")[,2],
#     force = F) %>%
#   arrange(!is.na(github), package, github)
# p = rbind(
#   p %>%
#     filter(!package %in% c('rasterfaster','leaflet')),
#   p[p$package=='rasterfaster',],
#   p[p$package=='leaflet',] %>%
#     mutate(
#       force=T))
# write_csv(p, 'prep/R_packages.csv')
# # see prep/install_packages.R for how the above session_info() gets used
# deployApp(appName='consmap') # to https://bdbest.shinyapps.io/consmap

