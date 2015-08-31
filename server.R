# server ----

# TODO: 
# - zoom to recalculated begin/end extent, update highlighted points on map, see get_bbox
# - add interactive point, see input$map_click, save CODE as sprintf('%0.4f,%0.4f', lon, lat)
# - since routes made global and exists(routes), check to/fro of routes by setting as another attr
# - add to port popup option to make beg/end pt of route

shinyServer(function(input, output, session) {

  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  # default route begin/end select values
  sel_beg_now <<- 'SHG'
  sel_end_now <<- 'KTM'
  
  run_routing = function(lonlat1, lonlat2){

#     xy1 = spTransform(lonlat1, crs(epsg3857))
#     xy2 = spTransform(lonlat2, crs(epsg3857))
    
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
    
    progress <- shiny::Progress$new()
    routes = list()
    for (i in 1:length(transforms)){ # i=8
      
      # progress bar
      progress$set(message = sprintf('routing transform %d (of %d): %s', i, length(transforms), transforms[i]), value = (i-0.5)/length(transforms))
      
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
    attr(routes, 'pt_beg') = lonlat[1]
    attr(routes, 'pt_end') = lonlat[2]
    attr(routes, 'nm_beg') = input$sel_beg
    attr(routes, 'nm_end') = input$sel_end
    
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
    print('global.R run_routing() d attr finished')
    
    # save if doesn't exist (eg redoing)
    rdata = sprintf('%s/data/routes/routes_%s-%s.Rdata', app_dir, input$sel_beg, input$sel_end)
    if (!file.exists(rdata)){
      print('global.R run_routing() rdata save')
      save(routes, file = rdata)
    }
    
    # finish progress
    progress$close()
    
    return(routes)
  }
  
  get_routes = reactive({
    
    # make reactive to btn_reroute, otherwise isolate
    input$btn_reroute
    
    print('get_routes()')
    isolate({
      
      rdata = sprintf('%s/data/routes/routes_%s-%s.Rdata', app_dir, input$sel_beg, input$sel_end)
      if ((!exists('routes') & file.exists(rdata)) | 
          (input$sel_beg!=sel_beg_now | input$sel_end!=sel_end_now & file.exists(rdata))){
        
        # load prerouted
        print('get_routes(): loading prerouted')
        load(rdata)
        stopifnot(exists(c('routes')))
        routes <<- routes
        
      } else if (exists('routes') & input$sel_beg==sel_beg_now & input$sel_end==sel_end_now){
        
        # skip since already loaded
        print('get_routes(): skipping')
        
      } else {
        
        print('get_routes(): run_routing()')
        
        # run routing ----
        lonlat1 = SpatialPoints(
          nodes %>%
            filter(code==input$sel_beg) %>%
            select(lon, lat) %>%
            as.data.frame(), crs(epsg4326))
        lonlat2 = SpatialPoints(
          nodes %>%
            filter(code==input$sel_end) %>%
            select(lon, lat) %>%
            as.data.frame(), crs(epsg4326))
        routes <<- run_routing(lonlat1, lonlat2)
      }
      
      updateTextInput(session, 'txt_transform', value = default_transform)
      
      # save routing global params
      sel_beg_now <<- input$sel_beg
      sel_end_now <<- input$sel_end
    })
    return(routes)
  })
  
  txt_tradeoff = reactive({
    
    # depends on txt_transform
    input$txt_transform
    
    d = attr(get_routes(), 'd')
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
  })

#   points <- eventReactive(input$btn_reroute, {
#     # lr: 48.282911, -121.955969 (lon, lng)
#     # ul: 54.552710, -134.326578
#     # lon: 55 - 48 = 7
#     # lng: -121 - -134 = 13
#     #     x = c(-134, -121)
#     #     y = c(48, 55)
#     #     n = 10
#     #     cbind(rnorm(n) * diff(x)/2 + mean(x), rnorm(n) * diff(y)/2 + mean(y))
#     isolate({
#       sprintf('%s to %s', input$sel_beg, input$sel_end)
#     })
#   })
  
  # chart ----
  d_hover <- function(x) {
    if(is.null(x)) return(NULL)
    d = attr(get_routes(), 'd')
    row <- d[d$transform == x$transform, ]
    paste0(names(row), ": ", format(row), collapse = "<br />")
  }
  
  d_click <- function(x) {
    if(is.null(x)) return(NULL)
    
    # update hidden text to update map
    updateTextInput(session, 'txt_transform', value = x$transform)

    # return values of selected transform
    d = attr(get_routes(), 'd')
    i <- which(d$transform == x$transform)
    paste0(names(d[i,]), ": ", format(d[i,]), collapse = "<br />")
  }
  
  # point colors for tradeoff plot
  pt_cols = c(on='blue', off='slategray')
  
  # ggvis tradeoff chart
  d_vis <- reactive({
    d = attr(get_routes(), 'd')
    d_stroke = rep(pt_cols[['off']], nrow(d))
    d_stroke[d$transform == input$txt_transform] = pt_cols[['on']]
    vals <- reactiveValues(stroke=d_stroke)
    
    d %>%
      ggvis(~conservation, ~industry, key := ~transform) %>% # 
      layer_paths(stroke := 'slategray') %>% 
      layer_points(stroke := ~vals$stroke, strokeWidth := 3) %>% # 
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
#     if (nrow(filter(pts, extent == input$sel_extent)) >= 2){
#       # return bbox of points
#       pts %>% 
#         as.data.frame() %>%
#         filter(extent == input$sel_extent) %>%
#         #filter(extent == 'British Columbia, Canada') %>%
#         SpatialPointsDataFrame(
#           data=., coords = .[,c('lon','lat')], proj4string = CRS(epsg4326)) %>%
#         extent() %>%
#         c(.@xmin, .@ymin, .@xmax, .@ymax) %>%
#         .[-1] %>% unlist() %>%
#         return()
#     } else {
      # return bbox of extent
      extents %>%
        filter(code == input$sel_extent) %>%
        select(lon_min, lat_min, lon_max, lat_max) %>%
        as.numeric() %>%
        return()
    # }
  })

  x = (r / cellStats(r,'max'))
  x_rng = c(cellStats(x,'min'), cellStats(x,'max'))
  
  # progress bar
  progress$set(message = "Loading map", value = 0.3)
  # TODO: move rendering of spp_ply out of main map so update overlay like route 
  #       and rm warning "Attempting to set progress, but progress already closed." when selecting new spp
  output$map <- renderLeaflet({
    d = attr(get_routes(), 'd')
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
        lng=~lon, lat=~lat, data=filter(nodes, group=='Ports'), layerId=~name, group='Ports',
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
          name, lon, lat, 
          formatC(int_ktons, 1, format='f', big.mark = ','), 
          formatC(dom_ktons, 1, format='f', big.mark = ','), 
          formatC(sum_ktons, 1, format='f', big.mark = ','))) %>%
      addCircleMarkers(
        lng=~lon, lat=~lat, data=filter(nodes, group=='Oceanic Access'), layerId=~name, group='Oceanic Access',
        radius=6, color='red', stroke=F, fillOpacity = 0.5,
        popup = ~sprintf(
          '<b>%s</b><br>
          location<br>
          - longitude: %0.2f<br>
          - latitude: %0.2f', 
          name, lon, lat)) %>%
       addLegend(
         'bottomleft', 
         pal = sp_pal,values = sp_rng, title = sp_title) %>%
      # Layers control
      addLayersControl(
        #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c('Route', 'Points', 'Ports', 'Oceanic Access', 'Species'),
        options = layersControlOptions(collapsed=T)
      ) %>%
      fitBounds(b[1], b[2], b[3], b[4])
  })
  
  # When map is clicked, show a popup, eventually add new points
  observe({
    
    leafletProxy('map') # %>% clearPopups()
    #event <- input$map_shape_click
    click <- input$map_click
    
    if (is.null(click)) return()
    
    isolate({
      leafletProxy('map') %>% 
        addPopups(click$lng, click$lat, 
                  sprintf('map_shape_click: %0.3f, %0.3f', click$lng, click$lat), 
                  layerId = 'click')
    })
  })
  
  
  
  # update route
  observeEvent({
    input$txt_transform
    get_routes()}, {
    # TODO: make reactive when using new route, even if same value of input$txt_transform

    # progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Getting route", value = 0.6)
  
    # add least cost path
    i = which(sapply(get_routes(), function(z) z$transform) == input$txt_transform)
    progress$set(message=sprintf('Getting route i=%d', i), value = 0.65)
    d = attr(get_routes(), 'd')
    
    progress$set(message = "Mapping route", value = 0.7)
    leafletProxy('map') %>%
      removeShape(c('route')) %>% 
      addPolylines(data = get_routes()[[i]][['route_gcs']], layerId='route', group='Route', color='blue') # , color='purple', weight=3)
    
    # progress bar
    progress$close()   
  })
    
  
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
  
  # close initial progress bar
  progress$close()
})

#shinyApp(ui, server)

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

