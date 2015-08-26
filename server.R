# server ----

#server <- function(input, output, session) {
shinyServer(function(input, output, session) {

  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
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
  
  
  progress$set(message = "Loading map", value = 0.3)
  # TODO: move rendering of spp_ply out of main map so update overlay like route 
  #       and rm warning "Attempting to set progress, but progress already closed." when selecting new spp
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
  
  #observeEvent(input$map1_marker_click, {
  observeEvent(input$txt_transform, {

    # progress bar
    if (!exists('progress')){
      progress <- shiny::Progress$new()
    }
    progress$set(message = "Adding route", value = 0.6)
    
    
    # add least cost path
    i = which(sapply(routes, function(z) z$transform) == input$txt_transform)
    leafletProxy('mymap') %>%
      removeShape(c('route')) %>% 
      addPolylines(data = routes[[i]][['route_gcs']], layerId='route', group='Route', color='blue') # , color='purple', weight=3)
    
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

