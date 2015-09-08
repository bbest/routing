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
#ui <- fluidPage(
shinyUI(fluidPage(
  useShinyjs(),
  
  navbarPage(
    "Conservation",
    tabPanel(
      "Routing", icon = icon('road'),
      
      fluidRow(
        helpText(HTML(renderMarkdown(text="**Instructions.** Click on a point in the tradeoff chart below to display the mapped route to the left and values below. 
                                     Map is zoomable/pannable and begin/end points changable.")))),
      
      hr(),
      
      fluidRow(
        column(
          3, 
          selectInput(
            'sel_extent', 'Study Area:',
            extents %>% 
              filter(routing==T) %$% 
              split(.[,c('name','code')], country) %>%
              lapply(function(x) setNames(x$code, x$name)),
            selected=default_study),
          
          selectInput(
            'sel_spp', 'Species:',
            data_frame(
              group='All', name='Composite species risk', code='ALL') %>%
              rbind(
                spp %>%
                  arrange(group, name) %>%
                  select(group, name, code)) %$% 
              split(.[,c('name','code')], group) %>%
              lapply(function(x) setNames(x$code, x$name)))),
        column(
          3, 
          selectInput(
            'sel_beg', 'Begin:',
            nodes  %$% 
              split(.[,c('name','code')], group) %>%
              lapply(function(x) setNames(x$code, x$name)),
            selected=default_beg),
          hidden(textInput('txt_beg', 'point begin code', value=default_beg)),
          
          selectInput(
            'sel_end', 'End:',
            nodes  %$% 
              split(.[,c('name','code')], group) %>%
              lapply(function(x) setNames(x$code, x$name)),
            selected=default_end),
          hidden(textInput('txt_end', 'point end code', value=default_end))),
        column(
          2, br(), br(), br(),
          actionButton('btn_reroute', 'Reroute')),
        column(
          4, br(), br(),
          selectInput(
            'sel_industry', 'Industry Profile:',
            c('Oil Tanker'='rt_oil','Shipping Tanker'='rt_ship','Cruise Ship'='rt_cruise')),
          hidden(helpText(
            id='hlp_industry', 
            'Eventually these industry profiles will enable customized species responses depending on types of impact.')))),
      
      fluidRow(
        column(
          8,
          leafletOutput('map', height='400px')),
        column(
          4,  # 
          hidden(textInput('txt_transform', 'selected transform', value = default_transform)),
          div(
            style = "height:400px; background-color:#f5f5f5",
            ggvisOutput("ggvis")))),
      
      hr(),
      
      fluidRow(
        column(
          8,
          helpText(HTML(renderMarkdown(
            text='**Background.** Welcome to Conservation Routing! Least cost routes are calculated based on different cost surfaces. 
            The initial cost surface applies a constant value for all cells resulting in a Euclidean path
            with the minimum distance. This path would be the least costly to industry, making it 
            the reference point (_min(dist)_) to which other routes are compared. Other paths are calculated
            by applying transformations to the conservation risk surface, which is calculated as the cumulative species score 
            weighted by extinction risk. The summation of conservation risk values traversed by the path determines
            the conservation score. The reference point (_min(cost)_) is subtracted from all values.')))),
        column(
          4,
          #DT::dataTableOutput('dt_tbl'))
          p(
            "Tradeoff selected: ",
            uiOutput('txt_tradeoff')
          ))),
      
      hr(),
      
      fluidRow(
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
            $$"))),
        column(
          4,
          # speceis weight plot
          plotOutput('plt_spp_weights')))),
    
    tabPanel(
      "Siting", icon = icon('map-marker'),
      helpText('Coming soon...')))))