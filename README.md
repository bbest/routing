# Conservation Mapping (consmap)
Conservation Mapping handled spatially as a routing or siting problem to minimize impact on endangered marine animals, for PhD dissertation at Duke.

Here's an example interaction of choosing different routes based on a tradeoff between conservation of species (by risk of encounter) versus cost to industry (in km extra distance):

![routing animation](https://raw.githubusercontent.com/bbest/consmap/master/prep/gif/routing_animation.gif)

This is very much a work in progress, and I'll eventually host the Shiny app on a [Duke Marine Geospatial Laboratory](http://mgel.env.duke.edu) server, but meanwhile you can preview the app at https://bdbest.shinyapps.io/consmap.

## Acknowledgements

Many thanks to:
- RStudio and especially [@jcheng5](http://github.com/jcheng5) for [Shiny](http://shiny.rstudio.com/) and the integrated [leaflet](https://rstudio.github.io/leaflet/) package, now with [raster image](https://rstudio.github.io/leaflet/raster.html) support.
