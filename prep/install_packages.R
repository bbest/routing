# install R packages necessary to run Shiny app
# TODO: check for essential version of leaflet

# get packages to run installs
for (pkg in c('readr','stringr','dplyr','devtools')){
  if (!require(pkg, character.only=T)){
    cat(sprintf('----\n%s: INSTALLING from CRAN\n...\n', pkg))
    install.packages(pkg)
  }  else {
    cat(sprintf('----\n%s: already installed\n', pkg))
  }
  library(pkg, character.only=T)
}

# read output from as.list(devtools::session_info())[[2]]
p = read_csv('https://raw.githubusercontent.com/bbest/consmap/master/prep/R_packages.csv')

# iterate through packages
#   installing from CRAN or Github as needed
for (i in 1:nrow(p)){
  pkg = p$package[i]
  gh  = p$github[i]
  if (!require(pkg, character.only=T)){
    if (is.na(gh)){
      cat(sprintf('----\n%s: INSTALLING from CRAN\n...\n', pkg))
      install.packages(pkg)
    } else {
      cat(sprintf('----\n%s: INSTALLING from GITHUB (%s)\n...\n', pkg, gh))
      install_github(gh)
    }
  } else {
    cat(sprintf('----\n%s: already installed\n', pkg))
  }
}

# run Shiny app from Github
shiny::runGitHub('bbest/consmap')
