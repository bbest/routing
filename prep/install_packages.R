
# get packages to run installs
for (pkg in c('readr','stringr','dplyr','devtools')){
  if (!require(pkg, character.only=T)) install.packages(pkg)
  library(pkg, character.only=T)
}

# read output from as.list(devtools::session_info())[[2]]
p = read_csv('https://raw.githubusercontent.com/bbest/consmap/master/prep/R_packages.csv') %>%
  mutate(
    github = str_match(source, "Github \\((.*)\\)")[,2]) %>%
  arrange(!is.na(github), package, github)

# iterate through packages
#   installing from CRAN or Github as needed
for (i in 1:nrow(p)){
  pkg = p$package[i]
  gh  = p$github[i]
  if (!require(pkg)){
    if (is.na(gh)){
      cat(sprintf('----\nINSTALLING from CRAN: %s\n...\n', pkg))
      install.packages(pkg)
    } else {
      cat(sprintf('----\nINSTALLING from GITHUB: %s\n...\n', gh))
      install_github(gh)
    }
  } 
}

# run Shiny app from Github
shiny::runGitHub('bbest/consmap')
