for (pkg in c('readr','stringr','dplyr','devtools')){
  if (!require(pkg)) install.packages(pkg)
  require(pkg)
}
p = read_csv('https://raw.githubusercontent.com/bbest/consmap/master/prep/R_packages.csv') %>%
  mutate(
    github = str_match(source, "Github \\((.*)\\)")[,2]) %>%
  arrange(!is.na(github), package, github)
for (i in 1:nrow(p)){
  pkg = p$package[i]
  if (!require(pkg)){
    if (is.na(p$github[i])){
      install.packages(pkg)
    } else {
      install_github(pkg)
    }
  } 
}