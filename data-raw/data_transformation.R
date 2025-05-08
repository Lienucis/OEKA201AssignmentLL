library(readr)
wine <- read_csv("https://raw.githubusercontent.com/joernih/OEKA201Assignment/refs/heads/main/data-raw/csv/wine.csv")
usethis::use_data(wine, overwrite = TRUE)
