# Originally written by Hadley Wickham, copied and modified from https://github.com/hadley/data-baby-names/ --------

library(RCurl)
library(plyr)
library(XML)

save_year <- function(year) {
  url <- "https://www.ssa.gov/cgi-bin/popularnames.cgi"
  data <- postForm(url, style = "post", 
                   "number" = "p", "top" = "1000", "year" = year,
                   verbose = TRUE) 
  writeLines(data, paste("original/", as.character(year), ".html", sep=""))
}

years <- 1880:2016
l_ply(years, save_year)