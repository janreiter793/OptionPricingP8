#
# Find the date for each dataset, where there are most Non-NA entries. That is
# the dates where we have the largest number of available opening prices for the
# options.
#
library(tidyverse)
library(magrittr)

files <- c("C:/Users/janre/Documents/uni/8. Semester/Projekt/data/AMZN_options.csv",
           "C:/Users/janre/Documents/uni/8. Semester/Projekt/data/BRK-B_options.csv",
           "C:/Users/janre/Documents/uni/8. Semester/Projekt/data/GOOG_options.csv",
           "C:/Users/janre/Documents/uni/8. Semester/Projekt/data/GOOGL_options.csv",
           "C:/Users/janre/Documents/uni/8. Semester/Projekt/data/TSLA_options.csv")

conclusion <- data.frame()
for(file in files) {
  data <- read.csv(file)
  count.data.for.dates <-
    data %>%
    group_by(date) %>%
    summarise(count = sum(!is.na(open))) %>%
    as.data.frame

  # Find the row with the highest number of available data
  conclusion %<>% rbind(
    count.data.for.dates[which.max(count.data.for.dates$count), ] %>%
    mutate(date = as.Date(date))
  )
}

rownames(conclusion) <- c("AMZN", "BRK-B", "GOOG", "GOOGL", "TSLA")
conclusion
