library(dslabs)
library(tidyverse)
library(dplyr)

data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea","Germany")) %>%
  select(country,year,fertility)
head(tidy_data)
tidy_data %>% 
  ggplot(aes(year,fertility,color=country)) +
  geom_point()

path <- system.file("extdata",package = "dslabs")
filename <- file.path(path,"fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data,country,'1960':'1967')
