install.packages("tidyverse")
library(tidyverse)
whr <- read_csv("https://raw.githubusercontent.com/elenary/StatsForDA/main/2016.csv")
View(whr)

hist(whr$`Happiness Score`)

