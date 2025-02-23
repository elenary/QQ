install.packages("tidyverse")
library(tidyverse)

whr <- read_csv("2016.csv")
View(whr)

str(whr)

whr$`Happiness Score`

# data structures (vectors) and data types --------------------------------------------------


vec1 <- c(3, 6, 1, 10) #numeric vector
vec2 <- c("blue", "orange", "yellow", "gray") #character vector
vec3 <- c(TRUE, FALSE, TRUE, FALSE) #logical vector
vec4 <- c(NA, NA, NA, NA) #NA -- not available
NA == 0 
NA == "NA" 
NA == NA

RT <- c(5.34, 7.34, 6.45, NA)

c(3, TRUE, "blue") #data types transformation
c(3, TRUE, FALSE, 5) #data types transformation

# character > numeric > logical

data1 <- data_frame(vec1, vec2, vec3, vec4, RT)

# descriptives ------------------------------------------------------------

whr$`Happiness Score`

str(whr$`Happiness Score`)

mean(whr$`Happiness Score`)
median(whr$`Happiness Score`)

sd(whr$`Happiness Score`)
min(whr$`Happiness Score`)
max(whr$`Happiness Score`)
range(whr$`Happiness Score`)

hist(whr$`Happiness Score`)

summary(whr$`Happiness Score`)


# filtering rows and selection columns ----------------------------------------------------------

#logical operators are:
# >, < , ==, !=
# = <- 
# & -- logical AND
# | -- logical OR
# ! -- logical not

a <- 5
b <- 7

a > b
a == b
a != b

whr %>% 
  filter(Region == "Latin America and Caribbean") %>% #filtering rows using logical expression
  select(`Happiness Score`) %>% #selection of a column
  pull() %>% #"pulling" of vector from data frame or converting column into a vector
  mean() 


whr %>% 
  filter(`Happiness Rank` <= 10) %>% 
  View()
  
whr %>% 
  filter(`Happiness Rank` > 147) %>% 
  View()

whr %>% 
  filter(`Happiness Rank` <= 10 | `Happiness Rank` > 147) %>% 
  select(Country, Region, `Happiness Rank`, `Happiness Score`) %>% 
  View()

# creation new columns ----------------------------------------------------

a <- 5
b <- 7

# ifelse(logical expression, what should R show if it is true, what if it is false)

ifelse(a > b, "a greater than b", "b greater than a")


whr %>% 
  mutate("Top10" = ifelse(`Happiness Rank` <= 10, "yes", "no"), 
         .after = `Happiness Rank`) %>% 
  View()

# ordering ----------------------------------------------------------------


whr %>% 
  arrange(Family) %>% #ordering by values of Family column
  select(Country, Region, `Happiness Rank`, `Happiness Score`, Family) %>% 
  View()

whr %>% 
  arrange(desc(Family)) %>% #ordering in descending order by values of Family column
  select(Country, Region, `Happiness Rank`, `Happiness Score`, Family) %>% 
  View()


# second dataset with NA loading ------------------------------------------

library(tidyverse)

whr2 <- read_csv("2016-2.csv")
# whr2 = read_csv("2016-2.csv") # alternative way to assign smth to variable name
# read_csv("2016-2.csv") -> whr2 # arrow direction always point to variable name

whr2 <- read_csv("https://raw.githubusercontent.com/elenary/StatsForDA/main/2016-2.csv")

View(whr2)

# NA processing----------------------------------------------------------------------
  
str(whr2$`Happiness Score`)

#descriprives stats

mean(whr2$`Happiness Score`, na.rm = TRUE)
median(whr2$`Happiness Score`, na.rm = TRUE)
sd(whr2$`Happiness Score`, na.rm = TRUE)
range(whr2$`Happiness Score`, na.rm = TRUE)

#other way to calculate descriptive stats inside pipe (with filtering data before)
whr2 %>% 
  filter(Region == "Western Europe" | Region == "Latin America and Caribbean") %>% 
  select(`Happiness Score`) %>% #still dataframe
  pull() %>% 
  mean(na.rm = TRUE)

whr2 %>% 
  filter(Region == "Western Europe") %>% 
  select(`Happiness Score`) %>% #still dataframe
  pull() %>% 
  mean(na.rm = TRUE)

whr2 %>% 
  drop_na() %>% #drop all rows if there is at least one NA
  select(`Happiness Score`) %>% #still dataframe
  pull() %>% 
  mean()

whr2 %>% 
  drop_na() %>%
  View()
  
whr2 %>% 
  filter(Region == "Middle East and Northern Africa") %>% 
  select(`Happiness Score`) %>% 
  pull() %>% 
  mean(na.rm = TRUE)

whr2 %>% 
  drop_na() %>% 
  View()

# data visualization ------------------------------------------------------

hist(whr$`Happiness Score`)

whr %>% 
  ggplot(aes(x = `Happiness Score`)) +
  geom_histogram() +
  theme_minimal()

whr %>% 
  ggplot(aes(x = `Happiness Score`)) +
  geom_histogram(bins = 20) +
  theme_minimal()

whr %>% 
  ggplot(aes(x = `Happiness Score`)) +
  geom_density(fill = "red", alpha = 0.3,
               color = "purple", linetype = "dashed", linewidth = 4) +
  theme_minimal()

whr2 %>% 
  ggplot(aes(x = `Happiness Score`)) +
  geom_density(fill = "green", alpha = 0.5,
               color = "pink", size = 5, linetype = "dashed") +
  theme_minimal()

whr %>% 
  filter(Region == "North America" | Region == "Latin America and Caribbean") %>% 
  ggplot(aes(y = `Trust (Government Corruption)`, fill = Region)) +
  geom_boxplot() +
  theme_minimal()
  
  
# correlation -------------------------------------------------------------

#looking at the variables disctributions
whr %>% 
  ggplot(aes(x = `Health (Life Expectancy)`)) +
  geom_density() +
  theme_minimal()

whr %>% 
  ggplot(aes(sample = `Health (Life Expectancy)`)) +
  geom_qq() +
  geom_qq_line()+
  theme_minimal()

whr %>% 
  ggplot(aes(x = `Economy (GDP per Capita)`)) +
  geom_density() +
  theme_minimal()

whr %>% 
  ggplot(aes(sample = `Economy (GDP per Capita)`)) +
  geom_qq() +
  geom_qq_line()+
  theme_minimal()

#descriprive stats of variables

summary(whr$`Economy (GDP per Capita)`)
summary(whr$`Health (Life Expectancy)`)

#scatterplot for assesing relationship between two variables

whr %>% 
  ggplot(aes(x = `Health (Life Expectancy)`, y = `Economy (GDP per Capita)`))+
  geom_point(color = "navy", size = 1) +
  theme_minimal()

#correlation test

options(scipen = 999) #switch off scientific notation to don't have an exponent in p-value

cor.test(whr$`Health (Life Expectancy)`, 
         whr$`Economy (GDP per Capita)`, conf.level = 0.95,
         method = "spearman")

  
  




  
  
  
  






