install.packages("tidyverse")
library(tidyverse)

whr <- read_csv("2016.csv")
View(whr)

str(whr)

whr$`Happiness Score`

# vectors and data types --------------------------------------------------


vec1 <- c(3, 6, 1, 10) #numeric vector
vec2 <- c("blue", "orange", "yellow", "gray") #character vector
vec3 <- c(TRUE, FALSE, TRUE, FALSE) #logical vector
vec4 <- c(NA, NA, NA, NA) #NA -- not available
NA == 1 
NA == "NA" 
NA == NA

RT <- c(5.34, 7.34, 6.45, NA)

c(3, TRUE, "blue") #data types transformation
c(3, TRUE, FALSE, 5) #data types transformation

character > numeric > logical

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
# lines(density(whr$`Happiness Score`))

summary(whr$`Happiness Score`)


# filtering rows and selection columns ----------------------------------------------------------

#logical operators
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


# NA ----------------------------------------------------------------------

whr2 <- read_csv("2016-2.csv")

str(whr2$`Happiness Score`)

mean(whr2$`Happiness Score`, na.rm = TRUE)
sd(whr2$`Happiness Score`, na.rm = TRUE)
sum(whr2$`Happiness Score`, na.rm = TRUE)

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

whr2 %>% 
  ggplot(aes(x = `Happiness Score`)) +
  geom_histogram(bins = 20) +
  theme_minimal()

whr2 %>% 
  ggplot(aes(x = `Happiness Score`)) +
  geom_density(fill = "green", alpha = 0.5,
               color = "pink", size = 5, linetype = "dashed") +
  theme_minimal()
  
whr2 %>% 
  filter(Region == "North America" | Region == "Latin America and Caribbean") %>% 
  ggplot(aes(y = `Trust (Government Corruption)`, fill = Region)) +
  geom_boxplot() +
  theme_minimal()


# correlation ------------------------------------------------------------

mean(whr$`Trust (Government Corruption)`)
median(whr$`Trust (Government Corruption)`)

summary(whr$`Trust (Government Corruption)`)
install.packages("skimr")
# library(skimr)

skimr::skim(whr$`Trust (Government Corruption)`)

whr %>% 
  ggplot(aes(x = `Trust (Government Corruption)`)) +
  geom_density() +
  theme_minimal()

whr %>% 
  ggplot(aes(sample = `Trust (Government Corruption)`)) +
  geom_qq() +
  geom_qq_line() +
  theme_minimal()

skimr::skim(whr$Generosity)

whr %>% 
  ggplot(aes(x = Generosity)) +
  geom_density() +
  theme_minimal()

whr %>% 
  ggplot(aes(sample = Generosity)) +
  geom_qq() +
  geom_qq_line() +
  theme_minimal()

whr %>% 
  ggplot(aes(x = Generosity, y = `Trust (Government Corruption)`)) +
  geom_point(size = 1, color = "navy") +
  theme_minimal()

cor.test(whr$`Trust (Government Corruption)`, whr$Generosity,
         conf.level = 0.95, method = "spearman")


  
# r = 0.03, p = 0.0045, sample size ~ 100000
# 
# r = 0.03, p = 0.3545, sample size: 10-100, false negative
#                                     ~ 10000, absence in GP
# 
# r = 0.34, p =  0.0045, sample size: 10-100
#   
# r = 0.34, p = 0.3545, sample size: 10-100 (absence in GP)


library(tidyverse)

whr <- read_csv("2016.csv")

table(whr$Region)

whr %>% 
  select(Country, Region, `Economy (GDP per Capita)`) %>% 
  View()
  

whr %>% 
  filter(Region == "Western Europe" | Region == "Sub-Saharan Africa") %>% 
  select(Country, Region, `Economy (GDP per Capita)`) -> whr_test

# <-, = -- the same


# t test ------------------------------------------------------------------

install.packages("skimr")
library(skimr)

# skim(whr_test$`Economy (GDP per Capita)`) -- the same as below

whr %>% 
  filter(Region == "Western Europe" | Region == "Sub-Saharan Africa") %>%
  select(`Economy (GDP per Capita)`) %>% 
  skim()

whr_test %>% 
  ggplot(aes(x = `Economy (GDP per Capita)`)) +
  geom_density(color = "navy", linewidth = 0.5) +
  theme_minimal()

whr_test %>% 
  ggplot(aes(x = `Economy (GDP per Capita)`)) +
  geom_histogram(color = "navy", linewidth = 0.5) +
  theme_minimal()

whr_test %>% 
  ggplot(aes(sample = `Economy (GDP per Capita)`)) +
  geom_qq(color = "navy", size = 0.5) +
  geom_qq_line() +
  theme_minimal()

whr_test %>% 
  wilcox.test(`Economy (GDP per Capita)` ~ Region, ., paired = FALSE,
              conf.level = 0.95)

options(scipen=999)




