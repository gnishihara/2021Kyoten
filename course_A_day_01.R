# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（A日程）
# 2021 / 09 / 10 (Day 1)
# Greg Nishihara

# パッケージのインストール
# install.packages("tidyverse")
# install.packages("lubridate")

# パッケージの読み込み #########################################################
library(tidyverse)
library(lubridate)

# 基本コーディング作業 #########################################################
# c(), list(), data.frame(), array()

a = c(20, 10, 30)
# b = list("A", "C", "Z")
b = c("A", "C", "Z")
data.frame(a, b)
array(data = c(1:9), dim = c(3, 3))
matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
array(data = c(1:3^3), dim = c(3, 3, 3))
1:9
seq(1, 9, by = 1)
seq(1, 9, by = 0.5)

# iris データの紹介 ############################################################
iris

# base R 操作 ##################################################################
colnames(iris) # 変数名
rownames(iris) # 行の名前

# $, [], [[]] ##################################################################

x = iris$Petal.Length
mean(x)
sd(x)
var(x)
sd(x) / sqrt(length(x)) 

iris[1, 1]

rownumber = 10
colnumber = 2
iris[rownumber, colnumber]

Z = array(data = c(1:3^3), dim = c(3, 3, 3))
Z[ 3 , 3, 3]

iris[[2]]

## tidyverse starts here #######################################################
# tibble 化 ####################################################################

iris2 = iris  |> as_tibble()

# mutate() #####################################################################

iris2 = iris2 |> mutate(Petal.Area = Petal.Length * Petal.Width) 
iris2 = iris2 |> mutate(Sepal.Area = Sepal.Length * Sepal.Width) 

iris2 |> mutate(Petal.Area = Petal.Length * Petal.Width,
                Sepal.Area = Sepal.Length * Sepal.Width) 

iris2 = iris2 |> 
  mutate(Petal.Area = Petal.Length * Petal.Width) |> 
  mutate(Sepal.Area = Sepal.Length * Sepal.Width) 

# select() #####################################################################

iris2 |> select(Petal.Area, Species)
iris2 |> select(!c(Petal.Area, Petal.Length, Petal.Width))
iris2 |> select(matches("(Petal)"))

# rename() #####################################################################

iris2 |> rename(PL = Petal.Length)
iris2 |> rename_with(toupper, matches("(Area)"))

# relocate() ###################################################################

iris2 |> relocate(Species, .before = Sepal.Length)
iris2 |> relocate(Petal.Area, Sepal.Area, .before = Species) 

# pull() #######################################################################

iris2 |> pull(Species)

# filter() #####################################################################

iris2 |> filter(str_detect(Species, "virginica"))
iris2 |> filter(str_detect(Species, "(virginica)|(versicolor)"))

iris2 |> filter(str_detect(Species, "setosa") & Petal.Length < 1)

iris2 |> filter(Petal.Length < 1.5)

iris2 |> filter(Petal.Length >= 1.5)
iris2 |> filter(!(Petal.Length < 1.5))

iris2 |> filter(between(Petal.Length, 2, 3))

# distinct() ###################################################################

iris2 |> distinct(Species)

bind_rows(iris2, iris2) |> arrange(Species, Petal.Length)

# slice() ######################################################################

iris2 |> slice(5:10)
iris2 |> slice_head(n = 10)
iris2 |> slice_tail(n = 3)

# arrange() ####################################################################

iris2 |> arrange(Petal.Length)
iris2 |> arrange(desc(Petal.Length))
iris2 |> arrange(desc(Petal.Length), desc(Petal.Width))

# group_by(), group_nest(), unnest() ###########################################

iris2 |> group_by(Species)

iris2 |> group_by(Species) |> summarise(Sepal.Length_mean = mean(Sepal.Length))

iris2 |> group_by(Species) |> 
  summarise(Sepal.Length_mean = mean(Sepal.Length),
            Sepal.Length_sd = sd(Sepal.Length))

iris2 |> group_by(Species) |> 
  summarise(across(matches("Petal"), list(m = mean, s = sd)))

iris2 |> group_by(Species) |> 
  summarise(across(matches("(Petal)|(Sepal)"), list(m = mean, s = sd)))

iris2 |> group_by(Species) |> 
  summarise(across(matches("(Petal)|(Sepal)"), 
                   list(mean = mean, sd = sd, samples = length, median = median,
                        minimum = min, maximum = max))) |> 
  pivot_longer(!Species, names_to = c("Part", "Measurement", "Statistic"),
               names_pattern = "(.*)\\.(.*)_(.*)",
               values_to = "value")

# separate(), unite() ##########################################################

iris2 |> group_by(Species) |> 
  summarise(across(matches("(Petal)|(Sepal)"), 
                   list(mean = mean, sd = sd, samples = length, median = median,
                        minimum = min, maximum = max))) |> 
  pivot_longer(!Species, names_to = "variable",
               values_to = "value") |> 
  separate(variable, into = c("Part", "Measurement", "Statistic")) |> 
  mutate(Genus = "Iris", .before = Species) |> 
  unite("GS", Genus, Species, sep = " ")


################################################################################ 

# read_csv() ###################################################################
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
URL2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTt_rEpmL7eWJtOc5MprXeihH4LTXkE9CyoLyPext6j3_9wbYAQAXQlEiSOs_Hse0hGLH6-zb6NJVKu/pub?output=csv"
URL3 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjodK7zDfhObB8OXSgfq0ZMJQh2d1Q__TFoSJ-6pPnsz50QE34xdJuJ5HQzR5RvyprB2GvsQYPw6Q4/pub?output=csv"
URL4 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRn0FUJhsKmyjTNPqahTZnhYvXP-ox6_ze1rwmMCFqpil3vyxsmfBgR-3NyGwfriH8z7xh4v38JemnU/pub?output=csv"

d1 = read_csv(URL1, skip = 1)
d2 = read_csv(URL2, skip = 1)
d3 = read_csv(URL3, skip = 1)
d4 = read_csv(URL4, skip = 1)

# lubridate::parse_datetime(), lubridate::ymd_hms() ################################

dall = bind_rows(d1, d2, d3, d4)

dall = dall |> 
  mutate(datetime = parse_datetime(`日付 時間, GMT+09:00`,
                               "%m/%d/%Y %I:%M:%S %p",
                               locale = locale("ja")),
         .before = `日付 時間, GMT+09:00`) |> 
  rename(light = matches("PAR"),
         wind = matches("風速"),
         gust = matches("突風"),
         mbar = matches("mbar")) |> 
  select(datetime, light, wind, gust, mbar)

# Remove trailing seconds ######################################################

dall = dall |> mutate(datetime = floor_date(datetime, unit = "minutes"))

# separate wind and PAR+mbar data ##############################################

winddata = dall |> select(datetime, wind, gust)
lightdata = dall |> select(datetime, light, mbar)

# write_csv() ##################################################################

winddata |> write_csv(file = "./winddata.csv")
lightdata |> write_csv(file = "./lightdata.csv")
 
################################################################################
# full_join(), inner_join() ####################################################

x = winddata |> slice_head(n = 100) |> slice_sample(n = 20)
y = lightdata |> slice_head(n = 100) |> slice_sample(n = 20)

full_join(x, y, by = "datetime")
inner_join(x, y, by = "datetime")

################################################################################
# Summary statistics
# Daily
winddata |> 
  mutate(date = as_date(datetime)) |> 
  group_by(date) |> 
  mutate(N = length(wind)) |> 
  ungroup() |> 
  filter(near(N, 144)) |> 
  group_by(date) |> 
  summarise(across(c(wind, gust),
                   list(mean = mean,
                        sd = sd,
                        max = max,
                        median = median)))

# Monthly

wd_summary = winddata |> 
  mutate(date = as_date(datetime)) |> 
  group_by(date) |> 
  mutate(N = length(wind)) |> 
  ungroup() |> 
  filter(near(N, 144)) |> 
  group_by(date) |> 
  summarise(across(c(wind, gust), mean)) |> 
  mutate(month = month(date),
         year = year(date)) |> 
  group_by(month, year) |> 
  summarise(across(c(wind, gust),
                   list(mean = mean, 
                        sd = sd,
                        min = min,
                        max = max))) |> 
  arrange(year, month)

wd_summary |> write_csv(file = "./winddata_summary.csv")
wd_summary |> write_rds(file = "./winddata_summary.rds")

read_csv("./winddata_summary.csv")
read_rds("./winddata_summary.rds")

################################################################################
# tibble()
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
URL2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTt_rEpmL7eWJtOc5MprXeihH4LTXkE9CyoLyPext6j3_9wbYAQAXQlEiSOs_Hse0hGLH6-zb6NJVKu/pub?output=csv"
URL3 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjodK7zDfhObB8OXSgfq0ZMJQh2d1Q__TFoSJ-6pPnsz50QE34xdJuJ5HQzR5RvyprB2GvsQYPw6Q4/pub?output=csv"
URL4 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRn0FUJhsKmyjTNPqahTZnhYvXP-ox6_ze1rwmMCFqpil3vyxsmfBgR-3NyGwfriH8z7xh4v38JemnU/pub?output=csv"

dall = tibble(url = c(URL1, URL2, URL3, URL4)) |> 
  mutate(data = map(url, read_csv, skip = 1))

dall = dall |> select(data) |> unnest(data) |> 
  rename(datetime = matches("日付"),
         light = matches("PAR"),
         wind = matches("風速"),
         gust = matches("突風"),
         mbar = matches("mbar"),
         id = `#`)

dall = dall |> 
  mutate(datetime = parse_datetime(datetime,
                                   "%m/%d/%Y %I:%M:%S %p",
                                   locale = locale("ja"))) |> 
  mutate(datetime = floor_date(datetime, "minutes")) |> 
  mutate(date = as_date(datetime)) |> 
  group_by(date) |> 
  filter(near(length(wind), 144))

# ggplot 

ggplot(wd_summary) +
  geom_point(aes(x = month, y = wind_mean),
             size = 5) +
  geom_errorbar(aes(x = month,
                    ymin = wind_mean - wind_sd,
                    ymax = wind_mean + wind_sd),
                width = 0) +
  theme_grey(30)















