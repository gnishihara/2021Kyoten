# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（A日程）
# Day 03

# パッケージの読み込み #########################################################
# 今日の演習につかうパッケージ
# install.packages() しましょう。

# Linux 環境だけやる
Sys.setlocale("LC_TIME", "en_US.UTF-8")

library(tidyverse)
library(lubridate)

library(showtext) # システムフォント
library(ggpubr)   # theme_pubr()
library(lemon)    # 作図用のパッケージ

# ggplot の設定 ################################################################
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")
# font_add_google("Noto Serif JP", family = "notoserifcjk")

theme_pubr(base_size = 24, base_family = "notosans") |> 
  theme_set()

# theme_grey() # デフォルトの theme

iris2 = iris |> as_tibble()

iris2 = iris2 |> rename(sl = matches("pal.L"),
                        sw = matches("pal.W"),
                        pl = matches("tal.L"),
                        pw = matches("tal.W"))

iris2 |> group_by(Species) |> summarise(sl_mean = mean(sl))
iris2 |> 
  group_by(Species) |> 
  summarise(across(.fns = mean))

iris2 |> 
  mutate(sl = ifelse(between(sl, 4, 5), NA, sl)) |> 
  group_by(Species) |> 
  summarise(across(.fns = mean, na.rm = TRUE))

iris2 |> 
  mutate(sl = ifelse(between(sl, 4, 5), NA, sl)) |> 
  group_by(Species) |> 
  summarise(across(.fns = ~mean(.x, na.rm = TRUE)))

iris2 |> 
  mutate(sl = ifelse(between(sl, 4, 5), NA, sl)) |> 
  group_by(Species) |> 
  summarise(across(.cols = c(sl, pl),
                   .fns = mean, 
                   na.rm = TRUE))

iris2 |> 
  mutate(sl = ifelse(between(sl, 4, 5), NA, sl)) |> 
  group_by(Species) |> 
  summarise(across(.cols = c(sl, pl),
                   .fns = list("mean" = mean, "var" = var), 
                   na.rm = TRUE))

se = function(value, na.rm = FALSE) {
  s = sd(value, na.rm)
  nadata = is.na(value) |> sum()
  n = length(value)
  # print(n-nadata)
  s / sqrt(n-nadata)
}

se2 = function(value, na.rm = FALSE) {
  s = sd(value, na.rm)
  n = length(value)
  # print(n)
  s / sqrt(n)
}

iris2 |> 
  mutate(sl = ifelse(between(sl, 4, 5), NA, sl)) |> 
  group_by(Species) |> 
  summarise(across(.cols = c(sl, pl),
                   .fns = list("stderr1" = se, 
                               "stderr2" = se2), 
                   na.rm = TRUE))
#############################

setosa     = iris2 |> filter(str_detect(Species, "setosa"))
virginica  = iris2 |> filter(str_detect(Species, "virgi"))
versicolor = iris2 |> filter(str_detect(Species, "versi"))

mse = lm(sl ~ pl, data = setosa)
mvi = lm(sl ~ pl, data = virginica)
mve = lm(sl ~ pl, data = versicolor)

mse |> summary()
mvi |> summary()
mve |> summary()

fitmodel = function(x) {
  lm(sl ~ pl, data = x)
}

iris3 = iris2 |> 
  group_nest(Species) |> 
  mutate(model = map(data, fitmodel)) |> 
  mutate(summary = map(model, summary))

library(broom)

iris2 |> 
  group_nest(Species) |> 
  mutate(model = map(data, fitmodel)) |> 
  mutate(summary = map(model, tidy)) |> 
  select(Species, summary) |> 
  unnest(summary)

iris2 |> 
  group_nest(Species) |> 
  mutate(model = map(data, fitmodel)) |> 
  mutate(summary = map(model, anova)) |> 
  mutate(summary = map(summary, tidy)) |> 
  select(Species, summary) |> 
  unnest(summary) |> 
  drop_na()



iris2 |> mutate(sl_area = sl * sw)

calc_area = function(x,y) {
  x * y
}
iris2 |> 
  mutate(sl_area = map2(sl, sw, calc_area)) |> 
  unnest(sl_area)

calc_area2 = function(x1, y1, x2, y2) {
  (x1 * y1) + (x2 * y2)
}

iris2 |> 
  mutate(area = pmap(list(sl,sw,pl,pw), calc_area2)) |> 
  unnest(area)

iris2 |> 
  mutate(area = pmap_dbl(list(sl,sw,pl,pw), calc_area2)) 

calc_slope = function(x) {
  m = lm(sl ~ sw, data = x)
  cfs = coefficients(m)
  cfs[2]
}

iris2 |> group_nest(Species) |> 
  mutate(slope = map(data, calc_slope)) |> 
  unnest(slope)

model = "sl ~ sw"
lm(as.formula(model), data = setosa)



calc_slope = function(x, f) {
  f = as.formula(f)
  m = lm(f, data = x)
  cfs = coefficients(m)
  cfs[2]
}

iris2 |> 
  group_nest(Species) |> 
  mutate(slope_slw = map(data, calc_slope, f = "sl ~ sw")) |> 
  mutate(slope_plw = map(data, calc_slope, f = "pl ~ pw")) |> 
  unnest(c(slope_slw, slope_plw))

##



tokyo = read_csv("https://www3.nhk.or.jp/n-data/opendata/coronavirus/nhk_news_covid19_prefectures_daily_data.csv", col_names = c("date", "prefcode", "pref", "new_cases", "new_cases_cs", "deaths", "deaths_cs"), skip = 1, show_col_types = F) |> mutate(date = as.Date(date), prefcode = as.double(prefcode)) |> filter(pref == "東京都") 
ggplot(tokyo) + 
  geom_line(aes(date, new_cases))  +
  scale_x_date(date_breaks = "3 months")

d1 = tokyo |> select(date, new_cases)

breaks = c("2020-03-01",
           "2020-06-01",
           "2020-10-01",
           "2021-03-01",
           "2021-07-01",
           "2021-10-01")

filter_tokyo = function(l,r,d){
  d |> 
    filter(between(date, l, r)) |> 
    # filter(near(max(new_cases), new_cases))
    slice_max(new_cases)
}

length(breaks)
d2 = tibble(left = breaks[-length(breaks)],
            right = breaks[-1]) |>
  mutate(left = ymd(left), right = ymd(right)) |> 
  mutate(data = list(d1)) |> 
  mutate(data = pmap(list(left, right, data),
                     filter_tokyo)) |> 
  unnest(data)

ggplot(tokyo) + 
  geom_line(aes(date, new_cases))  +
  geom_vline(aes(xintercept = date),
             data = d2) + 
  scale_x_date(date_breaks = "3 months") 

d2 |> select(date, new_cases) |> 
  write_excel_csv("Table01.csv")

mse |> anova() |> tidy() |> write_excel_csv("Anova01.csv")




# read_csv() ###################################################################
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
d1 = read_csv(URL1, skip = 1)

d1 = d1 |> 
mutate(datetime = parse_datetime(`日付 時間, GMT+09:00`,
                                 "%m/%d/%Y %I:%M:%S %p",
                                 locale = locale("ja")),
       .before = `日付 時間, GMT+09:00`) |> 
  rename(light = matches("PAR"),
         wind = matches("風速"),
         gust = matches("突風"),
         mbar = matches("mbar")) |> 
  select(datetime, light, wind, gust, mbar)

ggplot(d1) + 
  geom_line(aes(x = datetime, y = mbar))

# 位置にごとの偏差

d1 = d1 |> 
  mutate(date = as_date(datetime)) |> 
  group_by(date) |> 
  mutate(mbar_mean = mean(mbar)) |> 
  ungroup() |> 
  mutate(hensa = mbar - mbar_mean)

ggplot(d1) + 
  geom_line(aes(x = datetime, y = hensa)) +
  geom_line(aes(x = datetime, y = 0))
  









