# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（B日程）
# 線形モデル

# パッケージの読み込み #########################################################

library(tidyverse)
library(lubridate)
library(showtext)
library(ggpubr)
library(lemon)

# ggplot の設定 ################################################################
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")

theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()

# Data are in tons from the OWID Dataset Collection (https://github.com/owid/owid-datasets)

URL = "https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Global%20fish%20catch%20by%20end%20use%20(FishStat%20via%20SeaAroundUs)/Global%20fish%20catch%20by%20end%20use%20(FishStat%20via%20SeaAroundUs).csv"
dset = read_csv(URL)
japan = dset |> filter(str_detect(Entity, "Japan"))
korea = dset |> filter(str_detect(Entity, "South Korea"))

ggplot(japan) + geom_point(aes(x = Year, y = reported_landings/1000)) 
ggplot(korea) + geom_point(aes(x = Year, y = reported_landings/1000)) 

# lm() #########################################################################
japan2 = japan |> filter(between(Year, 1964, 1984))
ggplot(japan2) + geom_point(aes(x = Year, y = reported_landings/1000)) 

m1 = lm(reported_landings/1000 ~ Year, data = japan2)
summary(m1)

# diagnostic plots #############################################################
# plot.lm()
plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 5)

################################################################################
URL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRH8_QwdlSReHgksJaWeRgHJ6J5ELx_7zyFRN7ZVdUHl87vkbZiV9bN42Mf3do8InyTufpQAWF1rKJC/pub?output=csv"
z = read_lines(URL, skip = 2, n_max = 3)
z |> as_tibble_col() |> 
  separate(value, into = str_glue("x{1:75}"), sep = ",") |> 
  as.matrix() |> t()

x = c("temperature", "high-temperature", "rainfall", "max-rainfall")
y = c("nagasaki", "tokyo", "naha")
z = str_glue("{rep(y, each = 4)}_{rep(x, 3)}")

cnames = c("ym", z)
ctypes =c("cd", rep("-", 4), "d", rep("-", 4), "d", rep("-", 6), "d", rep("-", 6),
           "d", rep("-", 4), "d", rep("-", 4), "d", rep("-", 6), "d", rep("-", 6),
           "d", rep("-", 4), "d", rep("-", 4), "d", rep("-", 6), "d", rep("-", 6)) |> 
  paste(collapse = "")
weather = read_csv(URL, col_names = cnames, col_types = ctypes, skip = 6)

weather = weather |> 
  pivot_longer(!ym, names_to = c("location", "measurement"),
               names_pattern = "(.*)_(.*)") |> 
  separate(ym, c("year", "month"), convert = T)
################################################################################