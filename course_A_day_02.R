# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（A日程）
# Day 02
 
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


# iris データの tibble化 #######################################################

iris2 = iris |> as_tibble()

# geom_point() #################################################################

ggplot(iris2) + 
  geom_point(aes(x = Petal.Length, y = Petal.Width,
                 shape = Species, color = Species))

# geom_boxplot() ###############################################################

ggplot(iris2) + 
  geom_boxplot(aes(x = Species, y = Petal.Length,
                 fill = Species))

# scale_*() ####################################################################

ggplot(iris2) + 
  geom_point(aes(x = Petal.Length, y = Petal.Width,
                 shape = Species, color = Species)) +
  scale_color_viridis_d(end = 0.8)

# facet_*() ####################################################################

iris3 = iris2 |> pivot_longer(cols = matches("(Pet)|(Sep)"),
                              names_to = c("Location", "Measurement"),
                              names_pattern = "(.*)\\.(.*)")
ggplot(iris3) + 
  geom_boxplot(aes(x = Species, y = value,
                   fill = Species)) +
  facet_grid(rows = vars(Measurement),
             cols = vars(Location))

# ggarrange() ##################################################################

# ggsave() #####################################################################

# magick() #####################################################################

################################################################################
# read_csv() ###################################################################
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
URL2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTt_rEpmL7eWJtOc5MprXeihH4LTXkE9CyoLyPext6j3_9wbYAQAXQlEiSOs_Hse0hGLH6-zb6NJVKu/pub?output=csv"
URL3 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjodK7zDfhObB8OXSgfq0ZMJQh2d1Q__TFoSJ-6pPnsz50QE34xdJuJ5HQzR5RvyprB2GvsQYPw6Q4/pub?output=csv"
URL4 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRn0FUJhsKmyjTNPqahTZnhYvXP-ox6_ze1rwmMCFqpil3vyxsmfBgR-3NyGwfriH8z7xh4v38JemnU/pub?output=csv"

