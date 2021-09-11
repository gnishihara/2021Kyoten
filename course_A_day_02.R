# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（A日程）
# Day 02
 
# パッケージの読み込み #########################################################
# 今日の演習につかうパッケージ
# install.packages() しましょう。
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

theme_pubr(base_size = 24, base_family = "notosans") |> theme_set()

# theme_grey() # デフォルトの theme

# iris データの tibble化 #######################################################

iris2 = iris |> as_tibble()

# geom_point() #################################################################

ggplot(iris2) + 
  geom_point(aes(x = Petal.Length, 
                 y = Petal.Width,
                 shape = Species,
                 color = Species))

# geom_boxplot() ###############################################################
# alpha : 透明度 0~1
ggplot() + 
  geom_boxplot(aes(x = Species, 
                   y = Petal.Length,
                   fill = Species,
                   color = Species),
               alpha = 0.5, 
               data = iris2)

# scale_*() ####################################################################

ggplot(iris2) + 
  geom_point(aes(x = Petal.Length, 
                 y = Petal.Width,
                 shape = Species, 
                 color = Species),
             size = 5) +
  scale_color_viridis_d(end = 0.9)


ggplot() + 
  geom_boxplot(aes(x = Species, 
                   y = Petal.Length,
                   fill = Species,
                   color = Species),
               data = iris2) +
  scale_fill_viridis_d(end = 0.9, alpha = 0.5) +
  scale_color_viridis_d(end = 0.9, alpha = 0.8)



# RColorBrewer::display.brewer.all()

# facet_*() ####################################################################

iris3 = iris2 |> 
  pivot_longer(cols = matches("(Pet)|(Sep)"),
               names_to = c("Location", "Measurement"),
               names_pattern = "(.*)\\.(.*)")

# iris2 |> 
#   pivot_longer(cols = c(Petal.Length, Petal.Width,
#                         Sepal.Length, Sepal.Width)) |> 
#   separate(name, into = c("Location", "Measurement"))

ggplot(iris3) + 
  geom_boxplot(aes(x = Species, 
                   y = value,
                   fill = Species)) +
  facet_grid(rows = vars(Measurement),
             cols = vars(Location))

ggplot(iris3) + 
  geom_boxplot(aes(x = Species, 
                   y = value,
                   fill = Species)) +
  facet_grid(rows = vars(Measurement, Location))

ggplot(iris3) + 
  geom_boxplot(aes(x = Species, 
                   y = value,
                   fill = Species)) +
  facet_grid(cols = vars(Measurement, Location))

ggplot(iris3) + 
  geom_boxplot(aes(x = Species, 
                   y = value,
                   fill = Species)) +
  facet_wrap(facets = vars(Measurement, Location),
             ncol = 3)

ggplot(iris3) + 
  geom_boxplot(aes(x = Species, 
                   y = value,
                   fill = Species)) +
  facet_rep_grid(cols = vars(Measurement),
                 rows = vars(Location))

# ggarrange() ##################################################################
# ggpubr::ggarrange()

plot1 = ggplot(iris2) + 
  geom_point(aes(x = Petal.Length,
                 y = Petal.Width,
                 color = Species)) + 
  scale_color_viridis_d(end = 0.9)

plot2 = ggplot(iris2) + 
  geom_boxplot(aes(x = Species,
                 y = Petal.Length,
                 fill = Species)) + 
  scale_fill_viridis_d(end = 0.9)

ggarrange(plot1, plot2, common.legend = TRUE)

# ggsave() #####################################################################

# magick() #####################################################################

################################################################################
# read_csv() ###################################################################
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
URL2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTt_rEpmL7eWJtOc5MprXeihH4LTXkE9CyoLyPext6j3_9wbYAQAXQlEiSOs_Hse0hGLH6-zb6NJVKu/pub?output=csv"
URL3 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjodK7zDfhObB8OXSgfq0ZMJQh2d1Q__TFoSJ-6pPnsz50QE34xdJuJ5HQzR5RvyprB2GvsQYPw6Q4/pub?output=csv"
URL4 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRn0FUJhsKmyjTNPqahTZnhYvXP-ox6_ze1rwmMCFqpil3vyxsmfBgR-3NyGwfriH8z7xh4v38JemnU/pub?output=csv"

