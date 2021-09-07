# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（B日程）
# 一般化線形モデル

# パッケージの読み込み #########################################################

library(tidyverse)
library(lubridate)
library(statmod)
library(showtext)
library(ggpubr)
library(lemon)

# ggplot の設定 ################################################################
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")

theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()

# glm() ########################################################################
iris2 = iris |> as_tibble()
m1 = glm(Petal.Length ~ Species, data = iris2, family = Gamma("log"))
m2 = glm(Petal.Length ~ Species, data = iris2, family = gaussian)
summary(m1)

# diagnostic plots () ##########################################################

# qresiduals() #################################################################
library(statmod)
iris2 = iris2 |> 
  mutate(qres1 = qresiduals(m1),
         fit1 = fitted(m1),
         predict1 = predict.glm(m1, type = "link")) |> 
  mutate(qres2 = qresiduals(m2),
         fit2 = fitted(m2),
         predict2 = predict.glm(m2, type = "link"))

ggplot(iris2) + geom_point(aes(x = fit, y = qres))
p1 = ggplot(iris2) + geom_qq(aes(sample =  qres1)) + geom_qq_line(aes(sample = qres1))
p2 = ggplot(iris2) + geom_qq(aes(sample =  qres2)) + geom_qq_line(aes(sample = qres2))

ggpubr::ggarrange(p1,p2)

# 