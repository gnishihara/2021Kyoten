# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（B日程）
# 非線形モデル

# パッケージの読み込み #########################################################

library(tidyverse)
library(lubridate)
library(nlstools)
library(showtext)
library(ggpubr)
library(lemon)

# ggplot の設定 ################################################################
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")

theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()

