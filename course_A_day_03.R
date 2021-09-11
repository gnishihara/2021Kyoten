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
