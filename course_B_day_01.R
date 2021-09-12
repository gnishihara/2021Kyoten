# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（B日程）
# 線形モデル (Day 1)

# パッケージの読み込み #########################################################
# install.packages("")
library(tidyverse)
library(lubridate)
library(showtext)
library(ggpubr)
library(lemon)
library(broom)
library(emmeans)

# ggplot の設定 ################################################################
# フォントを指定するためのコードです。
font_add_google("Noto Sans", family = "notosans") # 英語フォント
# font_add_google("Noto Serif", family = "notoserif")　# 英語フォント
# font_add_google("Noto Sans JP", family = "notosanscjk") # 日本語フォント

# notosans をつかうなら次のコードを実行する
# theme_pubr(base_size = 10, base_family = "notosans") |> 
#   theme_set()

# フォントを指定したくないなら：   
theme_pubr(base_size = 10) |> theme_set()

# theme_pubr() は ggpubr のテーマです。
# theme_set() はここから先のggplotのテーマを指定するためです

# データはここで読み込んでいます。
# URL から読み込んでいます。
# Data are in tons from the OWID Dataset Collection (https://github.com/owid/owid-datasets)
URL = "https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Global%20fish%20catch%20by%20end%20use%20(FishStat%20via%20SeaAroundUs)/Global%20fish%20catch%20by%20end%20use%20(FishStat%20via%20SeaAroundUs).csv"
dset = read_csv(URL)
japan = dset |> filter(str_detect(Entity, "Japan"))
korea = dset |> filter(str_detect(Entity, "South Korea"))

ggplot(japan) + 
  geom_point(aes(x = Year, y = reported_landings/1000)) 

ggplot(korea) + 
  geom_point(aes(x = Year, y = reported_landings/1000)) 

# 回帰分析は lm() 
# このとき、
# (1)残渣は正規分布に従うこと
# (2)残渣は等分散であるあることが条件です。
# lm() #########################################################################
# 1964年から1984年のデータを抽出して解析に使います。

japan2 = japan |> filter(between(Year, 1964, 1984))

ggplot(japan2) + 
  geom_point(aes(x = Year, y = reported_landings/1000)) 

japan2 = japan2 |> 
  mutate(kton = reported_landings / 1000)

# lm() で回帰分析をします
# kton = b0 + b1 * Year + error
m1 = lm(kton ~ Year, data = japan2)
m1 |> summary() # モデル係数の出力（Wald's test）
m1 |> anova()   # モデルフィトの出力 (ANOVA table)

# Extract results with tidy() ##################################################
# broom::tidy() で処理すると、tibble が帰ってくる
m1 |> tidy()
anova(m1) |> tidy()

# diagnostic plots #############################################################
# モデルフィットの診断図
# ?plot.lm() でヘルプみれます。

plot(m1)

plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 5)

# 当てはめたモデルから期待値（モデルの値）と残渣を求めます

fitdata = japan2 |> 
  select(Year, kton) |> 
  mutate(fit = fitted(m1),
         residuals = residuals(m1))

ggplot(japan2) + 
  geom_point(aes(x = Year, y = kton, color = "観測値")) +
  geom_line(aes( x = Year, y = fit, color = "期待値"), 
            data = fitdata) +
  geom_segment(aes(x = Year, 
                   xend = Year,
                   y = fit,
                   yend = fit + residuals,
                   color = "残渣"), 
               data = fitdata)
  
# japan2 |> pull(kton) |> mean()
japan2$kton |> mean()

# 正規性と等分散性の検定はつぎのコードでできます。
# Testing for normality ########################################################
# Shapiro-Wilks test ###########################################################
# Null hypothesis: the sample came from a normally distributed population
# どちらかというと、残渣の正規性を確認しましょう。
# shapiro.test(japan2$kton) # 観測値の正規性
shapiro.test(fitdata$residuals) # 残渣の正規性

# ここには等分散性の検定コードです。
# 回帰分析のため、つぎのコードは使えないが、
# 診断図では確認できます。等分散性であれば、期待値に周りに
# 残渣は均一にばらつきます。
# Testing for homogeneity of variance ##########################################
# Only when there are factors.
# Bartlett's test (data must be normally distributed)
# bartlett.test()
# Levene's test (Less sensitive to non-normality)
# car::leveneTest(m1)
# Fligner-Killeen's test (non-parametric test and is robust to non-normal data)
# fligner.test()

# ggplot でも QQplot つくれます。

# qqplot はなぜ正規性の確認につかうのか？
# この図で説明。 rnorm() は正規分布に従う変数の疑似乱数関数です。
dout = tibble(x = rnorm(1000, mean = 10, sd = 1))
ggplot(dout) + geom_histogram(aes(x = x))

# ここで当てはめたモデルの残渣のQQplot です。
ggplot(dout) + 
  geom_qq(aes(sample = x)) +
  geom_qq_line(aes(sample = x),
               linetype = "dashed")

dout = tibble(x = rpois(1000, lambda = 5))
ggplot(dout) + geom_histogram(aes(x = x))

ggplot(dout) + 
  geom_qq(aes(sample = x)) +
  geom_qq_line(aes(sample = x),
               linetype = "dashed")

################################################################################
# 気温のデータを解析してみよう
# ここでは一元配置分散分析と一元配置共分散分析をします。
################################################################################
# データの構造が複雑なので、処理に手間がかかります。
URL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRH8_QwdlSReHgksJaWeRgHJ6J5ELx_7zyFRN7ZVdUHl87vkbZiV9bN42Mf3do8InyTufpQAWF1rKJC/pub?output=csv"
z = read_lines(URL, skip = 2, n_max = 3)
z |> as_tibble_col() |> 
  separate(value, into = str_glue("x{1:75}"), sep = ",") |> 
  as.matrix() |> t()

x = c("temperature", "high-temperature", "rainfall", "max-rainfall")
y = c("nagasaki", "tokyo", "naha")
z = str_glue("{rep(y, each = 4)}_{rep(x, 3)}")

cnames = c("ym", z)
ctypes = c("cd", rep("-", 4), "d", rep("-", 4), "d", rep("-", 6), "d", rep("-", 6),
           "d", rep("-", 4), "d", rep("-", 4), "d", rep("-", 6), "d", rep("-", 6),
           "d", rep("-", 4), "d", rep("-", 4), "d", rep("-", 6), "d", rep("-", 6)) |> 
  paste(collapse = "")

weather = read_csv(URL, 
                   col_names = cnames, 
                   col_types = ctypes, 
                   skip = 6)

weather = weather |> 
  pivot_longer(!ym, names_to = c("location", "measurement"),
               names_pattern = "(.*)_(.*)") |> 
  separate(ym, c("year", "month"), convert = T)
################################################################################
# 気温のデータを抽出してから解析します。
# 
temperature = weather |> 
  filter(str_detect(measurement, "^temperature$")) |> 
  drop_na()

ggplot(temperature) + 
  geom_point(aes( x = year, y = value, color = location))

ggplot(temperature) + 
  geom_boxplot(aes( x = location, 
                    y = value, 
                    fill = location))

# まずは一元配置分散分析
m0 = lm(value ~ location, data = temperature)
m0 |> summary()
m0 |> anova() 

################################################################################
# このコードでP値のサンプル数依存を説明
# サンプル数が増えるとP値は必ず下がります！
Z = tibble(N = c(3, 5:10, 25, 50, 100)) |> 
  mutate(data = list(temperature)) |> 
  mutate(data = map2(data,N,  \(x,n) {
    x |> group_by(location) |> sample_n(n)
  })) |> 
  mutate(pvalue = map_dbl(data, \(x) {
    m = lm(value ~ location, data = x)
    z = anova(m) |> tidy() |> drop_na()
    z$p.value
  }))
  
ggplot(Z) + 
  geom_point(aes(x = N, y = pvalue)) +
  geom_line(aes(x = N, y = pvalue)) +
  scale_y_continuous(trans = "log10")
################################################################################
# m0 モデルの診断図
par(mfrow = c(2,2))
plot(m0, which = 1)
plot(m0, which = 2)
plot(m0, which = 3)
plot(m0, which = 5)
par(mfrow = c(1,1))

# 残渣の正規性の検定とモデルの等分散性の検定
residuals(m0) |> shapiro.test()
car::leveneTest(m0)


# ここでは一元配置共分散分析
# 共編量は year 因子は location です
m1 = lm(value ~ year + location, data = temperature)
m1 |> summary() 
m1 |> anova()

# 診断図
par(mfrow = c(2,2))
plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 5)
par(mfrow = c(1,1))

# 残渣の正規性の検定
residuals(m1) |> shapiro.test()

# 残渣を求める
# type = "pearson" を渡すとピアソンの残渣が戻ってきます
# ほか、"working", "response", "deviance", "partial" がある
# デフォルトは working
# rstudent() で studentized 残渣を返せます
# rstandard() で standardized 残渣を返せます
temperature = 
  temperature |> 
  mutate(resid = residuals(m1),
         pearson = residuals(m1, type = "pearson")) |>
  mutate(pred = predict(m1))

# ggplot 風の診断図
ggplot(temperature) + 
  geom_boxplot(aes(x = location, y = resid))

ggplot(temperature) + 
  geom_qq(aes(sample = resid, color = location)) +
  geom_qq_line(aes(sample = resid, color = location)) +
  facet_wrap(vars(location), ncol = 3)

ggplot(temperature) + 
  geom_point(aes(x = pred, y = resid, color = location)) +
  geom_smooth(aes(x = pred, y = resid, color = location)) + 
  facet_wrap(vars(location), nrow = 3)

ggplot(temperature) + 
  geom_point(aes(x = year, y = value, color = location)) +
  geom_line(aes(x = year, y = pred, color = location))

# フルモデル：相互作用入の一元配置共分散分析
m2 = lm(value ~ year + location + year:location, 
        data = temperature)

# モデルの書き方は次のように諸略できます
# m2 = lm(value ~ year*location,  data = temperature)
m2 |> summary() 
m2 |> anova()
m1 |> anova() # 相互作用なしの分散分析表

# ggplot 風の診断図
temperature = 
  temperature |> 
  mutate(resid = residuals(m2),
         pearson = residuals(m2, type = "pearson")) |>
  mutate(pred = predict(m2))

ggplot(temperature) + 
  geom_boxplot(aes(x = location, y = resid))

ggplot(temperature) + 
  geom_qq(aes(sample = resid, color = location)) +
  geom_qq_line(aes(sample = resid, color = location)) +
  facet_wrap(vars(location), ncol = 3)

ggplot(temperature) + 
  geom_point(aes(x = pred, y = resid, color = location)) +
  geom_smooth(aes(x = pred, y = resid, color = location)) + 
  facet_wrap(vars(location), nrow = 3)

ggplot(temperature) + 
  geom_point(aes(x = year, y = value, color = location)) +
  geom_line(aes(x = year, y = pred, color = location))


# モデルの解釈をよくするため、切片の計算は1875 年にする
# こうすると、切片の係数は 1875 年のときの値になる。
# 
temperature = temperature |> mutate(year2 = year - 1875)
m2 = lm(value ~ year2 + location + year2:location, 
        data = temperature)

m2 |> summary() 
m2 |> anova()
m1 |> anova()

