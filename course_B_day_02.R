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
# library(car) # leveneTest() はここにある
library(emmeans) # 多重比較用パッケージ


# ggplot の設定 ################################################################
library(showtext)
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
font_add_google("Noto Sans JP", family = "nsanscjk")
font_add_google("Noto Serif JP", family = "nserifcjk")

theme_pubr(base_size = 10, 
           base_family = "notosans") |> theme_set()
theme_pubr(base_size = 20) |> theme_set()

# 線形モデルの多重比較（一日目のつづき） #########################################################

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

temperature |> group_by(location) |> 
  summarise(across(value, list(mean = mean, sd = sd)))

# emmeans パッケージからの関数
temperature = temperature |> mutate(year2 = year - 1875)

m0 = lm(value ~ 1, data = temperature) # 帰無モデル
m1a = lm(value ~ location, data = temperature)
m1b = lm(value ~ year2, data = temperature)
m2 = lm(value ~ year2 + location, data = temperature)
m3 = lm(value ~ year2 * location, data = temperature) # フルモデル

anova(m0, m1a, m2, m3, test = "F")
anova(m0, m1b, m2, m3, test = "F")
anova(m0, m3, test = "F")

# The full model compared to the null model had a F-value of 
# 195 with 5 and 1249 degrees-of-freedom and was 
# statistically significant with a Pvalue of < 0.0001.

m1 |> summary()
m2 |> summary()

emmeans(object = m1, 
        specs = pairwise ~ location, 
        adjust = "tukey") # TukeyHSD 法

emmeans(object = m2, 
        specs = pairwise ~ location, 
        adjust = "tukey") # TukeyHSD 法

# 切片のペアごと比較
emmeans(object = m2, 
        specs = pairwise ~ location, 
        var = "year2",
        adjust = "tukey") # TukeyHSD 法

# 傾きのペアごとの比較
emtrends(m2, specs = pairwise ~ location, 
         var = "year2", adjust = "tukey")

emmip(m1, ~location, CIs = TRUE)
emmip(m2, location~year2, cov.reduce = range)

################################################################################
library(nlme) # linear & non-linear mixed effects

temperature = temperature |> drop_na() |> 
  mutate(location = factor(location))

ggplot(temperature) + 
  geom_boxplot(aes(x = location, y = value))

# グループごとの分析が異なるとき
# gls() generalized least squares 
m1l = lm(value ~ year2 * location, data = temperature)
m1ml = gls(value ~ year2 * location, 
         data = temperature,
         weights = varIdent(form = ~1|location),
         method = "ML")
m1reml = gls(value ~ year2 * location, 
           data = temperature,
           weights = varIdent(form = ~1|location))

AIC(m1l, m1ml)
m1l |> summary() 
m1reml |> summary()

temperature2 = temperature |> 
  group_by(location, year) |> 
  summarise(value = mean(value)) |> 
  mutate(year2 = year - 1875)

m1 = lm(value ~ year2 * location, data = temperature2)
m1corCAR = gls(value ~ year2 * location, 
             data = temperature2,
             correlation = corCAR1(0.5, form = ~ year2 | location), 
             weights = varIdent(form = ~1|location))
m1varIdent = gls(value ~ year2 * location, 
               data = temperature2,
               weights = varIdent(form = ~1|location))
m1corCARml = gls(value ~ year2 * location, 
             data = temperature2,
             correlation = corCAR1(0.5, form = ~ year2 | location), 
             weights = varIdent(form = ~1|location),
             method = "ML")
m1varIdentml = gls(value ~ year2 * location, 
               data = temperature2,
               weights = varIdent(form = ~1|location),
               method = "ML")
AIC(m1, m1corCARml, m1varIdentml)

temperature2 = temperature2 |> ungroup() |> 
  mutate(resid = residuals(m1corCAR, type = "pearson")) |> 
  mutate(pred = predict(m1corCAR)) |> 
  mutate(residC = residuals(m1corCAR, type = "pearson")) |> 
  mutate(predC = predict(m1corCAR)) |> 
  mutate(residI = residuals(m1varIdent, type = "pearson")) |> 
  mutate(predI = predict(m1varIdent))

# bartlett.test(resid ~ location, data = temperature)
# fligner.test(resid ~ location, data = temperature)

ggplot(temperature2) + 
  geom_boxplot(aes(x = location, 
                   y = resid))

p0 = ggplot(temperature2) + 
  geom_point(aes(x = pred, y = resid, color = location)) +
  geom_smooth(aes(x = pred, y = resid, color = location)) + 
  facet_wrap(vars(location), nrow = 3)

p1 = ggplot(temperature2) + 
  geom_point(aes(x = predC, y = residC, color = location)) +
  geom_smooth(aes(x = predC, y = residC, color = location)) + 
  facet_wrap(vars(location), nrow = 3)

p2 = ggplot(temperature2) + 
  geom_point(aes(x = predI, y = residI, color = location)) +
  geom_smooth(aes(x = predI, y = residI, color = location)) + 
  facet_wrap(vars(location), nrow = 3)
ggarrange(p0, p1,p2)

p0 = ggplot(temperature2) + 
  geom_qq(aes(sample = resid, color = location)) +
  geom_qq_line(aes(sample = resid, color = location)) +
  facet_wrap(vars(location), ncol = 3)

p1 = ggplot(temperature2) + 
  geom_qq(aes(sample = residC, color = location)) +
  geom_qq_line(aes(sample = residC, color = location)) +
  facet_wrap(vars(location), ncol = 3)

p2 = ggplot(temperature2) + 
  geom_qq(aes(sample = residI, color = location)) +
  geom_qq_line(aes(sample = residI, color = location)) +
  facet_wrap(vars(location), ncol = 3)

ggarrange(p0, p1,p2)

ggplot(temperature) + 
  geom_point(aes(x = year, y = value, color = location)) +
  geom_line(aes(x = year, y = pred, color = location)) +
  facet_wrap(vars(location), nrow = 3)

# AIC, QQplot, 残渣の変動を確認したら、
# m1corCAR がベストモデル

AIC(m1, m1corCARml, m1varIdentml)

emmeans(m1corCAR, 
        specs = pairwise ~ location, adjust = "tukey")

emtrends(m1corCAR, 
         specs = pairwise ~ location, 
         var = "year2", adjust = "tukey",
         mode = "df.error")

emmip(m1, ~location, CIs = TRUE)
emmip(m1, location~year, cov.reduce = range)




################################################################################
################################################################################














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


 
################################################################################
################################################################################
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

temperature = weather |> filter(str_detect(measurement, "^rain")) |> drop_na()

m1 = glm(value ~ I(year - 1960) * location, 
         data = temperature, family = Gamma("log"))
summary(m1)

plot(m1)

temperature = temperature |> 
  mutate(resid = qresiduals(m1, dispersion = summary(m1)$dispersion)) |>
  mutate(lpred = predict(m1, type = "link"),
         pred  = predict.glm(m1, type = "response")) |> print()

ggplot(temperature) + geom_boxplot(aes(x = location, y = resid))

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
  geom_line(aes(x = year, y = pred, color = location)) +
  facet_wrap(vars(location), nrow = 3)




















