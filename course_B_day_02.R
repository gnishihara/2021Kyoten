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
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")

# theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()
theme_pubr(base_size = 10) |> theme_set()
# Multiple comparisons #########################################################


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

emmeans(m1, specs = pairwise ~ location, adjust = "tukey")
emtrends(m1, specs = pairwise ~ location, var = "year", adjust = "tukey")
emmip(m1, ~location, CIs = TRUE)
emmip(m1, location~year, cov.reduce = range)

################################################################################


library(nlme)
temperature = temperature |> drop_na() |> 
  mutate(location = factor(location))

m1 = gls(value ~ year2 * location, data = temperature,
         weights = varIdent(form=~1|location))

summary(m1) 

# Variance for each location
sigma = summary(m1)$sigma

tokyoS = intervals(m1, which = "var-cov") |> pluck("sigma") |> as_tibble_row() |> 
  mutate(location = "tokyo")
S = intervals(m1, which = "var-cov") |> pluck("varStruct") |> 
  tidyr::as_tibble(rownames = "location")

bind_rows(tokyoS,S) |> 
  mutate(sigma = c(1,sigma, sigma)) |> 
  mutate(across(c(lower, `est.`, upper),
                ~.*sigma))

plot(m1)

temperature = temperature |> mutate(resid = residuals(m1)) |> 
  mutate(pred = predict(m1))

temperature |> 
  group_nest(location) |> 
  mutate(shapiro = map(data, \(x){
    shapiro.test(x$resid)
  })) |> 
  mutate(tidy = map(shapiro, tidy)) |> 
  unnest(tidy)

bartlett.test(resid ~ location, data = temperature)
fligner.test(resid ~ location, data = temperature)

ggplot(temperature) + geom_boxplot(aes(x = location, y = resid))

ggplot(temperature) + 
  geom_point(aes(x = pred, y = resid, color = location)) +
  geom_smooth(aes(x = pred, y = resid, color = location)) + 
  facet_wrap(vars(location), nrow = 3)

ggplot(temperature) + 
  geom_qq(aes(sample = resid, color = location)) +
  geom_qq_line(aes(sample = resid, color = location)) +
  facet_wrap(vars(location), ncol = 3)

ggplot(temperature) + 
  geom_point(aes(x = year, y = value, color = location)) +
  geom_line(aes(x = year, y = pred, color = location)) +
  facet_wrap(vars(location), nrow = 3)


emmeans(m1, specs = pairwise ~ location, adjust = "tukey")

emtrends(m1, specs = pairwise ~ location, var = "year", adjust = "tukey",
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




















