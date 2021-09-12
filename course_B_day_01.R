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
font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")

# theme_pubr(base_size = 10, base_family = "notosans") |> 
#   theme_set()
theme_pubr(base_size = 10) |> theme_set()

# Data are in tons from the OWID Dataset Collection (https://github.com/owid/owid-datasets)

URL = "https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Global%20fish%20catch%20by%20end%20use%20(FishStat%20via%20SeaAroundUs)/Global%20fish%20catch%20by%20end%20use%20(FishStat%20via%20SeaAroundUs).csv"
dset = read_csv(URL)
japan = dset |> filter(str_detect(Entity, "Japan"))
korea = dset |> filter(str_detect(Entity, "South Korea"))

ggplot(japan) + 
  geom_point(aes(x = Year, y = reported_landings/1000)) 

ggplot(korea) + 
  geom_point(aes(x = Year, y = reported_landings/1000)) 

# lm() #########################################################################
japan2 = japan |> filter(between(Year, 1964, 1984))
ggplot(japan2) + 
  geom_point(aes(x = Year, y = reported_landings/1000)) 

japan2 = japan2 |> 
  mutate(kton = reported_landings / 1000)
# kton = b0 + b1 * Year + error
m1 = lm(kton ~ Year, data = japan2)
summary(m1)

# Extract results with tidy() ##################################################
m1 |> tidy()

anova(m1) |> tidy()

# diagnostic plots #############################################################
 
# plot.lm()
plot(m1)

plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 5)


fitdata = japan2 |> 
  select(Year, kton) |> 
  mutate(fit = fitted(m1),
         residuals = residuals(m1))

ggplot(japan2) + 
  geom_point(aes(x = Year, y = kton)) +
  geom_line(aes( x = Year, y = fit, color = "model"), 
            data = fitdata) +
  geom_segment(aes(x = Year, 
                   xend = Year,
                   y = fit,
                   yend = fit + residuals,
                   color = "residuals"), 
               data = fitdata)
  

japan2$kton |> mean()

# Testing for normality ########################################################
# Shapiro-Wilks test ###########################################################
# Null hypothesis: the sample came from a normally distributed population
# shapiro.test(japan2$kton) 
shapiro.test(fitdata$residuals)

# Testing for homogeneity of variance ##########################################
# Only when there are factors.
# Bartlett's test (data must be normally distributed)
# bartlett.test()
# Levene's test (Less sensitive to non-normality)
# car::leveneTest(m1)
# Fligner-Killeen's test (non-parametric test and is robust to non-normal data)
# fligner.test()

# QQplot

dout = tibble(x = rnorm(1000, mean = 10, sd = 1))
ggplot(dout) + geom_histogram(aes(x = x))

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

temperature = weather |> 
  filter(str_detect(measurement, "^temperature$")) |> 
  drop_na()

ggplot(temperature) + 
  geom_point(aes( x = year, y = value, color = location))

ggplot(temperature) + 
  geom_boxplot(aes( x = location, 
                    y = value, 
                    fill = location))

m0 = lm(value ~ location, data = temperature)
m0 |> summary()
m0 |> anova() 

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

par(mfrow = c(2,2))
plot(m0, which = 1)
plot(m0, which = 2)
plot(m0, which = 3)
plot(m0, which = 5)
par(mfrow = c(1,1))

residuals(m0) |> shapiro.test()
car::leveneTest(m0)




m1 = lm(value ~ year + location, data = temperature)
summary(m1) 
m1 |> anova()

par(mfrow = c(2,2))
plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 5)
par(mfrow = c(1,1))

residuals(m1) |> shapiro.test()

temperature = 
  temperature |> 
  mutate(resid = residuals(m1),
         pearson = residuals(m1, type = "pearson")) |>
  mutate(pred = predict(m1))

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


m2 = lm(value ~ year + location + year:location, 
        data = temperature)
# m2 = lm(value ~ year*location,  data = temperature)
m2 |> summary() 
m2 |> anova()
m1 |> anova()



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

temperature = temperature |> 
  mutate(year2 = year - 1875)
m2 = lm(value ~ year2 + location + year2:location, 
        data = temperature)

m2 |> summary() 
m2 |> anova()
m1 |> anova()

# Multiple comparisons #########################################################
library(emmeans)

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

m1 = gls(value ~ year * location, data = temperature,
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

















