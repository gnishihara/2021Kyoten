# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（B日程）
# 非線形モデル
# Good reference: 
# Baty et al. 2015 Journal of Statistical Software 66: 1-21

# Workflow
# 1) Prepare data
# 2) Define model
# 3) Try fitting with preview()
# 4) Fit models with nls()
# 5) Choose model
# 6) Diagnostic tests
# 7) Check marginal distribution of parameters
# 8) Calculate non-parametric confidence intervals

# パッケージの読み込み #########################################################

library(tidyverse)
library(lubridate)
library(broom)
library(showtext)
library(ggpubr)
library(lemon)
library(nlstools)
library(minpack.lm)
library(nlme)
library(gnlm)

# ggplot の設定 ################################################################
# font_add_google("Noto Sans", family = "notosans")
# font_add_google("Noto Serif", family = "notoserif")
# font_add_google("Noto Sans JP", family = "notosanscjk")

theme_pubr(base_size = 20) |> theme_set()

# Download and read data #######################################################
URL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vThUTjrAocQvXqjtAeSeK7pWMQuRAGDSU8UpooB0Z6o51J6zvv0_Prcna0aDbJ-BfrCgpGshBhzUOKv/pub?output=csv"
dset = read_csv(URL, skip = 1)

# Pivot the data ###############################################################
dset2 = dset |> 
  pivot_longer(cols = matches("[0-9]+"),
               names_to = "par",
               values_to = "mgl",
               names_transform = list(par = as.numeric))

# Take a look at the data ######################################################

ggplot(dset2) + 
  geom_point(aes(x = min, y = mgl)) + 
  facet_grid(rows = vars(sample),
             cols = vars(par))

# Calculate the rate ###########################################################
calculate_rate = function(z) {
  lm(mgl ~ min, data = z)
}

dset2 = dset2 |> 
  group_nest(sample, par, gww, ml) |> 
  mutate(model = map(data, calculate_rate)) |> 
  mutate(tidy = map(model, tidy)) |> 
  unnest(tidy) |> 
  filter(str_detect(term, "min")) |> 
  mutate(rate = estimate * ml / gww)
  
# Check the results ############################################################
ggplot(dset2) + 
  geom_point(aes(x = par, y = rate, 
                 color = as.factor(sample)))

# Fit the three types of models ################################################
# y1 = b0 + b1 * x
# y2 = b0 + b1 * x + b2 * x^2
# y3 = exp(b0 + b1 * x)
#  dy3/db0 = exp(b0 + b1 * x)
#  dy3/db1 = x*exp(b0 + b1 * x)
# y4 = b2 * exp(b0 + b1 * x)
#   dy4/db0 = b2 * exp(b0 + b1 * x)
#   dy4/db1 = b2 * x * exp(b0 + b1 * x)
#   dy4/db2 = exp(b0 + b1 * x)

model1 = function(b0, b1, b2, x) {
  b1 * x / (b2 + x) - b0
}

model2 = function(b0, b1, b2, x) {
  b1 * (1- exp(-b2 / b1 * x)) - b0
}

model3 = function(b0, b1, b2, x) {
  b1 * tanh(b2 / b1 * x) - b0
}

# Use nlstools to find good starting values ####################################
S = list(b0 = 10, b1 = 30, b2 = 20)
preview(rate ~ model1(b0,b1,b2,par), data = dset2,
        start = S, variable = 2)
S2 = list(b0 = 10, b1 = 50, b2 = 1)
preview(rate ~ model2(b0,b1,b2,par), data = dset2,
        start = S2, variable = 2)

S3 = list(b0 = 1, b1 = 10, b2 = 0.5)
preview(rate ~ model3(b0,b1,b2,par), data = dset2,
        start = S3, variable = 2)
# Fit the model ################################################################
m1 = nls(rate ~ model1(b0, b1, b2, par), data = dset2, start = S)
m2 = nls(rate ~ model2(b0, b1, b2, par), data = dset2, start = S2)
m3 = nls(rate ~ model3(b0, b1, b2, par), data = dset2, start = S3)

# Pick the model ###############################################################
AIC(m1,m2,m3)

# overview() ###################################################################
overview(m2)

# Diagnostic tests #############################################################
dset2 = dset2 |> ungroup() |> 
  mutate(residuals = residuals(m2),
         fitted = fitted(m2))
r2 = nlsResiduals(m2)
plot(r2)

# Shapiro-Wilks Test; null hypothesis is that the population is normally distributed
# Runs test;  null hypothesis is that the sign of the residual is random
test.nlsResiduals(r2)

# Look at the contour/confidence region of the parameters ######################
# m2rss = nlsContourRSS(m2)
m2cfr = nlsConfRegions(m2)
# plot(m2rss, bounds = TRUE)
plot(m2cfr, bounds = TRUE)


# Jackknife confidence intervals ###############################################
# Uses leave-one-out procedure
m2jack = nlsJack(m2)
plot(m2jack)

# Bootstrap confidence intervals ###############################################
# Uses non-parametric bootstrap
m2boot = nlsBoot(m2)
plot(m2boot, type = "boxplot")

# Plot the data and the fitted model ###########################################
library(modelr)

# モデル期待値の 95% 信頼区間をもとめるとき、
# 係数間の相関をむししたらNG
coefboot = m2boot$coefboot |> as_tibble()
coefboot |> summarise(across(everything(),
                             list(mean = mean,
                                  sd = sd,
                                  lower=~quantile(.x, 0.025),
                                  upper=~quantile(.x, 0.975)))) |> 
  pivot_longer(everything(), 
               names_to = c("parameter", "statistic"),
               names_pattern = "(.*)_(.*)") |> 
  pivot_wider(names_from = statistic,
              values_from = value)

ndata = coefboot |> 
  mutate(par = list(seq(0, max(dset2$par), length = 21))) |> 
  mutate(rate = pmap(list(b0, b1, b2, par), model2)) |> 
  unnest(c(par, rate)) |> 
  group_by(par) |> 
  summarise(across(rate, 
                   list(mean = mean, sd = sd,
                        lower = ~quantile(.x, 0.025),
                        upper = ~quantile(.x, 0.975))))

# ナイーブの係数と 95% 信頼区間
summary(m2) |> str()
cfs = summary(m2)$coefficients |>
  as_tibble(rownames = "coefficient") |> 
  select(!`t value`) |> 
  select(!matches("Pr")) |> 
  rename(se = `Std. Error`) |> 
  mutate(lower = Estimate - 1.96*se,
         upper = Estimate + 1.96*se) |> 
  select(-se)

PAR = seq(0, max(dset2$par), length = 21)
cfsE = cfs$Estimate
cfsL = cfs$lower
cfsU = cfs$upper
naive_cfs = tibble(par = PAR) |> 
  mutate(pred = model2(cfsE[1],
                       cfsE[2],
                       cfsE[3], PAR)) |> 
  mutate(lower = model2(cfsL[1],
                       cfsL[2],
                       cfsL[3], PAR)) |> 
  mutate(upper = model2(cfsU[1],
                       cfsU[2],
                       cfsU[3], PAR))
####
ggplot(dset2)+
  geom_point(aes(x = par, y = rate)) +
  # geom_ribbon(aes(x = par, 
  #                 ymin = rate_lower, 
  #                 ymax = rate_upper, fill = "bootstrap"),
  #             data = ndata, alpha = 0.8) +
  geom_ribbon(aes(x = par, 
                  ymin = lower, 
                  ymax = upper, fill = "naive"),
              data = naive_cfs, alpha = 0.8) +
  # geom_line(aes(x = par, y = rate_mean, color = "bootstrap"),
  #           data = ndata, size = 2) +
  # geom_line(aes(x = par, y = pred, color = "naive"),
  #           data = naive_cfs, size = 2) +
  scale_color_viridis_d(end = 0.9) 

dset3 = dset2 |> 
  group_by(par) |> 
  summarise(across(rate, list(mean = mean, sd = sd, n = length))) |> 
  mutate(rate_se = rate_sd / sqrt(rate_n))

xlabel = "'Photosynthetic Active Radiation'~(mu*mol~m^{-1}~hr^{-1})"
ylabel = "'Photosynthesis rate'~(mu*g~g[ww]~min^{-1})"
ggplot() + 
  geom_point(aes(x = par, y = rate_mean), data = dset3) +
  geom_errorbar(aes(x = par, 
                    ymin = rate_mean - rate_se,
                    ymax = rate_mean + rate_se), data = dset3,
                width = 0.5) +
  geom_line(aes(x = par, y = rate_mean), data = ndata)  +
  geom_ribbon(aes(x = par, 
                  ymin = rate_lower,
                  ymax = rate_upper), data = ndata,
              alpha = 0.2) +
  scale_x_continuous(parse(text = xlabel)) + 
  scale_y_continuous(parse(text = ylabel))


# Using the Levenberg-Marquadt algorithm #######################################
# This algorithm can improve the ability to fit a difficult model
# nls() uses the Gauss-Newton algorithm as default
library(minpack.lm)

m2lm = nlsLM(rate ~ model2(b0, b1, b2, par), data = dset2, 
             lower = c(0, 0,0), start = S2)
summary(m2lm)


ggplot(dset2) + 
  geom_point(aes(x = par, y = rate)) +
  facet_wrap("sample")

# サンプルごとに非線形モデルをあてはめる方法
# 
# dset2 = dset2 |> mutate(sample = factor(sample))
# 
# nlsList(rate ~ model2(b0, b1, b2, par) + 1|sample, 
#         data = dset2, start = S2)

# Fit a model to each sample 
fitmodel2 = function(z) {
  nls(rate ~ model2(b0, b1, b2, par), data = z, 
         start = c(b0 = 10, b1 = 30, b2 = 10)) 
}

dset3 = dset2 |> 
  group_nest(sample) |> 
  mutate(model = map(data, fitmodel2)) |> 
  mutate(coefs = map(model, broom::tidy)) |> 
  unnest(coefs)

ggplot(dset3) + 
  geom_point(aes(x = term, y = estimate, color = sample),
             position = position_jitter(0.1))
dset3 |> 
  group_by(term) |> 
  summarise(across(estimate, 
            list(mean = mean,
                 sd = sd))) |> 
  mutate(se = estimate_sd / sqrt(10))

################################
# 複数グループの比較
# 非線形モデルの決定家数の解説
# ここからB日程３日目午前の実習内容
library(tidyverse)
library(nlstools)
library(emmeans)

Puromycin2 = Puromycin |> as_tibble()

ggplot(Puromycin2) + 
  geom_point(aes(x = conc, y = rate, color = state ),
             size = 5)

mm_model = function(vmax,k,x) {
  # Michaelis Menten model
  vmax * x / (k + x)
}

S = list(v = 200, k = 0.10)
preview(rate ~ mm_model(v, k, conc),
        data = Puromycin2,
        variable = 1,
        start = S)
# pooled model.
m0 = nls(rate ~ mm_model(v,k,conc),
         data = Puromycin, 
         start = S)
summary(m0)

pdata = Puromycin2 |> distinct(conc, state)
pdata = pdata |> mutate(yhat = predict(m0, newdata = pdata))

# pooled model
ggplot(Puromycin2) + 
  geom_point(aes(x = conc, y = rate, color = state ),
             size = 5) +
  geom_line(aes(x = conc, y = yhat, color = state),
            data = pdata) +
  facet_wrap("state")


# full model.
cfs0 = coefficients(m0)
S = list(v = rep(cfs0[1], 2),
         k = rep(cfs0[2], 2))

m3 = nls(rate ~ mm_model(v[state], k[state],conc),
         data = Puromycin2, 
         start = S)
summary(m3)

pdata3 = Puromycin2 |> distinct(conc, state)
pdata3 = pdata3 |> mutate(yhat = predict(m3, newdata = pdata3))

# pooled and full model
ggplot(Puromycin2) + 
  geom_point(aes(x = conc, y = rate), size = 5) +
  geom_line(aes(x = conc, y = yhat, color = "pooled"),
            data = pdata) +
  geom_line(aes(x = conc, y = yhat, color = "full"),
            data = pdata3) +
  facet_wrap("state")

# v-state model, k-state model
S = list(v = cfs0[1],
         k = rep(cfs0[2], 2))
mv = nls(rate ~ mm_model(v, k[state],conc),
         data = Puromycin2, 
         start = S)
S = list(v = rep(cfs0[1], 2),
         k = cfs0[2])
mk= nls(rate ~ mm_model(v[state], k,conc),
         data = Puromycin2, 
         start = S)

broom::tidy(m0)
broom::tidy(mk)
broom::tidy(mv)
broom::tidy(m3)

# モデル選択
AIC(m0, mk, mv, m3) |> 
  as_tibble(rownames = "model") |> 
  arrange(AIC) |> 
  mutate(dAIC = AIC - lag(AIC))

# mk の期待値
pdatak = Puromycin2 |> distinct(conc, state)
pdatak = pdatak |> mutate(yhat = predict(mk, newdata = pdatak))

# pooled, full, mk model
ggplot(Puromycin2) + 
  geom_point(aes(x = conc, y = rate), size = 5) +
  geom_line(aes(x = conc, y = yhat, color = "pooled"),
            data = pdata) +
  geom_line(aes(x = conc, y = yhat, color = "full"),
            data = pdata3) +
  geom_line(aes(x = conc, y = yhat, color = "mk"),
            data = pdatak) +
  facet_wrap("state")

# 残渣確認

Puromycin2 = Puromycin2 |> 
  mutate(yhat0 = fitted(m0),
         yhat3 = fitted(m3),
         yhatk = fitted(mk),
         yres0 = residuals(m0),
         yres3 = residuals(m3),
         yresk = residuals(mk))

# qqplot
Puromycin3 = Puromycin2 |> 
  pivot_longer(cols = matches("(yhat)|(yres)"),
               names_to = c(".value", "model"),
               names_pattern = "(y[a-z]{3})(.)")

library(ggpubr)
theme_pubr(base_size = 20) |> theme_set()

ggplot(Puromycin3) + 
  geom_qq(aes(sample = yres, color = model)) + 
  geom_qq_line(aes(sample = yres, color = model))  +
  facet_wrap("model")


Puromycin3 = Puromycin3 |> 
  mutate(yresstd = sqrt(abs(yres / sd(yres))))

ggplot(Puromycin3) +
  geom_point(aes(x = yhat, 
                 y = yres, 
                 color = model), size = 3) +
  # geom_smooth(aes(x = yhat, y = yres, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 3) +
  facet_wrap("model")

ggplot(Puromycin3) +
  geom_point(aes(x = yhat, 
                 y = yresstd, 
                 color = model), size = 3) +
  geom_smooth(aes(x = yhat, y = yresstd, color = model)) +
  facet_wrap("model")


ggplot(Puromycin3) +
  geom_point(aes(x = yhat, 
                 y = yresstd, 
                 color = state), size = 3) +
  geom_smooth(aes(x = yhat, y = yresstd, color = state),
              se = FALSE) +
  facet_wrap("model")

mk |> summary()

# emmeans(mk, specs = pairwise ~ state) # モデルの工夫が必要

# 複数の係数があるとできない。
# mkcfr = nlsConfRegions(mk)


# 決定係数
# R2 = 1 - unexplained variation / total variation
# ∑(y - ybar)^2 = ∑(yhat - ybar)^2 + ∑(y - yhat)^2
# Total variation = Explained variation + Unexplained variation

x = c(1, 2, 3, 4, 5, 6)
y = c(15, 37, 52, 59, 83, 92)

ggplot() + 
  geom_point(aes(x = x, y = y), size = 3) +
  geom_line(aes(x = x, y = fitted(mout)), color = "red") +
  geom_hline(yintercept = mean(y))

r21 = function(y, yhat) {
  rss = (y - yhat)^2
  rst = (y - mean(y))^2
  1 - sum(rss)/sum(rst)
}
r22 = function(y, yhat) {
  numerator = (yhat - mean(y))^2
  denominator = (y - mean(y))^2
  sum(numerator)/sum(denominator)
}
r23 = function(y, yhat){ 
  numerator = (yhat - mean(yhat))^2
  denominator = (y - mean(y))^2
  sum(numerator)/sum(denominator)
}

mout = lm(Sepal.Length ~ Petal.Length, data = iris)
r21(iris$Sepal.Length, fitted(mout))
r22(iris$Sepal.Length, fitted(mout))
r23(iris$Sepal.Length, fitted(mout))

# y = a * x ^b
# 決定係数 (coefficient of determination)
# 当てはめの良さ (goodness-of-fit)は決定係数で表現できない
# 
mout = nls(y ~ a * x^b, start = list(a = 1, b = 1)) 
mout |> summary()
r21(y, fitted(mout))
r22(y, fitted(mout))
r23(y, fitted(mout))

mnull = lm(y~1)
mline = lm(y~x)

AIC(mnull) # AICの上限
(AIC(mnull) - AIC(mout)) / AIC(mnull) 
(AIC(mnull) - AIC(mline)) / AIC(mnull) 

# Non-linear models with a fixed effect ########################################
library(nlme)
dset2 = dset2 |> mutate(sample = factor(sample))

S2 = list(b0 = rep(10, 10),
          b1 = rep(30, 10),
          b2 = rep(10, 10))
m2g = gnls(rate ~ model2(b0, b1, b2, par), 
           data = dset2, b0+b1+b2 ~ sample,
           start = S2)
summary(m2g)
m2g = gnls(rate ~ model2(b0, b1, b2, par), 
           data = dset2, b0+b1+b2 ~ 0+sample,
     start = S2)
AIC(m2)
AIC(m2g)
summary(m2g)
m2r = residuals(m2g)

coefficients(m2g) |> as_tibble_col() |> 
  mutate(param = names(coefficients(m2g))) |> 
  separate(param, c("parameter", "sample")) |> 
  group_by(parameter) |> 
  summarise(m = mean(value),
            s = sd(value),
            n = length(value)) |> 
  mutate(se = s / sqrt(n))
summary(m2)

dset2 = dset2 |> ungroup() |> mutate(resid = m2r) |> 
  mutate(nresid = resid / m2g$sigma) |> 
  mutate(sqresid = sqrt(abs(nresid)),
         fit = fitted(m2g))

ggplot(dset2) + geom_point(aes(x = fit, y = sqresid))


m2g = nlme(rate ~ model2(b0, b1, b2, par), 
           data = dset2, 
           fixed = b0+b1+b2 ~ 1,
           random = b0+b1+b2 ~ 1,
           start = coefficients(m2), 
           control = list(msMaxIter = 10000))
summary(m2g)
coef(m2g) |> as_tibble(rownames = "sample")
