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
m2rss = nlsContourRSS(m2)
m2cfr = nlsConfRegions(m2)
plot(m2rss, bounds = TRUE)
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

coefboot = m2boot$coefboot |> as_tibble()

ndata = coefboot |> 
  mutate(par = list(seq(0, max(dset2$par), length = 21))) |> 
  mutate(rate = pmap(list(b0, b1, b2, par), model2)) |> 
  unnest(c(par, rate)) |> 
  group_by(par) |> 
  summarise(across(rate, 
                   list(mean = mean, sd = sd,
                        lower = ~quantile(.x, 0.025),
                        upper = ~quantile(.x, 0.975))))

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
             lower = c(0, 0,0), start = S)
summary(m2lm)


nlsList(rate ~ model2(b0, b1, b2, par) + 1|sample, 
        data = dset2, start = S)

# Fit a model to each sample 

fitmodel2 = function(z) {
  nls(rate ~ model2(b0, b1, b2, par), data = z, 
         start = c(b0 = 10, b1 = 30, b2 = 10)) 
}

dset2 |> 
  group_nest(sample) |> 
  mutate(model = map(data, fitmodel2)) |> 
  mutate(coefs = map(model, broom::tidy)) |> 
  unnest(coefs)

# Non-linear models with a fixed effect ########################################
library(nlme)
dset2 = dset2 |> mutate(sample = factor(sample))

S2 = list(b0 = rep(10, 10),
          b1 = rep(30, 10),
          b2 = rep(10, 10))
m2g = gnls(rate ~ model2(b0, b1, b2, par), data = dset2, b0+b1+b2 ~ sample,
           start = S2)
summary(m2g)
m2g = gnls(rate ~ model2(b0, b1, b2, par), data = dset2, b0+b1+b2 ~ 0+sample,
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
