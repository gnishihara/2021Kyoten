# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（A日程）
# Day 02
 
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

ggsave(filename = "day2plot.png") # 最後に表示された図を保存

ggsave(filename = "day2plot01.png",
       plot = plot1, 
       width = 200, height = 200, unit = "mm",
       dpi = 600)

ggsave(filename = "day2plot01.pdf",
       plot = plot1, 
       device = cairo_pdf,
       width = 200, height = 200, unit = "mm")

# magick #####################################################################
library(magick)
library()

img = image_read_pdf("day2plot01.pdf", density = 600)
img |> image_write("day2plot01.png")

################################################################################
# read_csv() ###################################################################
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
URL2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTt_rEpmL7eWJtOc5MprXeihH4LTXkE9CyoLyPext6j3_9wbYAQAXQlEiSOs_Hse0hGLH6-zb6NJVKu/pub?output=csv"
URL3 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjodK7zDfhObB8OXSgfq0ZMJQh2d1Q__TFoSJ-6pPnsz50QE34xdJuJ5HQzR5RvyprB2GvsQYPw6Q4/pub?output=csv"
URL4 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRn0FUJhsKmyjTNPqahTZnhYvXP-ox6_ze1rwmMCFqpil3vyxsmfBgR-3NyGwfriH8z7xh4v38JemnU/pub?output=csv"

dall = tibble(url = c(URL1, URL2, URL3, URL4)) |> 
  mutate(data = map(url, read_csv, skip = 1))

dall = dall |> select(data) |> unnest(data)

dall = dall |> 
  rename(id = `#`,
         datetime = matches("日付"),
         light = matches("PAR"),
         wind = matches("風速"),
         gust = matches("突風"),
         mbar = matches("mbar")) |> 
  mutate(datetime = parse_datetime(datetime,
                                    "%m/%d/%Y %I:%M:%S %p",
                                    locale = locale("ja"))) |> 
  mutate(datetime = floor_date(datetime, "minutes"))

dall = dall |> 
  mutate(date = as_date(datetime)) |> 
  group_by(date) |> 
  filter(near(length(wind), 144))

dall2 = dall |> group_by(date) |> 
  summarise(across(c(wind, gust), mean))

dall2 = dall2 |> mutate(year = year(date),
                month = month(date),
                ym = floor_date(date, "month"))

dall2 = dall2 |> group_by(ym, year, month) |> 
  summarise(across(c(gust, wind),
                   list(mean = mean, 
                        sd = sd,
                        sample = length))) 

ggplot(dall2) + 
  geom_point(aes(x = month, y = wind_mean))

# ylabel = "Wind speed (m/s)"
ylabel = "'Wind speed'~(m~s^{-1})" # ?plotmath

ggplot(dall2) + 
  geom_point(aes(x = ym, y = wind_mean),
             size = 5) +
  geom_errorbar(aes(x = ym,
                    ymin = wind_mean - wind_sd/sqrt(wind_sample),
                    ymax = wind_mean + wind_sd/sqrt(wind_sample)),
                width = 0,
                size = 2) +
  scale_x_date("Year-Month",
               date_breaks = "months",
               date_labels = "%Y-%m") +
  scale_y_continuous(parse(text = ylabel),
                     limit = c(0, 2),
                     breaks = seq(0,2, by = 0.5))

dall_daily = dall |> group_by(date) |> 
  summarise(across(c(wind, gust), mean))

dall_daily = dall_daily |> 
  mutate(ym = floor_date(date, "month"))

ggplot() + 
  geom_point(aes(x = ym, y = wind_mean), 
             data = dall2,
             size = 1.0) +
  geom_errorbar(aes(x = ym,
                    ymin = wind_mean - wind_sd/sqrt(wind_sample),
                    ymax = wind_mean + wind_sd/sqrt(wind_sample)),
                data = dall2,
                width = 0,
                size = 0.75) +
  geom_point(aes(x = ym, y = wind),
             data = dall_daily,
             alpha = 0.5,
             position = position_jitter(width = 5),
             size = 0.5) +
  geom_text(aes(x = ym, y = 0, 
                label = sprintf("%0.3f ± %0.3f", 
                                wind_mean, 
                                wind_sd/sqrt(wind_sample))),
            data = dall2,
            size = 2,
            family = "notosans") + 
  scale_x_date("Year-Month",
               date_breaks = "months",
               date_labels = "%Y-%b") +
  scale_y_continuous(parse(text = ylabel),
                     limit = c(0, 6),
                     breaks = seq(0, 6, by = 2)) +
  theme(text = element_text(size = 10))

showtext_auto()
ggsave("windspeed.pdf", width = 2*80, 
       height = 80, units = "mm")

ggsave("windspeed.svg", width = 2*80, 
       height = 80, units = "mm")

  
"2020-10-01"

dall2$ym

gnn_date = function() {
  function(x) {
    m = format(x, "%b")
    # m = str_sub(m, start = 1, end = 1)
    y = format(x, "%Y")
    ifelse(duplicated(y), m, sprintf("%s\n%s", m,y))
  }
}


ggplot() + 
  geom_point(aes(x = ym, y = wind_mean), 
             data = dall2,
             size = 1.0) +
  geom_errorbar(aes(x = ym,
                    ymin = wind_mean - wind_sd/sqrt(wind_sample),
                    ymax = wind_mean + wind_sd/sqrt(wind_sample)),
                data = dall2,
                width = 0,
                size = 0.75) +
  geom_point(aes(x = ym, y = wind),
             data = dall_daily,
             alpha = 0.5,
             position = position_jitter(width = 5),
             size = 0.5) +
  geom_text(aes(x = ym, y = 0, 
                label = sprintf("%0.3f ± %0.3f", 
                                wind_mean, 
                                wind_sd/sqrt(wind_sample))),
            data = dall2,
            size = 2,
            family = "notosans") + 
  scale_x_date("Year-Month",
               date_breaks = "months",
               labels = gnn_date()) +
  scale_y_continuous(parse(text = ylabel),
                     limit = c(0, 6),
                     breaks = seq(0, 6, by = 2)) +
  theme(text = element_text(size = 10))

showtext_auto()
ggsave("windspeed.pdf", width = 2*80, 
       height = 80, units = "mm")

################################################################################
################################################################################
################################################################################
################################################################################
# Back to Iris
library(viridis)

iris2 = iris |> as_tibble()
# ?plotmath
iris2 = iris2 |> 
  mutate(Species = str_glue("italic('Iris {Species}')"))
# italic(Iris versicolor)
# 
xlabel = "Sepal width (cm)"
ylabel = "Sepal length (cm)"
ggplot(iris2) + 
  geom_point(aes(x = Sepal.Width, 
                 y = Sepal.Length, 
                 color = Species,
                 shape = Species),
             size = 3, alpha = 0.5) +
  scale_shape_discrete(labels = scales::parse_format()) +
  scale_color_viridis(discrete = TRUE, 
                      end = 0.9,
                      labels = scales::parse_format()) +
  scale_x_continuous(xlabel, 
                     limits = c(2, 5),
                     breaks = seq(2, 5, by = 1)) +
  scale_y_continuous(ylabel,
                     limits = c(4, 8),
                     breaks = seq(4, 8, by = 1)) +
  guides(color = guide_legend(title = "",
                              override.aes = list(size = 5),
                              label.hjust = 0),
         shape = guide_legend(title = "",
                              label.hjust = 0)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_blank())

showtext_auto()
ggsave("iris.pdf", width = 2.5*80, 
       height = 2*80, units = "mm")


iris3 = iris2 |> 
  group_by(Species) |> 
  summarise(across(matches("Sepal"),
                   list(mean = mean,
                        sd = sd)))

xlabel = "Sepal width (cm)"
ylabel = "Sepal length (cm)"

ggplot(iris3) +
  geom_errorbar(aes(x = Sepal.Width_mean,
                    y = Sepal.Length_mean,
                    xmin = Sepal.Width_mean - Sepal.Width_sd,
                    xmax = Sepal.Width_mean + Sepal.Width_sd,
                    color = Species),
                width = 0) +
  geom_errorbar(aes(x = Sepal.Width_mean,
                    y = Sepal.Length_mean,
                    ymin = Sepal.Length_mean - Sepal.Length_sd,
                    ymax = Sepal.Length_mean + Sepal.Length_sd,
                    color = Species),
                width = 0) +
  geom_point(aes(x = Sepal.Width_mean,
                 y = Sepal.Length_mean,
                 color = Species,
                 shape = Species),
             size = 5) +
  scale_shape_discrete(labels = scales::parse_format()) +
  scale_color_viridis(discrete = TRUE, 
                      end = 0.9,
                      labels = scales::parse_format()) +
  scale_x_continuous(xlabel, 
                     limits = c(2, 5),
                     breaks = seq(2, 5, by = 1)) +
  scale_y_continuous(ylabel,
                     limits = c(4, 8),
                     breaks = seq(4, 8, by = 1)) +
  guides(color = guide_legend(title = "",
                              override.aes = list(size = 5,
                                                  linetype = NA),
                              label.hjust = 0),
         shape = "none") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_blank())



  showtext_auto()
ggsave("iris2.pdf", width = 2.5*80, 
       height = 2*80, units = "mm")



xlabel = "Sepal width (cm)"
ylabel = "Sepal length (cm)"
ggplot(iris2) + 
  geom_point(aes(x = Sepal.Width, 
                 y = Sepal.Length, 
                 color = Species,
                 shape = Species),
             size = 3, alpha = 0.5) +
  geom_smooth(aes(x = Sepal.Width,
                  y = Sepal.Length,
                  color = Species),
              method = "lm",
              formula = y ~ x,
              se = FALSE) +
  scale_shape_discrete(labels = scales::parse_format()) +
  scale_color_viridis(discrete = TRUE, 
                      end = 0.9,
                      labels = scales::parse_format()) +
  scale_x_continuous(xlabel, 
                     limits = c(2, 5),
                     breaks = seq(2, 5, by = 1)) +
  scale_y_continuous(ylabel,
                     limits = c(4, 8),
                     breaks = seq(4, 8, by = 1)) +
  guides(color = guide_legend(title = "",
                              override.aes = list(size = 5,
                                                  linetype = NA,
                                                  fill = NA),
                              label.hjust = 0),
         shape = "none",
         linetype = "none") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_blank())

showtext_auto()
ggsave("iris_line.pdf", width = 2.5*80, 
       height = 2*80, units = "mm")


