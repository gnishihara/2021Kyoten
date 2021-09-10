# 2021年度 長崎大学全国教育関係共同利用拠点
# 公開臨海実習
# 水産海洋データ解析演習（A日程）
# 2021 / 09 / 10 (Day 1)
# Greg Nishihara

# パッケージのインストール
# install.packages("tidyverse")
# install.packages("lubridate")

# パッケージの読み込み #########################################################
library(tidyverse)
library(lubridate)

# 基本コーディング作業 #########################################################
# c(), list(), data.frame(), array()

a = c(20, 10, 30)
# b = list("A", "C", "Z")
b = c("A", "C", "Z")
data.frame(a, b)
array(data = c(1:9), dim = c(3, 3))
matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
array(data = c(1:3^3), dim = c(3, 3, 3))
1:9
seq(1, 9, by = 1)
seq(1, 9, by = 0.5)

# iris データの紹介 ############################################################
iris

# base R 操作 ##################################################################
colnames(iris) # 変数名
rownames(iris) # 行の名前

# $, [], [[]] ##################################################################

x = iris$Petal.Length
mean(x)
sd(x)
var(x)
sd(x) / sqrt(length(x)) 

iris[1, 1]

rownumber = 10
colnumber = 2
iris[rownumber, colnumber]

Z = array(data = c(1:3^3), dim = c(3, 3, 3))
Z[ 3 , 3, 3]

iris[[2]]

## tidyverse starts here #######################################################
# tibble 化 ####################################################################

iris2 = iris  |> as_tibble()

# mutate() #####################################################################
 
# select() #####################################################################

# rename() #####################################################################

# relocate() ###################################################################

# pull() #######################################################################

# filter() #####################################################################

# distinct() ###################################################################

# slice() ######################################################################

# arrange() ####################################################################

# group_by(), group_nest(), unnest() ###########################################

# group_map(), group_modify() ##################################################

# drop_na(), replace_na() ######################################################

# separate(), unite() ##########################################################
 
################################################################################ 

# read_csv() ###################################################################
# HOBO H21-002 Microstation Data @ ECSER
URL1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0FUr9fZ8SbFw3UGS6lulZqbzqW34jtlRj5VmKN8S8QcS4vjYmRafC7v6fwoNljMUlJVTlYRkbrui5/pub?output=csv"
URL2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTt_rEpmL7eWJtOc5MprXeihH4LTXkE9CyoLyPext6j3_9wbYAQAXQlEiSOs_Hse0hGLH6-zb6NJVKu/pub?output=csv"
URL3 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSjodK7zDfhObB8OXSgfq0ZMJQh2d1Q__TFoSJ-6pPnsz50QE34xdJuJ5HQzR5RvyprB2GvsQYPw6Q4/pub?output=csv"
URL4 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRn0FUJhsKmyjTNPqahTZnhYvXP-ox6_ze1rwmMCFqpil3vyxsmfBgR-3NyGwfriH8z7xh4v38JemnU/pub?output=csv"

# parse_date(), ymd_hms() ######################################################

# separate wind and PAR+mbar data ##############################################

# write_csv() ##################################################################
 
################################################################################
# full_join(), inner_join() ####################################################

# left_join(), right_join() ####################################################

# bind_rows(), bind_cols() #####################################################

################################################################################
