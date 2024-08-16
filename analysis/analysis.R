library(readr)
library(dplyr)
library(ggplot2)

master <- read.csv('/Users/himickey/project_test/master_data.csv')

#(a)記述統計
# 1. 各列に含まれるNAの数を数えなさい。
cat('missing values:\n')
print(colSums(is.na(master)))

# 2. 問題背景などを知る上で役に立つ記述統計を作成しなさい
cat(rep('-',20),'Sumarry Statistics',rep('-',20),'\n')
print(summary(master))

# 3. 4年卒業率の平均推移を計算し、図で示す
mean_gradrate <- master %>%
  group_by(year) %>%
  summarize(mean_gradrate4yr = mean(gradrate4yr, na.rm = TRUE))

ggplot(mean_gradrate, aes(x = year, y = mean_gradrate4yr)) +
  geom_line() +
  labs(x = "Year", y = "4-year Graduation Rate", title = "Figure1. Four-Year Graduation Rates") +
  theme_minimal()

# 4. semester導入率を計算し、図で示す
semester_introduction_rate <- master %>%
  group_by(year) %>%
  summarize(semester_rate = sum(semester, na.rm = TRUE) / sum(!is.na(semester)))

ggplot(semester_introduction_rate, aes(x = year, y = semester_rate)) +
  geom_line() +
  labs(x = "Year", y = "Fraction of schools on semesters", title = "Figure2. Fraction of Schools on Semesters") +
  theme_minimal()


# 5. 3つの変数を横軸、「4年卒業率」を縦軸にとった散布図を作成
master <- master %>%
  mutate(
    per_women_cohort = w_cohortsize / totcohortsize,
    per_white_cohort = white_cohortsize / totcohortsize
  )

plot_scatter <- function(df, x_var) {
  ggplot(df, aes(x = !!x_var, y = gradrate4yr)) +
    geom_point() +
    labs(x = quo_name(x_var), y = "4-year Graduation Rate", title = paste("Scatter Plot of 4-year Graduation Rate vs", quo_name(x_var))) +
    theme_minimal()
}

variables <- c('per_women_cohort', 'per_white_cohort', 'instatetuition')

for (variable in variables) {
  print(plot_scatter(master, sym(variable)))
}

#rlangパッケージを使わない書き方
#variables <- c('per_women_cohort', 'per_white_cohort', 'instatetuition')

#for (variable in variables) {
#  p <- ggplot(master, aes_string(x = variable, y = 'gradrate4yr')) +
#    geom_point() +
#    labs(x = variable, y = "4-year Graduation Rate", title = paste("Scatter Plot of 4-year Graduation Rate vs", variable)) +
#    theme_minimal()

#  print(p)
#}

#(b)回帰分析
model <- lm(gradrate4yr ~ after, data = master)
result <- summary(model)

result_table <- data.frame(
  Estimate = coef(result)[, "Estimate"],
  Std_Error = coef(result)[, "Std. Error"],
  t_value = coef(result)[, "t value"],
  Pr = coef(result)[, "Pr(>|t|)"]
)

print(result_table)