
data <- data.frame(
  Student = paste("Student", 1:15), # Names of students
  Class = rep(c("Class 1", "Class 2", "Class 3"), each = 5), # Class assignments
  Korean = c(100, 100, 100, 100, 95, 82, 87, 75, 89, 91, 80, 50, 66, 77, 84), # Korean scores
  Math = c(100, 100, 100, 90, 95, 84, 79, 90, 86, 83, 78, 82, 88, 81, 85)    # Math scores
)

data

mean(data$Korean)
sd(data$Korean)
mean(data$Math)
sd(data$Math)


# Independent t-test comparing Math scores between Class 1 and Class 2
independent_ttest <- t.test(Math ~ Class, data = subset(data, Class %in% c("Class 1", "Class 2")), var.equal = TRUE) # Testing mean difference between classes
independent_ttest # Display the test result

anova_result <- aov(Math ~ Class, data = data)
summary(anova_result)




## t-test 직접 계산

class1 <- data$Korean[data$Class == "Class 1"]
class2 <- data$Korean[data$Class == "Class 2"]

# 평균 계산
mean1 <- mean(class1)
mean2 <- mean(class2)

# 분산 계산
var1 <- sum((class1 - mean1)^2) / (length(class1) - 1)
var2 <- sum((class2 - mean2)^2) / (length(class2) - 1)

# 표본 크기
n1 <- length(class1)
n2 <- length(class2)

# t-값 계산
t_value <- (mean1 - mean2) / sqrt((var1 / n1) + (var2 / n2))

# 자유도 계산 (Welch's t-test)
df <- n1 + n2 - 2

cat("t-value:", t_value, "\n")
cat("Degrees of freedom:", df, "\n")


p_value <- 2 * (1 - pt(abs(t_value), df))

# 출력
cat("P-value:", p_value, "\n")


## ANOVA 직접 계산

# 1. 전체 평균 계산 (Overall Mean)
overall_mean <- mean(data$Math)

# 2. 그룹별 평균 계산
group_means <- tapply(data$Math, data$Class, mean)

# 3. 그룹별 데이터 크기
group_sizes <- tapply(data$Math, data$Class, length)

# 4. 그룹 간 제곱합 (SS_between)
# SS_between = Σ n_i * (mean_i - overall_mean)^2
SS_between <- sum(group_sizes * (group_means - overall_mean)^2)

# 5. 그룹 내 제곱합 (SS_within)
# SS_within = Σ Σ (X_ij - mean_i)^2
SS_within <- sum(tapply(data$Math, data$Class, function(group) sum((group - mean(group))^2)))

# 6. 자유도 계산
k <- length(group_means)          # 그룹 수
N <- nrow(data)                   # 전체 데이터 포인트 수
df_between <- k - 1               # 그룹 간 자유도
df_within <- N - k                # 그룹 내 자유도

# 7. 평균 제곱 계산 (Mean Squares)
# MS_between = SS_between / df_between
# MS_within = SS_within / df_within
MS_between <- SS_between / df_between
MS_within <- SS_within / df_within

# 8. F-값 계산
# F = MS_between / MS_within
F_value <- MS_between / MS_within

# 9. p-값 계산 (P-value for the F-statistic)
# p = P(F > F_value | df_between, df_within)
p_value <- pf(F_value, df_between, df_within, lower.tail = FALSE)

