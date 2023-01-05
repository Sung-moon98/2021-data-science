library("tidyverse")
library("data.table")
library("PerformanceAnalytics") 
options(repr.plot.width=10, repr.plot.height=10)

DF <- fread("Pre_Season_Batter.csv") %>% as_tibble

# 데이터 전처리
input <- DF %>% na.omit() %>%
  select(-`height/weight`, -year_born, -position, -career, -starting_salary)
input <- input %>% mutate(H = H - `2B` - `3B` - HR)

# team KIA 선택
input <- input %>% filter(team == "KIA")

# 단일회귀분석 1
# R(득점)과 H(안타)
plot(input$H, input$R, col = "blue", xlab="H", ylab="R", pch=19)  # 득점과 안타의 시각화

m1 <- lm(R ~ H, data=input)    # 모델생성
abline(m1)   # 회귀선 그리기
summary(m1)  # 모델 분석

new1 <- data.frame(H = c(1, 6, 3, 10, 7))
p1 = predict(m1, new1)     # 임의의 수를 이용한 예측
print(p1)

# 단일회귀분석 2
# R(득점)과 AB(타수)
plot(input$AB, input$R, col = "red", xlab="AB", ylab="R", pch=19)  # 득점과 타수의 시각화

m2 <- lm(R ~ AB, data=input)    # 모델생성
abline(m2)   # 회귀선 그리기
summary(m2)  # 모델 분석

new2 <- data.frame(AB = c(22, 35, 13, 27, 10))
p2 = predict(m2, new2)     # 임의의 수를 이용한 예측
print(p2)


library(scatterplot3d)

# 다중회귀분석 1
# R(득점)과 AB(타수), H(안타)
s1 = scatterplot3d(input$AB, input$R, input$H, pch=20, type = 'h')    # 3차원 평면상에 산점도를 나타낸다

m3 <- lm(R ~ AB+H, data = input)    # 모델생성
s1$plane3d(m3)    # 회귀평면 그리기
summary(m3)       # 모델 분석

new3 <- data.frame(AB = c(22, 35, 13, 27, 10), H = c(1, 6, 3, 10, 7))
p3 <- predict(m3, new3)     # 임의의 수를 이용한 예측
print(p3)


# 다중회귀분석 2
# R(득점)과 TB(루타 수), G(출전게임 수)
s2 = scatterplot3d(input$TB, input$R, input$G, pch=20, type='h')       # 3차원 평면상에 산점도를 나타낸다

m4 <- lm(R ~ TB+G, data=input)    # 모델생성
s2$plane3d(m4)        # 회귀평면 그리기기
summary(m4)           # 모델 분석

new4 <- data.frame(TB = c(11, 8, 17, 14, 20), G = c(6, 10, 3, 5, 8))
p4 <- predict(m4, new4)
p4
