library(survival)
data("colon", package = "survial")
colon <- as_tibble(colon)

# 데이터 전처리
clean_colon <- na.omit(colon)
clean_colon <- clean_colon %>% filter(etype == 1)

# 데이터를 training set(90%)과 test set(10%)으로 나눈다, status 열을 기준으로!
library(caret)
library(ggpubr)
set.seed(31)
index <- createDataPartition(y = clean_colon$status, p = 0.9, list = FALSE) 
head(index, 5)
train <- clean_colon[index, ]
test <- clean_colon[-index, ]

# 데이터 시각화
plot(train)

# extent, age, sex, nodes와 status의 상관관계를 파악하기위해 시각화
p1 <- train %>% ggplot(aes(extent, status)) + geom_jitter(aes(col = factor(status)), height=0.1, width=0.1)
p2 <- train %>% ggplot(aes(age, status)) + geom_jitter(aes(col = factor(status)), height=0.1, width=0.1)
p3 <- train %>% ggplot(aes(sex, status)) + geom_jitter(aes(col = factor(status)), height=0.1, width=0.1)
p4 <- train %>% ggplot(aes(nodes, status)) + geom_jitter(aes(col = factor(status)), height=0.1, width=0.1)

ggarrange(p1, p2, p3, p4, labels = c("extent", "age", "sex", "nodes"))

# train 데이터 전처리, 설명변수로 의미가 없는 변수들 제외
train_set <- train %>% select(-id, -time, -etype, -study)
summary(train_set)

# 모델생성
m <- glm(status~., data = train_set, family = binomial)
summary(m)

# 유의하지 않은 설명변수들을 제외하고 새롭게 모델생성
m1 <- step(m, direction = "backward")
summary(m1)
m2 <- update(m1, ~. -obstruct)
summary(m2)

# test 데이터를 이용한 예측
predict(m2, test, type = "response") %>%
  tibble(predict_status = .) %>% bind_cols(test, .) %>% View()
