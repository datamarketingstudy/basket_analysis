library(readxl)
library(dplyr)
library(ggplot2)
library(dplyr)


# 데이터 읽기

g_df <- read.csv("rawdata/groceries.csv", header = FALSE)

str(g_df)
head(g_df)

# 데이터 준비 : 거래 데이터를 위한 희소 행렬 생성

library(arules)

groceries <- read.transactions("rawdata/groceries.csv", sep = ",")
summary(groceries)

inspect(groceries[1:5])

# 아이템 지지도 시각화 : 아이템 빈도 그래프

itemFrequency(groceries[,1:6])

itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# 거래 데이터 시각화 : 희소 행렬 도표화

image(groceries[1:5])
image(sample(groceries, 100)) # 100개 샘플

# 데이터에 대한 모델 훈련

groceriesrule <- apriori(data = groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
summary(groceriesrule)

inspect(groceriesrule[1:3])

# 모델 성능 개선

View(groceriesrule)

inspect(sort(groceriesrule, by = "lift")[1:5])

berryrules <- subset(groceriesrule, items %in% "berries")
inspect(berryrules)

# 연관규칙을 파일이나 데이터 프레임에 저장

write(groceriesrule, file = "groceriesrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
