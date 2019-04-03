data_total <- read.csv("C:/대학원/2018-2/1. 전공/임상시험자료분석2/프로젝트/기본DB/data_2013_2015(수정).csv",na.strings=".")
colnames(data) <- c("ID","year","region","sex","age","age_month","DE1_pr","BP8","BP1","BP6_10","BP6_2","BP6_31","HE_glu","HE_DM")
data_total <- data_total[,-6]

table(data_total$HE_DM)


#-----------------------------------------------------------------------------------------------------------#

############################################
###########   LIBRARY & DATA   #############
############################################
library(tidyverse)
library(car)
library(nnet)
data <- read.csv("C:/대학원/2018-2/1. 전공/임상시험자료분석2/프로젝트/기본DB/data_2013_2015(수정).csv", header = T, na.strings = ".")
colnames(data) <- c("ID","year","region","sex","age","age_month","DE1_pr","BP8","BP1","BP6_10","BP6_2","BP6_31","HE_glu","HE_DM")

#나이 20세 이상 & 연령대 변수 생성
data$age <- parse_number(data$age)
data <- data %>% filter(age >= 20) %>% mutate(age_new = (age %/% 10 ) * 10) %>% select(-age_month)

#정상 : 9095명 / 공복혈당장애 : 3189명 / 당뇨병 : 1602명
data %>% group_by(HE_DM) %>% summarise(n = n())

#당뇨병 유병 여부 결측치 제거 & 우울증 변수들 비해당 제거
data <- data %>% filter(HE_DM %in% c(1, 2, 3))

#수면시간 99 제거 & 스트레스 인지 정도 9 제거 & 수면시간 6, 7, 8 기준 새로운 변수 생성
data <- data %>% filter(BP8 != 99 & BP1 != 9)
data$BP8 <- parse_number(data$BP8)
data$BP8_new <- ifelse(data[,"BP8"] <= 6, "low", ifelse(data[,"BP8"] == 7, "medium", "high"))



############################################
###########        EDA       ###############
############################################

#당뇨병 유병 여부 histogram
ggplot(data, aes(HE_DM, fill = as.factor(HE_DM))) + geom_bar() + xlab("당뇨병 유병 여부") +
  ggtitle("Histogram of 당뇨병 유병 여부") + theme_grey() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  scale_fill_manual(values = c("snow1", "Rosy Brown 1", "red1"), name = c("당뇨병 유병 여부"), labels = c("정상", "공복혈당장애", "당뇨병"))

#연령대 histogram
ggplot(data, aes(as.factor(age_new), fill = as.factor(age_new))) + geom_bar() + xlab("연령대") + ggtitle("Histogram of age") +
  theme_grey() + scale_fill_brewer(palette = "Reds", name = c("연령대")) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#당뇨병 여부 별 연령대
ggplot(data, aes(age)) +
  geom_density(aes(group = as.factor(HE_DM), colour = as.factor(HE_DM)), size = 1.3) + theme_grey() +
  ggtitle("당뇨병 여부 별 연령대") + xlab("연령대") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  scale_colour_brewer(palette = "Reds", name = c("당뇨병 여부"), labels = c("정상", "공복혈당장애", "당뇨병"))

#당뇨병 여부 별 수면시간
ggplot(data %>% filter(BP8 != 99), aes(as.factor(BP8))) +
  geom_density(aes(group = as.factor(HE_DM), colour = as.factor(HE_DM)), size = 1.3) + theme_grey() +
  scale_color_manual(values = c("rosy brown 2", "brown1", "red4"), name = c("당뇨병 여부"), labels = c("정상", "공복혈당장애", "당뇨병")) +
  ggtitle("당뇨병 여부 별 수면시간 분포") + xlab("수면시간") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))



###수면시간 확정
# #수면시간
# ggplot(data %>% filter(BP8 != 99), aes(as.factor(BP8))) +
#   geom_density(aes(group = as.factor(HE_DM), colour = as.factor(HE_DM), fill = as.factor(HE_DM)), alpha = 0.2) + theme_grey() +
#   scale_color_manual(values = c("rosy brown 2", "brown1", "red4"), name = c("당뇨병 여부"), labels = c("정상", "공복혈당장애", "당뇨병")) +
#   scale_fill_manual(values = c("rosy brown 2", "brown1", "red4"), name = c("당뇨병 여부"), labels = c("정상", "공복혈당장애", "당뇨병")) +
#   ggtitle("당뇨병 여부 별 수면시간 분포") + xlab("수면시간") +
#   theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#남녀 별 공복혈당수치 분포
ggplot(data, aes(as.factor(sex), HE_glu, group = as.factor(sex), colour = as.factor(sex))) +
  geom_boxplot() + theme_grey() + scale_color_manual(values = c("darkblue", "red3"), name = c("성별"), labels = c("남자", "여자")) +
  ggtitle("남녀 별 공복혈당수치 분포") + xlab("공복혈당수치") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  ylab("공복혈당수치") + scale_x_discrete(labels = c("남자", "여자"))

#공복혈당수치 histogram
ggplot(data, aes(HE_glu)) + geom_histogram(colour = "red1", fill = "white") + xlab("공복혈당수치") + ggtitle("Histogram of 공복혈당수치") +
  theme_grey() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#log공복혈당수치 histogram
ggplot(data, aes(log(HE_glu))) + geom_histogram(colour = "red1", fill = "white") + xlab("공복혈당수치") + ggtitle("Histogram of log(공복혈당수치)") +
  theme_grey() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#연령대 별 공복혈당수치 분포
ggplot(data, aes(as.factor(age_new), HE_glu, group = as.factor(age_new), colour = as.factor(age_new))) +
  geom_boxplot() + theme_grey() + scale_color_brewer(palette = "YlOrRd", name = c("연령대"), labels = c("20대", "30대", "40대", "50대", "60대", "70대", "80대")) +
  ggtitle("연령대 별 공복혈당수치 분포") + xlab("공복혈당수치") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  ylab("공복혈당수치") + scale_x_discrete(labels = c("20대", "30대", "40대", "50대", "60대", "70대", "80대"))



# ##maps
# 
# install.packages(c("ggmap","raster","rgeos","maptools"))
# install.packages("rgdal")
# library(ggmap); library(raster); library(rgeos); library(maptools)
# library(rgdal)
# korea <- shapefile("C:/대학원/2018-2/1. 전공/임상시험자료분석2/프로젝트/image/CTPRVN_201804/TL_SCCO_CTPRVN.shp")
# korea <- spTransform(korea,CRS("+proj=longlat"))
# korea_map <- fortify(korea)
# merge_result <- merge(korea_map,data, by="region")

#지역별 공복혈당수치 
data$region_new <- ifelse(data$region %in% c(1, 4, 8), 1,
                          ifelse(data$region %in% c(9, 16), 5,
                                 ifelse(data$region %in% c(2, 3, 14, 15), 2,
                                        ifelse(data$region %in% c(5, 12, 13), 3, 4))))

ggplot(data, aes(as.factor(region_new), log(HE_glu))) + ylab("log공복혈당수치") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  geom_boxplot(aes(colour = as.factor(region_new))) + ggtitle("지역 별 log공복혈당수치") +
  scale_color_brewer(palette = "YlOrRd", name = c("지역"), labels = c("서울, 경기, 인천", "부산, 울산, 대구, 경상", "광주, 전라", "대전, 충청", "강원, 제주"))


############################################
###########      Modeling    ###############
############################################
#lm
model1 <- lm(HE_glu ~ as.factor(sex) + as.factor(age_new) + BP8_new + as.factor(BP1) + as.factor(region_new), data = data)
summary(model1)
anova(model1)
#모든 변수 유의

#binomial glm (단, 정상 = 0, 공복혈당장애 & 당뇨병 = 1)
data$HE_DM_new <- ifelse(data$HE_DM == 1, 0, 1)
model2 <- glm(HE_DM_new ~ as.factor(sex) + as.factor(age_new) + BP8_new + as.factor(BP1) + as.factor(region_new), family = binomial, data = data)
summary(model2)
#연령대 높아질수록 당뇨 위험 높다.
#광주, 전라도 당뇨 위험 높다. >> 왠지 음식과 관련 있을 듯
#BP1 : 스트레스 인지 낮아질수록 당뇨 위험 적다.
#수면시간 : medium 그룹이 당뇨 위험 제일 적고 low, high는 당뇨 위험 높다.

#poisson glm
model3 <- glm(HE_glu ~ as.factor(sex) + as.factor(age_new) + BP8_new + as.factor(BP1) + as.factor(region_new), family = poisson, data = data)
summary(model3)
summary(predict(model3, type = "response"))