data_total <- read.csv("C:/���п�/2018-2/1. ����/�ӻ�����ڷ�м�2/������Ʈ/�⺻DB/data_2013_2015(����).csv",na.strings=".")
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
data <- read.csv("C:/���п�/2018-2/1. ����/�ӻ�����ڷ�м�2/������Ʈ/�⺻DB/data_2013_2015(����).csv", header = T, na.strings = ".")
colnames(data) <- c("ID","year","region","sex","age","age_month","DE1_pr","BP8","BP1","BP6_10","BP6_2","BP6_31","HE_glu","HE_DM")

#���� 20�� �̻� & ���ɴ� ���� ����
data$age <- parse_number(data$age)
data <- data %>% filter(age >= 20) %>% mutate(age_new = (age %/% 10 ) * 10) %>% select(-age_month)

#���� : 9095�� / ����������� : 3189�� / �索�� : 1602��
data %>% group_by(HE_DM) %>% summarise(n = n())

#�索�� ���� ���� ����ġ ���� & ����� ������ ���ش� ����
data <- data %>% filter(HE_DM %in% c(1, 2, 3))

#����ð� 99 ���� & ��Ʈ���� ���� ���� 9 ���� & ����ð� 6, 7, 8 ���� ���ο� ���� ����
data <- data %>% filter(BP8 != 99 & BP1 != 9)
data$BP8 <- parse_number(data$BP8)
data$BP8_new <- ifelse(data[,"BP8"] <= 6, "low", ifelse(data[,"BP8"] == 7, "medium", "high"))



############################################
###########        EDA       ###############
############################################

#�索�� ���� ���� histogram
ggplot(data, aes(HE_DM, fill = as.factor(HE_DM))) + geom_bar() + xlab("�索�� ���� ����") +
  ggtitle("Histogram of �索�� ���� ����") + theme_grey() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  scale_fill_manual(values = c("snow1", "Rosy Brown 1", "red1"), name = c("�索�� ���� ����"), labels = c("����", "�����������", "�索��"))

#���ɴ� histogram
ggplot(data, aes(as.factor(age_new), fill = as.factor(age_new))) + geom_bar() + xlab("���ɴ�") + ggtitle("Histogram of age") +
  theme_grey() + scale_fill_brewer(palette = "Reds", name = c("���ɴ�")) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#�索�� ���� �� ���ɴ�
ggplot(data, aes(age)) +
  geom_density(aes(group = as.factor(HE_DM), colour = as.factor(HE_DM)), size = 1.3) + theme_grey() +
  ggtitle("�索�� ���� �� ���ɴ�") + xlab("���ɴ�") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  scale_colour_brewer(palette = "Reds", name = c("�索�� ����"), labels = c("����", "�����������", "�索��"))

#�索�� ���� �� ����ð�
ggplot(data %>% filter(BP8 != 99), aes(as.factor(BP8))) +
  geom_density(aes(group = as.factor(HE_DM), colour = as.factor(HE_DM)), size = 1.3) + theme_grey() +
  scale_color_manual(values = c("rosy brown 2", "brown1", "red4"), name = c("�索�� ����"), labels = c("����", "�����������", "�索��")) +
  ggtitle("�索�� ���� �� ����ð� ����") + xlab("����ð�") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))



###����ð� Ȯ��
# #����ð�
# ggplot(data %>% filter(BP8 != 99), aes(as.factor(BP8))) +
#   geom_density(aes(group = as.factor(HE_DM), colour = as.factor(HE_DM), fill = as.factor(HE_DM)), alpha = 0.2) + theme_grey() +
#   scale_color_manual(values = c("rosy brown 2", "brown1", "red4"), name = c("�索�� ����"), labels = c("����", "�����������", "�索��")) +
#   scale_fill_manual(values = c("rosy brown 2", "brown1", "red4"), name = c("�索�� ����"), labels = c("����", "�����������", "�索��")) +
#   ggtitle("�索�� ���� �� ����ð� ����") + xlab("����ð�") +
#   theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#���� �� ���������ġ ����
ggplot(data, aes(as.factor(sex), HE_glu, group = as.factor(sex), colour = as.factor(sex))) +
  geom_boxplot() + theme_grey() + scale_color_manual(values = c("darkblue", "red3"), name = c("����"), labels = c("����", "����")) +
  ggtitle("���� �� ���������ġ ����") + xlab("���������ġ") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  ylab("���������ġ") + scale_x_discrete(labels = c("����", "����"))

#���������ġ histogram
ggplot(data, aes(HE_glu)) + geom_histogram(colour = "red1", fill = "white") + xlab("���������ġ") + ggtitle("Histogram of ���������ġ") +
  theme_grey() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#log���������ġ histogram
ggplot(data, aes(log(HE_glu))) + geom_histogram(colour = "red1", fill = "white") + xlab("���������ġ") + ggtitle("Histogram of log(���������ġ)") +
  theme_grey() + theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12))

#���ɴ� �� ���������ġ ����
ggplot(data, aes(as.factor(age_new), HE_glu, group = as.factor(age_new), colour = as.factor(age_new))) +
  geom_boxplot() + theme_grey() + scale_color_brewer(palette = "YlOrRd", name = c("���ɴ�"), labels = c("20��", "30��", "40��", "50��", "60��", "70��", "80��")) +
  ggtitle("���ɴ� �� ���������ġ ����") + xlab("���������ġ") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  ylab("���������ġ") + scale_x_discrete(labels = c("20��", "30��", "40��", "50��", "60��", "70��", "80��"))



# ##maps
# 
# install.packages(c("ggmap","raster","rgeos","maptools"))
# install.packages("rgdal")
# library(ggmap); library(raster); library(rgeos); library(maptools)
# library(rgdal)
# korea <- shapefile("C:/���п�/2018-2/1. ����/�ӻ�����ڷ�м�2/������Ʈ/image/CTPRVN_201804/TL_SCCO_CTPRVN.shp")
# korea <- spTransform(korea,CRS("+proj=longlat"))
# korea_map <- fortify(korea)
# merge_result <- merge(korea_map,data, by="region")

#������ ���������ġ 
data$region_new <- ifelse(data$region %in% c(1, 4, 8), 1,
                          ifelse(data$region %in% c(9, 16), 5,
                                 ifelse(data$region %in% c(2, 3, 14, 15), 2,
                                        ifelse(data$region %in% c(5, 12, 13), 3, 4))))

ggplot(data, aes(as.factor(region_new), log(HE_glu))) + ylab("log���������ġ") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.51, vjust = 2.12)) +
  geom_boxplot(aes(colour = as.factor(region_new))) + ggtitle("���� �� log���������ġ") +
  scale_color_brewer(palette = "YlOrRd", name = c("����"), labels = c("����, ���, ��õ", "�λ�, ���, �뱸, ���", "����, ����", "����, ��û", "����, ����"))


############################################
###########      Modeling    ###############
############################################
#lm
model1 <- lm(HE_glu ~ as.factor(sex) + as.factor(age_new) + BP8_new + as.factor(BP1) + as.factor(region_new), data = data)
summary(model1)
anova(model1)
#��� ���� ����

#binomial glm (��, ���� = 0, ����������� & �索�� = 1)
data$HE_DM_new <- ifelse(data$HE_DM == 1, 0, 1)
model2 <- glm(HE_DM_new ~ as.factor(sex) + as.factor(age_new) + BP8_new + as.factor(BP1) + as.factor(region_new), family = binomial, data = data)
summary(model2)
#���ɴ� ���������� �索 ���� ����.
#����, ���� �索 ���� ����. >> ���� ���İ� ���� ���� ��
#BP1 : ��Ʈ���� ���� ���������� �索 ���� ����.
#����ð� : medium �׷��� �索 ���� ���� ���� low, high�� �索 ���� ����.

#poisson glm
model3 <- glm(HE_glu ~ as.factor(sex) + as.factor(age_new) + BP8_new + as.factor(BP1) + as.factor(region_new), family = poisson, data = data)
summary(model3)
summary(predict(model3, type = "response"))