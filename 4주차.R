sleeping_dog <- read.csv("C:/대학원/2018-2/1. 전공/임상시험자료분석2/과제/sleeping-dog.csv")
attach(sleeping_dog)
hw4 <- NULL
hw4 <- rbind(hw4, cbind(Dog, 1, TRT1))
hw4 <- rbind(hw4, cbind(Dog, 2, TRT2))
hw4 <- rbind(hw4, cbind(Dog, 3, TRT3))
hw4 <- rbind(hw4, cbind(Dog, 4, TRT4))
detach(sleeping_dog)
hw4 <- data.frame(hw4)
colnames(hw4) <- c("Dog", "TRT", "RES")
hw4$TRT <- as.factor(hw4$TRT)

table(hw4$TRT)
tapply(hw4$RES, hw4$TRT, function(x) {mean(x, na.rm=TRUE)})
tapply(hw4$RES, hw4$TRT, function(x) {sd(x, na.rm=TRUE)})

library(ggplot2)
ggplot(data=hw4, aes(x=TRT, y=RES, group=TRT))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)

summary(lm(RES ~ TRT, data=hw4))
anova(lm(RES ~ TRT, data=hw4))

library(multcomp)
amod <- aov(RES ~ TRT, data=hw4)
glht(amod, data=hw4, linfct = mcp(TRT="Dunnet"), conf.level=0.95)
TukeyHSD(amod)
