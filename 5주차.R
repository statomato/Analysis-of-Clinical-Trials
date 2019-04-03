##임삼시험자료분석2##
trial <- read.csv("C:/대학원/2018-2/1. 전공/임상시험자료분석2/과제/trial.csv")
trial$TRT <- factor(trial$TRT); trial$CENTER <- factor(trial$CENTER); trial$SEX <- factor(trial$SEX)
library(ggplot2)
library(lsmeans)

#1번
lm1 <- lm(SCORE ~ AGE, data=trial)
summary(lm1)
anova(lm1)

#2번
lm2 <- lm(SCORE ~ AGE+TRT, data=trial)
summary(lm2)
anova(lm2)
ggplot(data = trial, aes(x = AGE, y = SCORE,colour = TRT)) + geom_point() + geom_smooth(method = lm, se=F)
lsmeans(lm2, "TRT")

#3번
lm3 <- lm(SCORE ~ AGE*TRT, data=trial)
summary(lm3)
anova(lm3)
ggplot(data = trial, aes(x = AGE, y = SCORE,colour = TRT)) + geom_point() + geom_smooth(method = lm, se=F)

#4번
lm4 <- lm(SCORE ~ AGE+TRT+CENTER, data=trial)
summary(lm4)
anova(lm4)
lsmeans(lm4, "TRT")
