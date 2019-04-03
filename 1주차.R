library(tidyverse)
trial <- read_csv("C:/대학원/2018-2/1. 전공/임상시험자료분석2/L2-자료분석 I data/EX3-1.csv")

#1
#2)
plot(seq(-4,4,0.1),dt(seq(-4,4,0.1),df=99),type="l")
abline(v=qt(0.975,df=99),col="red")

#4)
t.test(trial$SCORE, mu=20)

##2
#1)
tapply(trial$SCORE, trial$TRT, function(x) {mean(x,na.rm=T)})
tapply(trial$SCORE, trial$TRT, function(x) {sd(x,na.rm=T)})
summary(lm(SCORE ~ TRT, data=trial))
anova(lm(SCORE ~ TRT, data=trial))

library(multcomp)
trial2 <- read.csv("C:/대학원/2018-2/1. 전공/임상시험자료분석2/L2-자료분석 I data/EX3-1.csv")
amod <- aov(SCORE ~ TRT, data=trial2)
glht(amod, data=trial2, linfct = mcp(TRT = "Dunnet"), cof.level=0.95)

#2)
tapply(trial$SCORE, paste(trial$TRT, trial$CENTER, sep=":"),mean)
tapply(trial$SCORE, paste(trial$TRT, trial$CENTER, sep=":"),sd)

summary(lm(SCORE ~ TRT + CENTER + TRT*CENTER, data=trial2))
anova(lm(SCORE ~ TRT + CENTER + TRT*CENTER, data=trial2))

#3)
summary(lm(SCORE ~ TRT + CENTER, data=trial2))
anova(lm(SCORE ~ TRT + CENTER, data=trial2))
