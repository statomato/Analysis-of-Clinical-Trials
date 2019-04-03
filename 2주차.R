library(nlme); library(ggplot2)
str(BodyWeight)
head(BodyWeight)
summary(BodyWeight)

ggplot(BodyWeight, aes(x=1, y=weight, fill=Diet)) + geom_boxplot()
ggplot(BodyWeight, aes(x=Rat, y=weight, fill=Diet)) + geom_boxplot()

#ggplot(BodyWeight, aes(x=Time,y=weight, col=Diet)) + geom_point()
#ggplot(BodyWeight, aes(x=Time,y=weight)) + geom_point(col=BodyWeight$Diet) + geom_path(col=BodyWeight$Diet)

ggplot(BodyWeight, aes(x=Time, y=weight, col=Rat))+geom_line()+facet_wrap(~ factor(BodyWeight$Diet))

ex1.1 <- lm(weight~factor(Time)+factor(Diet), data=BodyWeight)
ex1.2 <-lm(weight~factor(Time)+factor(Diet)+factor(Rat)+factor(Diet)*factor(Time), data=BodyWeight)
anova(ex1.2)
summary(ex1.2)
ex1.3 <-lme(weight~factor(Time)+factor(Diet)+factor(Diet)*factor(Time),random=~1|factor(Rat),data=BodyWeight)
anova(ex1.3)
summary(ex1.3)


library(faraway)
str(vision)
head(vision)
summary(vision)
vision$npower <- rep(1:4,14)

ggplot(vision, aes(x=power, y=acuity, fill=eye)) + geom_boxplot()
ggplot(vision, aes(x=npower, y=acuity, linetype=eye))+geom_line()+facet_wrap(~ factor(vision$subject))

#ggplot(vision, aes(x=power,y=acuity, col=subject)) + geom_point()

#summary(lm(acuity~power+eye, data=vision))
ex2.1 <-lm(acuity~power+eye+power*eye, data=vision)
summary(ex2.1)
anova(ex2.1)

ex2.2 <-lme(acuity~power+eye,random=~1|factor(subject),data=vision)
summary(ex2.2)
anova(ex2.2)


install.packages("writexl")
library(writexl)

writexl::write_xlsx(BodyWeight, path = "C:/대학원/2018-2/1. 전공/임상시험자료분석2/과제/BodyWeight.csv")
writexl::write_xlsx(vision, path = "C:/대학원/2018-2/1. 전공/임상시험자료분석2/과제/vision.csv")
