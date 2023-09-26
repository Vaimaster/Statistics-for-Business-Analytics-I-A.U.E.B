setwd(dir="C:\\Users\\User\\Desktop\\Μεταπτυχιακό\\1) Statistics for Business Analytics I\\R Labs\\Graded Assignments")
library(foreign)
library(psych)
library(Hmisc)

# Question 1

salary <- read.spss("salary.sav",to.data.frame=TRUE)
str(salary)

# Question 2

numeric.only <- sapply(salary, class) == "numeric"
numeric.only
salary_num <- salary[numeric.only]
salary_num <- salary_num[,-1]
head(salary_num)
summary(salary_num)

p <- ncol(salary_num)
par(mfrow=c(2,3))
for (i in 1:p){
  hist(salary_num[,i], main=names(salary_num)[i], probability=TRUE)
  lines(density(salary_num[,i]), col=2)
  index <- seq( min(salary_num[,i]), max(salary_num[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(salary_num[,i]),
                  sd(salary_num[,i]) )
  lines( index, ynorm, col=3, lty=3, lwd=3 )
}
dev.off()

# Question 3

library('nortest')
shapiro.test(salary_num$salbeg)
lillie.test(salary_num$salbeg)
ks.test(salary_num$salbeg, 'pnorm')

qqnorm(salary_num$salbeg)
qqline(salary_num$salbeg,col=3, lty=3, lwd=3 )

library(lawstat)
mean(salary_num$salbeg)
median(salary_num$salbeg)
symmetry.test(salary_num$salbeg)

wilcox.test(salary_num$salbeg, mu=1000)
# so we reject the H0 that the avg of beginning salary of a typical 
# employee is equal to 1000$

# Question 4

salary_num$saldiff <- salary_num$salnow - salary_num$salbeg

shapiro.test(salary_num$saldiff)
lillie.test(salary_num$saldiff)
ks.test(salary_num$saldiff,'pnorm')

qqnorm(salary_num$saldiff)
qqline(salary_num$saldiff,col=3, lty=3, lwd=3 )

mean(salary_num$saldiff)
median(salary_num$saldiff)
symmetry.test(salary_num$saldiff)

wilcox.test(salary_num$saldiff, mu=0)
# so we reject the H0 that there isn't any significant difference
# between beggining salary and current salary

# Question 5

salary_gen <- salary[,c("salbeg","sex")]

by( salary_gen$salbeg, salary_gen$sex, lillie.test)
by( salary_gen$salbeg, salary_gen$sex, shapiro.test)

par(mfrow=c(1,2))
qqnorm(salary_gen$salbeg[which(salary_gen$sex=='MALES')], main = 'Normanl Q-Q Plot for Males')
qqline(salary_gen$salbeg[which(salary_gen$sex=='MALES')],col=3, lty=3, lwd=3 )
qqnorm(salary_gen$salbeg[which(salary_gen$sex=='FEMALES')], main = 'Normanl Q-Q Plot for Females')
qqline(salary_gen$salbeg[which(salary_gen$sex=='FEMALES')],col=3, lty=3, lwd=3 )
dev.off()

mean(salary_gen$salbeg[which(salary_gen$sex=='MALES')])
median(salary_gen$salbeg[which(salary_gen$sex=='MALES')])
symmetry.test(salary_gen$salbeg[which(salary_gen$sex=='MALES')])
mean(salary_gen$salbeg[which(salary_gen$sex=='FEMALES')])
median(salary_gen$salbeg[which(salary_gen$sex=='FEMALES')])
symmetry.test(salary_gen$salbeg[which(salary_gen$sex=='FEMALES')])

wilcox.test(salary_gen$salbeg[which(salary_gen$sex=='MALES')],salary_gen$salbeg[which(salary_gen$sex=='FEMALES')])
# so we reject the H0 that the median in beginning salary of males
# is equal to the median in the beginning salary of females

boxplot(salbeg~sex, data = salary_gen)
boxres<-boxplot(salbeg~sex, data = salary_gen, plot=F)
out.index <- which(salary_gen$salbeg %in% boxres$out)

# Question 6

age_cut <- cut2(salary_num$age,g=3)
head(age_cut)
salary_num <- data.frame(salary_num,age_cut)

anova1 <- aov(salbeg~age_cut, data = salary_num)
anova1
summary(anova1)

lillie.test(anova1$residuals)
shapiro.test(anova1$residuals)

bartlett.test(salbeg~age_cut, data = salary_num)
fligner.test(salbeg~age_cut, data = salary_num)
library(car)
leveneTest(salbeg~age_cut, data = salary_num)

qqnorm(anova1$residuals)
qqline(anova1$residuals,col=3, lty=3, lwd=3 )

mean(anova1$residuals)
median(anova1$residuals)
symmetry.test(anova1$residuals)

mean(salary_num$salbeg[which(salary_num$age_cut=='[23.0,29.7)')])
median(salary_num$salbeg[which(salary_num$age_cut=='[23.0,29.7)')])
symmetry.test(salary_num$salbeg[which(salary_num$age_cut=='[23.0,29.7)')])

mean(salary_num$salbeg[which(salary_num$age_cut=='[29.7,39.8)')])
median(salary_num$salbeg[which(salary_num$age_cut=='[29.7,39.8)')])
symmetry.test(salary_num$salbeg[which(salary_num$age_cut=='[29.7,39.8)')])

mean(salary_num$salbeg[which(salary_num$age_cut=='[39.8,64.5]')])
median(salary_num$salbeg[which(salary_num$age_cut=='[39.8,64.5]')])
symmetry.test(salary_num$salbeg[which(salary_num$age_cut=='[39.8,64.5]')])

median(anova1$residuals)
median(anova1$residuals)
symmetry.test(anova1$residuals)

kruskal.test(salbeg~age_cut, data = salary_num)

pairwise.wilcox.test(salary_num$salbeg,salary_num$age_cut)
# so we reject the H0 that the median in beginning salary
# is equal to all three groups

boxplot(salbeg~age_cut, data = salary_num)
boxres<-boxplot(salbeg~age_cut, data = salary_num, plot=F)
out.index <- which(salary_num$salbeg %in% boxres$out)

# Question 7

tab1 <- table(salary$sex, salary$minority)
tab1

prop.table(tab1)
prop.table(tab1, 1)
prop.table(tab1, 2)

prop.test(tab1)
chisq.test(tab1)
# We don't reject the H0 so these are independent groups

#Second way of showing results
require(gmodels)
CrossTable(salary$sex, salary$minority)
CrossTable(salary$sex, salary$minority, digits = 1, format = "SPSS")
CrossTable(salary$sex, salary$minority, digits = 1, format = "SPSS", prop.r = T,
           prop.c = F, prop.t = F, prop.chisq = F, chisq = T, fisher = T, mcnemar = F)

#Third way of showing results
require(sjPlot)
require(sjstats)
sjt.xtab(salary$sex, salary$minority, show.cell.prc = F, show.col.prc = F, show.exp = F)
sjp.xtab(salary$sex, salary$minority)