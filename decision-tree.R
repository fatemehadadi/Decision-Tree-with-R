dat=read.csv(file.choose(),header=TRUE,stringsAsFactors=TRUE)
install.packages(c("rpart","rpart.plot","C50"))
library("rpart")
library("rpart.plot")
library("C50")
str(dat)
levels(dat$marital.status)
levels(dat$workclass)
levels(dat$marital.status)[2:4]="Married"
levels(dat$workclass)[c(2,3,8)]="Gov"
levels(dat$workclass)[c(5,6)]="self"
levels(dat$marital.status)
levels(dat$workclass)

dat$age.z=(dat$age - mean(dat$age))/sd(dat$age)
dat$education.num.z=(dat$education.num - mean(dat$education.num))/sd(dat$education.num)
dat$capital.gain.z=(dat$capital.gain - mean(dat$capital.gain))/sd(dat$capital.gain)
dat$capital.loss.z=(dat$capital.loss - mean(dat$capital.loss))/sd(dat$capital.loss)
dat$hours.per.week.z=(dat$hours.per.week - mean(dat$hours.per.week))/sd(dat$hours.per.week)
cartfit=rpart(income~age.z+education.num.z+capital.gain.z+capital.loss.z+hours.per.week.z+race+sex+workclass+marital.status,data=dat,method="class")
print(cartfit)
rpart.plot(cartfit)

#C4.5
x=dat[,c(2,6,9,10,16,17,18,19,20)]

y=dat$income
c50fit=C5.0(x,y)
summary(c50fit)