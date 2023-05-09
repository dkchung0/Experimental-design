r1 = rep(60,6)
r2 = c(60,4.85,60,4.57,2.49,3.22)
r3 = c(60,60,4.39,60,7.93,9.83)
r4 = c(7.16,4.40,3.8,3.34,2.28,2.41)
r5 = c(60,2.82,2.50,2.81,1.58,2.17)
r6 = c(3.41,1.83,3.61,2.23,2.10,2.42)
r = c(r1,r2,r3,r4,r5,r6)

boxplot(r)
median(r)
plot(sort(r))

x1 = rep("2.5(B)+2.5(C)",3)
x2 = rep("5(B)+5(C)",3)
x = rep(c(x1,x2),6)

y1 = rep("2.5ml",12)
y2 = rep("5ml",12)
y3 = rep("7.5ml",12)
y = c(y1,y2,y3)

z1 = rep("No",6)
z2 = rep("Yes",6)
z = rep(c(z1,z2),3)

data = cbind(x,y,z,r)

library(data.table)
data = data.table(data)
colnames(data) = c("Baking_soda_+_Citric_acid","Water","Shake","Time")
data$Time = as.numeric(data$Time)
data$`Baking_soda_+_Citric_acid` = as.factor(data$`Baking_soda_+_Citric_acid`)
data$Water = as.factor(data$Water)
data$Shake = as.factor(data$Shake)
str(data)



# 以小蘇打和檸檬酸公克數差別分兩組 算處組平均
group2.5 = mean(data[`Baking_soda_+_Citric_acid`=="2.5(B)+2.5(C)",Time])
group5 = mean(data[`Baking_soda_+_Citric_acid`=="5(B)+5(C)",Time])
group2.5
group5

# 以水量差別分三組 算處組平均
w2.5 = mean(data[Water=="2.5ml",Time])
w5 = mean(data[Water=="5ml",Time])
w7.5 = mean(data[Water=="7.5ml",Time])
w2.5
w5
w7.5

# 以有沒有搖分兩組 算處組平均
N = mean(data[Shake=="No",Time])
Y = mean(data[Shake=="Yes",Time])
N
Y



attach(data)

library(ggplot2)

# 看資料分佈情況
ggplot(data,aes(x=`Baking_soda_+_Citric_acid`,y=Time,color=`Baking_soda_+_Citric_acid`))+
  geom_boxplot() 

ggplot(data,aes(x=Water,y=Time,color=Water))+
  geom_boxplot() 

ggplot(data,aes(x=Shake,y=Time,color=Shake))+
  geom_boxplot() 


ggplot(data,aes(x=Water,y=Time,color=Shake))+
  geom_boxplot() 

ggplot(data,aes(x=Water,y=Time,color=Shake))+
  geom_boxplot() + 
  facet_grid(.~`Baking_soda_+_Citric_acid`)



plot.design(data$Time~.,data=data)



# 看因子間交互作用
interaction.plot(`Baking_soda_+_Citric_acid`,Water,Time,lwd=2,col=2)
interaction.plot(`Baking_soda_+_Citric_acid`,Shake,Time,lwd=2,col=3)
interaction.plot(Water,Shake,Time,lwd=2,col=4)


anova = aov(Time~`Baking_soda_+_Citric_acid`*Water*Shake)
summary(anova)


anova1 = aov(Time~`Baking_soda_+_Citric_acid`+Water+Shake)
summary(anova1)
predict(anova1,data)


hi = lm(Time~`Baking_soda_+_Citric_acid`+Water+Shake,data)
summary(hi)


# Residual vs fitted_values
plot(anova1$fitted.values,anova1$residuals,cex=3,pch=16,col=5)
text(anova1$fitted.values,anova1$residuals,cex=2,pch=16,col=1,
     names(anova1$residuals))
abline(h=0,lwd=3,col=2)

hist(anova1$residuals)


# QQ plot
a=anova1$residuals
b=order(anova1$residuals)
plot(qnorm(1:36/37) , a[b] , pch=16,
     xlab="normal_quantiles",ylab="residuals",cex=3,col=6)
text(qnorm(1:36/37) , a[b] ,cex=2,pch=16,col=1,
     names(a)[b[1:36]])
qqline(anova1$residuals,lwd=3,col=4)



# 檢查變異數假設
install.packages("car")
library(car)
itsme = lm(Time~`Baking_soda_+_Citric_acid`+Water+Shake)
anova(itsme)
ncvTest(anova(itsme))

# 檢查常態假設
shapiro.test(anova1$residuals)

# 檢查獨立性
durbinWatsonTest(anova1) 





