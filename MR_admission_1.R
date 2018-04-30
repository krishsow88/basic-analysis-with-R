
View(timesData_popu_original)
attach(timesData_popu_original)
cols<-c("world_rank","university_name","country","teaching","international","research","citations","income","total_score","num_students","students_staff_ratio","international_students","year","num_international_students")
for(i in cols){
  timesData_popu_original[,i]=as.numeric(timesData_popu_original[,i])
}
View(timesData_popu_original)
head(timesData_popu_original)
tail(timesData_popu_original)
na.omit(timesData_popu_original)
testna<- timesData_popu_original$income
is.na(testna)
which(is.na(testna))
test[which(is.na(testna))]<-1

#model11<-lm(total_score~world_rank+teaching+international+research+citations+income+num_students+international_students+year,data=timesData_popu_original)
model11<-lm(teaching+international+research+citations+income,data=timesData_popu_original)
summary(model11)
wspredict(model12,data.frame("world_rank"=660,"teaching"=390.8,"international"=760.1,"research"=44.8,"citations"=841.9,"income"=8800,"num_students"=6600,"international_students"=798,"year"=2017))
back<-step(lm(total_score~world_rank+teaching+international+research+citations+income+num_students+international_students+year,data=timesData_popu_original),direction = "backward")
forward<-step(lm(total_score~world_rank+teaching+international+research+citations+income+num_students+international_students+year,data=timesData_popu_original),direction = "forward")
both<-step(lm(total_score~world_rank+teaching+international+research+citations+income+num_students+international_students+year,data=timesData_popu_original),direction = "both")
summary(back)
anova(model11)


#considering only 4 variables
view(popu11_15)
summary(popu11_15)
boxplot(popu11_15)
auto.sel<- subset(popu11_15, select = c(teaching,international,research,citations,income,total_score))
pairs.default(auto.sel, col="red")
str(popu11_15)
index<- sample(1:nrow(popu11_15),size = 0.55*nrow(popu11_15))
train.popu = popu11_15[index,]
test.popu = popu11_15[index,]
head(train.popu)
head(test.popu)

class(train.popu$teaching)
class(train.popu$international)
class(train.popu$research)
class(train.popu$citations)
fit4<- lm(total_score~teaching+international+research+citations,data = train.popu)
summary(fit4) #99.95
fit3<- lm(total_score~citations+teaching+research+international,data = train.popu)
summary(fit3) #98.45
fit2<- lm(total_score~research+teaching,data = train.popu)
summary(fit2) #84.97
fit_IC<- lm(total_score~international+citations,data = train.popu)
summary(fit_IC) #30.46
fit_ICI<- lm(total_score~international+citations+income,data = train.popu)
summary(fit_ICI) #43.37

#prediction bw train and test
pred<- predict(fit4,test.popu)
head(pred)
head(test.popu)

 Back_Step<- step(lm(total_score~teaching+international+research+citations+income,data=popu11_15),direction = "backward")
forward_step<- step(lm(total_score~teaching+international+research+citations+income,data=popu11_15),direction = "forward")
both_step<- step(lm(total_score~teaching+international+research+citations+income,data=popu11_15),direction = "both")

plot(fit3)
plot(fit4)
predict_train<- predict(fit4,
                        newdata = subset(train.popu, select=c(teaching,international,citations,research,income), se.fit= TRUE, interval= "prediction", level=0.95))
predict_test<- predict(fit4,
                                 newdata = subset(test.popu, select= c(teaching,international,citations,research,income)))
head(predict_test)
head(predict_train)
summary(predict_test)
summary(predict_train)
summary(fit4)

train.popu_corr<- round(cor(predict_train,train.popu),2)
train.popu_RAME<- round(sqrt(mean(predict_train,train.popu)^2))
train.popu_MAE<- round(mean(abs(predict_train,train.popu)))
x<-c(train.popu_corr^2,train.popu_RAME,train.popu_MAE)

test.popu_corr<- round(cor(predict_test,test.popu),2)
test.popu_RAME<- round(sqrt(mean(predict_test,test.popu)^2))
test.popu_MAE<- round(mean(abs(predict_test,test.popu)))
x<-c(test.popu_corr^2,test.popu_RAME,test.popu_MAE)

predict(model12,data.frame("teaching"=39.8,"international"=76.1,"research"=44.8,"citations"=84.9,"income"=88))


