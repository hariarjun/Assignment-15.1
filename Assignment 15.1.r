#Question:
#a. Predict the no of comments in next H hrs
#b. Use regression technique
#Answer:
slr <- read.csv("D:/slr.csv")
slr1<- slr
View(slr1)
dim(slr1)
str(slr1)

library(psych)
describe(slr1)
summary(slr1)

#Visualization :

hist(slr1$Advt ,xlab = "advt", ylab = "Frequency",main="Histogram of advt",col="Blue")
hist(slr1$Sales ,xlab = "sales", ylab = "Frequency",main="Histogram of sales",col="Red")

plot(slr1$Advt,slr1$Sales)


#Performing linear regression

model<- lm(slr1$Advt~slr1$Sales)
model


#Predicting

Pr <- predict(lm(slr1$Sales~slr1$Advt))
Pr

pr <- predict(model,newdata= slr1Test,type = "response")
table(slr1$Advt,pred>= 0.5)

conf<- table(slr1$Advt,pr)
conf

predict(model)
Pr=predict(model)
slr1$predicted =NA
slr1$predicted =Pr

slr1$error =model$residuals


error<- residuals(lm(slr1$Sales~slr1$Advt))
error

summary(error)

#interpreting the summary
summary(model)

#result of all of our models 
summary(model)
summary(model1)
summary(model2)

#model coefficients
model
model1
model2

slr1$coefficients<- NA
slr1$coefficients<- model$coefficients
slr1$coefficients


#c. Report the training accuracy and test accuracy

set.seed(1)
split<- sample.split(slr1$Advt,SplitRatio = 0.70)
slr1Train <- subset(slr1,split == TRUE)
slr1Test<- subset(slr1, split == FALSE)

#training
model1<- lm(slr1Train$Advt~slr1Train$Sales)
model1

summary(model1)
# The accuracy is 0.926

#testing 

model2<- lm(slr1Test$Advt~slr1Test$Sales)
model2

summary(model2)
