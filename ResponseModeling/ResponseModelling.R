library(dplyr)
library(Hmisc)
#reading the file
orders<-read.csv("C:/Users/Kanchanwali/Documents/ResponseModelling/RFMData.csv",sep=',',header=T,stringsAsFactors = FALSE)
####Conversion of date format
orders$OrderDate <- as.Date(as.character(orders$OrderDate), format = "%d/%m/%Y")

#Generating First purchase date and Last purchase date for the Customers
RFM <- orders %>%
  arrange(CustID, OrderDate) %>%
  group_by(CustID) %>%
  mutate(FPD = min(OrderDate))%>%
  mutate(LPD = max(OrderDate)) %>%
  ungroup()

#Creating a subset for last 6 months and confirming the response in last 7 month
RFM_LastSixMonth = subset(RFM,OrderDate >"2007-12-01" & OrderDate < "2008-05-31")
RFM_June = subset(RFM,OrderDate > "2008-06-01" & OrderDate < "2008-07-01")

#Calculating Recency(weeks),Frequency,Monetary for subset
Frequency_6<-as.numeric(by(RFM_LastSixMonth$OrderID, RFM_LastSixMonth$CustID, length))
Monetary_6<-as.numeric(by(RFM_LastSixMonth$Amount, RFM_LastSixMonth$CustID, sum))
RFM_LastSixMonth<-RFM_LastSixMonth[!duplicated(RFM_LastSixMonth[,"CustID"]),]
lastDate1<-max(RFM_LastSixMonth$LPD)
Recency_6<-as.numeric(lastDate1 - RFM_LastSixMonth$LPD) %/% 7
RFM_response = data.frame(RFM_LastSixMonth$CustID,Recency_6,Monetary_6,Frequency_6)

#Calculating Recency(weeks),Frequency,Monetary for subset
Frequency_7<-as.numeric(by(RFM_June$OrderID, RFM_June$CustID, length))
Monetary_7<-as.numeric(by(RFM_June$Amount, RFM_June$CustID, sum))
RFM_June<-RFM_June[!duplicated(RFM_June[,"CustID"]),]
lastDate2<-max(RFM_June$LPD)
Recency_7<-as.numeric(lastDate2 - RFM_June$LPD) %/% 7
RFM_response_June = data.frame(RFM_June$CustID,Recency_7,Monetary_7,Frequency_7)
dim(RFM_response_June)
RFM_response$Visit= ifelse(  RFM_response$RFM_LastSixMonth.CustID  %in% RFM_response_June$RFM_June.CustID,"1","0")
RFM_response$Visit=as.factor(RFM_response$Visit)
table(RFM_response$Visit)

str(RFM_response_June)
#write.csv(RFM_response$Summary,"RFM_response_summary.csv",row.names=F)
#Sampling
library(caret)
data <- createDataPartition(RFM_response$Visit, p=0.6, list=FALSE)
Train <- RFM_response[ data, ]
Test <- RFM_response[ -data, ]

#Naive bayes
library(e1071)
model.naiveBayes <- naiveBayes(Visit ~ ., data = Train)
probs <- predict(model.naiveBayes, Test[-5],type='raw') 
pred.naiveBayes <- predict(model.naiveBayes, Test[-5],probability=TRUE)
confusionMatrix(pred.naiveBayes,Test$Visit)
tab <- table(Predicted=pred.naiveBayes,actual=Test$Visit)
tab
1-sum(diag(tab))/sum(tab)
plot(pr.curve(Test$Visit,pred.naiveBayes,curve=TRUE))

# build the models with two different kernel functions respectively
#model.svm.linear <- svm(Visit ~ ., data = Train, kernel="linear",probability = TRUE)
#summary(model.svm.linear)
# prediction with the two models respectively
#pred.svm.linear <- predict(model.svm.linear,Test[,-5],probability=TRUE)
#confusionMatrix(pred.svm.linear,Test$Visit)
#attr(pred.svm.linear, "probabilities")[1:6,]
#plot(pr.curve(Test$Visit,pred.svm.linear,curve=TRUE))

#####
model.svm.radial <- svm(Visit ~ ., data = Train, kernel="radial",probability = TRUE)
pred.svm.radial <- predict(model.svm.radial, Test[,-5],probability=TRUE)
confusionMatrix(pred.svm.radial,Test$Visit)
tab <- table(Predicted=pred.svm.radial,actual=Test$Visit)
tab
1-sum(diag(tab))/sum(tab)
plot(pr.curve(Test$Visit,pred.svm.radial,curve=TRUE))
