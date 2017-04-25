library(date)
library(sqldf)
library(ggplot2)
library(dplyr)
library(Hmisc)
#The Original RFMData 
orders<-read.csv("/Users/mutturaj/Desktop/Kanchan Reasearch/module1work/Datafiles/RFMData.csv",sep=',',header=T,stringsAsFactors = FALSE)

orders$OrderDate <- as.Date(as.character(orders$OrderDate), format = "%d/%m/%Y")

##### RFM MODEL ####
RFM <- orders %>%
  arrange(CustID, OrderDate) %>%
  group_by(CustID) %>%
  mutate(FPD = min(OrderDate))%>%
  mutate(LPD = max(OrderDate)) %>%
  ungroup()
# Getting the Minimum and Maximum Dates 
lastDate<-max(RFM$LPD)
lastDate
FirstDate<-min(RFM$FPD)
str(RFM)


#Calculating Recency(weeks),Frequency,Monetary,Tenure(weeks) 

Frequency<-as.numeric(by(RFM$OrderID, RFM$CustID, length))
Monetary<-as.numeric(by(RFM$Amount, RFM$CustID, sum))
RFM<-RFM[!duplicated(RFM[,"CustID"]),]
Tenure<-as.numeric(lastDate - RFM$FPD) %/% 7
Recency<-as.numeric(lastDate - RFM$LPD) %/% 7

RFM_table <- data.frame(RFM$CustID,RFM$FPD,RFM$LPD,Tenure,Recency,Frequency,Monetary)


#RFM_table<-order(Recency,Frequency,Monetary,decreasing=F)

##Creating R,F,M ranks

RFM_table$F <- cut(RFM_table$Frequency,4,labels=F)
RFM_table$M <- cut(RFM_table$Monetary,4,labels=F)
RFM_table$R <- cut(RFM_table$Recency,4,labels=F)

#rankR 1 is very recent while rankR 5 is least recent
table(RFM_table$R)
table(RFM_table$F)
table(RFM_table$M)

table(RFM_table[,8:10])

#groupRFM 
groupRFM <- RFM_table$R*100 + RFM_table$F*10 + RFM_table$M
RFM_table <- cbind(RFM_table,groupRFM)
table(groupRFM)
#Plot
pairs(RFM_table[,8:10])

qplot(x=groupRFM,data=RFM_table)+
  geom_histogram(binwidth = 30)+
  scale_x_continuous(limits=c(100,500))


################

##################################################
install.packages("CHAID", repos="http://R-Forge.R-project.org")
#library(CHAID)
#library(help=CHAID)

#names(RFM_table)

#table(RFM_table$Repurchase)

#dt.chaid  <- chaid(RFM_table$Repurchase~ Recency+Frequency , 
                   control = chaid_control(minprob = 0.001,
                                           minsplit = 500,minbucket = 200),
                   data=termCRFM_table)

#plot(dt.chaid, 
#     uniform = T, 
#     compress = T, 
#     margin = 0.2, 
#     branch = 0.3)
# Label on Decision Tree
#text(dt.chaid, 
#     use.n = T, 
#     digits = 3, 
#     cex = 0.6)
#summary(dt.chaid)





#########################################
# Use the logarithm for the variables:
Rerfm <- data.frame(RFM[1],RFM[10],RFM[11],RFM[12])
Rerfm$Recency <- log(RFM$Recency)
Rerfm$Frequency <- log(RFM$Frequency)
Rerfm$Monetary <- log(RFM$Monetary)

head(Rerfm)
library(plotly)
# Run K-means (nstart = 20) and 5 different groups
Rerfm <- Rerfm %>% filter(Rerfm$Frequency>0)
Rerfm_ <- Rerfm %>% select(Rerfm$Recency:Rerfm$Monetary)
Rerfm_km <- kmeans(Rerfm_ , centers = 5, nstart = 20)

# Plot  
library(plotly)
p <- plot_ly(Rerfm, x = Rerfm$Recency, y = Rerfm$Monetary, z = Rerfm$Frequency, 
             type = "scatter3d", mode = "markers", 
             color=Rerfm_km$cluster) %>% layout(showlegend = FALSE) 
p %>% layout(showlegend = FALSE)

#Segmenting them as active(last 12 months= 52 weeks),warm(13 to 24 months= 104 weeks),
#cold(25 months and above=156 weeks)
RFM$segment[which(RFM$Recency <= 52)] = "active"
RFM$segment[which(RFM$Recency <= 104 & RFM$Recency > 52 )] = "warm"
RFM$segment[which(RFM$Recency <= 156 & RFM$Recency > 104)] = "cold"
RFM$segment[which(RFM$Recency > 156)] = "inactive"
table(RFM$segment)

#Pie Chart
#pie(table(RFM$segment), col = topo.colors(24))

#Plots of Recency,Frequency,Monetary,Tenure
library(gridExtra)
p1=qplot(x=Recency ,data=RFM_table)+
  geom_histogram(color=I('black'),fill=I('#F79420'))
p2=qplot(x=Tenure,data=RFM_table,binwidth=12,color=I('black'),fill=I('#F79420'))+
  scale_x_continuous()+
  scale_y_continuous(limits=c(0,30000))
p3=qplot(x=Frequency ,data=RFM_table)+
  geom_histogram(color=I('black'),fill=I('#F79420'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_continuous(limits=c(25,100))
p4=qplot(x=Monetary ,data=RFM_table)+
  geom_histogram(color=I('black'),fill=I('#F79420'))+
  scale_x_continuous(limits=c(0,100))
grid.arrange(p1,p2,p3,p4,ncol=2)

p11=qplot(x=RFM$rankR ,data=RFM)+
  geom_histogram(color=I('black'),fill=I('#F79420'))
p12=qplot(x=RFM$rankF ,data=RFM)+
  geom_histogram(color=I('black'),fill=I('#F79420'))
p13=qplot(x=RFM$rankM ,data=RFM)+
  geom_histogram(color=I('black'),fill=I('#F79420'))
grid.arrange(p11,p12,p13,ncol=2)

cohort <- orders %>%
  arrange(CustID, OrderDate) %>%
  group_by(CustID) %>%
  mutate(FPD = min(OrderDate), 
         CohortMonthly = paste0('Cohort-', format(FPD, format='%Y-%m')),           
         LifetimeMonth = ifelse(OrderDate==FPD, 1, ceiling(as.numeric(OrderDate-FPD)/30))) %>% 
  ungroup()

cohort$LifetimeMonth <- formatC(cohort$LifetimeMonth, width=2, format='d', flag='0')
cohort$LifetimeMonth <- paste('M', cohort$LifetimeMonth, sep='')

comb.cohorts <- cohort %>%
  group_by(CohortMonthly, LifetimeMonth) %>%
  summarise(Revenue = sum(Amount),NumberOfCustomers = n_distinct(CustID))

table(cohort$LifetimeMonth)
ggplot(comb.cohorts, aes(x=LifetimeMonth, y=NumberOfCustomers, group=CohortMonthly, color=CohortMonthly)) + geom_line()

unique(unlist(orders$Category, use.names = FALSE))
