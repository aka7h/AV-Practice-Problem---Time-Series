
library(data.table)


setDT(ntrain)
setDT(ntest)

range_data <- function (x) {(x-min(x))/( max(x)-min(x))}

x<- data.matrix(ntrain$Count)
min_data <-min(x)
max_data <-max(x)
ntrain$Count_N <- range_data (x)
ntest$Count_N <- NA

ntrain$Decompose <- as.numeric(decomp$random)
ntest$Decompose <- NA

unscale_data<-function(x ,max_x,min_x)
{x *(max_x-min_x)+min_x}

full <- rbind(ntrain,ntest)
setDT(full)

summary(full)

full$Day <- as.factor(lubridate::day(full$Datetime))
full$Year <- as.factor(lubridate::year(full$Datetime))
full$Month <- as.factor(lubridate::month(full$Datetime))
full$Hrs <- as.factor(lubridate::hour(full$Datetime))
full$WDays <- as.factor(lubridate::wday(full$Datetime))
full$Holidays <- as.factor(ifelse(chron::is.weekend(full$Datetime),1,0))
full$Week <- as.factor(lubridate::week(full$Datetime))
full$ndays <- days_in_month(full$Datetime)
full$Dates <- date(full$Datetime)

ggplot(full[istrain==1,])+geom_line(aes(Dates,Count_N))

mean.per.month <- full[istrain==1,list(avg=Count,std=sd(Count),Dates=Dates),by=c("Month","Year")]
ggplot(mean.per.month, aes(x=Dates,y=avg,color=as.factor(Month)))+geom_line()

mean.per.week <- full[istrain==1,list(avg=Count,std=sd(Count)),by=c("Dates","Year")]
ggplot(mean.per.week, aes(x=Dates,y=avg,color=as.factor(Year)))+geom_line()+geom_smooth(method = "loess",color="black",span=1/5)

mean.per.weekend <- full[istrain==1&Holidays==1,list(avg=Count),by=c("Dates","Year")]
mean.per.weekend
ggplot(mean.per.weekend, aes(x=Dates,y=avg,color=as.factor(Year)))+geom_line()+geom_smooth(method = "gam",color="black",span=1/5)

#Hourly
ggplot(full[istrain==1,], aes(x=as.factor(Hrs),y=Count))+geom_boxplot()+geom_jitter(col="red", width = 0.2,size=0.4)
ggplot(full[istrain==1,], aes(x=as.factor(Hrs),y=Count,fill=as.factor(Year)))+geom_boxplot()+scale_y_log10()+geom_jitter(col="red", width = 0.2,size=0.4)

#Day
ggplot(full[istrain==1,], aes(x=as.factor(Day),y=Count_N))+geom_boxplot()+geom_jitter(col="red", width = 0.2,size=0.4)
ggplot(full[istrain==1,], aes(x=as.factor(Day),y=Count_N,fill=as.factor(Year)))+geom_boxplot()+scale_y_log10()+geom_jitter(col="red", width = 0.2,size=0.4)


ggplot(full[istrain==1&Year==2012,], aes(x=as.factor(Hrs),y=Count))+geom_line()
ggplot(full[istrain==1&Year==2013,], aes(x=as.factor(Hrs),y=Count))+geom_line()
ggplot(full[istrain==1&Year==2014,], aes(x=as.factor(Hrs),y=Count))+geom_line()


#### define the peek hours for each year, month, weekday and weekend

# full[,"Count_l":=log(Count)]
#lag by 1 hr, 6 hrs, 12 hrs, 24 hrs, 1week, 1 month, quaterly , halfyearly, yearly.
# full[,"Lag_1_year":=Lag(Count,k=365*24)]
# # full[,"Lag_2_year":=Lag(Count_N,k=(365*2)*24)]
# full[,"Lag_6_months":=Lag(Count,k=(364/2)*24)]
# full[,"Lag_3_months":=Lag(Count,k=(364/4)*24)]
# full[,"Lag_1":=Lag(Count,k=1*24)]
# full[,"Lag_2":=Lag(Count,k=2*24)]
# full[,"Lag_3":=Lag(Count,k=3*24)]
# full[,"Lag_4":=Lag(Count,k=4*24)]
# full[,"Lag_5":=Lag(Count,k=5*24)]
# full[,"Lag_6":=Lag(Count,k=6*24)]
# full[,"Lag_7":=Lag(Count,k=7*24)]
# full[,"Lag_1_hrs":=Lag(Count,k=1)]
# full[,"Lag_6_hrs":=Lag(Count,k=6)]
# full[,"Lag_12_hrs":=Lag(Count,k=12)]

full[,"Mean.Per.Month":=mean(Count,na.rm=T),by=c("Month","Year")]
full[,"Mean.Per.Hrs":=mean(Count,na.rm=T),by=c("Hrs","Year")]
full[,"Mean.Per.Week":=mean(Count,na.rm=T),by=c("Week","Year")]

full[,"Med.Per.Month":=median(Count,na.rm=T),by=c("Month","Year")]
full[,"Med.Per.Hrs":=median(Count,na.rm=T),by=c("Hrs","Year")]
full[,"Med.Per.Week":=median(Count,na.rm=T),by=c("Week","Year")]

full[]

head(full)
summary(full[istrain==1,])

full[is.na(full)] <- -999999

x <- colnames(full)[c(1,6,7,9:13,29:31)]
y <- 'Count'




## Lets us the h2o Random Forest and check how the data is

#removing the initial 1 year of data since its influencing more.

train_x <- full[istrain==1&Dates>'2013-07-25',c(x,y),with=FALSE]
valid_x <- full[istrain==1&Dates>'2014-07-25',c(x,y),with=FALSE]
test_x <- full[istrain==0,c(x,y),with=FALSE]

ndata2 <- ts(train_x$Count,frequency = 24)


pred_ets2 <- ets(ndata2)
pred_ets2
plot(pred_ets2)
plot(forecast(pred_ets2))

fcst_ets2 <- forecast(pred_ets,h=5112)
plot(fcst_ets2)

pred_ets



ggplot()+geom_line(aes(x=1:5112,y=(fcst_ets2$mean)),colour="blue",size=1)


##### h2o random forest
library(h2o)

h2o.init(nthreads=-1)


h_tr <- as.h2o(train_x)
h_va <- as.h2o(valid_x)
h_te <- as.h2o(test_x)

rfname <- paste('ak_h2o_rf',format(Sys.time(),"%d%H%M%S"),sep = '_')#rfname
rf <- h2o.randomForest(training_frame = h_tr,validation_frame=h_va,x=x,y=y,ntrees = 1000,
                       score_tree_interval = 10,distribution = "gaussian",model_id = rfname)
summary(rf)

lmname <- paste('ak_h2o_lm',format(Sys.time(),"%d%H%M%S"),sep = '_')
lm <- h2o.glm(training_frame = h_tr,validation_frame=h_va,x=x,y=y,family = "gaussian",model_id = lmname)
summary(lm)
lm

plot(rf)
plot(lm)

train_pred_rf <- h2o.predict(h2o.getModel(rfname),h_tr)

RMSE(train_x$Count,as.vector(train_pred_rf$predict))

train_pred_lm <- h2o.predict(h2o.getModel(lmname),h_tr)

RMSE(train_x$Count,as.vector(train_pred_lm$predict))

valid_pred_rf <- h2o.predict(h2o.getModel(rfname),h_va)

RMSE(valid_x$Count,as.vector(valid_pred_rf$predict))

valid_pred_lm <- h2o.predict(h2o.getModel(lmname),h_va)

RMSE(valid_x$Count,as.vector(valid_pred_lm$predict))

valid_l <- 1:length(valid_x$ID)
train_l <- 1:length(train_x$ID)

ggplot()+geom_line(aes(x=train_l,y=train_x$Count),colour="red",size=1)+
  geom_line(aes(x=train_l,y=as.vector(train_pred_rf$predict)),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Train Actual vs Predicted")

ggplot()+geom_line(aes(x=valid_l,y=valid_x$Count),colour="red",size=1)+
  geom_line(aes(x=valid_l,y=as.vector(valid_pred_rf$predict)),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Valid Actual vs Predicted")

ggplot()+geom_line(aes(x=train_l,y=train_x$Count),colour="red",size=1)+
  geom_line(aes(x=train_l,y=as.vector(train_pred_lm$predict)),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Train Actual vs Predicted")

ggplot()+geom_line(aes(x=valid_l,y=valid_x$Count),colour="red",size=1)+
  geom_line(aes(x=valid_l,y=as.vector(valid_pred_lm$predict)),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Valid Actual vs Predicted")

#both gave almost good prediction

test_pred_rf <- h2o.predict(h2o.getModel(rfname),h_te)

test_pred_lm <- h2o.predict(h2o.getModel(lmname),h_te)
  
# test_rf_pred <- unscale_data(as.vector(test_pred_rf$predict),max_data,min_data)
# 
test_lm_pred <- as.vector(test_pred_lm$predict)
test_rf_pred <- as.vector(test_pred_rf$predict)

RMSE(fcst_ets2$mean,as.vector(test_pred_rf$predict)*2)

RMSE(fcst_ets2$mean,as.vector(test_pred_lm$predict))

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets2$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=as.vector(test_pred_rf$predict)*2),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Valid Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets2$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=as.vector(test_pred_lm$predict)*2),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Valid Actual vs Predicted")

ensemble1 <- test_lm_pred*0.5+test_rf_pred*0.5
ensemble2 <- test_lm_pred*0.8+test_rf_pred*0.2
ensemble3 <- test_lm_pred*0.3+test_rf_pred*0.7
ensemble4 <- as.vector(fcst_ets2$mean)*0.5+test_lm_pred*0.125+test_rf_pred*0.125
ensemble5 <- as.vector(fcst_ets2$mean)*0.9+test_lm_pred*0.1
ensemble6 <- test_rf_pred*0.2+as.vector(fcst_ets2$mean)*0.8
ensemble7 <- +test_rf_pred*0.3+as.vector(fcst_ets2$mean)*0.7
ensemble8 <- as.vector(fcst_ets2$mean)*0.2+test_lm_pred*0.4+test_rf_pred*0.4

ensemble9 <- test_lm_pred*0.8+as.vector(fcst_ets2$mean)*0.2
ensemble10 <- +test_rf_pred*0.8+as.vector(fcst_ets2$mean)*0.2

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble1),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble2),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble3),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble4),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble5),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble6),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")

ggplot()+geom_line(aes(x=1:length(test_x$ID),y=fcst_ets$mean),colour="red",size=1)+
  geom_line(aes(x=1:length(test_x$ID),y=ensemble7),colour="blue",size=1)+
  labs(x="Time Series",y="Count_N",title="Actual vs Predicted")



submission <- data.frame('ID'=test$ID,'Count'=as.vector(test_pred_lm$predict)*2)
colnames(submission) <- c('ID','Count')
filename <- paste('ak_lm_times_2_',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
