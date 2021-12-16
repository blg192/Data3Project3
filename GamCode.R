
library(gam)
library(tidyverse)
##new clusters

precip_clus_cat
X=data.frame(SST_clustwide)

length(X)

Y12= precip_clus_cat %>% filter(Cluster==12)
nrow(precip_clus_cat)
tot.dat<- Y12 %>% inner_join(X, by = c("Date"="Date"))
tot.dat.1<- tot.dat[,-c(1:5)]
tot.dat.1$cat<- as.numeric(as.factor(tot.dat.1$cat))
length(tot.dat.1)
library(gam)



#Building the Gamma model using full data
library(tidyverse)
set.seed(12345)
Y= precip_clus_cat
Y<- Y %>% inner_join(X, by="Date")
Y$cat<- as.numeric(as.factor(Y$cat))
nrow(Y)
#data Jan2017 and later
test<- Y[4462921:4538380,]
#data before Jan 2017
train<- Y[1:4462920,]
train$Date<- as.factor(as.numeric(train$Date))
gam.2<- gam(cat~SeaCluster1 +SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5, data = train)
#predicted model
summary(gam.2)

#Find the MSEs using the model using each cluster
#Cluster 12
Y12= Y %>% filter(Cluster==12)
test12<- Y12[90253:91778,]
test12<- test12 %>% inner_join(X, by = "Date")
colnames(test12)=colnames(Y)

pred.gam12<- data.frame(predict(gam.2, test12))

mse.tab12<- data.frame(c(pred.gam12),
                       c(test12$cat))
colnames(mse.tab12)<- c("Predicted", "Actual")

mse.tab12$Diff<- (mse.tab12$Predicted - mse.tab12$Actual)^2

mean(mse.tab12$Diff)
#0.7794147

#Cluster 11
Y11= Y %>% filter(Cluster==11)
nrow(Y11)
test11<- Y11[123373:125458,]
test11<- test11 %>% inner_join(X, by = "Date")
colnames(test11)=colnames(Y)

pred.gam11<- data.frame(predict(gam.2, test11))

mse.tab11<- data.frame(c(pred.gam11),
                       c(test11$cat))
colnames(mse.tab11)<- c("Predicted", "Actual")

mse.tab11$Diff<- (mse.tab11$Predicted - mse.tab11$Actual)^2

mean(mse.tab11$Diff)
#0.6398478

#Cluster 10
Y10= Y %>% filter(Cluster==10)
nrow(Y10)
test10<- Y10[268273:272808,]
test10<- test10 %>% inner_join(X, by = "Date")
colnames(test10)=colnames(Y)

pred.gam10<- data.frame(predict(gam.2, test10))

mse.tab10<- data.frame(c(pred.gam10),
                       c(test10$cat))
colnames(mse.tab10)<- c("Predicted", "Actual")

mse.tab10$Diff<- (mse.tab10$Predicted - mse.tab10$Actual)^2

mean(mse.tab10$Diff)
#0.6898226

#Cluster 9
Y9= Y %>% filter(Cluster==9)
nrow(Y9)
test9<- Y9[726157:738434,]
test9<- test9 %>% inner_join(X, by = "Date")
colnames(test9)=colnames(Y)

pred.gam9<- data.frame(predict(gam.2, test9))

mse.tab9<- data.frame(c(pred.gam9),
                       c(test9$cat))
colnames(mse.tab9)<- c("Predicted", "Actual")

mse.tab9$Diff<- (mse.tab9$Predicted - mse.tab9$Actual)^2

mean(mse.tab9$Diff)
#0.6617359

#Cluster 8
Y8= Y %>% filter(Cluster==8)
nrow(Y8)
test8<- Y8[105157:106934,]
test8<- test8 %>% inner_join(X, by = "Date")
colnames(test8)=colnames(Y)

pred.gam8<- data.frame(predict(gam.2, test8))

mse.tab8<- data.frame(c(pred.gam8),
                      c(test8$cat))
colnames(mse.tab8)<- c("Predicted", "Actual")

mse.tab8$Diff<- (mse.tab8$Predicted - mse.tab8$Actual)^2

mean(mse.tab8$Diff)
#0.7245501

#Cluster 7
Y7= Y %>% filter(Cluster==7)
nrow(Y7)
test7<- Y7[744373:756958,]
test7<- test7 %>% inner_join(X, by = "Date")
colnames(test7)=colnames(Y)

pred.gam7<- data.frame(predict(gam.2, test7))

mse.tab7<- data.frame(c(pred.gam7),
                      c(test7$cat))
colnames(mse.tab7)<- c("Predicted", "Actual")

mse.tab7$Diff<- (mse.tab7$Predicted - mse.tab7$Actual)^2

mean(mse.tab7$Diff)
#0.6599537

#Cluster 8
Y6= Y %>% filter(Cluster==6)
nrow(Y6)
test6<- Y6[826345:840316,]
test6<- test6 %>% inner_join(X, by = "Date")
colnames(test6)=colnames(Y)

pred.gam6<- data.frame(predict(gam.2, test6))

mse.tab6<- data.frame(c(pred.gam6),
                      c(test6$cat))
colnames(mse.tab6)<- c("Predicted", "Actual")

mse.tab6$Diff<- (mse.tab6$Predicted - mse.tab6$Actual)^2

mean(mse.tab6$Diff)
#0.7093298

#Cluster 5
Y5= Y %>% filter(Cluster==5)
nrow(Y5)
test5<- Y5[224389:228182,]
test5<- test5 %>% inner_join(X, by = "Date")
colnames(test5)=colnames(Y)

pred.gam5<- data.frame(predict(gam.2, test5))

mse.tab5<- data.frame(c(pred.gam5),
                      c(test5$cat))
colnames(mse.tab5)<- c("Predicted", "Actual")

mse.tab5$Diff<- (mse.tab5$Predicted - mse.tab5$Actual)^2

mean(mse.tab5$Diff)
#0.7434327

#Cluster 4
Y4= Y %>% filter(Cluster==4)
nrow(Y4)
test4<- Y4[279037:283754,]
test4<- test4 %>% inner_join(X, by = "Date")
colnames(test4)=colnames(Y)

pred.gam4<- data.frame(predict(gam.2, test4))

mse.tab4<- data.frame(c(pred.gam4),
                      c(test4$cat))
colnames(mse.tab4)<- c("Predicted", "Actual")

mse.tab4$Diff<- (mse.tab4$Predicted - mse.tab4$Actual)^2

mean(mse.tab4$Diff)
#0.7354513


#Cluster 3

Y3= Y %>% filter(Cluster==3)
nrow(Y3)
test3<- Y3[756793:769588,]
test3<- test3 %>% inner_join(X, by = "Date")
colnames(test3)=colnames(Y)

pred.gam3<- data.frame(predict(gam.2, test3))

mse.tab3<- data.frame(c(round(pred.gam3)),
                      c(test3$cat))
colnames(mse.tab3)<- c("Predicted", "Actual")

mse.tab3$Diff<- (round(mse.tab3$Predicted) - mse.tab3$Actual)^2

mean(mse.tab3$Diff)
#0.7434627

#Cluster 2
Y2= Y %>% filter(Cluster==2)
nrow(Y2)

test2<- train %>% filter(Cluster==2)
test2<- test2 %>% inner_join(X, by = "Date")
colnames(test2)=colnames(Y)

pred.gam2<- data.frame(predict(gam.2, test2))

mse.tab2<- data.frame(c(pred.gam2),
                      c(test2$cat))
colnames(mse.tab2)<- c("Predicted", "Actual")

mse.tab2$Diff<- (mse.tab2$Predicted - mse.tab2$Actual)^2

mean(mse.tab2$Diff)

#Cluster 1
Y1= Y %>% filter(Cluster==1)
nrow(Y1)
Y1[165000:171768,]
test1<- Y1[105157:106934,]
test1<- test1 %>% inner_join(X, by = "Date")
colnames(test1)=colnames(Y)

pred.gam1<- data.frame(predict(gam.2, test1))

mse.tab1<- data.frame(c(pred.gam1),
                      c(test1$cat))
colnames(mse.tab1)<- c("Predicted", "Actual")

mse.tab1$Diff<- (mse.tab1$Predicted - mse.tab1$Actual)^2

mean(mse.tab1$Diff)
#1.087216







#Building the Gamma model using full data (continuous)

set.seed(12345)
Y= precip_clus_cat
Y<- Y %>% inner_join(X, by="Date")
Y$cat<- as.numeric(as.factor(Y$cat))
nrow(Y)
#data Jan2017 and later
test<- Y[4462921:4538380,]
#data before Jan 2017
train<- Y[1:4462920,]
gam.1<- gam(Precipitation~s(SeaCluster1,2) +SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5, data = train)
#predicted model
summary(gam.1)

pred<- predict(gam.1, test)
dat.fram<- data.frame(pred, test$Precipitation )

dat.fram$Diff<- (dat.fram$pred - dat.fram$test.Precipitation)^2
mean(dat.fram$Diff)

#cluster 12
pred.gam12.cont<- data.frame(predict(gam.1, test12))

mse.tab.cont.12<- data.frame(c(pred.gam12.cont),
                       c(test12[,5]))
colnames(mse.tab.cont.12)<- c("Predicted", "Actual")

mse.tab.cont.12$Diff<- (mse.tab.cont.12$Predicted - mse.tab.cont.12$Actual)^2

mean(mse.tab.cont.12$Diff)
#22.41128

#Cluster 11
pred.gam11.cont<- data.frame(predict(gam.1, test11))

mse.tab11.cont<- data.frame(c(pred.gam11.cont),
                       c(test11[,5]))
colnames(mse.tab11.cont)<- c("Predicted", "Actual")

mse.tab11.cont$Diff<- (mse.tab11.cont$Predicted - mse.tab11.cont$Actual)^2

mean(mse.tab11.cont$Diff)
#6.429696

#Cluster 10
pred.gam10.cont<- data.frame(predict(gam.1, test10))

mse.tab10.cont<- data.frame(c(pred.gam10.cont),
                       c(test10[,5]))
colnames(mse.tab10.cont)<- c("Predicted", "Actual")

mse.tab10.cont$Diff<- (mse.tab10.cont$Predicted - mse.tab10.cont$Actual)^2

mean(mse.tab10.cont$Diff)
#5.361076

#Cluster 9

pred.gam9.cont<- data.frame(predict(gam.1, test9))

mse.tab9.cont<- data.frame(c(pred.gam9.cont),
                      c(test9[,5]))
colnames(mse.tab9.cont)<- c("Predicted", "Actual")

mse.tab9.cont$Diff<- (mse.tab9.cont$Predicted - mse.tab9.cont$Actual)^2

mean(mse.tab9.cont$Diff)
#1.771952

#Cluster 8
pred.gam8.cont<- data.frame(predict(gam.1, test8))

mse.tab8.cont<- data.frame(c(pred.gam8.cont),
                      c(test8[,5]))
colnames(mse.tab8.cont)<- c("Predicted", "Actual")

mse.tab8.cont$Diff<- (mse.tab8.cont$Predicted - mse.tab8.cont$Actual)^2

mean(mse.tab8.cont$Diff)
#13.6852

#Cluster 7
pred.gam7.cont<- data.frame(predict(gam.1, test7))

mse.tab7.cont<- data.frame(c(pred.gam7.cont),
                      c(test7[,5]))
colnames(mse.tab7.cont)<- c("Predicted", "Actual")

mse.tab7.cont$Diff<- (mse.tab7.cont$Predicted - mse.tab7.cont$Actual)^2

mean(mse.tab7.cont$Diff)
#3.318387

#Cluster 8
pred.gam6.cont<- data.frame(predict(gam.1, test6))

mse.tab6.cont<- data.frame(c(pred.gam6.cont),
                      c(test6[,5]))
colnames(mse.tab6.cont)<- c("Predicted", "Actual")

mse.tab6.cont$Diff<- (mse.tab6.cont$Predicted - mse.tab6.cont$Actual)^2

mean(mse.tab6.cont$Diff)
#2.193427

#Cluster 5
pred.gam5.cont<- data.frame(predict(gam.1, test5))

mse.tab5.cont<- data.frame(c(pred.gam5.cont),
                      c(test5[,5]))
colnames(mse.tab5.cont)<- c("Predicted", "Actual")

mse.tab5.cont$Diff<- (mse.tab5.cont$Predicted - mse.tab5.cont$Actual)^2

mean(mse.tab5.cont$Diff)
#4.303495

#Cluster 4
pred.gam4.cont<- data.frame(predict(gam.1, test4))

mse.tab4.cont<- data.frame(c(pred.gam4.cont),
                      c(test4[,5]))
colnames(mse.tab4.cont)<- c("Predicted", "Actual")

mse.tab4.cont$Diff<- (mse.tab4.cont$Predicted - mse.tab4.cont$Actual)^2

mean(mse.tab4.cont$Diff)
#2.732513


#Cluster 3
pred.gam3.cont<- data.frame(predict(gam.1, test3))

mse.tab3.cont<- data.frame((pred.gam3.cont),
                      c(test3[,5]))
colnames(mse.tab3.cont)<- c("Predicted", "Actual")

mse.tab3.cont$Diff<- (mse.tab3.cont$Predicted - mse.tab3.cont$Actual)^2

mean(mse.tab3.cont$Diff)
#1.364074

#Cluster 2
pred.gam2.cont<- data.frame(predict(gam.1, test2))

mse.tab2.cont<- data.frame(c(pred.gam2.cont),
                      c(test2[,5]))
colnames(mse.tab2.cont)<- c("Predicted", "Actual")

mse.tab2.cont$Diff<- (mse.tab2.cont$Predicted - mse.tab2.cont$Actual)^2

mean(mse.tab2.cont$Diff)
#6.931621

#Cluster 1
pred.gam1.cont<- data.frame(predict(gam.1, test1))

mse.tab1.cont<- data.frame(c(pred.gam1.cont),
                      c(test1[,5]))
colnames(mse.tab1.cont)<- c("Predicted", "Actual")

mse.tab1.cont$Diff<- (mse.tab1.cont$Predicted - mse.tab1.cont$Actual)^2

mean(mse.tab1.cont$Diff)
#28.77473

#avg MSE over all clusters 8.5. Not good

#lagged data tau=6 months
nrow(X)
Y= precip_clus_cat
X.lagged<-data.frame(as.character(X$Date))

X$YDate<- c(X.lagged[-c(1:6),], NA,NA,NA,NA,NA,NA)
Lag.dat<- X %>% inner_join(Y[1:4462920,], by= c("YDate"="Date"))
test.lag<- X %>% inner_join(Y[4462921:4538380,], by= c("YDate"="Date"))

gam.mod.lag<- gam(Precipitation~s(SeaCluster1,2) +SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5, data = Lag.dat)
summary(gam.mod.lag)
nrow(Lag.dat)

pred.lag<- predict(gam.mod.lag,test.lag)
lag.pred<- data.frame(c(pred.lag),
                      c(test.lag$Precipitation))
lag.pred$DiffSq<- (lag.pred$c.pred.lag. - lag.pred$c.test.lag.Precipitation.)^2
mean(lag.pred$DiffSq)


plot.Gam(gam.mod.lag)
#by cluster

#cluster 12
test.12<- Lag.dat %>% filter(Cluster==12)
pred.lag12<- data.frame(predict(gam.mod.lag, test.12))

mse.lag.12<- data.frame(c(pred.lag12),
                             c(test.12[,11]))
colnames(mse.lag.12)<- c("Predicted", "Actual")

mse.lag.12$Diff<- (mse.lag.12$Predicted - mse.lag.12$Actual)^2

mean(mse.lag.12$Diff)
#18.65545

#Cluster 11
test.11<- Lag.dat %>% filter(Cluster==11)
pred.lag11<- data.frame(predict(gam.mod.lag, test.11))

mse.lag.11<- data.frame(c(pred.lag11),
                        c(test.11[,11]))
colnames(mse.lag.11)<- c("Predicted", "Actual")

mse.lag.11$Diff<- (mse.lag.11$Predicted - mse.lag.11$Actual)^2

mean(mse.lag.11$Diff)
#7.091281

#Cluster 10
test.10<- Lag.dat %>% filter(Cluster==10)
pred.lag10<- data.frame(predict(gam.mod.lag, test.10))

mse.lag.10<- data.frame(c(pred.lag10),
                        c(test.10[,11]))
colnames(mse.lag.10)<- c("Predicted", "Actual")

mse.lag.10$Diff<- (mse.lag.10$Predicted - mse.lag.10$Actual)^2

mean(mse.lag.10$Diff)
#6.113391

#Cluster 9
test.9<- Lag.dat %>% filter(Cluster==9)
pred.lag9<- data.frame(predict(gam.mod.lag, test.9))

mse.lag.9<- data.frame(c(pred.lag9),
                        c(test.9[,11]))
colnames(mse.lag.9)<- c("Predicted", "Actual")

mse.lag.9$Diff<- (mse.lag.9$Predicted - mse.lag.9$Actual)^2

mean(mse.lag.9$Diff)
#1.860159

#Cluster 8
test.8<- Lag.dat %>% filter(Cluster==8)
pred.lag8<- data.frame(predict(gam.mod.lag, test.8))

mse.lag.8<- data.frame(c(pred.lag8),
                       c(test.8[,11]))
colnames(mse.lag.8)<- c("Predicted", "Actual")

mse.lag.8$Diff<- (mse.lag.8$Predicted - mse.lag.8$Actual)^2

mean(mse.lag.8$Diff)
#9.208827


#Cluster 7
test.7<- Lag.dat %>% filter(Cluster==7)
pred.lag7<- data.frame(predict(gam.mod.lag, test.7))

mse.lag.7<- data.frame(c(pred.lag7),
                       c(test.7[,11]))
colnames(mse.lag.7)<- c("Predicted", "Actual")

mse.lag.7$Diff<- (mse.lag.7$Predicted - mse.lag.7$Actual)^2

mean(mse.lag.7$Diff)
#2.977706

#Cluster 6
test.6<- Lag.dat %>% filter(Cluster==6)
pred.lag6<- data.frame(predict(gam.mod.lag, test.6))

mse.lag.6<- data.frame(c(pred.lag6),
                       c(test.6[,11]))
colnames(mse.lag.6)<- c("Predicted", "Actual")

mse.lag.6$Diff<- (mse.lag.6$Predicted - mse.lag.6$Actual)^2

mean(mse.lag.6$Diff)
#2.210704
#Cluster 5
test.5<- Lag.dat %>% filter(Cluster==5)
pred.lag5<- data.frame(predict(gam.mod.lag, test.5))

mse.lag.5<- data.frame(c(pred.lag5),
                       c(test.5[,11]))
colnames(mse.lag.5)<- c("Predicted", "Actual")

mse.lag.5$Diff<- (mse.lag.5$Predicted - mse.lag.5$Actual)^2

mean(mse.lag.5$Diff)
#4.017784

#Cluster 4
test.4<- Lag.dat %>% filter(Cluster==4)
pred.lag4<- data.frame(predict(gam.mod.lag, test.4))

mse.lag.4<- data.frame(c(pred.lag4),
                       c(test.4[,11]))
colnames(mse.lag.4)<- c("Predicted", "Actual")

mse.lag.4$Diff<- (mse.lag.4$Predicted - mse.lag.4$Actual)^2

mean(mse.lag.4$Diff)
#2.923751


#Cluster 3
test.3<- Lag.dat %>% filter(Cluster==3)
pred.lag3<- data.frame(predict(gam.mod.lag, test.3))

mse.lag.3<- data.frame(c(pred.lag3),
                       c(test.3[,11]))
colnames(mse.lag.3)<- c("Predicted", "Actual")

mse.lag.3$Diff<- (mse.lag.3$Predicted - mse.lag.3$Actual)^2

mean(mse.lag.3$Diff)
#1.923343

#Cluster 2
test.2<- Lag.dat %>% filter(Cluster==2)
pred.lag2<- data.frame(predict(gam.mod.lag, test.2))

mse.lag.2<- data.frame(c(pred.lag2),
                       c(test.2[,11]))
colnames(mse.lag.2)<- c("Predicted", "Actual")

mse.lag.2$Diff<- (mse.lag.2$Predicted - mse.lag.2$Actual)^2

mean(mse.lag.2$Diff)
#6.9317

#Cluster 1
test.1<- Lag.dat %>% filter(Cluster==1)
pred.lag1<- data.frame(predict(gam.mod.lag, test.1))

mse.lag.1<- data.frame(c(pred.lag1),
                       c(test.1[,11]))
colnames(mse.lag.1)<- c("Predicted", "Actual")

mse.lag.1$Diff<- (mse.lag.1$Predicted - mse.lag.1$Actual)^2

mean(mse.lag.1$Diff)
#7.981747

(22.5031 + 6.113391 + 7.091281 + 1.860159 + 9.208807 + 2.977706 + 2.212704 + 4.017784 + 2.923751 + 1.923343 + 6.9317 + 7.981747)/12 
#lagged model is better. mse 6.31212