# KAGGLE COMPETITION ON COUPON PREDICTION
# SETWD

setwd('~/Kaggle/Coupon Prediction')

# LOAD TRAINING DATASETS FIRST
# COUPON_AREA_TRAIN, COUPON_DETAIL_TRAIN, COUPON_LIST_TRAIN, USER_LIST, COUPON_VIST_TRAIN

# LOAD MASTER DATA FIRST: USER_LIST, COUPON_LIST_TRAIN, COUPON_AREA_TRAIN
library(data.table)

user <- fread("user_list.csv", sep=",", header=T, verbose=T)
coupon_list <- fread("coupon_list_train.csv", sep=",", header=T, verbose=T)
coupon_area <- fread("coupon_area_train.csv", sep=",", header=T, verbose=T)

coupon_area <- read.table("coupon_area_train.txt", sep=",", header=T, skipNul = T)

#TRANSLATE THE JAPANESE CAPSULTE TEXT TO ENGLISH USING FREQUENCIES
#IGNORE GENRE NAME FOR NOW



jap_cap <- data.frame(capsule=table(coupon_list$CAPSULE_TEXT), stringsAsFactors = F)
jap_cap <- jap_cap[order(-jap_cap$capsule.Freq), ]
names(jap_cap) <- c("CAPSULE_TEXT","CAPSULE_FREQ")

eng_cap <- data.frame(capsule.translation = c("Delivery service",
             "Food",
             "Hotel",
             "Hair salon",
             "Japanese hotel",
             "Relaxation",
             "Other",
             "Spa",
             "Leisure",
             "Lesson",
             "Nail and eye salon",
             "Gift card",
             "Resort inn",
             "Japanse guest house",
             "Health and medical",
             "Web service",
             "Beauty",
             "Vacation rental",
             "Lodge",
             "Class",
             "Correspondence course",
             "Guest house",
             "Public hotel",
             "Event",
             "Beauty")
)

eng_cap_trans <- data.frame(cbind(jap_cap, eng_cap))

coupon_list_trans <- merge(coupon_list, eng_cap_trans, by=c("CAPSULE_TEXT"), all.x=T)
write.csv(coupon_list_trans, file="coupon_list_trans.csv")

# MERGE USER LIST WITH VIEW AND PURCHASE LOG

visit <- fread("coupon_visit_train.csv", sep=",", header=T, verbose=T)
purchase <- fread("coupon_detail_train.csv", sep=",", header=T, verbose=T)

visit$view_date <- visit$I_DATE
purchase$purchase_date <- purchase$I_DATE

#MERGE USER AND VIEW FIRST

user_viewed <- merge(user, visit, by = "USER_ID_hash", all.x = T, allow.cartesian=T)
user_viewed$I_DATE <- NULL
user_viewed$WITHDRAW_DATE <- NULL
user_viewed$COUPON_ID_hash <- user_viewed$VIEW_COUPON_ID_hash 
user_viewed$VIEW_COUPON_ID_hash <- NULL
#2833248

prop.table(table(user_viewed$PURCHASE_FLG))
#         0          1 
# 0.95680154 0.04319846 

# MERGE WITH PURCHASE TABLE

user_view_purchase <- merge(user_viewed, purchase, by=c("USER_ID_hash", "COUPON_ID_hash","PURCHASEID_hash"), all.x =T, allow.cartesian = T)
user_view_purchase$I_DATE <- NULL
#2833248 OBS

# sample <- user_view_purchase[user_view_purchase$COUPON_ID_hash=="38beeadfe3f97e640367eddae4a8c1b5" & 
#                              user_view_purchase$USER_ID_hash=="0000b53e182165208887ba65c079fc21", ]

#MERGE COUPON_LIST_TRANS with USER_VIEW_PURCHASE
coupon_list_trans$CAPSULE_TEXT <- NULL
coupon_list_trans$GENRE_NAME <- NULL


master <- merge(user_view_purchase, coupon_list_trans, by="COUPON_ID_hash", all.y = T)
#2517207 obs. Some of the obs got dropped since they weren't in the master coupon list that was provided




write.csv(master, file="master.csv")

master <-fread("master.csv", header=T)
# master <- as.data.table(master)




#AGGREGATING DATA AT USER_ID_hash level

DT1 <-master[, .(cnt_purchase=sum(PURCHASE_FLG),
                 cnt_items_purchase=sum(ITEM_COUNT, na.rm=T),
                 avg_disc=mean(1 - (DISCOUNT_PRICE/CATALOG_PRICE)),
                 avg_price_rate=mean(PRICE_RATE),
                 cnt_sessions=sum(!duplicated(SESSION_ID_hash)),
                 cnt_session_pages=.N),
                 by = USER_ID_hash]
                 
#22749        

head(DT1)

quantile(DT1$cnt_session_pages, probs = seq(0.1,1,.1), na.rm=T)

x <- DT1[DT1$cnt_purchase>0,]$cnt_purchase
x <- na.omit(x)
plot(density(x), xlab = 'Count of Purchases', ylab = '% of Users', main = 'Distribution of Users by Purchases')


h <- hist(x[x < 100], breaks = 50, col = 'green', xlab = 'Count of Purchases', ylab = '% of Users', main = 'Distribution of Users by Purchases')
xfit<-seq(min(x[x < 100]),max(x[x < 100]),length=40) 
yfit<-dnorm(xfit,mean=mean(x[x < 100]),sd=sd(x[x < 100])) 
yfit <- yfit*diff(h$mids[1:2])*length(x[x < 100]) 
lines(xfit, yfit, col="blue", lwd=2)
  
  
DT1$pur <- ifelse(DT1$cnt_purchase>0, 1, 0)
prop.table(table(DT1$pur))
#       0         1 
# 0.1474415 0.8525585 

barplot(prop.table(table(DT1$cnt_purchase)))



#       10%         20%         30%         40%         50%         60%         70%         80%         90%        100% 
#   0.00000000  0.03976230  0.07718239  0.11642655  0.16666667  0.22222222  0.33333333  0.50000000  0.83468468 50.00000000 
#        10%       20%       30%       40%       50%       60%       70%       80%       90%      100% 
#   0.2285714 0.2978723 0.3457944 0.4000000 0.4444444 0.5000000 0.5333333 0.6000000 0.7000000 1.0000000 

DT1[DT1$pct_convert>1,]
sample <- master[master$USER_ID_hash=="6f54a14211272ca3adfe6b265ba7c275", ]


#CREATING NEW VARIABLES
DT1$pct_convert <- DT1$cnt_purchase/DT1$cnt_session_pages #Gives per page viewed, number of purchases made
DT1$items_per_purchase <- ifelse(DT1$cnt_purchase >0, DT1$cnt_items_purchase/DT1$cnt_purchase, 0)  #Gives purchase, number items bought
DT1$page_viewed_pct <- DT1$cnt_session_pages/DT1$cnt_sessions #Gives per session, how many pages browsed
DT1$avg_disc <- round(DT1$avg_disc*100, 2) #Gives avg. discount redeemed


quantile(DT1$pct_convert, probs = seq(0.1,1,.1), na.rm=T)
quantile(DT1$avg_disc, probs = seq(0.1,1,.1), na.rm=T)
quantile(DT1$items_per_purchase, probs = seq(0.1,1,.1), na.rm=T)
quantile(DT33.7$page_viewed_pct, probs = seq(0.1,1,.1), na.rm=T)

#          10%        20%        30%        40%        50%        60%        70%        80%        90%       100% 
#   0.00000000 0.01725173 0.03260870 0.04672897 0.06350123 0.08333333 0.11144895 0.16666667 0.27272727 1.00000000 

#     10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
#   28.04 33.66 36.72 38.90 40.56 42.05 43.45 44.92 47.09 92.86 

#       10%       20%       30%       40%       50%       60%       70%       80%       90%      100% 
#   0.000000  1.000000  1.000000  1.000000  1.000000  1.125000  1.333333  1.600000  2.000000 30.000000 

#       10%       20%       30%       40%       50%       60%       70%       80%       90%      100% 
#   1.416667  1.654020  1.848659  2.000000  2.200000  2.490165  2.800000  3.250000  4.166667 45.666667

write.csv(DT1, file="DT1.csv")

# CREATING DATA WITH TIME STAMPS

master$viewdate <- substr(master$view_date, 1, 10)
barplot(prop.table(table(master$viewdate)))


DT2 <-master[, .(cnt_purchase=sum(PURCHASE_FLG),
                 cnt_items_purchase=sum(ITEM_COUNT, na.rm=T),
                 avg_disc=mean(1 - (DISCOUNT_PRICE/CATALOG_PRICE)),
                 cnt_sessions=sum(!duplicated(SESSION_ID_hash)),
                 cnt_session_pages=.N),
             by = .(USER_ID_hash, viewdate, capsule.translation)]

#1273587 OBS

write.csv(DT2, file="DT2.csv")

DT2$month_view <- substr(DT2$viewdate,1,7)

DT2 <- DT2[order(DT2$USER_ID_hash, DT2$viewdate), ]

# CREATE DUMMY VARS FOR MONTH VIEW AND CAPSULE.TRANSLATION
library(caret)

month <- predict(dummyVars(~ month_view, data = DT2), newdata = DT2)
capsule <- predict(dummyVars(~capsule.translation, data=DT2), newdata=DT2)

DT22 <- as.data.table(cbind(DT2, month, capsule))


write.csv(DT22, file="DT22.csv")


# NEED TO SEE THE DATE OF PURCHASE/VIEW VS. WHEN COUPON WAS RELEASED

coupon_list <- fread("coupon_list_train.csv", sep=",", header=T, verbose=T)


DT3 <-master[, .(cnt_purchase=sum(PURCHASE_FLG),
                 cnt_items_purchase=sum(ITEM_COUNT, na.rm=T),
                 avg_disc=mean(1 - (DISCOUNT_PRICE/CATALOG_PRICE)),
                 cnt_sessions=sum(!duplicated(SESSION_ID_hash)),
                 cnt_session_pages=.N),
             by = .(USER_ID_hash, REG_DATE,	SEX_ID,	AGE, viewdate, COUPON_ID_hash)]


# 1877565 OBS

DT3 <- DT3[order(DT3$USER_ID_hash, DT3$viewdate, DT3$COUPON_ID_hash),]

DT33 <- merge(DT3, coupon_list_trans, by="COUPON_ID_hash")
DT33 <- DT33[order(DT33$USER_ID_hash, DT33$viewdate, DT33$COUPON_ID_hash),]


DT33[, ':=' (viewdate=as.Date(viewdate), dispfrom=as.Date(DISPFROM), validfrom=as.Date(VALIDFROM), validend=as.Date(VALIDEND))]
DT33[, ':=' (days_since=difftime(viewdate, dispfrom, units="days"), validity=difftime(validend, validfrom, units="days"))]

# DT33 IS SAME AS DT1 AND DT2, AGGREGATED AT DIFFERENT LEVELS
# PERFORM SIMILAR TRANSFORMATIONS TO CREATE NEW VARS LIKE WE DID FOR DT1 AND DT2

# CREATE SIMILAR VARS AS IN DT1

DT33$pct_convert <- DT33$cnt_purchase/DT33$cnt_session_pages #Gives per page viewed, number of purchases made
DT33$items_per_purchase <- ifelse(DT33$cnt_purchase >0, DT33$cnt_items_purchase/DT33$cnt_purchase, 0)  #Gives purchase, number items bought
DT33$page_viewed_pct <- DT33$cnt_session_pages/DT33$cnt_sessions #Gives per session, how many pages browsed
DT33$avg_disc <- round(DT33$avg_disc*100, 2) #Gives avg. discount redeemed

head(DT33[,.(COUPON_ID_hash, USER_ID_hash, viewdate, cnt_purchase, cnt_session_pages, page_viewed_pct, capsule.translation)], 20)

# CREATE SIMILAR VARS AS IN DT2

capsule <- predict(dummyVars(~capsule.translation, data=DT33), newdata=DT33)
DT33.2 <- as.data.table(cbind(DT33, capsule))

#DROP UNNECESSARY VARIABLES

drop_vars = c(
               "CAPSULE_TEXT",
               "GENRE_NAME",
               "CATALOG_PRICE",
               "DISCOUNT_PRICE",
               "DISPFROM",
               "DISPEND",
               "DISPPERIOD",
               "VALIDFROM",
               "VALIDEND",
               "VALIDPERIOD",
               "USABLE_DATE_MON",
               "USABLE_DATE_TUE",
               "USABLE_DATE_WED",
               "USABLE_DATE_THU",
               "USABLE_DATE_FRI",
               "USABLE_DATE_SAT",
               "USABLE_DATE_SUN",
               "USABLE_DATE_HOLIDAY",
               "USABLE_DATE_BEFORE_HOLIDAY",
               "validity")

DT33.3 <- DT33.2[, (drop_vars) := NULL]

DT33.3[, ':=' (regsince=as.Date(REG_DATE))]
DT33.3[, ':=' (reg_since=difftime(viewdate, regsince, units="days"))]
DT33.4 <- DT33.3[, c("COUPON_ID_hash", "USER_ID_hash", "capsule.translation", "viewdate", "CAPSULE_FREQ", "dispfrom", "validend",
          "validfrom", "regsince", "REG_DATE") := NULL ]


large_area <- predict(dummyVars(~large_area_name, data=DT33.4), newdata=DT33.4)
ken_name <- predict(dummyVars(~ken_name, data=DT33.4), newdata=DT33.4)
small_area_name <- predict(dummyVars(~small_area_name, data=DT33.4), newdata=DT33.4)

DT33.5 <- as.data.table(cbind(DT33.4, large_area, ken_name, small_area_name))

DT33.5[, c("large_area_name","ken_name","small_area_name") := NULL]

write.csv(DT33.5, file = "DT33.5.csv")
DT33.5 <- fread("DT33.5.csv", header=T)


prop.table(table(DT33.5$cnt_purchase))
barplot(prop.table(table(DT33.5$cnt_purchase)))

# 0            1            2            3            4            5            6            7            8            9           10 
# 9.423008e-01 5.206480e-02 4.514360e-03 7.573643e-04 2.055855e-04 9.214067e-05 2.449983e-05 9.054285e-06 9.586890e-06 2.663025e-06 1.278252e-05 
# 11           12           13           15           18           24           30 
# 1.597815e-06 1.065210e-06 1.597815e-06 5.326050e-07 5.326050e-07 5.326050e-07 5.326050e-07 


DT33.5$label <- ifelse(DT33.5$cnt_purchase>0,1,0)
prop.table(table(DT33.5$label))

prop.table(table(DT33.5$page_viewed_pct))

boxplot(DT33.5[label==1]$page_viewed_pct, DT33.5[label==0]$page_viewed_pct)

# 
# library(gmodels)
# CrossTable(DT33.5$label, DT33.5$page_viewed_pct, prop.chisq=T, dnn=c('purhcase', 'pages browsed'))



# 0          1 
# 0.94230077 0.05769923 

DT33.5$cnt_purchase <- NULL

install.packages("gbm")
install.packages("dplyr")


library(gbm)
library(dplyr)

DT33.6 <- na.omit(DT33.5)
DT33.6 <- DT33.6[,(c("V1","cnt_items_purchase","pct_convert","items_per_purchase","SEX_ID", "AGE")):=NULL]
#853999

#replace large_area names
n <- substr(names(DT33.6),1, 5)
names(DT33.6)[which(n=='large')] <- c("C1", "C2","C3","C4","C5","C6","C7","C8","C9")

ken <- NULL
for (i in 1:length(which(n=='ken_n'))) {
  ken[i]=paste("K",i, sep="")
}
  
names(DT33.6)[which(n=='ken_n')] <- ken

sma <- NULL
for (i in 1:length(which(n=='small'))) {
  sma[i]=paste("S",i, sep="")
}

names(DT33.6)[which(n=='small')] <- sma


#TESTING MODEL.FRAME TO CHURN VARS TO USE FOR THE MODELING EXERCISE
sample <- DT33.6[sample(nrow(DT33.6), 100000),]
lrmodel <- glm(label~., data =sample, family='binomial')
summary(lrmodel)

#SELECT SPECIFIC VARS FOR MODELLING GBM
#KEEP VARS

keepvars = c(
  "page_viewed_pct",
  "capsule.translation.Delivery service",
  "days_since",
  "capsule.translation.Other",
  "PRICE_RATE",
  "capsule.translation.Hotel",
  "capsule.translation.Japanese hotel",
  "capsule.translation.Leisure",
  "label",
  "reg_since",
  "capsule.translation.Gift card",
  "capsule.translation.Lodge",
  "capsule.translation.Resort inn",
  "capsule.translation.Vacation rental",
  "C1",
  "C4",
  "C6",
  "C7",
  "C8",
  "K2",
  "K4",
  "K5",
  "K6",
  "K7",
  "K12",
  "K16",
  "K17",
  "K19",
  "K20",
  "K29",
  "K30",
  "K31",
  "K34",
  "K35",
  "K36",
  "K41",
  "S4",
  "S12",
  "S21",
  "S25",
  "S28",
  "S30",
  "S40",
  "S47")
  
DT33.7 <- DT33.6[, keepvars, with=F]

write.csv(DT33.7, file="DT33.7")
DT33.7 <- fread("DT33.7", header=T)

DT33.7$label <- as.factor(DT33.7$label)

# RUNNING BOX PLOTS FOR EACH VARIABLE
for (i in 1:ncol(DT33.7)) {
  boxplot(DT33.7[i]~label, data = DT33.7, horizontal=T, names = names(DT33.7[i]))
}
boxplot(DT33.7[label==1]$page_viewed_pct, DT33.7[label==0]$page_viewed_pct)



## RUNNING CHI-SQ TESTS WITH LABEL AND ALL VARS EXCEPT REG_SINCE, PRICE & PAGE_VIEWED_PCT

df <- as.data.frame(DT33.7[, c("V1","PRICE_RATE","reg_since"):=NULL])

# chisq values
funto <- function(x, y) {
  chisq.test(df[,x],df[,y])$statistic
}

chisq <- outer(names(df),names(df),FUN=Vectorize(funto))
diag(chisq) <- 1  
rownames(chisq) <- names(df)
colnames(chisq) <- names(df)
chisq

write.csv(chisq, file="chisq.csv")

chisq_hm <- heatmap(chisq, Rowv=NA, Colv=NA, scale="row", 
                    col=cm.colors(256), verbose=T)

####PAGE_VIEWED_PCT IS A STRONGEST VARIABLE, WAY TOO HIGH CHISQ COMPARED TO REST
####CLOSE CHI SQ TESTING HERE

label = DT33.7$label
train = DT33.7[, label :=NULL]


model = gbm.fit(x=train, y=label, distribution="bernoulli", 
                n.trees=10000, shrinkage=0.01, keep.data=T, 
                interaction.depth = 2, n.minobsinnode = 30,
                verbose=T, nTrain = round(nrow(DT33.7)*0.8))

save(list = c("model"), file="model.RData")

summary(model)

#  10000        0.1985          0.1965     0.0100   -0.0000
# gbm.perf(model)
# Using test method...
# [1] 9997

# page_viewed_pct                                           page_viewed_pct 81.464581399
# capsule.translation.Delivery service capsule.translation.Delivery service  5.050150295
# days_since                                                     days_since  4.258904675
# PRICE_RATE                                                     PRICE_RATE  3.575187111
# capsule.translation.Other                       capsule.translation.Other  1.746702718
# reg_since                                                       reg_since  1.430691647
# capsule.translation.Hotel                       capsule.translation.Hotel  0.813798589
# capsule.translation.Japanese hotel     capsule.translation.Japanese hotel  0.800773231
# capsule.translation.Gift card               capsule.translation.Gift card  0.380797407
# S21                                                                   S21  0.170797944
# S30                                                                   S30  0.101228041
# capsule.translation.Resort inn             capsule.translation.Resort inn  0.023240661
# C7                                                                     C7  0.020780923
# C8                                                                     C8  0.018314905
# S4                                                                     S4  0.015891937
# K5                                                                     K5  0.011986154
# S25                                                                   S25  0.010402972
# K12                                                                   K12  0.009392099
# K35                                                                   K35  0.008810904
# K31                                                                   K31  0.007880986
# capsule.translation.Leisure                   capsule.translation.Leisure  0.007556473
# S47                                                                   S47  0.007034034
# K34                                                                   K34  0.006699015
# K20                                                                   K20  0.006579334
# capsule.translation.Vacation rental   capsule.translation.Vacation rental  0.005650876
# K29                                                                   K29  0.005061787
# C6                                                                     C6  0.004718234
# K36                                                                   K36  0.004307640
# S40                                                                   S40  0.004161101
# K17                                                                   K17  0.003873441
# C4                                                                     C4  0.003158105
# S28                                                                   S28  0.003024023
# K4                                                                     K4  0.002713884
# K30                                                                   K30  0.002481504
# S12                                                                   S12  0.002268736
# K2                                                                     K2  0.002203124
# K41                                                                   K41  0.002136149
# K6                                                                     K6  0.001976820
# capsule.translation.Lodge                       capsule.translation.Lodge  0.001426421
# K16                                                                   K16  0.001423115
# C1                                                                     C1  0.001231583


gbm.more(model, n.new.trees = 5000, verbose = T)

# 15000        0.1976          0.1960     0.0100   -0.0000
# 
# gbm.more(object = model, n.new.trees = 5000, verbose = T)
# A gradient boosted model with bernoulli loss function.
# 15000 iterations were performed.
# The best test-set iteration was 14895.
# There were 43 predictors of which 43 had non-zero influence.

trainrows =  round(nrow(DT33.7)*0.8)
testrows = trainrows+1

traindata <- DT33.7[1:trainrows,]
testdata <- DT33.7[testrows:nrow(DT33.7), ]

traindata$predict <- predict.gbm(object=model, newdata=traindata, n.trees=9997, single.tree = T, type="response")
testdata$predict <- predict.gbm(object=model, newdata=testdata, n.trees=9997, single.tree = T, type="response")

prop.table(table(traindata$label))
quantile(traindata$predict, probs=seq(0,1,0.1), na.rm=T)
quantile(testdata$predict, probs=seq(0,1,0.1), na.rm=T)


# TRAINING A GLMNET LASSO MODEL

library(glmnet)

y = as.vector(DT33.7[,label])
x = as.matrix(DT33.7[, label:=NULL])

masterlm <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)
plot(masterlm)
masterlm$lambda.min

coef(masterlm, s = "lambda.min")
DT33.77 <- as.data.frame(DT33.7)

DT33.7$pred_fit <- predict(masterlm, x, s = "lambda.min", type="response")

### IMPORTANT UPDATE
#!! AFTER RUNNING THROUGH KS, THERE'S SOMETHING WRONG WITH GLMNET
## WANT TO RUN GLM PAATI MODEL INSTEAD

glmdata <- DT33.7[, c("pred_fit", "V1", "page_viewed_pct"):=NULL]

masterlm <- glm(label~., family = "binomial", data = glmdata)
glmdata$pred_fit <- predict(masterlm, glmdata, type="response")

quantile(glmdata$pred_fit, probs=seq(0,1,0.1), na.rm = T)

glmdata_sor <- glmdata[order(glmdata$pred_fit),]
plot(glmdata_sor$pred_fit)

#       0%          10%          20%          30%          40%          50%          60% 
# 2.220446e-16 7.921132e-04 1.419087e-03 5.161746e-03 6.740967e-03 1.093411e-02 2.040825e-02 
#       70%          80%          90%         100% 
#   2.615777e-02 4.027496e-02 1.714724e-01 1.000000e+00 

DT33.7_sor <- DT33.7[order(-DT33.7$pred_fit),]
plot(DT33.7_sor$pred_fit)

DT33.7_sor$esc <- DT33.7_sor$label
DT33.7_sor$nonesc <- 1 - DT33.7_sor$esc
table(DT33.7_sor$esc)

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i)
  }
  return (ifelse(x<deciles[1], 1,
                 ifelse(x<deciles[2], 2,
                        ifelse(x<deciles[3], 3,
                               ifelse(x<deciles[4], 4,
                                      ifelse(x<deciles[5], 5,
                                             ifelse(x<deciles[6], 6,
                                                    ifelse(x<deciles[7], 7,
                                                           ifelse(x<deciles[8], 8,
                                                                  ifelse(x<deciles[9], 9, 10))))))))))
}

DT33.7_sor$scrdec <- 11 - decile(DT33.7_sor$pred_fit)
DT33.7_sor$not_esc <- 1 - DT33.7_sor$esc



#modeldat
#msort$scrdec <- 11 - decile(msort$pred)
#msort$roll_fwd <- as.numeric(levels(msort$roll_fwd))[msort$roll_fwd]
#msort$not_roll_fwd <- 1 - msort$roll_fwd


#write.csv(modeldata_sor, file="apr 15 data scored.csv")
#write.csv(msort, file="apr 15 data scored v2.csv")


library(plyr)

groupd <- ddply(DT33.7_sor, c("scrdec"), summarise,
                N = length(esc),
                sumesc = sum(esc),
                sumnesc = sum(not_esc),
                avg_esc_scr = mean(pred_fit)
                
)
groupd


groupd$cum_esc <- as.numeric(formatC(cumsum(groupd$sumesc)))
groupd$cum_nesc <- as.numeric(formatC(cumsum(groupd$sumnesc), format="f"))
groupd$cum_n <- as.numeric(formatC(cumsum(groupd$N)))

tot_esc <- as.numeric(groupd$cum_esc[10])
tot_nesc <- as.numeric(groupd$cum_nesc[10])

groupd$cum_esc_pct <- groupd$cum_esc / tot_esc
groupd$cum_nesc_pct <- groupd$cum_nesc / tot_nesc

groupd$KS <- abs(groupd$cum_esc_pct - groupd$cum_nesc_pct) * 100
groupd
max(groupd$KS)




