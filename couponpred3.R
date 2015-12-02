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

master <-fread("master.csv", header=T)


DT1 <-master[, .(cnt_purchase=sum(PURCHASE_FLG),
                 cnt_items_purchase=sum(ITEM_COUNT, na.rm=T),
                 avg_disc=mean(1 - (DISCOUNT_PRICE/CATALOG_PRICE)),
                 avg_price_rate=mean(PRICE_RATE),
                 cnt_sessions=sum(!duplicated(SESSION_ID_hash)),
                 coupons_viewed=sum(!duplicated(COUPON_ID_hash)),
                 cnt_session_pages=.N),
             by = .(USER_ID_hash, SEX_ID, AGE, COUPON_ID_hash)]
#1602884

x <- DT1[DT1$cnt_purchase>0,]$cnt_purchase
h <- hist(x[x < 100], breaks = 50, col = 'green', xlab = 'Count of Purchases', ylab = '% of Users', main = 'Distribution of Users by Purchases')
xfit<-seq(min(x[x < 100]),max(x[x < 100]),length=40) 
yfit<-dnorm(xfit,mean=mean(x[x < 100]),sd=sd(x[x < 100])) 
yfit <- yfit*diff(h$mids[1:2])*length(x[x < 100]) 
lines(xfit, yfit, col="blue", lwd=2)

table(DT1$cnt_purchase)
barplot(prop.table(table(DT1$cnt_purchase)))
quantile(DT1$cnt_purchase, probs = seq(0.1,1,.1), na.rm=T)
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#   0    0    0    0    0    0    0    0    0   30 

DT1$pct_convert <- DT1$cnt_purchase/DT1$coupons_viewed #Gives per coupon viewed, number of purchases made
DT1$pct_engaged <- DT1$cnt_sessions/DT1$coupons_viewed #Gives per coupon viewed, number of sessions engaged in
DT1$items_per_purchase <- ifelse(DT1$cnt_purchase >0, DT1$cnt_items_purchase/DT1$cnt_purchase, 0)  #Gives purchase, number items bought
DT1$page_viewed_pct <- DT1$cnt_session_pages/DT1$cnt_sessions #Gives per session, how many pages browsed
DT1$avg_disc <- round(DT1$avg_disc*100, 2) #Gives avg. discount redeemed

# SUBSETTING ALL PURCHASE TRANSACTIONS

table(master$PURCHASE_FLG)

sample1 <- master[PURCHASE_FLG==1]

#SET KEY
setkey(sample1, USER_ID_hash, purchase_date)
sample1$purdate <- as.Date(sample1$purchase_date)

# SELECT FIRST AND LAST PURCHASE DATES FOR EACH USER ID
sample2 <-sample1[, .(first_purchase_date=min(purdate),
                      last_purchase_date=max(purdate)),
                  by = .(USER_ID_hash)]


sample2$ref_date = as.Date("2012-06-24")
sample2[, ':=' (first_last = difftime(last_purchase_date, first_purchase_date, units="days"), 
                days_since_last = (difftime(ref_date, last_purchase_date, units="days")))]

sample2$first_last <- as.numeric(sample2$first_last)
sample2$days_since_last <- as.numeric(sample2$days_since_last)


head(sample2)
sample3 <- sample2[, c("cnt_purchase", "ref_date") := NULL]


# MERGING BOTH THE FILES

DT1.1 <- merge(DT1, sample3, by="USER_ID_hash", all.x = T)
write.csv(DT1.1, file="DT1.1.csv")
DT1.1 <- fread("DT1.1.csv", header=T)
DT1.1 <- DT1.1[2:nrow(DT1.1),]

DT1.1$first_purchase_date <- NULL
DT1.1$last_purchase_date <- NULL
DT1.1$avg_disc <- NULL


DT1.1$female <- ifelse(DT1.1$SEX_ID=="f", 1, 0)
DT1.1$SEX_ID <- NULL


coup <- read.csv("coupon_list_trans.csv", header=T)


coup <- coup[c("COUPON_ID_hash", "large_area_name","ken_name","small_area_name","PRICE_RATE",
              "VALIDFROM","VALIDEND","capsule.translation")]

library(caret)
large_area <- predict(dummyVars(~large_area_name, data=coup), newdata=coup)
ken_name <- predict(dummyVars(~ken_name, data=coup), newdata=coup)
small_area_name <- predict(dummyVars(~small_area_name, data=coup), newdata=coup)
capsule <- predict(dummyVars(~capsule.translation, data=coup), newdata=coup)


coup.1 <- as.data.table(cbind(coup, capsule, large_area, ken_name, small_area_name))
coup.1 <- coup.1[, c("large_area_name","ken_name","small_area_name"):=NULL]




#replace large_area names
n <- substr(names(coup.1),1, 5)
names(coup.1)[which(n=='large')] <- c("C1", "C2","C3","C4","C5","C6","C7","C8","C9")

ken <- NULL
for (i in 1:length(which(n=='ken_n'))) {
  ken[i]=paste("K",i, sep="")
}
names(coup.1)[which(n=='ken_n')] <- ken

sma <- NULL
for (i in 1:length(which(n=='small'))) {
  sma[i]=paste("S",i, sep="")
}
names(coup.1)[which(n=='small')] <- sma
head(coup.1)

DT1c <- merge(DT1.1, coup.1, by="COUPON_ID_hash")
write.csv(DT1c, file="DT1c.csv")
DT1c <- fread("DT1c.csv", header=T)

#Creating depedent variable
DT1c$label <- ifelse(DT1c$cnt_purchase>0, 1, 0)
prop.table(table(DT1c$label))
# 0.93277551 0.06722449 

dv <- c("COUPON_ID_hash", "V1", "USER_ID_hash", "cnt_purchase", "cnt_items_purchase",
        "items_per_purchase", "days_since_last", "first_last", "VALIDFROM", "VALIDEND",
        "capsule.translation", "pct_convert")

DT1c.1 <- DT1c[, (dv) := NULL]
DT1c.1$V1 <- NULL

# MODELING FOR LABEL i.e. propensity to purhcase a coupon
# TRAINING A GLMNET LASSO MODEL

library(glmnet)

y = as.vector(DT1c.1[,label])
copy_DT1c.1 = DT1c.1
copy_DT1c.1[, c("V1","label","preds"):=NULL]

x = as.matrix(copy_DT1c.1)

library(glmnet)
masterlm <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(masterlm)

save(list = c("masterlm"), file="masterlm.RData")

masterlm$lambda.min
# [1] 9.542323e-05
masterlm$lambda.1se
# 0.0005092451

coef(masterlm, s="lambda.min")

DT1c.1$preds <- predict(masterlm, x, type="response", s="lambda.min")
write.csv(DT1c.1, file="DT1c.1.csv")

DT1c.1 <- fread("DT1c.1.csv", header=T)
head(DT1c.1)

DT1c.1 <- DT1c.1[order(DT1c.1$preds),]
plot(DT1c.1$preds)

DT1c.1$label <- DT1c$label

library(pROC)
auc <- roc(DT1c.1$label, DT1c.1$preds)
auc$auc
# Area under the curve: 0.9618
plot(auc)

DT1c.1_sor <- DT1c.1[order(-DT1c.1$preds),]


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

DT1c.1_sor$scrdec <- 11 - decile(DT1c.1_sor$preds)
DT1c.1_sor$nonlabel <- 1 - DT1c.1_sor$label

library(plyr)

groupd <- ddply(DT1c.1_sor, c("scrdec"), summarise,
                N = length(label),
                sumesc = sum(label),
                sumnesc = sum(nonlabel),
                avg_esc_scr = mean(preds)
                
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



# ORGANIZING DATA BY USER ID

DT1c$pred <- DT1c.1$preds
DT1c <- DT1c[order(DT1c$USER_ID_hash, -DT1c$pred), ]
user_coup <- DT1c[!duplicated(DT1c$USER_ID_hash),] #UNIQUE USER ID
unique_coup <- user_coup[!duplicated(user_coup$COUPON_ID_hash),]
unique_coup <- unique_coup[, COUPON_ID_hash]

setkey(coup.1, COUPON_ID_hash)
mas_coup = coup.1[unique_coup]
head(mas_coup)
     

# GETTING COUP.1 AND MAS_COUP TO SAME VARIABLE DIMNESIONS
# SINCE THE FEATURES DIFFER DUE TO KEN_NAME AND SMALL_AREA NAME

# SCORING TEST DATASET

coupon_list <- read.csv("coupon_list_test.csv",  header=T)
coupon_area <- read.csv("coupon_area_test.csv",  header=T)

# read coupon_list_trans created during training
coupon_list_trans <- read.csv("coupon_list_trans.csv", header = T)

# de-duping for unique capsule text and translation
c1 <- unique(coupon_list_trans[, c(2, 27)])

c2 <- merge(coupon_list, c1, by=c("CAPSULE_TEXT"), all.x=T)



c22 <- c2[c("COUPON_ID_hash", "large_area_name","ken_name","small_area_name",
               "VALIDFROM","VALIDEND","capsule.translation", "PRICE_RATE")]

library(caret)
large_area <- predict(dummyVars(~large_area_name, data=c22), newdata=c22)
ken_name <- predict(dummyVars(~ken_name, data=c22), newdata=c22)
small_area_name <- predict(dummyVars(~small_area_name, data=c22), newdata=c22)
capsule <- predict(dummyVars(~capsule.translation, data=c22), newdata=c22)


c221 <- as.data.table(cbind(c22, capsule, large_area, ken_name, small_area_name))
c221 <- c221[, c("large_area_name","ken_name","small_area_name"):=NULL]




#replace large_area names
n <- substr(names(c221),1, 5)
names(c221)[which(n=='large')] <- c("C1", "C2","C3","C4","C5","C6","C7","C8","C9")

ken <- NULL
for (i in 1:length(which(n=='ken_n'))) {
  ken[i]=paste("K",i, sep="")
}
names(c221)[which(n=='ken_n')] <- ken

sma <- NULL
for (i in 1:length(which(n=='small'))) {
  sma[i]=paste("S",i, sep="")
}
names(c221)[which(n=='small')] <- sma
head(c221)

c221$avg_price_rate = c221$PRICE_RATE
c221$PRICE_RATE <- NULL


c221$avg_price_rate <- c221$PRICE_RATE
c221$avg_price_rate <- NULL

n1 <- names(c221)
n2 <- names(mas_coup)

x <- which(n2 %in% n1)
x
# [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25
# [26]  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50
# [51]  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  85  86  87  88
# [76]  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
# [101] 114 115 116 117 118 119 120 121 122 123 124 125 126

mas_coup2 <- as.data.frame(mas_coup)[, c(2,6:72,86:127)]
c222 <- as.data.frame(c221)[,  c(5:114)]

write.csv(mas_coup2, file='mas_coup2.csv')
write.csv(c222, file='c222.csv')

head(c222)

# ONLY normalize PRICE_RATE variable
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
mc.s <- mas_coup2
mc.s$PRICE_RATE <- normalize(mas_coup2$PRICE_RATE)

c222.s = c222 
c222.s$PRICE_RATE <- normalize(c222$PRICE_RATE)

#TRYING HIER CLUSTERING FIRST
d1 <- dist(mc.s, method = "minkowski")

#hier clustring
f1 <- hclust(d1, method="ward")
plot(f1)

h1 <- f1$height
subs <- round(h1 - c(0,h1[-length(h1)]), 3) # subtract next height
which.max(subs)


g1 <- cutree(f1, k=50)
table(g1)
mas_coup2$cluster = g1

# Attempting to run kmeans
# Determine number of clusters
wss <- (nrow(mc.s)-1)*sum(apply(mc.s,2,var))
for (i in 2:100) wss[i] <- sum(kmeans(mc.s, 
                                      centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# The plot shows us ever decreasing value in WSS with cluster size
# pointing towards the fact that each row is a cluster in itself or
# in other words no elegant clusters found from k-means


# attempt to check number of clusters using mixed models
install.packages("mclust")
library(mclust)

f12 <- Mclust(mc.s)
summary(f12)
# the model shows only 1 component which is ellipsoidal multivariate normal
# we will continue to use hier clustering with 20 trees from viewing the dendo

prop.table(table(mas_coup2$cluster))

# The value cluster is the label for running knn
# use scaled dataset

#mc.s <- as.data.frame(scale(mas_coup2[, 1:110]))
labels <- as.factor(mas_coup2$cluster)

# RUNNING kNN
install.packages("class")
library(class)

p <- knn(mc.s, c222.s, k = 21, cl = labels)

c221$pred_cluster <- p
table(c221$pred_cluster)

pc <- as.data.frame(c221)[, c(1, 5, 115)]
pc <- pc[order(pc$pred_cluster),]

pc$cluster <- pc$pred_cluster

mas_coup$cluster <- mas_coup2$cluster
cc <- as.data.frame(mas_coup)[, c(1, 141)]
uc <- merge(user_coup, cc, by="COUPON_ID_hash", all.x=T)

uc1 <- as.data.frame(uc)[, c(3, 7, 158)]

puc <- merge(uc1, pc, by="cluster", all=T)
#186151      

puc <- puc[order(puc$USER_ID_hash), ]
puc$diff <- abs(puc$avg_price_rate - puc$PRICE_RATE)/100
head(puc)

puc <- puc[order(puc$USER_ID_hash, puc$diff), ]
puc1 <- puc[!duplicated(puc$USER_ID_hash), ]

puc2 <- as.data.frame(puc1)[, c(2, 4)]
write.csv(puc2, file="puc2.csv")

