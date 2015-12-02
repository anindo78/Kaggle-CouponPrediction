master <-fread("master.csv", header=T)

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

DT2 <-master[, .(cnt_purchase=sum(PURCHASE_FLG),
                 cnt_items_purchase=sum(ITEM_COUNT, na.rm=T),
                 avg_disc=mean(1 - (DISCOUNT_PRICE/CATALOG_PRICE)),
                 avg_price_rate=mean(PRICE_RATE),
                 cnt_sessions=sum(!duplicated(SESSION_ID_hash)),
                 coupons_viewed=sum(!duplicated(COUPON_ID_hash)),
                 cnt_session_pages=.N),
             by = .(USER_ID_hash, SEX_ID, AGE)]
#22749

DT2$pct_convert <- DT2$cnt_purchase/DT2$coupons_viewed #Gives per coupon viewed, number of purchases made
DT2$pct_engaged <- DT2$cnt_sessions/DT2$coupons_viewed #Gives per coupon viewed, number of sessions engaged in
DT2$items_per_purchase <- ifelse(DT2$cnt_purchase >0, DT2$cnt_items_purchase/DT2$cnt_purchase, 0)  #Gives purchase, number items bought
DT2$page_viewed_pct <- DT2$cnt_session_pages/DT2$cnt_sessions #Gives per session, how many pages browsed
DT2$avg_disc <- round(DT2$avg_disc*100, 2) #Gives avg. discount redeemed

DT2$female <- ifelse(DT2$SEX_ID=="f", 1, 0)
DT2$SEX_ID <- NULL

CDT2 = DT2

### the big task of appending the two files

# FIND A COMMON COUPON FROM TWO FILES c221 and coup.1

# coup.1 = 19413 obs, 139 variables
# c221 = 310 obs, 114 variables





