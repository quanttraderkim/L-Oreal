#total_hyper-----------------------
total_hyper_haircare <- read.csv(
  "C:/Users/corpkr.businessplann/Desktop/작업중/최종분석/하이퍼 분석 원본/total_hyper_haircare.csv",
  header = TRUE
)
total_hyper_haircare <- total_hyper_haircare[
  complete.cases(total_hyper_haircare[, "sell.out.unit"]), ]

total_hyper_haircare <- total_hyper_haircare %>%
  filter(productivity != 0)

#store-sub df 전처리 
store_sub_function <- function(x,y){df <- total_hyper_haircare %>%
  filter(store == x) %>%
  filter(subcategory == y)
  return(df)
}
emart_oil <- store_sub_function("emart", "oil")
emart_tr5 <- store_sub_function("emart", "TR5 hairpack")
emart_other <- store_sub_function("emart", "other")

tesco_oil <- store_sub_function("tesco", "oil")
tesco_tr5 <- store_sub_function("tesco", "TR5 hairpack")
tesco_other <- store_sub_function("tesco", "other")

lotte_oil <- store_sub_function("lotte", "oil")
lotte_tr5 <- store_sub_function("lotte", "TR5 hairpack")
lotte_other <- store_sub_function("lotte", "other")

#각 store productivity mean

store_pm_function <- function(data=x){mean.df <- as.data.frame(tapply(
  data$productivity, data$month, mean))
  mean.df$month <- as.numeric(rownames(mean.df))
  names(mean.df) <- c("productivity", "month")
  return(mean.df)
}

mean_emart_oil <- store_pm_function(data = emart_oil)
mean_emart_other <- store_pm_function(data = emart_other)
mean_emart_tr5 <- store_pm_function(data = emart_tr5)

mean_tesco_oil <- store_pm_function(data = tesco_oil)
mean_tesco_other <- store_pm_function(data = tesco_other)
mean_tesco_tr5 <- store_pm_function(data = tesco_tr5)

mean_lotte_oil <- store_pm_function(data = lotte_oil)
mean_lotte_other <- store_pm_function(data = lotte_other)
mean_lotte_tr5 <- store_pm_function(data = lotte_tr5)

#month-productivity mean graph-------
#E-mart
ggplot(mean_emart_oil, aes(x=month, y=productivity,
                               colour = "oil"))+
  geom_point()+geom_line()+geom_smooth()+
  geom_line(data=mean_emart_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_point(data=mean_emart_other, aes(x=month, y=productivity,
                                          colour="other"))+
  geom_smooth(data=mean_emart_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_line(data=mean_emart_tr5, aes(x=month, y=productivity,
                                         colour="TR5 hairpack"))+
  geom_point(data=mean_emart_tr5, aes(x=month, y=productivity,
                                          colour="TR5 hairpack"))+
  geom_smooth(data=mean_emart_tr5, aes(x=month, y=productivity,
                                       colour="TR5 hairpack"))+
    
  ggtitle("E-mart haircare average productivity by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#tesco
ggplot(mean_tesco_oil, aes(x=month, y=productivity,
                           colour = "oil"))+
  geom_point()+geom_line()+geom_smooth()+
  geom_line(data=mean_tesco_other, aes(x=month, y=productivity,
                                       colour="other"))+
  geom_point(data=mean_tesco_other, aes(x=month, y=productivity,
                                        colour="other"))+
  geom_smooth(data=mean_tesco_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_line(data=mean_tesco_tr5, aes(x=month, y=productivity,
                                     colour="TR5 hairpack"))+
  geom_point(data=mean_tesco_tr5, aes(x=month, y=productivity,
                                      colour="TR5 hairpack"))+
  geom_smooth(data=mean_tesco_tr5, aes(x=month, y=productivity,
                                       colour="TR5 hairpack"))+
  
  ggtitle("Tesco haircare average productivity by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#lotte
ggplot(mean_lotte_oil, aes(x=month, y=productivity,
                           colour = "oil"))+
  geom_point()+geom_line()+geom_smooth()+
  geom_line(data=mean_lotte_other, aes(x=month, y=productivity,
                                       colour="other"))+
  geom_point(data=mean_lotte_other, aes(x=month, y=productivity,
                                        colour="other"))+
  geom_smooth(data=mean_lotte_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_line(data=mean_lotte_tr5, aes(x=month, y=productivity,
                                     colour="TR5 hairpack"))+
  geom_point(data=mean_lotte_tr5, aes(x=month, y=productivity,
                                      colour="TR5 hairpack"))+
  geom_smooth(data=mean_lotte_tr5, aes(x=month, y=productivity,
                                       colour="TR5 hairpack"))+
  
  ggtitle("Lotte haircare average productivity by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#-----각 store productivity sum

store_ps_function <- function(data=x){sum.df <- as.data.frame(tapply(
  data$productivity, data$month, sum))
sum.df$month <- as.numeric(rownames(sum.df))
names(sum.df) <- c("productivity", "month")
return(sum.df)
}

sum_emart_oil <- store_ps_function(data = emart_oil)
sum_emart_other <- store_ps_function(data = emart_other)
sum_emart_tr5 <- store_ps_function(data = emart_tr5)

sum_tesco_oil <- store_ps_function(data = tesco_oil)
sum_tesco_other <- store_ps_function(data = tesco_other)
sum_tesco_tr5 <- store_ps_function(data = tesco_tr5)

sum_lotte_oil <- store_ps_function(data = lotte_oil)
sum_lotte_other <- store_ps_function(data = lotte_other)
sum_lotte_tr5 <- store_ps_function(data = lotte_tr5)

#month-productivity sum graph-------
#E-mart
ggplot(sum_emart_oil, aes(x=month, y=productivity,
                           colour = "oil"))+
  geom_point()+geom_line()+geom_smooth()+
  geom_line(data=sum_emart_other, aes(x=month, y=productivity,
                                       colour="other"))+
  geom_point(data=sum_emart_other, aes(x=month, y=productivity,
                                        colour="other"))+
  geom_smooth(data=sum_emart_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_line(data=sum_emart_tr5, aes(x=month, y=productivity,
                                     colour="TR5 hairpack"))+
  geom_point(data=sum_emart_tr5, aes(x=month, y=productivity,
                                      colour="TR5 hairpack"))+
  geom_smooth(data=sum_emart_tr5, aes(x=month, y=productivity,
                                       colour="TR5 hairpack"))+
  
  ggtitle("E-mart haircare productivity sum by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#tesco
ggplot(sum_tesco_oil, aes(x=month, y=productivity,
                           colour = "oil"))+
  geom_point()+geom_line()+geom_smooth()+
  geom_line(data=sum_tesco_other, aes(x=month, y=productivity,
                                       colour="other"))+
  geom_point(data=sum_tesco_other, aes(x=month, y=productivity,
                                        colour="other"))+
  geom_smooth(data=sum_tesco_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_line(data=sum_tesco_tr5, aes(x=month, y=productivity,
                                     colour="TR5 hairpack"))+
  geom_point(data=sum_tesco_tr5, aes(x=month, y=productivity,
                                      colour="TR5 hairpack"))+
  geom_smooth(data=sum_tesco_tr5, aes(x=month, y=productivity,
                                       colour="TR5 hairpack"))+
  
  ggtitle("Tesco haircare productivity sum by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#lotte
ggplot(sum_lotte_oil, aes(x=month, y=productivity,
                           colour = "oil"))+
  geom_point()+geom_line()+geom_smooth()+
  geom_line(data=sum_lotte_other, aes(x=month, y=productivity,
                                       colour="other"))+
  geom_point(data=sum_lotte_other, aes(x=month, y=productivity,
                                        colour="other"))+
  geom_smooth(data=sum_lotte_other, aes(x=month, y=productivity,
                                         colour="other"))+
  geom_line(data=sum_lotte_tr5, aes(x=month, y=productivity,
                                     colour="TR5 hairpack"))+
  geom_point(data=sum_lotte_tr5, aes(x=month, y=productivity,
                                      colour="TR5 hairpack"))+
  geom_smooth(data=sum_lotte_tr5, aes(x=month, y=productivity,
                                       colour="TR5 hairpack"))+
  
  ggtitle("Lotte haircare productivity sum by month")+
  scale_x_continuous(breaks=c(rep(1:18)))


#TR5-----------------------
#Subcategory month-productivity&price graph
mpp_tr5_function <- function(data=z, a){
    data_productivity <- as.data.frame(tapply(
      data$productivity, data$month, sum))
    data_price <- as.data.frame(tapply(
      data$price, data$month, mean))
    data_productivity$month <- as.numeric(rownames(data_productivity))
    data <- cbind(data_productivity, data_price)
    names(data) <- c("productivity", "month", "price")
    data %>%
    ggplot(aes(month, productivity*10000,
                          colour = "productivity*10000"))+
    geom_point()+geom_line()+geom_smooth()+
    geom_line(data=data, aes(x=month, y=price,
                                  colour="price"))+
    geom_point(data=data, aes(x=month, y=price,
                                   colour="price"))+
    geom_smooth(data=data, aes(x=month, y=price,
                                    colour="price"))+
    
    ggtitle(a)+
    scale_x_continuous(breaks=c(rep(1:18)))
    
    data %>%
    group_by(month) %>%
    summarise(productivity)
}

mpp_tr5_function(data=emart_tr5, "E-mart TR5 hairpack")
mpp_tr5_function(data=tesco_tr5, "Tesco TR5 hairpack")
mpp_tr5_function(data=lotte_tr5, "Lotte TR5 hairpack")

#Oil-----------------------
#Subcategory month-productivity&price graph
#oil별로 나눠야... 어떻게할지 조금 더 고민 해봐야... product 별로 filter?
#그냥 sum 멕이기? 어떤 걸로 할지 고민! 직접 매뉴얼로 나누는게 나을수도.
#product 수가 month에 따라 변해서.. 

mpp_oil_function <- function(data=a, b){data %>%
    ggplot(aes(month, productivity*10000,
               colour = "productivity*10000"))+
    geom_point()+geom_line()+geom_smooth()+
    geom_line(data=data, aes(x=month, y=price,
                             colour="price"))+
    geom_point(data=data, aes(x=month, y=price,
                              colour="price"))+
    geom_smooth(data=data, aes(x=month, y=price,
                               colour="price"))+
    facet_wrap(~product)+
    
    ggtitle(b)+
    scale_x_continuous(breaks=c(rep(1:18)))
}

mpp_oil_function(data=emart_oil, "E-mart Oil")
mpp_oil_function(data=tesco_oil, "Tesco Oil")
mpp_oil_function(data=lotte_oil, "Lotte Oil")

#set 비교 
mpp_oil_set_function <- function(data=a, b){
  set_df <- data[grep("Set", data$product.name), ]

  nset_df <- data[-grep("Set", data$product.name), ]
  
  mean_set_df <- as.data.frame(tapply(
    set_df$productivity, set_df$month, mean))
  mean_set_price <- as.data.frame(tapply(
    set_df$price, set_df$month, mean))
  mean_set_df$month <- as.numeric(rownames(mean_set_df))
  mean_set_df <- cbind(mean_set_df, mean_set_price)
  names(mean_set_df) <- c("productivity", "month", "price")
  
  mean_nset_df <- as.data.frame(tapply(
    nset_df$productivity, nset_df$month, mean))
  mean_nset_price <- as.data.frame(tapply(
    nset_df$price, nset_df$month, mean))
  mean_nset_df$month <- as.numeric(rownames(mean_nset_df))
  mean_nset_df <- cbind(mean_nset_df, mean_nset_price)
  names(mean_nset_df) <- c("productivity", "month", "price")
  
    mean_set_df %>%
    ggplot(aes(month, productivity*30000,
               colour = "set productivity*30000"))+
    geom_point()+geom_line()+geom_smooth()+
    geom_line(data=mean_set_df, aes(x=month, y=price,
                             colour="set price"))+
    geom_point(data=mean_set_df, aes(x=month, y=price,
                              colour="set price"))+
    geom_smooth(data=mean_set_df, aes(x=month, y=price,
                               colour="set price"))+
  
    geom_line(data=mean_nset_df, aes(x=month, y=price,
                                 colour="not set price"))+
    geom_point(data=mean_nset_df, aes(x=month, y=price,
                                  colour="not set price"))+
    geom_smooth(data=mean_nset_df, aes(x=month, y=price,
                                   colour="not set price"))+  
    geom_line(data=mean_nset_df, aes(x=month, y=productivity*30000,
                                  colour="not set productivity*30000"))+
    geom_point(data=mean_nset_df, aes(x=month, y=productivity*30000,
                                   colour="not set productivity*30000"))+
    geom_smooth(data=mean_nset_df, aes(x=month, y=productivity*30000,
                                    colour="not set productivity*30000"))+  
    
    
    ggtitle(b)+
    scale_x_continuous(breaks=c(rep(1:18)))
    c(mean(mean_set_df$productivity),
      mean(mean_nset_df$productivity))
}

mpp_oil_set_function(data=emart_oil, "E-mart Oil Set vs. not set")
mpp_oil_set_function(data=tesco_oil, "Tesco Oil Set vs. not set ")
mpp_oil_set_function(data=lotte_oil, "Lotte Oil Set vs. not set")

pm_oil_function <- function(data=a, b){data %>%
    filter(product.name = b) %>%
    mean(productivity)
}
#Set-nset 라인별 productivity 비교

pm_oil_function <- function(data=a){data %>%
  group_by(product.name) %>%
  summarise(productivity = mean(productivity))
}

pm_oil_function(emart_oil)
pm_oil_function(tesco_oil)
pm_oil_function(lotte_oil)

#--set 비교 sum으로 
spp_oil_set_function <- function(data=a, b){
  set_df <- data[grep("Set", data$product), ]
  
  nset_df <- data[-grep("Set", data$product), ]
  
  sum_set_df <- as.data.frame(tapply(
    set_df$productivity, set_df$month, sum))
  mean_set_price <- as.data.frame(tapply(
    set_df$price, set_df$month, mean))
  sum_set_df$month <- as.numeric(rownames(sum_set_df))
  sum_set_df <- cbind(sum_set_df, mean_set_price)
  names(sum_set_df) <- c("productivity", "month", "price")
  
  sum_nset_df <- as.data.frame(tapply(
    nset_df$productivity, nset_df$month, sum))
  mean_nset_price <- as.data.frame(tapply(
    nset_df$price, nset_df$month, mean))
  sum_nset_df$month <- as.numeric(rownames(sum_nset_df))
  sum_nset_df <- cbind(sum_nset_df, mean_nset_price)
  names(sum_nset_df) <- c("productivity", "month", "price")
  
  sum_set_df %>%
    ggplot(aes(month, productivity*30000,
               colour = "set productivity*30000"))+
    geom_point()+geom_line()+geom_smooth()+
    geom_line(data=sum_set_df, aes(x=month, y=price,
                                    colour="set price"))+
    geom_point(data=sum_set_df, aes(x=month, y=price,
                                     colour="set price"))+
    geom_smooth(data=sum_set_df, aes(x=month, y=price,
                                      colour="set price"))+
    
    geom_line(data=sum_nset_df, aes(x=month, y=price,
                                     colour="not set price"))+
    geom_point(data=sum_nset_df, aes(x=month, y=price,
                                      colour="not set price"))+
    geom_smooth(data=sum_nset_df, aes(x=month, y=price,
                                       colour="not set price"))+  
    geom_line(data=sum_nset_df, aes(x=month, y=productivity*30000,
                                     colour="not set productivity*30000"))+
    geom_point(data=sum_nset_df, aes(x=month, y=productivity*30000,
                                      colour="not set productivity*30000"))+
    geom_smooth(data=sum_nset_df, aes(x=month, y=productivity*30000,
                                       colour="not set productivity*30000"))+  
    
    
    ggtitle(b)+
    scale_x_continuous(breaks=c(rep(1:18)))
    c(mean(sum_set_df$productivity),
                    mean(sum_nset_df$productivity))
}
spp_oil_set_function(data=emart_oil, "E-mart Oil Set vs. not set[sum]")
spp_oil_set_function(data=tesco_oil, "Tesco Oil Set vs. not set[sum] ")
spp_oil_set_function(data=lotte_oil, "Lotte Oil Set vs. not set[sum]")

#----oil product 수 
for(i in 1:18){
  i
  y <- nrow(filter(emart_oil, month==i))
  cat(y)
}

for(i in 1:18){
  i
  y <- nrow(filter(tesco_oil, month==i))
  cat(y)
}

for(i in 1:18){
  i
  y <- nrow(filter(lotte_oil, month==i))
  cat(y)
}
#----month 별 product 성과
productsum_function <- function(data=x){
  sum_df <- as.data.frame(tapply(
  data$productivity, data$month, sum))
  
  mean_price <- as.data.frame(tapply(
  data$price, data$month, mean))
  
  sum_df2 <- cbind(sum_df, mean_price)
  sum_df2$month <- as.numeric(rownames(sum_df2))
  
  
  names(sum_df2) <- c("productivity", "price", "month")
  return(sum_df2)
  
}
sum_emart_oil <- productsum_function(data=emart_oil)
sum_tesco_oil <- productsum_function(data=tesco_oil)
sum_lotte_oil <- productsum_function(data=lotte_oil)


emart_oil %>%
  ggplot(aes(month, productivity))+
  geom_bar(stat = "identity", position = "stack", aes(fill = product.name))+
  geom_smooth(data = sum_emart_oil, aes(x=month, y=productivity,
                                        colour = "productivity sum"))+
  ggtitle("E-mart Oil")+
  scale_x_continuous(breaks=c(rep(1:18)))

tesco_oil %>%
  ggplot(aes(month, productivity))+
  geom_bar(stat = "identity", position = "stack", aes(fill = product.name))+
  geom_smooth(data = sum_tesco_oil, aes(x=month, y=productivity,
                                        colour = "productivity sum"))+
  ggtitle("Tesco Oil")+
  scale_x_continuous(breaks=c(rep(1:18)))

lotte_oil %>%
  ggplot(aes(month, productivity))+
  geom_bar(stat = "identity", position = "stack", aes(fill = product.name))+
  geom_smooth(data = sum_lotte_oil, aes(x=month, y=productivity,
                                        colour = "productivity sum"))+
  ggtitle("Lotte Oil")+
  scale_x_continuous(breaks=c(rep(1:18)))

#relative performance
emart_oil %>%
  ggplot(aes(month, productivity))+
  geom_bar(stat = "identity", position = "fill", aes(fill = product.name))+
  geom_smooth(data = sum_emart_oil, aes(x=month, y=productivity,
                                        colour = "productivity sum"))+
  ggtitle("E-mart Oil")+
  scale_x_continuous(breaks=c(rep(1:18)))+
  ylab("ratio")

tesco_oil %>%
  ggplot(aes(month, productivity))+
  geom_bar(stat = "identity", position = "fill", aes(fill = product.name))+
  geom_smooth(data = sum_tesco_oil, aes(x=month, y=productivity,
                                        colour = "productivity sum"))+
  ggtitle("Tesco Oil")+
  scale_x_continuous(breaks=c(rep(1:18)))+
  ylab("ratio")

lotte_oil %>%
  ggplot(aes(month, productivity))+
  geom_bar(stat = "identity", position = "fill", aes(fill = product.name))+
  geom_smooth(data = sum_lotte_oil, aes(x=month, y=productivity,
                                        colour = "productivity sum"))+
  ggtitle("Lotte Oil")+
  scale_x_continuous(breaks=c(rep(1:18)))+
  ylab("ratio")

sum_df %>%
  ggplot(aes(month, productivity*10000))+
  geom_line()+geom_smooth()+
  geom_point(data=sum_df, aes(x=month, y=price, 
                              colour = "average price"))+
  scale_x_continuous(breaks=c(rep(1:18)))

