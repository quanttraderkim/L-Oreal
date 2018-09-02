#-------------------------------------------------
options(scipen=100)
#emart--------
#month 별 평균 구해서 데이터 프레임 만들기
mean_emart_haircare <- as.data.frame(tapply(emart_haircare$sell.out.unit,
                                            emart_haircare$month, mean))
mean_emart_haircare$month <- rep(1:18)
mean_emart_haircare$price <- tapply(emart_haircare$price,
                                            emart_haircare$month, mean)
names(mean_emart_haircare) <- c("average.sell.out.unit",
                                "average.price") #이름부여


  
#sum으로 보기 
sum_emart_haircare <- as.data.frame(tapply(emart_haircare$sell.out.unit,
                                            emart_haircare$month, sum))
sum_emart_haircare$month <- rep(1:18)
sum_emart_haircare$price <- tapply(emart_haircare$price,
                                            emart_haircare$month, mean)
names(sum_emart_haircare) <- c("sell.out.unit.sum", "price.mean") #이름부여
ggplot(sum_emart_haircare, aes(x=month, y=sell.out.unit.sum,
                               colour = "unit sales"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  geom_line(data=sum_emart_haircare, aes(x=month, y=price.mean,
                        colour="price"))+
  geom_point(data=sum_emart_haircare, aes(x=month, y=price.mean,
                        colour="price"))+
  geom_smooth(data=sum_emart_haircare, aes(x=month, y=price.mean), method = lm)+
  ggtitle("E-mart Hair care sell-out unit sum & price mean by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#상관도 분석
with(emart_haircare, cor(price, sell.out.unit, 
                             method = "spearman"))

with(sum_emart_haircare, cor(price.mean, sell.out.unit.sum, 
                             method = "spearman"))

with(mean_emart_haircare, cor(average.price, average.sell.out.unit, 
                             method = "spearman"))
#subcategory correlation
x <- emart_haircare %>%
  filter(subcategory == "oil")

with(x, cor(price, sell.out.unit, 
                         method = "spearman"))

x <- emart_haircare %>%
  filter(subcategory == "TR5 hairpack")

with(x, cor(price, sell.out.unit, 
            method = "spearman"))

x <- emart_haircare %>%
  filter(subcategory == "other")

with(x, cor(price, sell.out.unit, 
            method = "spearman"))
#price와 unit 산점도 보기 
emart_haircare %>% ggplot(aes(price,`Sell-out unit`))+geom_point()

#productivity 그래프
emart_store <- c(rep(145,5), 146, 147, 146, 147, rep(148,4), rep(147,3), 148, 147)
days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30)
sum_emart_haircare$productivity <- 
  ((sum_emart_haircare$sell.out.unit.sum)/emart_store)/days
ggplot(sum_emart_haircare, aes(x=month, y=productivity))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  ggtitle("E-mart Hair care unit sum's productivity by month")+
  scale_x_continuous(breaks=c(rep(1:18)))


#average 그래프
ggplot(mean_emart_haircare, aes(x=month, y=average.sell.out.unit))+
  geom_line()+geom_point()+geom_smooth(method = lm)+
  ggtitle("E-mart Hair care average sell out unit by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#월별 product 개수(sum을 증가시키는 요인인 product 수가 어떻게 변화하는지 보기)

for(i in 1:18){
  i
  y <- nrow(filter(emart_haircare, month==i))
  return(y)
}

number.of.products <- c(14, 14, 11, 13, 13, 12, 11, 12, 12, 12, 13, 12, 14, 14, 13, 18, 18, 18)
sum_emart_haircare$number.of.products <-number.of.products 

ggplot(sum_emart_haircare, aes(x=month, y=number.of.products))+
  geom_point(colour='red')+geom_line(colour='red')+geom_smooth(method = lm)+
  ggtitle("E-mart Hair care number of products")+
  scale_x_continuous(breaks=c(rep(1:18)))

pairs(sum_emart_haircare)
cor(sum_emart_haircare$number.of.products, sum_emart_haircare$sell.out.unit.sum)

#월별 헤어케어 프로모션 일수
number.of.promotion.days <- c(4,10,14,15,5,15,14,7,16,14,7,14,20,1,14,14,0,14)
sum_emart_haircare$number.of.promotion.days <-number.of.promotion.days

ggplot(sum_emart_haircare, aes(x=month, y=number.of.promotion.days))+
  geom_point(colour='red')+geom_line(colour='red')+geom_smooth(method = lm)+
  ggtitle("E-mart Hair care number of promotion days")+
  scale_x_continuous(breaks=c(rep(1:18)))

cor(sum_emart_haircare$number.of.promotion.days, sum_emart_haircare$sell.out.unit.sum)

#oil select
emart_oil<- read.csv(
  "C:/Users/corpkr.businessplann/Desktop/작업중/하이퍼 분석 원본/emarthaircarecsv.csv",
  header=TRUE
)
emart_oil <- filter(emart_oil, subcat == "oil")
emart_oil <- na.omit(emart_oil)
emart_oil <- emart_oil[,-9]
emart_oil <- rename(emart_oil, c(cat = "category", unit = "sell.out.unit", 
                                 subcat = "subcategory",rev = "sell.out.value"))
#oil sum
sum_emart_oil <- as.data.frame(tapply(emart_oil$sell.out.unit,
                                            emart_oil$month, sum))
sum_emart_oil$month <- rep(1:18)
sum_emart_oil$price <- tapply(emart_oil$price,
                                            emart_oil$month, mean)
names(sum_emart_oil) <- c("sell.out.unit.sum", "month", "average.price")

#oil plot
ggplot(sum_emart_oil, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  geom_line(data=sum_emart_haircare, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_point(data=sum_emart_haircare, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_smooth(data=sum_emart_haircare, aes(x=month, y=average.price), method = lm)+
  ggtitle("E-mart hair oil sell-out unit sum & price mean by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#oil plot without price
ggplot(sum_emart_oil, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  ggtitle("E-mart hair oil sell-out unit sum by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#1~3월과 4월(첫 set 출시 이후)이후 비교
summary(filter(sum_emart_oil, month<=3))
summary(filter(sum_emart_oil, month>=4))
summary(filter(sum_emart_oil, month>=11))

#TR5 select 
TR5<- read.csv(
  "C:/Users/corpkr.businessplann/Desktop/작업중/하이퍼 분석 원본/emarthaircarecsv.csv",
  header=TRUE
)
TR5 <- filter(TR5, subcat == "TR5 hairpack")
TR5 <- TR5[,-9]
TR5 <- na.omit(TR5)
TR5 <- rename(TR5, c(cat = "category", unit = "sell.out.unit", 
                                 subcat = "subcategory",rev = "sell.out.value"))

#TR5 sum
sum_emart_tr5 <- as.data.frame(tapply(TR5$sell.out.unit,
                                           TR5$month, sum))
sum_emart_tr5$month <- rep(1:18)
sum_emart_tr5$price <- tapply(TR5$price,TR5$month, mean)
names(sum_emart_tr5) <- c("sell.out.unit.sum", "month", "average.price")


#tr5 plot without price
ggplot(sum_emart_tr5, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  ggtitle("E-mart hair TR5 hairpack sell-out unit sum by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#1~5와 6월(상시할인) 이후 비교
summary(filter(sum_emart_tr5, month<=5))
summary(filter(sum_emart_tr5, month>=6))

ggplot(sum_emart_tr5, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  geom_line(data=sum_emart_tr5, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_point(data=sum_emart_tr5, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_smooth(data=sum_emart_tr5, aes(x=month, y=average.price), method = lm)+
  ggtitle("E-mart hair TR5 hairpack sell-out unit sum & average price by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#haircolor
emart_haircolor<- read.csv(
  "C:/Users/corpkr.businessplann/Desktop/작업중/하이퍼 분석 원본/emart_haircolor_csv.csv",
  header=TRUE
)
emart_haircolor <- na.omit(emart_haircolor)
emart_haircolor <- rename(emart_haircolor, c(cat = "category", unit = "sell.out.unit", 
                                 subcat = "subcategory",rev = "sell.out.value"))

#haircolor sum
sum_emart_haircolor <- as.data.frame(tapply(emart_haircolor$sell.out.unit,
                                            emart_haircolor$month, sum))
sum_emart_haircolor$month <- rep(1:18)
sum_emart_haircolor$price <- tapply(emart_haircolor$price,
                                            emart_haircolor$month, mean)
names(sum_emart_haircolor) <- c("sell.out.unit.sum", "month", "average.price") #이름부여

#haircolor sum plot
ggplot(sum_emart_haircolor, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  geom_line(data=sum_emart_haircolor, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_point(data=sum_emart_haircolor, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_smooth(data=sum_emart_haircolor, aes(x=month, y=average.price), method = lm)+
  ggtitle("E-mart haircolor sell-out unit sum & average price by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#월별 product 개수(sum을 증가시키는 요인인 product 수가 어떻게 변화하는지 보기)

for(i in 1:18){
  i
  y <- nrow(filter(emart_haircolor, month==i))
  cat(y)
}

number.of.products <- c(7,6,rep(7,7),rep(8,3),rep(7,6))
sum_emart_haircolor$number.of.products <-number.of.products 

ggplot(sum_emart_haircolor, aes(x=month, y=number.of.products))+
  geom_point(colour='red')+geom_line(colour='red')+geom_smooth(method = lm)+
  ggtitle("E-mart Haircolor number of products")+
  scale_x_continuous(breaks=c(rep(1:18)))+
  scale_y_continuous(breaks = c(rep(6:8)))

#월별 haircolor 프로모션 일수
number.of.promotion.days <- c(4, 10, 14, 0, 14, 14, 4, 10, 12, 5, 9, 14, 0, 13, 3, 12, 21, 2)
sum_emart_haircolor$number.of.promotion.days <-number.of.promotion.days

ggplot(sum_emart_haircolor, aes(x=month, y=number.of.promotion.days))+
  geom_point(colour='red')+geom_line(colour='red')+geom_smooth(method = lm)+
  ggtitle("E-mart Hair color number of promotion days")+
  scale_x_continuous(breaks=c(rep(1:18)))

cor(sum_emart_haircolor$number.of.promotion.days, sum_emart_haircolor$sell.out.unit.sum)

#haircolor productivity plot
sum_emart_haircolor$productivity <- 
  ((sum_emart_haircolor$sell.out.unit.sum)/emart_store)/days
ggplot(sum_emart_haircolor, aes(x=month, y=productivity, colour = "productivity"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  ggtitle("E-mart Haircolor unit sum's productivity by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#creme select
creme <- filter(emart_haircolor, subcategory == "creme")

#creme sum
sum_emart_creme <- as.data.frame(tapply(creme$sell.out.unit,
                                           creme$month, sum))
sum_emart_creme$month <- rep(1:18)
sum_emart_creme$price <- tapply(creme$price,creme$month, mean)
names(sum_emart_creme) <- c("sell.out.unit.sum", "month", "average.price")


#creme plot with price
ggplot(sum_emart_creme, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  geom_line(data=sum_emart_creme, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_point(data=sum_emart_creme, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_smooth(data=sum_emart_creme, aes(x=month, y=average.price), method = lm)+
  ggtitle("E-mart haircolor excellence creme sell-out unit sum & average price by month")+
  scale_x_continuous(breaks=c(rep(1:18)))


#double tube select
doubletube <- filter(emart_haircolor, subcategory == "double tube")

#double tube sum
sum_emart_doubletube <- as.data.frame(tapply(doubletube$sell.out.unit,
                                           doubletube$month, sum))
sum_emart_doubletube$month <- rep(1:18)
sum_emart_doubletube$price <- tapply(doubletube$price, doubletube$month, mean)
names(sum_emart_doubletube) <- c("sell.out.unit.sum", "month", "average.price")


#double tube plot with price
ggplot(sum_emart_doubletube, aes(x=month, y=sell.out.unit.sum, colour="unit sum"))+
  geom_point()+geom_line()+geom_smooth(method = lm)+
  geom_line(data=sum_emart_doubletube, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_point(data=sum_emart_doubletube, aes(x=month, y=average.price,
                        colour="average price"))+
  geom_smooth(data=sum_emart_doubletube, aes(x=month, y=average.price), method = lm)+
  ggtitle("E-mart haircolor excellence doubletube sell-out unit sum & average price by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#creme & double tube plot
ggplot(sum_emart_creme, aes(x=month, y=sell.out.unit.sum, colour="creme unit sum"))+
  geom_point()+geom_line()+
  geom_line(data=sum_emart_doubletube, aes(x=month, y=sell.out.unit.sum,
                                      colour="double tube unit sum"))+
  geom_point(data=sum_emart_doubletube, aes(x=month, y=sell.out.unit.sum,
                                       colour="double tube unit sum"))+
  geom_line(data=sum_emart_doubletube, aes(x=month, y=average.price,
                                           colour="doubletube's average price"))+
  geom_point(data=sum_emart_doubletube, aes(x=month, y=average.price,
                                            colour="doubletube's average price"))+
  geom_line(data=sum_emart_creme, aes(x=month, y=average.price,
                                      colour="creme's average price"))+
  geom_point(data=sum_emart_creme, aes(x=month, y=average.price,
                                       colour="creme's average price"))+
  ggtitle("E-mart haircolor excellence creme & doubletube sell-out unit sum by month")+
  scale_x_continuous(breaks=c(rep(1:18)))

#discount impact

#haircare discount impact 전처리 (door effect없으므로 unit에서 days만 나눔)
emart_haircare_discountimpact<- read.csv(
  "C:/Users/corpkr.businessplann/Desktop/작업중/하이퍼 분석 원본/emarthaircarecsv.csv",
  header=TRUE
)
emart_haircare_discountimpact <- na.omit(emart_haircare_discountimpact)
emart_haircare_discountimpact$productivity.not.discount <- 
  (emart_haircare_discountimpact$sell.out.unit.not.discount)/
  (emart_haircare_discountimpact$not.discount.days)
emart_haircare_discountimpact$productivity.discount <- 
  (emart_haircare_discountimpact$sell.out.unit.discount)/
  (emart_haircare_discountimpact$promotion.days)
emart_haircare_discountimpact$discount.impact <- 
  (emart_haircare_discountimpact$productivity.discount)/
  (emart_haircare_discountimpact$productivity.not.discount)

#promotion 안한 달 제거 
emart_haircare_discountimpact<- filter(emart_haircare_discountimpact, promotion.days != 0)

#상시할인해서 할인임팩트 측정하기 어려운 TR5 제외
emart_haircare_discountimpact_without_TR5 <- filter(emart_haircare_discountimpact,
  subcategory != "TR5 hairpack")

#haircare discount impact summary
summary(emart_haircare_discountimpact_without_TR5)
summaryBy(productivity.discount + productivity.not.discount ~ subcategory,
  data=emart_haircare_discountimpact_without_TR5, FUN=c(mean, median))

#haircolor discount impact 전처리 (door effect없으므로 unit에서 days만 나눔)
emart_haircolor_discountimpact<- read.csv(
  "C:/Users/corpkr.businessplann/Desktop/작업중/하이퍼 분석 원본/emart_haircolor_csv.csv",
  header=TRUE
)
emart_haircolor_discountimpact <- na.omit(emart_haircolor_discountimpact)
emart_haircolor_discountimpact$productivity.not.discount <- 
  (emart_haircolor_discountimpact$sell.out.unit.not.discount)/
  (emart_haircolor_discountimpact$not.discount.days)
emart_haircolor_discountimpact$productivity.discount <- 
  (emart_haircolor_discountimpact$sell.out.unit.discount)/
  (emart_haircolor_discountimpact$promotion.days)
emart_haircolor_discountimpact$discount.impact <- 
  (emart_haircolor_discountimpact$productivity.discount)/
  (emart_haircolor_discountimpact$productivity.not.discount)

#promotion 안한 달 제거 
emart_haircolor_discountimpact<- filter(emart_haircolor_discountimpact, 
                                        promotion.days != 0)

#haircare discount impact summary
summary(emart_haircolor_discountimpact)
summaryBy(productivity.discount + productivity.not.discount ~ subcategory,
          data=emart_haircolor_discountimpact, FUN=c(mean, median))

#-----------------------------------------------------
#CPD Drug 전처리
total_all <-  read.csv("C:/Users/fauac/Dropbox/최종분석/drug 분석 원본/total.csv"
                       ,header=TRUE)
#결측값 제거
total_all <- total_all[complete.cases(total_all[, "sell.out.unit"]), ]

#productivity 0인 것 제거
total_all <- total_all %>%
  filter(productivity != 0)

oy_all <- total_all %>%
  filter(store == "olive young")
ws_all <- total_all %>%
  filter(store == "watsons")
ls_all <- total_all %>%
  filter(store == "lohbs")

#Eye 시각화
#Eye 전처리
total_eye <- total_all %>%
  filter(category == "MNY.eye")

total_eye_discount <- 
  total_eye %>%
  filter(discount.rate.group!= 0)

total_eye_discount <- total_eye_discount %>%
  mutate(impact = (productivity / 0.04)*100)

oy_eye <- total_eye %>%
  filter(store == "olive young")
ws_eye <- total_eye %>%
  filter(store == "watsons")
ls_eye <- total_eye %>%
  filter(store == "lohbs")

#Eye 통계량
oy_eye %>%
  filter(discount.rate.group== 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))

oy_eye %>%
  filter(discount.rate.group != 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))

ws_eye %>%
  filter(discount.rate.group== 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))

ws_eye %>%
  filter(discount.rate.group != 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))
ls_eye %>%
  filter(discount.rate.group== 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))

ls_eye %>%
  filter(discount.rate.group != 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))

x<- data.frame("subcategory" = c(rep(c("Brow", "Liner","Mascara", "Shadow"),3)), 
               "Impact" = c(150, 125, 329, 275, 150, 125, 300, 400, 200, 150,
                            240, 200),
               "store" = c(rep("olive young", 4), rep("watsons", 4),
                           rep("lohbs", 4)))
x %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(subcategory, Impact, fill = store))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = cols)+
  ggtitle("MNY eye discount impact")

#색
cols <- c("lohbs" = "indianred1", "watsons" = "deepskyblue2", 
          "olive young" = "green4")

#discount rate group을 범주형으로
total_eye_discount$discount.rate.group <- 
  as.factor(total_eye_discount$discount.rate.group)

#Eye plot by discount rate group
total_eye_discount %>%
  filter(store=="olive young")%>%
  ggplot(aes(discount.rate.group, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col="gray", alpha=0.5)+
  ggtitle("Olive young MNY eye impact by discount rate group")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")

total_eye_discount %>%
  filter(store=="watsons")%>%
  ggplot(aes(discount.rate.group, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col="gray", alpha=0.5)+
  ggtitle("Watsons MNY eye impact by discount rate group")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")

total_eye_discount %>%
  filter(store=="lohbs")%>%
  ggplot(aes(discount.rate.group, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col="gray", alpha=0.5)+
  ggtitle("Lohbs MNY eye impact by discount rate group")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")

#summary by discount rate grup function
summary_by_discountrategrup <- function(x,y){total_eye_discount %>%
  filter(subcategory == x) %>%
  filter(store == y) %>%
  filter(discount.rate != 0) %>%
  group_by(discount.rate.group) %>%
  summarize(median(impact))
}

summary_by_discountrategrup("brow", "olive young")
summary_by_discountrategrup("liner", "olive young")
summary_by_discountrategrup("mascara", "olive young")
summary_by_discountrategrup("shadow", "olive young")

summary_by_discountrategrup("brow", "watsons")
summary_by_discountrategrup("liner", "watsons")
summary_by_discountrategrup("mascara", "watsons")
summary_by_discountrategrup("shadow", "watsons")

summary_by_discountrategrup("brow", "lohbs")
summary_by_discountrategrup("liner", "lohbs")
summary_by_discountrategrup("mascara", "lohbs")
summary_by_discountrategrup("shadow", "lohbs")
summarize()

#subcategory 순
  
total_eye_discount %>%
  filter(subcategory=="brow") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate.group, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Brow")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                              lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>%
  filter(subcategory=="liner") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate.group, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Liner")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>%
  filter(subcategory=="mascara") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate.group, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Mascara")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>%
  filter(subcategory=="shadow") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate.group, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Shadow")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#Eye plot by place
#place 순서 맞추기
total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
#place 짜기
  filter(store == "olive young") %>%
  ggplot(aes(place, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col='gray')+
  ggtitle("Olive young MNY eye impact by place")+
  scale_y_continuous(name="Impact(%)")


total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  filter(store == "watsons") %>%
  ggplot(aes(place, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col='gray')+
  ggtitle("Watsons MNY eye impact by place")+
  scale_y_continuous(name="Impact(%)")


total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  filter(store == "lohbs") %>% 
  ggplot(aes(place, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col='gray')+
  ggtitle("Lohbs MNY eye impact by place")+
  scale_y_continuous(name="Impact(%)")

#subcategory 기준
#brow
total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "brow") %>% 
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot()+facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Brow")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")
  
  
#liner
total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "liner") %>% 
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Liner")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#shadow
total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "shadow") %>% 
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Shadow")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#mascara
total_eye_discount %>% 
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "mascara") %>% 
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Mascara")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#summary by place function
summary_by_place <- function(x,y){total_eye_discount %>%
    filter(subcategory == x) %>%
    filter(store == y) %>%
    filter(discount.rate != 0) %>%
    group_by(place) %>%
    summarize(median(impact))
}

summary_by_place("brow", "olive young")
summary_by_place("brow", "watsons")
summary_by_place("brow", "lohbs")


summary_by_place("liner", "olive young")
summary_by_place("liner", "watsons")
summary_by_place("liner", "lohbs")

summary_by_place("mascara", "olive young")
summary_by_place("mascara", "watsons")
summary_by_place("mascara", "lohbs")


summary_by_place("shadow", "olive young")
summary_by_place("shadow", "watsons")
summary_by_place("shadow", "lohbs")
#gwp
total_eye_discount %>%
  ggplot(aes(gwp, impact, col = "green4"))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  facet_wrap(~store)+
  geom_jitter(col='gray')+
  ggtitle("Olive young MNY eye impact by gwp")+
  scale_y_continuous(limits=(c(0,2000)), name="Impact(%)")


total_eye_discount %>%
  filter(store == "watsons") %>%
  ggplot(aes(gwp, impact, col = "deepskyblue2"))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col='gray')+
  ggtitle("Watsons MNY eye impact by gwp")+
  scale_y_continuous(limits=(c(0,2000)), name="Impact(%)")


total_eye_discount %>%
  filter(store == "lohbs") %>%
  ggplot(aes(gwp, impact, col = "indianred2"))+
  geom_boxplot(alpha=.5)+facet_grid(~subcategory)+
  geom_jitter(col='gray')+
  ggtitle("Lohbs MNY eye impact by gwp")+
  scale_y_continuous(limits=(c(0,2000)), name="Impact(%)")


total_eye_discount %>%
  filter(store == "olive young") %>%
  ggplot(aes(gwp, impact, colour = "olive young"))+
  geom_boxplot(alpha=.8)+facet_grid(~subcategory)+
  geom_boxplot(data=filter(total_eye_discount, store == "watsons"),
               aes(gwp, impact, colour = "watsons"), alpha=.8)+
  geom_boxplot(data=filter(total_eye_discount, store == "lohbs"),
               aes(gwp, impact, colour = "lohbs"), alpha=.8)+
  scale_y_continuous(limits=(c(0,2000)), name="Impact(%)")

  ggtitle("Olive young MNY eye impact by gwp")+
  scale_y_continuous(name="Impact(%)")
  
#subcategory 별 gwp
  
total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                            c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "brow") %>% 
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Brow")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                               lohbs = "indianred2"))+
   stat_summary(fun.y=mean, colour="darkred", geom="point")
  
total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "liner") %>% 
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Liner")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "mascara") %>% 
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Mascara")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "shadow") %>% 
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Shadow")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#end에서의 GWP

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "brow") %>% 
  filter(place != "POG") %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Brow[END]")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "mascara") %>% 
  filter(place != "POG") %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Mascara[END]")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "shadow") %>% 
  filter(place != "POG") %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Shadow[END]")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#POG GWP
total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "shadow") %>% 
  filter(place == "POG") %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Shadow[POG]")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "mascara") %>% 
  filter(place == "POG") %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Mascara[POG]")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")


total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "mascara") %>% 
  filter(place == "END1") %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Mascara")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#summary by gwp function
summary_by_gwp <- function(x,y){total_eye_discount %>%
    filter(subcategory == x) %>%
    filter(store == y) %>%
    group_by(gwp) %>%
    summarize(median(impact))
}

summary_by_gwp("brow", "olive young")
summary_by_gwp("brow", "watsons")
summary_by_gwp("brow", "lohbs")


summary_by_gwp("liner", "olive young")
summary_by_gwp("liner", "watsons")
summary_by_gwp("liner", "lohbs")

summary_by_gwp("mascara", "olive young")
summary_by_gwp("mascara", "watsons")
summary_by_gwp("mascara", "lohbs")


summary_by_gwp("shadow", "olive young")
summary_by_gwp("shadow", "watsons")
summary_by_gwp("shadow", "lohbs")

#summary end gwp
summary_by_gwp_end <- function(x,y){total_eye_discount %>%
    filter(subcategory == x) %>%
    filter(store == y) %>%
    filter(place != "POG") %>%
    group_by(gwp) %>%
    summarize(median(impact))
}

summary_by_gwp_end("shadow", "olive young")
summary_by_gwp_end("shadow", "watsons")
summary_by_gwp_end("shadow", "lohbs")

summary_by_gwp_end("mascara", "olive young")
summary_by_gwp_end("mascara", "watsons")
summary_by_gwp_end("mascara", "lohbs")

#set
total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "brow") %>% 
  ggplot(aes(set, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Brow")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "liner") %>% 
  ggplot(aes(set, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Liner")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "mascara") %>% 
  ggplot(aes(set, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Mascara")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  filter(subcategory == "shadow") %>% 
  ggplot(aes(set, impact, fill = store))+
  geom_boxplot(alpha=.5)+facet_grid(~store)+
  geom_jitter(col='gray')+
  ggtitle("Shadow")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#summary set

summary_by_set <- function(x,y){total_eye_discount %>%
    filter(subcategory == x) %>%
    filter(store == y) %>%
    group_by(set) %>%
    summarize(median(impact))
}

summary_by_set("brow", "olive young")
summary_by_set("brow", "watsons")
summary_by_set("brow", "lohbs")

summary_by_set("liner", "olive young")
summary_by_set("liner", "watsons")
summary_by_set("liner", "lohbs")

summary_by_set("shadow", "olive young")
summary_by_set("shadow", "watsons")
summary_by_set("shadow", "lohbs")

summary_by_set("mascara", "olive young")
summary_by_set("mascara", "watsons")
summary_by_set("mascara", "lohbs")

#set in place in olive young  

total_eye_discount %>% 
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  filter(place != "END") %>%
  filter(subcategory == "mascara") %>% 
  ggplot(aes(set, impact, fill = place))+
  geom_boxplot(alpha=.5)+facet_grid(~place)+
  geom_jitter(col='gray')+
  ggtitle("Mascara[olive young]")+
  scale_y_continuous(name="Impact(%)", limits=c(0,2000))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#summary set in place in olive young 
summary_by_set_place <- function(x,y){total_eye_discount %>%
    filter(subcategory == x) %>%
    filter(store == "oilve young") %>%
    filter(set == y) %>%
    group_by(place) %>%
    summarize(median(impact))
}

summary_by_set_place("mascara", "FALSE")

#Mascara로 내려가자
#place
oy_eye_discount %>%
  filter(subcategory == "mascara") %>%
    mutate(place=factor(place, levels=
                          c("POG", "END3", "END2", "END1"))) %>%
    ggplot(aes(discount.rate.group, impact))+
    geom_boxplot(alpha=.5)+facet_grid(~place)+
    geom_jitter(col='gray')+
    ggtitle("Olive young MNY mascara impact by discount rate group & place")+
    scale_y_continuous(name="Impact(%)")

#gwp
oy_eye_discount %>%
  filter(subcategory == "mascara") %>%
    mutate(place=factor(place, levels=
                          c("POG", "END3", "END2", "END1"))) %>%
    ggplot(aes(gwp, impact))+
    geom_boxplot(alpha=.5)+facet_grid(~place)+
    geom_jitter(col='gray')+
    ggtitle("Olive young MNY mascara impact by gwp & place")+
    scale_y_continuous(name="Impact(%)")

#set
oy_eye_discount %>%
  filter(subcategory == "mascara") %>%
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  ggplot(aes(set, impact))+
  geom_boxplot(alpha=.5)+facet_grid(~place)+
  geom_jitter(col='gray')+
  ggtitle("Olive young MNY mascara impact by set & place")+
  scale_y_continuous(name="Impact(%)")



#magnum WP
oy_eye %>%
  filter(product == "Magnum WP RENO") %>%
  ggplot(aes(month, productivity, colour ="Magnum WP productivity"))+
  geom_point()+geom_line()+
  #magnum Wp discount rate(same as barbie's discount rate)
  geom_point(data=filter(oy_eye, product == "Magnum WP RENO"), 
             aes(x=month, y=discount.rate, colour="discount rate"))+
  geom_line(data=filter(oy_eye, product == "Magnum WP RENO"),
            aes(x=month, y=discount.rate, colour="discount rate"))+
  
  #magnum barbie productivity
  geom_point(data=filter(oy_eye, product == "Magnum Barbie"), 
             aes(x=month, y=productivity, colour="Magnum Barbie productivity"))+
  geom_line(data=filter(oy_eye, product == "Magnum Barbie"),
            aes(x=month, y=productivity, colour="Magnum Barbie productivity"))+
  scale_x_continuous(breaks=c(rep(1:18)))+
  ggtitle("Olive young MNY Magnum WP vs. Barbie")

#[not brandsale] magnum WP
total_nbs %>%
  filter(product == "Magnum WP RENO") %>%
  mutate(store = factor(store, levels=c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(month, productivity, colour ="Magnum WP productivity"))+
  geom_point()+geom_line()+
  #magnum Wp discount rate(same as barbie's discount rate)
  geom_point(data=filter(total_nbs, product == "Magnum WP RENO"), 
             aes(x=month, y=discount.rate, colour="discount rate"))+
  geom_line(data=filter(total_nbs, product == "Magnum WP RENO"),
            aes(x=month, y=discount.rate, colour="discount rate"))+
  
  #magnum barbie productivity
  geom_point(data=filter(total_nbs, product == "Magnum Barbie"), 
             aes(x=month, y=productivity, colour="Magnum Barbie productivity"))+
  geom_line(data=filter(total_nbs, product == "Magnum Barbie"),
            aes(x=month, y=productivity, colour="Magnum Barbie productivity"))+
  scale_x_continuous(breaks=c(rep(1:18)))+
  facet_grid(~store)+
  ggtitle("MNY Magnum WP vs. Barbie[except brandsale period]")
#------
#magnum WP anova
magnum_nbs <- oy_eye %>%
  filter(product == "Magnum WP RENO")
magnum_nbs <- magnum_nbs[-c(3,6,9,12,15,18),]
magnum_nbs <- magnum_nbs %>%
  mutate(discount.rate.factor=factor(discount.rate))

magnum_productivity_lm <- lm(productivity ~ discount.rate.factor, data=magnum_nbs)
summary(magnum_productivity_lm)

#magnum WP 수량형 x 수량형 y 분ㅅ
#산점도
total_nbs <- total_all %>%
  filter(month != 3 & month != 6 & month != 9 & month != 12 & month != 15
         & month != 18)

total_nbs %>%
  filter(product == "Magnum WP RENO") %>%
  mutate(store = factor(store, levels=c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate, productivity)) + geom_jitter()+
  geom_smooth()+
  ggtitle("MNY Magnum WP")+
  facet_grid(~store)

#상관도
x<- filter(total_nbs, product == "Magnum WP RENO")
with(x, cor(discount.rate, productivity, method = "spearman"))

#회귀
magnum_wp_lm <- lm(productivity ~ discount.rate, data=x)
summary(magnum_wp_lm)

predict(magnum_wp_lm, newdata=data.frame(
  discount.rate = c(0, 0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45)),
  se.fit=TRUE)

opar <- par(mfrow = c(2,2), oma=c(0, 0, 1.1, 0))
plot(magnum_wp_lm, las=1) 
par(opar)

#로버스트 선형회귀분석
install.packages("MASS")

#Anova
x <- total_nbs %>%
  mutate(discount.rate = factor(discount.rate)) %>%
  filter(product == "Magnum WP RENO")

magnum_wp_lm2 <- lm(productivity ~ discount.rate, data=x)
summary(magnum_wp_lm2)

predict(magnum_wp_lm2, newdata=data.frame(discount.rate = c("0.2", "0.25", "0.29", "0.35")))

#magnum barbie
total_nbs %>%
  filter(product == "Magnum Barbie") %>%
  mutate(store = factor(store, levels=c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate, productivity)) + geom_jitter()+
  geom_smooth(method="lm")+
  ggtitle("MNY Magnum Barbie")+
  facet_grid(~store)

# 상관도
x<- filter(total_nbs, product == "Magnum Barbie")
with(x, cor(discount.rate, productivity, method = "spearman"))

#회귀
magnum_barbie_lm <- lm(productivity ~ discount.rate, data=x)
summary(magnum_barbie_lm)

predict(magnum_barbie_lm, newdata=data.frame(
  discount.rate = c(0, 0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45)),
  se.fit=TRUE)

opar <- par(mfrow = c(2,2), oma=c(0, 0, 1.1, 0))
plot(magnum_barbie_lm, las=1) 
par(opar)

#mascara로 보기 
total_nbs %>%
  filter(subcategory == "mascara") %>%
  mutate(store = factor(store, levels=c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(discount.rate, productivity)) + geom_jitter()+
  geom_smooth(method="lm")+
  ggtitle("MNY Mascara")+
  facet_grid(~store)

#mascara 회귀 
x<- filter(total_nbs, subcategory == "mascara")
with(x, cor(discount.rate, productivity, method = "spearman"))
cor(x$discount.rate, x$productivity)

mascara_lm <- lm(productivity ~ discount.rate, data=x)
summary(mascara_lm)

opar <- par(mfrow = c(2,2), oma=c(0, 0, 1.1, 0))
plot(mascara_lm, las=1) 
par(opar)

#저장저장
#kissme 할차례 
kissme <- read.csv("C:/Users/corpkr.businessplann/Desktop/작업중/최종분석/kissme.csv",
                   header=TRUE)
kissme<- rename(kissme, c(unit = "sell.out.unit", ppu = "price", ppu.inc = "price.increase",
                          rev.inc = "revenue.increase"))
kissme_nbs <- kissme %>%
  filter(month != 3 & month != 6 & month != 9 & month != 12)

#month에 따라 unit 어떻게 움직이는지

total_nbs %>%
  filter(product == "Magnum WP RENO") %>%
  filter(store == "olive young") %>%
  ggplot(aes(month, sell.out.unit, colour="Magnum WP"))+
  geom_point()+geom_line()+
  geom_point(data=kissme_nbs, aes(x=month, y=sell.out.unit, colour="kissme"))+
  geom_line(data=kissme_nbs, aes(x=month, y=sell.out.unit, colour="kissme"))+
  ggtitle("MNY Magnum WP vs Kissme [olive young, W/O brand sale period]")+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))+


#kissme 가격 추가

  geom_point(data=kissme_nbs, aes(x=month, y=price, colour="kissme price"))+
  geom_line(data=kissme_nbs, aes(x=month, y=price, colour="kissme price"))+
  
  geom_line(data=magnum_nbs_oy, aes(x=month, y=discount.rate*100000, 
                                    colour="Magnum WP discount.rate"))+
  geom_point(data=magnum_nbs_oy, aes(x=month, y=discount.rate*100000, 
                                    colour="Magnum WP discount.rate"))
#magnum WP discount rate add
total_nbs %>%
  filter(product == "Magnum WP RENO") %>%
  filter(store == "olive young") %>%
  ggplot(aes(month, discount.rate, colour="red"))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))

#correlation
#kissme unit - price
cor(kissme$sell.out.unit, kissme$price)
#kissme unit - magnum wp unit
cor(magnum_nbs_oy$sell.out.unit, magnum_nbs_oy$discount.rate)

magnum_nbs_oy <- magnum_nbs_total %>%
  filter(store == "olive young")

kissme_magnum <- data.frame("kissme" = c(7897, 15879, 3193, 17811, 11327, 65954, 77030,
                            13763, 9429, 10386, 12479, 50761),
               "magnum WP" = c(9408, 5607, 8479, 5220, 16698, 6742, 6863, 5163,
                               10382, 11388, 4579, 9056),
               "month" = c(rep(1:12)))

kissme_magnum_nbs <- kissme_magnum %>%
  filter(month != 3 & month != 6 & month != 9 & month != 12)

with(kissme_magnum_nbs, cor(kissme, magnum.WP, method = "spearman"))

#sumamry_magnumWP & kissme

total_nbs %>%
  filter(product == "Magnum WP RENO") %>%
  filter(store == "olive young") %>%
  group_by(month) %>%
  summarise(mean_productivity = mean(sell.out.unit), 
            mean_discount_rate = mean(discount.rate))

kissme_nbs %>%
  group_by(month) %>%
  summarise(mean_sell.out.unit = mean(sell.out.unit),
            mean_price = mean(price))


#-----------------------------------------------------------------------
#Haircare

total_haircare <- total_all %>%
  filter(category == "OAP.haircare")

total_haircare_discount <- 
  total_haircare %>%
  filter(discount.rate.group!= 0)

total_haircare_discount <- total_haircare_discount %>%
  mutate(impact = (productivity / 0.04)*100)

#discount rate
#oil
total_haircare_discount %>%
  filter(subcategory=="oil") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(factor(discount.rate.group), impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Oil")+
  scale_y_continuous(limits=(c(0,1500)), name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#TR5
total_haircare_discount %>%
  filter(subcategory=="TR5 hairpack") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(factor(discount.rate.group), impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("TR5 hairpack")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#Others
total_haircare_discount %>%
  filter(subcategory=="others") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(factor(discount.rate.group), impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Others")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#place
#oil
total_haircare_discount %>%
  filter(subcategory=="oil") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Oil")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#TR5
total_haircare_discount %>%
  filter(subcategory=="TR5 hairpack") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("TR5 hairpack")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#other
total_haircare_discount %>%
  filter(subcategory=="others") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  mutate(place=factor(place, levels=
                        c("POG", "END3", "END2", "END1"))) %>%
  filter(place != "END") %>%
  ggplot(aes(place, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Others")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#GWP
#oil
total_haircare_discount %>%
  filter(subcategory=="oil") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Oil")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#TR5
total_haircare_discount %>%
  filter(subcategory=="TR5 hairpack") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("TR5 hairpack")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#others
total_haircare_discount %>%
  filter(subcategory=="others") %>%
  mutate(store = factor(store, levels = 
                          c("olive young", "watsons", "lohbs"))) %>%
  ggplot(aes(gwp, impact, fill = store))+
  geom_boxplot(alpha=.5)+
  facet_grid(~store)+
  geom_jitter(col="gray")+
  ggtitle("Others")+
  scale_y_continuous(name="Impact(%)")+
  scale_fill_manual(values=c('olive young' = "green4", watsons = "deepskyblue2",
                             lohbs = "indianred2"))+
  stat_summary(fun.y=mean, colour="darkred", geom="point")

#-----------------------------------
#Oil drill-down
#Oil의 각 product sales가 month에 따라 어떻게 움직이는지 
#oil 전체
oil <- total_all %>%
  filter(subcategory == "oil")

sum_oil <- as.data.frame(tapply(oil$sell.out.unit,
                                    oil$month, sum))

sum_oil$month <- c(rep(1:18))
names(sum_oil) <- c("sell.out.unit", "month")

sum_oil %>%
  ggplot(aes(month, sell.out.unit))+
  geom_point()+geom_line()+geom_smooth(method=loess)+
  ggtitle("Oil[Drug sum, all]")+
  scale_x_continuous(breaks=c(rep(1:18)))

#oil nbs
oil_nbs <- total_nbs %>%
  filter(subcategory == "oil")

sum_oil_nbs <- as.data.frame(tapply(oil_nbs$sell.out.unit,
                                           oil_nbs$month, sum))
sum_oil_nbs$month <- c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17)
names(sum_oil_nbs) <- c("sell.out.unit", "month")

sum_oil_nbs %>%
  ggplot(aes(month, sell.out.unit))+
  geom_point()+geom_line()+geom_smooth(method=loess)+
  ggtitle("Oil[Drug sum, nbs]")+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))

sum_oil_nbs %>%
  group_by(month) %>%
  

#oil-----------
#oil light
oil_nbs %>%
  filter(product == "Extra Ordinary Oil Light 100ml") %>%
  filter(store == "oilve young") %>%
  ggplot(aes(month, sell.out.unit, colour = "oil light"))+
  geom_point()+geom_line()+
  #oil light price
  geom_point(data=filter(oil_nbs, 
                         product == "Extra Ordinary Oil Light 100ml"), 
             aes(x=month, y=discount.rate*1000, colour="oil light_discount rate"))+
  geom_line(data=filter(oil_nbs, 
                        product == "Extra Ordinary Oil Light 100ml"), 
            aes(x=month, y=discount.rate*1000, colour="oil light_discount rate"))+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))+
  ggtitle("oil light")

#oil pink
oil_nbs %>%
  filter(product == "EXTRAORD OIL Pink Oil 100ml") %>%
  filter(store == "oilve young") %>%
  ggplot(aes(month, sell.out.unit, colour = "pink oil"))+
  geom_point()+geom_line()+
  #oil light price
  geom_point(data=filter(oil_nbs, 
                         product == "EXTRAORD OIL Pink Oil 100ml"), 
             aes(x=month, y=discount.rate*1000, 
                 colour="pink oil_discount rate"))+
  geom_line(data=filter(oil_nbs, 
                        product == "EXTRAORD OIL Pink Oil 100ml"), 
            aes(x=month, y=discount.rate*1000, 
                colour="pink oil_discount rate"))+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))+
  ggtitle("pink oil")

#rich oil
oil_nbs %>%
  filter(product == "Extraordinary Oil Rich 100ml") %>%
  ggplot(aes(month, sell.out.unit, colour = "rich oil"))+
  geom_point()+geom_line()+
  #rich oil price
  geom_point(data=filter(oil_nbs, 
                         product == "Extraordinary Oil Rich 100ml"), 
             aes(x=month, y=discount.rate*1000, 
                 colour="rich oil_discount rate"))+
  geom_line(data=filter(oil_nbs, 
                        product == "Extraordinary Oil Rich 100ml"), 
            aes(x=month, y=discount.rate*1000, 
                colour="rich oil_discount rate"))+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))+
  facet_wrap(~store)+
  ggtitle("rich oil")

#Rich Brown oil
oil_nbs %>%
  filter(product == "Extra Ordinary Oil Rich Brown 100ml") %>%
  ggplot(aes(month, sell.out.unit, colour = "Rich Brown oil"))+
  geom_point()+geom_line()+
  #Rich Brown oil price
  geom_point(data=filter(oil_nbs, 
                         product == "Extra Ordinary Oil Rich Brown 100ml"), 
             aes(x=month, y=discount.rate*1000, 
                 colour="Rich Brown oil_discount rate"))+
  geom_line(data=filter(oil_nbs, 
                        product == "Extra Ordinary Oil Rich Brown 100ml"), 
            aes(x=month, y=discount.rate*1000, 
                colour="Rich Brown oil_discount rate"))+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8, 10:11, 13:14, 16:17))+
  facet_wrap(~store)+
  ggtitle("Rich Brown oil")
#-----------
#perpect serum
perpect_serum <- read.csv("C:/Users/fauac/Dropbox/최종분석/perpect serum.csv",
                          header = TRUE)

perpect_serum <- na.omit(perpect_serum)
perpect_serum_nbs <- perpect_serum %>%
  filter(month != 3) %>%
  filter(month != 9)

#----------------
#퍼펙트 세럼 라이트
perpect_serum_nbs %>%
  filter(product == "미쟝센 퍼펙트세럼라이트 70ML") %>%
  ggplot(aes(month, sell.out.unit, colour = "perpect serum light"))+
  geom_point()+geom_line()+
  #퍼펙트 세럼 라이트 가겨
  geom_point(data=filter(perpect_serum_nbs, 
                         product == "미쟝센 퍼펙트세럼라이트 70ML"), 
             aes(x=month, y=price, colour="perpect serum light_price"))+
  geom_line(data=filter(perpect_serum_nbs, 
                        product == "미쟝센 퍼펙트세럼라이트 70ML"), 
            aes(x=month, y=price, colour="perpect serum light_price"))+
  scale_x_continuous(breaks=c(1:2, 4:8))+
  ggtitle("perpect serum light")
    
  #퍼펙트 세럼   
perpect_serum_nbs %>%
  filter(product == "미쟝센 데미지케어 퍼펙트세럼") %>%
  ggplot(aes(month, sell.out.unit, colour = "perpect serum"))+
  geom_point()+geom_line()+
  #퍼펙트 세럼 가겨
  geom_point(data=filter(perpect_serum_nbs, 
                         product == "미쟝센 데미지케어 퍼펙트세럼"), 
             aes(x=month, y=price, colour="perpect serum_price"))+
  geom_line(data=filter(perpect_serum_nbs, 
                        product == "미쟝센 데미지케어 퍼펙트세럼"), 
            aes(x=month, y=price, colour="perpect serum_price"))+
  scale_x_continuous(breaks=c(1:2, 4:8))+
  ggtitle("perpect serum")

#퍼펙트 세럼 리치 
perpect_serum_nbs %>%
  filter(product == "미쟝센 퍼펙트세럼 리치 70ML") %>%
  ggplot(aes(month, sell.out.unit, colour = "perpect serum rich"))+
  geom_point()+geom_line()+
  #퍼펙트 세럼  리치 가겨
  geom_point(data=filter(perpect_serum_nbs, 
                         product == "미쟝센 퍼펙트세럼 리치 70ML"), 
             aes(x=month, y=price, colour="perpect serum rich_price"))+
  geom_line(data=filter(perpect_serum_nbs, 
                        product == "미쟝센 퍼펙트세럼 리치 70ML"), 
            aes(x=month, y=price, colour="perpect serum rich_price"))+
  scale_x_continuous(breaks=c(1:2, 4:8))+
  ggtitle("perpect serum rich")

#퍼펙트 세럼 기획
perpect_serum_nbs %>%
  filter(product == "미쟝센 퍼펙트세럼 기획 70ml (1+1)new") %>%
  ggplot(aes(month, sell.out.unit, colour = "perpect serum 1+1"))+
  geom_point()+geom_line()+
  #퍼펙트 세럼 기획 가겨
  geom_point(data=filter(perpect_serum_nbs, 
                         product == "미쟝센 퍼펙트세럼 기획 70ml (1+1)new"), 
             aes(x=month, y=price, colour="perpect serum 1+1_price"))+
  geom_line(data=filter(perpect_serum_nbs, 
                        product == "미쟝센 퍼펙트세럼 기획 70ml (1+1)new"), 
            aes(x=month, y=price, colour="perpect serum 1+1_price"))+
  scale_x_continuous(breaks=c(1:2, 4:8))+
  ggtitle("perpect serum 1+1")

#---------  
#미장센 전체
sum_perpect_serum_nbs <- as.data.frame(tapply(perpect_serum_nbs$sell.out.unit,
                                perpect_serum_nbs$month, sum))
sum_perpect_serum_nbs$month <- c(1:2, 4:6, 7:8)
names(sum_perpect_serum_nbs) <- c("sell.out.unit", "month")
sum_perpect_serum_nbs$price <- tapply(perpect_serum_nbs$price,
                                    perpect_serum_nbs$month, mean)
sum_perpect_serum_nbs <- sum_perpect_serum_nbs[-5,]

sum_perpect_serum_nbs %>%
  ggplot(aes(month, sell.out.unit, colour="unit sale"))+
  geom_point()+geom_line()+
  geom_point(data=sum_perpect_serum_nbs, 
             aes(x=month, y=price*10, colour="price*10"))+
  geom_line(data=sum_perpect_serum_nbs, 
             aes(x=month, y=price*10, colour="price*10"))+
  ggtitle("perfect serum[Drug sum, nbs]")+
  scale_x_continuous(breaks=c(1:2, 4:5, 7:8))

sum_perpect_serum <- as.data.frame(tapply(perpect_serum$sell.out.unit,
                                              perpect_serum$month, sum))
sum_perpect_serum$month <- c(1:9)
names(sum_perpect_serum) <- c("sell.out.unit", "month")
sum_perpect_serum$price <- tapply(perpect_serum$price,
                                      perpect_serum$month, mean)
sum_perpect_serum%>%
  ggplot(aes(month, sell.out.unit, colour="unit sale"))+
  geom_point()+geom_line()+
  geom_point(data=sum_perpect_serum, 
             aes(x=month, y=price*10, colour="price*10"))+
  geom_line(data=sum_perpect_serum, 
            aes(x=month, y=price*10, colour="price*10"))+
  ggtitle("perpect serum[Drug sum]")+
  scale_x_continuous(breaks=c(1:9))

#지수표기를 숫자로 
options(scipen = 100) 

#지표 보기
#summary unit sales by month 
summary_by_product <- function(x,y){oil %>%
    filter(store == x) %>%
    filter(product == y) %>%
    group_by(month) %>%
    summarize(mean(sell.out.unit))
}

summary_by_product("olive young", "Extraordinary Oil Rich 100ml")


oil_light <- oil %>%
  filter(product == "Extra Ordinary Oil Light 100ml")
oil_rich <- oil %>%
  filter(product == "Extraordinary Oil Rich 100ml")
oil_richbrown <- oil %>%
  filter(product == "Extra Ordinary Oil Rich Brown 100ml")
oil_pink <- oil %>%
  filter(product == "EXTRAORD OIL Pink Oil 100ml")
oil_macaron_brown <- oil %>%
  filter(product == "Brown Oil Macaron Edition KR `17")
oil_macaron_rose <- oil %>%
  filter(product == "Rose Oil Macaron Edition KR `17")

oil_real <- rbind(oil_light, oil_rich, oil_richbrown, oil_pink,
             oil_macaron_brown, oil_macaron_rose)

oil_real %>%
  ggplot(aes(month, sell.out.unit, fill = product))+
  geom_bar(stat = "identity", position = "stack")+
  ggtitle("oil")+
  scale_x_continuous(breaks=c(1:18))

oil_real %>%
  ggplot(aes(month, sell.out.unit, fill = product))+
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("oil")+
  scale_x_continuous(breaks=c(1:18))

oil_real %>%
  ggplot(aes(month, sell.out.unit, fill = store))+
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("oil")+
  scale_x_continuous(breaks=c(1:18))

perpect_serum %>%
  ggplot(aes(month, sell.out.unit, fill = product))+
  geom_bar(stat = "identity", position = "stack")+
  ggtitle("perpect_serum")+
  scale_x_continuous(breaks=c(1:9))

#------------------
#TR5

TR5 <- total_all %>%
  filter(subcategory == "TR5 hairpack")

#TR5 unit sale sum
sum_TR5 <- as.data.frame(tapply(TR5$sell.out.unit,
                                TR5$month, sum))

sum_TR5$month <- c(rep(1:18))
names(sum_TR5) <- c("sell.out.unit", "month")

#TR5 productivity sum
sum_TR5_productivity <- as.data.frame(tapply(TR5$productivity,
                                             TR5$month, sum))
sum_TR5_productivity$month <- c(rep(1:18))
names(sum_TR5_productivity) <- c("productivity", "month")

#unit sale & productivtiy
sum_TR5_total <- cbind(sum_TR5, sum_TR5_productivity)

# unit sale sum plot
sum_TR5 %>%
  ggplot(aes(month, sell.out.unit))+
  geom_point()+geom_line()+geom_smooth(method=loess)+
  ggtitle("TR5[Drug sum, all]")+
  scale_x_continuous(breaks=c(rep(1:18)))+
  
  #oliveyoung dr
  geom_line(data=filter(TR5, store == "olive young"),
            aes(x= month, y=discount.rate*100000, colour = "discount rate"))+
  geom_point(data=filter(TR5, store == "olive young"),
            aes(x= month, y=discount.rate*100000, colour = "discount rate"))

#productivity sum & unitsale plot
sum_TR5_total %>%
  ggplot(aes(month, productivity*10000, colour = "productivity"))+
  geom_point()+geom_line()+geom_smooth(method=loess)+
  ggtitle("TR5[Drug sum_productivity, all]")+
  scale_x_continuous(breaks=c(rep(1:18)))+
  
  #oliveyoung dr
  geom_line(data=sum_TR5_total,
            aes(x= month, y=sell.out.unit, colour = "unit sales"))+
  geom_point(data=sum_TR5_total,
             aes(x= month, y=sell.out.unit, colour = "unit sales"))
#TR5 nbs
TR5_nbs <- total_all %>%
  filter(month != 3) %>%
  filter(month != 9) %>%
  filter(month != 12) %>%
  filter(month != 15) %>%
  filter(month != 18) %>%
  filter(subcategory == "TR5 hairpack")
sum_TR5_productivity_nbs <- sum_TR5_productivity %>%
  filter(month != 3) %>%
  filter(month != 9) %>%
  filter(month != 12) %>%
  filter(month != 15) %>%
  filter(month != 18) 

sum_TR5_nbs <- as.data.frame(tapply(TR5_nbs$sell.out.unit,
                                    TR5_nbs$month, sum))
sum_TR5_nbs$month <- c(1:2, 4:8, 10:11, 13:14, 16:17)
names(sum_TR5_nbs) <- c("sell.out.unit", "month")

sum_TR5_nbs %>%
  ggplot(aes(month, sell.out.unit, colour = "unit sale"))+
  geom_point()+geom_line()+geom_smooth(method=loess)+
  ggtitle("TR5[Drug sum, nbs]")+
  scale_x_continuous(breaks=c(1:2, 4:8, 10:11, 13:14, 16:17))+
  geom_line(data=sum_TR5_productivity_nbs,
            aes(x= month, y=productivity*10000, 
                colour = "productivity*10000"))+
  geom_point(data=sum_TR5_productivity_nbs,
             aes(x= month, y=productivity*10000, 
                 colour = "productivity*10000"))+
  geom_smooth(data=sum_TR5_productivity_nbs,
              aes(x= month, y=productivity*10000, 
                  colour = "productivity*10000"))
  
  #oliveyoung dr
  geom_line(data=filter(TR5_nbs, store == "olive young"),
            aes(x= month, y=discount.rate*100000, 
                colour = "olive young's discount rate"))+
  geom_point(data=filter(TR5_nbs, store == "olive young"),
             aes(x= month, y=discount.rate*100000,
                 colour = "olive young's discount rate"))+
  #watsons dr
  geom_line(data=filter(TR5_nbs, store == "watsons"),
          aes(x= month, y=discount.rate*100000, 
              colour = "watsons's discount rate"))+
  geom_point(data=filter(TR5_nbs, store == "watsons"),
             aes(x= month, y=discount.rate*100000,
                 colour = "watsons's discount rate"))+

  #lohbs dr
  geom_line(data=filter(TR5_nbs, store == "lohbs"),
          aes(x= month, y=discount.rate*100000, 
              colour = "lohbs's discount rate"))+
  geom_point(data=filter(TR5_nbs, store == "lohbs"),
             aes(x= month, y=discount.rate*100000,
                 colour = "lohbs's discount rate"))

#TR5에서 store 비중

TR5 %>%
  ggplot(aes(month, sell.out.unit, fill = store))+
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("TR5 store")+
  scale_x_continuous(breaks=c(1:18))

TR5_nbs %>%
  ggplot(aes(month, sell.out.unit, fill = store))+
  geom_bar(stat = "identity", position = "stack")+
  ggtitle("TR5 store[nbs]")+
  scale_x_continuous(breaks=c(1:2, 4:8, 10:11, 13:14, 16:17))

TR5_nbs %>%
  ggplot(aes(month, sell.out.unit, fill = store))+
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("TR5 store[nbs]")+
  scale_x_continuous(breaks=c(1:2, 4:8, 10:11, 13:14, 16:17))

#TR5 product 비중
TR5_170 <- TR5 %>%
  filter(product == "TR5 Instant Miracle 170ml")
TR5_KOR <- TR5 %>%
  filter(product == "TR5 Instant Miracle 2016 KOR")

TR5_real <- rbind(TR5_170, TR5_KOR)

TR5_real %>%
  ggplot(aes(month, sell.out.unit, fill = product))+
  geom_bar(stat = "identity", position = "stack")+
  geom_smooth()+
  ggtitle("TR5 product")+
  scale_x_continuous(breaks=c(1:18))

#--------------------
#브랜드세일 
#브랜드세일 전처리
brandsale <-  read.csv("C:/Users/fauac/Dropbox/최종분석/drug 분석 원본/brandsale.csv"
                       ,header=TRUE)
#결측값 제거
brandsale <- brandsale[complete.cases(brandsale[, "sell.out.unit"]), ]

#productivity 0인 것 제거
brandsale <- brandsale %>%
  filter(productivity != 0)

#impact 
brandsale_2 <- rbind(
  brandsale %>%
    filter(category == "MNY.eye") %>%
    mutate(impact = (productivity/0.04)*100),
  brandsale %>%
    filter(category == "MNY.lip") %>%
    mutate(impact = (productivity/0.01)*100),
  brandsale %>%
    filter(category == "MNY.face") %>%
    mutate(impact = (productivity/0.04)*100),
  brandsale %>%
    filter(category == "OAP.makeup") %>%
    mutate(impact = (productivity/0.01)*100),
  brandsale %>%
    filter(category == "OAP.haircare") %>%
    mutate(impact = (productivity/0.04)*100),
  brandsale %>%
    filter(category == "OAP.haircolor") %>%
    mutate(impact = (productivity/0.08)*100),
  brandsale %>%
    filter(category == "OAP.mencare") %>%
    mutate(impact = (productivity/0.01)*100),
  brandsale %>%
    filter(category == "OAP.skincare") %>%
    mutate(impact = (productivity/0.02)*100)
)

#overwrite
brandsale <- brandsale_2
#summary - overall
brandsale %>%
  group_by(store) %>%
  summarize(median(impact), mean(impact))

#summary
brandsale_impact <- function(x){brandsale %>%
    filter(store == x) %>% 
    group_by(category) %>%
    summarize(median(impact), mean(impact))
}

brandsale_impact("olive young")
brandsale_impact("watsons")
brandsale_impact("lohbs")

#summary - productivity
brandsale_productivity <- function(x){brandsale %>%
    filter(store == x) %>% 
    group_by(category) %>%
    summarize(median(productivity), mean(productivity))
}

x <- rbind(
  brandsale_productivity("olive young"),
  brandsale_productivity("watsons"),
  brandsale_productivity("lohbs")
)

x <- rename(x, c('median(productivity)' = "productivity"))
x<- x %>%
  mutate(store = c(rep("olive young", 8), rep("watsons", 8),
                   rep("lohbs", 8)))


x <- x %>%
  mutate(productivity.not.discount =
           c(0.04, 0.04, 0.01, 0.04, 0.08, 0.01, 0.01, 0.02,
                    0.04, 0.04, 0.01, 0.02,  0.06, 0.01, 0.01, 0.01,
                    0.02, 0.03, 0.01, 0.03, 0.04, 0.01, 0.01, 0.01)) %>%
  mutate(impact = (productivity/productivity.not.discount)*100)


#summary - productivity average comparison
total_nbs %>%
  group_by(category) %>%
  summarize(median(productivity), mean())

#월별 그래프
sum_brandsale <- as.data.frame(tapply(brandsale$sell.out.unit,
                                    TR5_nbs$month, sum))
sum_TR5_nbs$month <- c(1:2, 4:8, 10:11, 13:14, 16:17)
names(sum_TR5_nbs) <- c("sell.out.unit", "month")

#30% discount average
total_all %>%
  filter(discount.rate.group == 30) %>%
  group_by(store) %>%
  summarize(median(productivity))

total_nbs %>%
  filter(discount.rate.group == 0) %>%
  group_by(store) %>%
  summarize(median(productivity))

total_nbs %>%
  filter(discount.rate.group != 0) %>%
  group_by(store) %>%
  summarize(median(productivity))
  
total_all%>%
  filter(discount.rate.group != 0) %>%
  group_by(store) %>%
  summarize(median(productivity))

#OAP make-up
#브랜드세일시
temp_function <- function(x){brandsale_makeup %>%
    filter(store == x) %>%
    group_by(subcategory) %>%
    summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")
#할인시
temp_function <- function(x){total_nbs %>%
  filter(store == x) %>%
  filter(category == "OAP.makeup") %>%
  filter(discount.rate.group == 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")

temp_function <- function(x){total_nbs %>%
  filter(store == x) %>%
  filter(category == "OAP.makeup") %>%
  filter(discount.rate.group != 0) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")

temp_function <- function(x){total_nbs %>%
  filter(store == x) %>%
  filter(category == "OAP.makeup") %>%
  filter(discount.rate.group == 30) %>%
  group_by(subcategory) %>%
  summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")

#base magique
#브랜드세일시
temp_function <- function(x){brandsale_makeup %>%
    filter(store == x) %>%
    filter(product == "Base Magique") %>%
    summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")
#할인통계
temp_function <- function(x){total_nbs %>%
    filter(store == x) %>%
    filter(product == "Base Magique") %>%
    filter(category == "OAP.makeup") %>%
    filter(discount.rate.group == 0) %>%
    summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")

temp_function <- function(x){total_nbs %>%
    filter(store == x) %>%
    filter(product == "Base Magique") %>%
    filter(category == "OAP.makeup") %>%
    filter(discount.rate.group != 0) %>%
    summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")

temp_function <- function(x){total_nbs %>%
    filter(store == x) %>%
    filter(product == "Base Magique") %>%
    filter(category == "OAP.makeup") %>%
    filter(discount.rate.group == 30) %>%
    summarize(median(productivity))
}
temp_function("olive young")
temp_function("watsons")
temp_function("lohbs")

#base magique 차지 비중 

x <- brandsale_makeup %>%
  filter(subcategory == "face")

sum_basemagique <- as.data.frame(tapply(x$impact,
                                  x$product, median))
sum_basemagique <- na.omit(sum_basemagique)
names(sum_basemagique) <- c("productivity")
sum_basemagique <- sum_basemagique %>%
  mutate(product = c("Base Magique", "LM BB Essence Ivory", 
                     "LM BB Essence Porcelain",
                     "LM Cushion SET N3 Nude", "LM Cushion SET R2 Rose", 
                     "Lucent Magique Primer", "True Match Blur Cream", 
                     "True Match Liquid FDT F1", "True Match Liquid FDT F2", 
                     "True Match Liquid FDT G1", "True Match Liquid FDT N1"))


sum_basemagique %>%
  ggplot(aes(0, productivity, fill = product))+
  geom_bar(stat = "identity", position = "fill")+
  ggtitle("OAP face")

#face 제품 median 으로 impact 구하는 게 아니라 product 별로
#비할인 대비 impact 구할 수 있는 방법 찾아서 해야함! 
