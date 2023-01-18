#1.1
library(dplyr)
library("tidyverse")
brooklyn_2016<-data.frame(read.csv("~/Downloads/2016_brooklyn.csv"))
brooklyn_2017<-data.frame(read.csv("~/Downloads/2017_brooklyn.csv"))
brooklyn_2018<-data.frame(read.csv("~/Downloads/2018_brooklyn.csv"))
brooklyn_2019<-data.frame(read.csv("~/Downloads/2019_brooklyn.csv"))
brooklyn_2020<-data.frame(read.csv("~/Downloads/2020_brooklyn.csv"))

names(brooklyn_2016)<-c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
                        'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft',
                        'grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
names(brooklyn_2017)<-c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
                        'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft',
                        'grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
names(brooklyn_2018)<-c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
                        'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft',
                        'grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
names(brooklyn_2019)<-c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
                        'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft',
                        'grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
names(brooklyn_2020)<-c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
                        'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft',
                        'grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

#1.2

summary(brooklyn_2016)
summary(brooklyn_2017)
summary(brooklyn_2018)
summary(brooklyn_2019)
summary(brooklyn_2020)

brooklyn_2016$comunits <- lapply(brooklyn_2016$comunits, function(col) ( gsub("-", "", col) ) )
brooklyn_2016$totunits <- lapply(brooklyn_2016$totunits, function(col) ( gsub("-", "", col) ) )
brooklyn_2016$price <- lapply(brooklyn_2016$price, function(col) ( gsub("-", "", col) ) )
brooklyn_2018$address<-trimws(brooklyn_2018$address)
brooklyn_2018$bldclasssale<-trimws(brooklyn_2018$bldclasssale)
brooklyn_2018$price<-gsub("\\$","",brooklyn_2018$price)

b_2016<-tail(brooklyn_2016, -4)
b_2017<-tail(brooklyn_2017, -4)
b_2018<-tail(brooklyn_2018, -4)
b_2019<-tail(brooklyn_2019, -4)
b_2020<-tail(brooklyn_2020, -6)


b_2016 <- b_2016 %>% mutate_at(c('borough', 'block', 'resunits', 'comunits', 'totunits'), as.numeric)
b_2016$landsqft<-gsub("\\,","",b_2016$landsqft)
b_2016$grosssqft<-gsub("\\,","",b_2016$grosssqft)
b_2016$price<-gsub("\\,","",b_2016$price)

b_2016 <- b_2016 %>% mutate_at(c('landsqft', 'grosssqft', 'yrbuilt', 'price'), as.numeric)

b_2017$landsqft<-gsub("\\,","",b_2017$landsqft)
b_2017$grosssqft<-gsub("\\,","",b_2017$grosssqft)
b_2017$price<-gsub("\\,","",b_2017$price)

b_2018$landsqft<-gsub("\\,","",b_2018$landsqft)
b_2018$grosssqft<-gsub("\\,","",b_2018$grosssqft)
b_2018$price<-gsub("\\,","",b_2018$price)

b_2019$landsqft<-gsub("\\,","",b_2019$landsqft)
b_2019$grosssqft<-gsub("\\,","",b_2019$grosssqft)
b_2019$price<-gsub("\\,","",b_2019$price)

b_2020$landsqft<-gsub("\\,","",b_2020$landsqft)
b_2020$grosssqft<-gsub("\\,","",b_2020$grosssqft)
b_2020$price<-gsub("\\,","",b_2020$price)

b_2017 <- b_2017 %>% mutate_at(c('borough', 'block', 'resunits', 'comunits', 'totunits'), as.numeric)
b_2018 <- b_2018 %>% mutate_at(c('borough', 'block', 'resunits', 'comunits', 'totunits'), as.numeric)
b_2019 <- b_2019 %>% mutate_at(c('borough', 'block', 'resunits', 'comunits', 'totunits'), as.numeric)
b_2020 <- b_2020 %>% mutate_at(c('borough', 'block', 'resunits', 'comunits', 'totunits'), as.numeric)

b_2017 <- b_2017 %>% mutate_at(c('landsqft', 'grosssqft', 'yrbuilt', 'price'), as.numeric)
b_2018 <- b_2018 %>% mutate_at(c('landsqft', 'grosssqft', 'yrbuilt', 'price'), as.numeric)
b_2019 <- b_2019 %>% mutate_at(c('landsqft', 'grosssqft', 'yrbuilt', 'price'), as.numeric)
b_2020 <- b_2020 %>% mutate_at(c('landsqft', 'grosssqft', 'yrbuilt', 'price'), as.numeric)

summary(b_2016)
summary(b_2017)
summary(b_2018)
summary(b_2019)
summary(b_2020)

brooklyn_all <- rbind(b_2016, b_2017, b_2018, b_2019, b_2020)

#1.3
brooklyn_filter <-dplyr::filter(brooklyn_all, grepl('A|R', bldclasssale) & totunits==1 & resunits ==1 
                                & grosssqft > 0 & !is.na(price))
#2.1
brooklyn_filter$date<-as.Date(brooklyn_filter$date, "%m/%d/%y")

summary(brooklyn_filter)
#neighborhood, taxclasscurr, block, lot, zip  - categorical

brooklyn_filter$neighborhood<-as.factor(brooklyn_filter$neighborhood)
brooklyn_filter$zip<-as.factor(brooklyn_filter$zip)
brooklyn_filter$taxclasscurr<-as.factor(brooklyn_filter$taxclasscurr)
brooklyn_filter$taxclasssale<-as.factor(brooklyn_filter$taxclasssale)
brooklyn_filter$bldclasscat<-as.factor(brooklyn_filter$bldclasscat)
brooklyn_filter$lot<-as.factor(brooklyn_filter$lot)

brooklyn_filter$bldclasssale<-trimws(brooklyn_filter$bldclasssale)
brooklyn_filter$bldclasssale<-as.factor(brooklyn_filter$bldclasssale)


brooklyn_filter$neighborhood<-trimws(brooklyn_filter$neighborhood)

brooklyn_filter<-brooklyn_filter %>% distinct()
#which( colnames(brooklyn_filter)=="yrbuilt" )

#brooklyn_filter[, 17][brooklyn_filter[, 17] == 0] <- NA


library("lubridate")

brooklyn_filter$quarter<- brooklyn_filter %>% 
  mutate(date = parse_date_time(date, "%y-%m-%d")) %>% 
  mutate(quarter = quarter(date, with_year = TRUE))

brooklyn_filter$quarter<- quarter(ymd(brooklyn_filter$date), with_year=TRUE, fiscal_start=1)
head(brooklyn_filter)
Q3til2019 <- c(2016.3,2017.3,2018.3, 2019.3)
brooklyn_filter$Q3until2019 <- brooklyn_filter$quarter%in%Q3til2019

Q4til2019 <- c(2016.4,2017.4,2018.4, 2019.4)
brooklyn_filter$Q4until2019 <- brooklyn_filter$quarter%in%Q4til2019

Q3of20 <- c(2020.3)
brooklyn_filter$Q32020 <- brooklyn_filter$quarter%in%Q3of20

Q4of20 <- c(2020.4)
brooklyn_filter$Q42020 <- brooklyn_filter$quarter%in%Q4of20



#2.2
brooklyn_new <-dplyr::filter(brooklyn_filter, price>100000 & price<5500000)
length(brooklyn_new2$borough)
model1 <- lm((price)~ grosssqft+zip+Q3until2019+Q4until2019+Q32020+Q42020+taxclasssale,data=brooklyn_new)

model1 <- lm(log(price)~ grosssqft+zip+Q3until2019+Q4until2019+Q32020+Q42020,data=brooklyn_new)
summary(model1)

model1 <- lm((price)~ grosssqft+zip+yrbuilt+taxclasssale,data=brooklyn_new)
model1 <- lm((price)~ sqrt(grosssqft)+zip+yrbuilt+taxclasssale,data=brooklyn_new2)

model1 <- lm(sqrt(price)~ grosssqft+zip+yrbuilt+taxclasssale,data=brooklyn_new)

plot(model1$fitted.values,model1$residuals)
ks.test(model1$residuals/summary(model1)$sigma,pnorm)

plot(brooklyn_new$landsqft, brooklyn_new$price)
plot(brooklyn_new$grosssqft, brooklyn_new$date)
plot(brooklyn_new$yrbuilt, brooklyn_new$price)
plot(brooklyn_new$quarters, brooklyn_new$price)

plot(brooklyn_new$zip, brooklyn_new$price)
plot(brooklyn_new$neighborhood, brooklyn_new$price)
plot(brooklyn_new$bldclasssale, brooklyn_new$price)

plot(brooklyn_new2$yrbuilt, brooklyn_new2$price)

#dropping mansions
finaldf <-dplyr::filter(brooklyn_new, bldclasssale!="A7")

model1 <- lm(sqrt(price)~ grosssqft+zip+sqrt(landsqft)+sqrt(yrbuilt),data=brooklyn_new2)

model2 <- lm(sqrt(price)~ grosssqft+zip+sqrt(landsqft),data=brooklyn_new2)
summary(model1)
summary(model2)
sqrt(mean((model1$fitted.values^2-brooklyn_new2$price)^2))
sqrt(mean((model2$fitted.values^2-brooklyn_new2$price)^2))

model1 <- lm(sqrt(price)~ grosssqft+zip+sqrt(landsqft)+sqrt(yrbuilt),data=brooklyn_new)
sqrt(mean((model1$fitted.values^2-brooklyn_new$price)^2))

final_model <- lm(sqrt(price)~ grosssqft+zip+sqrt(landsqft)+sqrt(yrbuilt),data=finaldf)
summary(final_model)
sqrt(mean((final_model$fitted.values^2-finaldf$price)^2))

saveRDS(list(model=final_model, data=finaldf), file='monicazhang.RDS')

part2_model <- lm(sqrt(price)~ grosssqft+zip+sqrt(yrbuilt)+
                    quarter, data=finaldf)
part2_model <- lm(sqrt(price)~ grosssqft+zip+sqrt(yrbuilt)+
                    quarter, data=finaldf)
part2_model <- lm((price)~ grosssqft+zip+sqrt(yrbuilt)+
                    Q3until2019+Q4until2019+Q32020+Q42020, data=finaldf)
View(finaldf)
finaldf$quarter<-as.factor(finaldf$quarter)
summary(part2_model)

sqrt(mean((part2_model$fitted.values^2-finaldf$price)^2))

part2_model <- lm(sqrt(price)~ grosssqft+zip+
                    +Q3until2019+Q4until2019+Q32020+Q42020, data=finaldf)



part2_model <- lm(sqrt(price)~ grosssqft+zip+sqrt(yrbuilt)+quarter, data=finaldf)



plot(brooklyn_new$quarter,brooklyn_new$grosssqft)
part2_model <- lm((price)~ grosssqft*quarter+zip, data=finaldf)

sqrt(mean((part2_model$fitted.values^2-finaldf$price)^2))

part2_model <- lm(sqrt(price)~ grosssqft+zip+quarter, data=finaldf)
plot_model(part2_model, type = "pred", terms = c("quarter[2019.3, 2019.4, 2020.3,2020.4]", 
                                                 "grossqftdecile")
)
plot_model(part2_model, type = "pred", terms = c("quarter[2020.3,2020.4]", 
                                                 "grossqftdecile")
)
axis.labels = NULL
View(part2_model)


summary(part2_model)
confint(part2_model)

ggplot(part2_model, aes(x=quarter, y=price, fill=cond)) +
  guides(fill=none) +
  coord_flip() +
  stat_summary(fun.data = confint(part2_model), geom="boxplot")

part2_model <- lm(sqrt(price)~ grosssqft+zip+Q3until2019+Q4until2019+Q32020+Q42020, data=finaldf)

finaldf$Q42020<-as.factor(finaldf$Q42020)

part2_model <- lm((price)~ grosssqft+zip+Q3until2019+Q4until2019+Q32020+Q42020, data=finaldf)
summary(part2_model)
confint(part2_model)


ggplot(data = finaldf, aes(x=quarter, y = price)) + 
  geom_point() +
  labs(x = 'Year Quarter', y = 'Purchase Price') +
  ggtitle(label = 'Scatter Plot for Median Household Income \n and Baccalaureate Attainment')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method=lm)

sqrt(mean((model1$fitted.values^2-brooklyn_new2$price)^2))

ggplot(data = part2_model, aes(x = (part2_model$fitted.values)^2, y = part2_model$residuals)) + 
  geom_point() + 
  geom_hline(yintercept=0, linetype = 'dashed', color = 'red') +
  labs(x = 'fitted value', y = 'residuals') +
  ggtitle(label = 'Residuals Plot') +
  theme(plot.title = element_text(hjust = 0.5))

bptest(part2_model)
shapiro.test(part2_model$residuals)

qqnorm(part2_model$residuals)
qqline(part2_model$residuals, col = "steelblue", lwd = 2)
dwtest(part2_model)

# Conclusion: not normally distirbuted

library(ggplot2)
cidf<-data.frame(confint(part2_model, level = 0.9))

brooklyn_filter$grossqft_rank <- rank(brooklyn_filter$grosssqft, ties.method = "first")
brooklyn_filter$grossqftdecile <- cut(brooklyn_filter$grossqft_rank, quantile(brooklyn_filter$grossqft_rank, probs=0:10/10), include.lowest=TRUE, labels=FALSE)  
brooklyn_filter$grossqftdecile <- as.factor(brooklyn_filter$grossqftdecile)

View(cidf)
ggplot(cidf, aes(x = beta, y = beta, ymin = x2.5, ymax = x97.5)) +
  geom_pointrange()
brooklyn_new$quarter<-as.factor(brooklyn_new$quarter)

m1 <- lm((price)~ grossqftdecile*quarter+zip, data=brooklyn_new)
summary(m1)
confint(m1)

ggplot(m1, aes(x=quarter, y=price, fill=grossqftdecile)) + geom_boxplot()

m2 <- lm(sqrt(price)~ grossqftdecile*Q3until2019+grossqftdecile*Q4until2019+grossqftdecile*Q32020+
           grossqftdecile*Q42020, data=brooklyn_new)
summary(m2)
confint(m2)

install.packages("sjPlot")
library(sjPlot)
library(sjmisc)
library(ggplot2)

plot_model(m1, type = "pred", terms = c("quarter", "grossqftdecile"))


ggplot(data = brooklyn_new, aes(x=quarter*grossqftdecile, y = price)) + 
  geom_point() +
  labs(x = 'Year Quarter', y = 'Purchase Price') +
  ggtitle(label = 'Scatter Plot for Median Household Income \n and Baccalaureate Attainment')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method=lm)

confint(m1)
summary(m1)
ggplot(data=m3, aes(x=quarter,y=yvar, color=grossqftdecile)) + geom_line()


m3 <- lm(sqrt(price)~ grossqftdecile*quarter+zip, data=brooklyn_new)
summary(m3)
plot_model(m3, type = "pred", terms = c("quarter", "grossqftdecile"))
confint(m3)
plot_model(m3, type = "int", mdrt.values = "meansd")


