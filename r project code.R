library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

df_train<-read.csv("C:\\Users\\acer\\\\Downloads\\Future-Sales-Prediction--Time-Series-Analysis-in-R-main\\Future-Sales-Prediction--Time-Series-Analysis-in-R-main\\sales_train.csv")
head(df_train)
dim(df_train)
typeof(df_train)
class(df_train)
df_items<-read.csv("C:\\Users\\acer\\Downloads\\Future-Sales-Prediction--Time-Series-Analysis-in-R-main\\Future-Sales-Prediction--Time-Series-Analysis-in-R-main\\items.csv")
df_sales=df_train %>% left_join(df_items,by=c("item_id"))
df_sales$item_name<-NULL
head(df_sales)
rm(df_train)
rm(df_items)
glimpse(df_sales)
#converting date column from character to date format
df_sales$date=dmy(df_sales$date)
df_sales$year<-year(df_sales$date)
df_sales$month<-month(df_sales$date)
df_sales$day<-day(df_sales$date)
df_sales$weekday<-weekdays(df_sales$date)

df_sales$year<-as.factor(df_sales$year)
df_sales$weekday<-as.factor(df_sales$weekday)

df_sales<-df_sales%>%group_by(year,month,shop_id,item_id)%>%mutate(item_cnt_month=sum(item_cnt_day))
head(df_sales)

glimpse(df_sales)

summary(df_sales)

cbind(colSums(is.na(df_sales)))

is.null(df_sales)

n_distinct(df_sales$shop_id)

n_distinct(df_sales$item_id)
n_distinct(df_sales$item_category_id)

most_items=df_sales%>%group_by(shop_id)%>%summarize(total_items_per_shop=n_distinct(item_id))%>%arrange(desc(total_items_per_shop))%>%head(15)%>%ungroup()
head(most_items)
#Visualization
ggplot(data=most_items,aes(x=reorder(as.factor(shop_id),total_items_per_shop),y=total_items_per_shop,fill=as.factor(shop_id)))+
  geom_bar(stat="identity",color="Black")+
  labs(title="Shops and number of items in that shop",x="Shop ID",y="Number of items",fill="Shop ID")+
  geom_text(aes(label=total_items_per_shop),size=3,angle=90,hjust=1.2)



