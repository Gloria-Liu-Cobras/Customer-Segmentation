rm(list=ls())
install.packages('magritter', repos = 'https://cloud.r-project.org')

library(olapR) 
library(rfm)
library(knitr)
library(kableExtra)
library(magrittr)
#detach("plyr", unload=TRUE)

library(dplyr)
library(ggplot2)
library(DT)
library(grDevices)
library(RColorBrewer)

options(knitr.table.format="html")
options(tibble.width=Inf)
detach("package:plyr", unload=TRUE) 
library(lazyeval)
library(devtools)
library(easyGgplot2)
library(rfm)
library(knitr)
library(kableExtra)
library(magrittr)
library(dplyr)
library(ggplot2)
library(DT)
library(grDevices)
library(RColorBrewer)
library(rmarkdown)
library(ggridges)
options(knitr.table.format = "html")
options(tibble.width = Inf)
if(devtools::find_rtools()) 
  Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip")) 
devtools::install_github("rsquaredacademy/rfm")
#rfm_launch_app()
#### OLAP DATA ####
setwd("I:/DataAnalytics/_1Source")
olap_lookup <- read.csv("Olap Connections.csv", stringsAsFactors = FALSE )
cnnstr <- olap_lookup[olap_lookup$Friendly_Name =="PROD_DailyDMR", "Connection_String"]
olapCnn <- OlapConnection(cnnstr)
#startdate and enddate specified in the range 2016-12-01
#C<-' SELECT NON EMPTY { [Measures].[Qty Sold], [Measures].[Sales Amount] } ON COLUMNS, NON EMPTY { ([ShipTo Customers].[Dim DMR Customer Key].[Dim DMR Customer Key].ALLMEMBERS * [ShipTo Customers].[ShipTo Group Name].[ShipTo Group Name].ALLMEMBERS * [Invoices].[Invoice Number].[Invoice Number].ALLMEMBERS * [Item].[Short SKU].[Short SKU].ALLMEMBERS * [ShipTo Customers].[ShipTo City].[ShipTo City].ALLMEMBERS * [Date].[Date].[Date].ALLMEMBERS * [ShipTo Customers].[ShipTo State].[ShipTo State].ALLMEMBERS ) } DIMENSION PROPERTIES MEMBER_CAPTION, MEMBER_UNIQUE_NAME ON ROWS FROM ( SELECT ( { [Date].[Fiscal Year].&[2017], [Date].[Fiscal Year].&[2018], [Date].[Fiscal Year].&[2019] } ) ON COLUMNS FROM ( SELECT ( { [DMR Partner Warehouse].[Partner Warehouse Hierarchy].[DMR Partner].&[CDW (USA)] } ) ON COLUMNS FROM [DMR POS-INV])) WHERE ( [DMR Partner Warehouse].[Partner Warehouse Hierarchy].[DMR Partner].&[CDW (USA)], [Date].[Fiscal Year].CurrentMember ) CELL PROPERTIES VALUE, BACK_COLOR, FORE_COLOR, FORMATTED_VALUE, FORMAT_STRING, FONT_NAME, FONT_SIZE, FONT_FLAGS'
C<-" SELECT NON EMPTY { [Measures].[Qty Sold], [Measures].[Sales Amount] } ON COLUMNS, NON EMPTY { ([BillTo Customers].[BillTo Company].[BillTo Company].ALLMEMBERS * [BillTo Customers].[BillTo Group Name].[BillTo Group Name].ALLMEMBERS * [BillTo Customers].[BillTo Country].[BillTo Country].ALLMEMBERS * 
[BillTo Customers].[BillTo City].[BillTo City].ALLMEMBERS * [Invoices].[Invoice Number].[Invoice Number].ALLMEMBERS * [Item].[Short SKU].[Short SKU].ALLMEMBERS * [Date].[Date].[Date].ALLMEMBERS ) } DIMENSION PROPERTIES MEMBER_CAPTION, MEMBER_UNIQUE_NAME ON ROWS FROM ( SELECT ( {
[Date].[Fiscal Year].&[2016], [Date].[Fiscal Year].&[2017], [Date].[Fiscal Year].&[2018] } ) ON COLUMNS 
FROM ( SELECT ( { [DMR Partner Warehouse].[Partner Warehouse Hierarchy].[DMR Partner].&[CDW (USA)] } ) ON COLUMNS 
FROM [DMR POS-INV])) WHERE ( [DMR Partner Warehouse].[Partner Warehouse Hierarchy].[DMR Partner].&[CDW (USA)], [Date].[Fiscal Year].CurrentMember )"
Customer <- execute2D(olapCnn, C)
Customer <- Customer[c(1,3,5,7,9,11,13,15,16)]
colnames(Customer)<-c("Customer_Key","Customer_ID","Country","City","Invoice_Number","SKU","Order_Date","Qty_Sold","Sales_Amount")
Customer$Order_Date<-as.Date(Customer$Order_Date,format="%m/%d/%y")

Customer$Customer_ID<-ifelse(Customer$Customer_ID=="No Group Name Defined",Customer$Customer_Key,Customer$Customer_ID)
rfmdata<-Customer%>%select(Customer_ID,Order_Date,Sales_Amount)
rownames(rfmdata)<-rfmdata$Customer_ID
length(unique(Customer$Customer_ID)) ##13479

detach("package:plyr", unload=TRUE) 
lastdate<-max(Customer$Order_Date)
df_rfm<-Customer%>%filter(Sales_Amount>0)%>%group_by(Customer_ID)%>%
  summarize(Date=max(Order_Date),recency=as.numeric(lastdate-max(Order_Date)),
            frequency=n_distinct(Invoice_Number),
            monetary=sum(Sales_Amount))


#total

# df_rfm<-Customer%>%group_by(Customer_ID)%>%
#   summarise(Date=max(Order_Date),recency=as.numeric(Sys.Date()-max(Order_Date)),
#             frequency=n_distinct(Invoice_Number),
#             monetary=sum(Sales_Amount)/n_distinct(Invoice_Number))
summary(df_rfm)
ggplot(df_rfm,aes(recency))+geom_histogram(binwidth = 30)
df_rfm%>%filter(frequency<30)%>%ggplot(aes(frequency))+geom_histogram(binwidth=1)
df_rfm%>%filter(monetary<3000)%>%filter(monetary>0)%>%ggplot(aes(monetary))+geom_histogram(binwidth = 20)

hist(df_rfm$recency)
table(df_rfm$recency)



################################################################################ # Function # getIndependentScore(df,r=5,f=5,m=5) 
# 
# Description 
# Scoring the Recency, Frequency, and Monetary in r, f, and m in aliquots independently 
# 
# Arguments 
# df - A data frame returned by the function of getDataFrame 
# r - The highest point of Recency 
# f - The highest point of Frequency 
# m - The highest point of Monetary 
# 
# Return Value 
# Returns a new data frame with four new columns of "R_Score","F_Score","M_Score", and "Total_Score".
################################################################################# 
getIndependentScore <- function(df,r=5,f=5,m=5) 
  { if (r<=0 || f<=0 || m<=0) return  #order and the score 
  df <- df[order(df$recency,-df$frequency,-df$monetary),] 
  R_Score <- scoring(df,"recency",r) 
  df <- cbind(df, R_Score) 
  df <- df[order(-df$frequency,df$recency,-df$monetary),]
  F_Score <- scoring(df,"frequency",f) 
  df <- cbind(df, F_Score) 
  df <- df[order(-df$monetary,df$recency,-df$frequency),] 
  M_Score <- scoring(df,"monetary",m) 
  df <- cbind(df, M_Score) 
  #order the dataframe by R_Score, F_Score, and M_Score desc 
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  # caculate the total score 
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  df <- cbind(df,Total_Score) 
  return (df) } 
# end of function getIndependentScore
a<-getIndependentScore(df_rfm,r=5,f=5,m=5)
summary(a)

################################################################################
# Function
#             getScoreWithBreaks(df,r,f,m)
#
# Description
#             Scoring the Recency, Frequency, and Monetary in r, f, and m which are vector object containing a series of breaks
#
# Arguments
#             df - A data frame returned by the function of getDataFrame
#             r -  A vector of Recency breaks
#             f -  A vector of Frequency breaks
#             m -  A vector of Monetary breaks
#
# Return Value
#             Returns a new data frame with four new columns of "R_Score","F_Score","M_Score", and "Total_Score".
#
#################################################################################

getScoreWithBreaks <- function(df,r,f,m) {
  
  ## scoring the Recency
  len = length(r)
  R_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,R_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=r[i-1]
    }
    p2=r[i]
    
    if(dim(df[p1<df$recency & df$recency<=p2,])[1]>0) df[p1<df$recency & df$recency<=p2,]$R_Score = len - i+ 2
  }
  
  ## scoring the Frequency     
  len = length(f)
  F_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,F_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=f[i-1]
    }
    p2=f[i]
    
    if(dim(df[p1<df$frequency & df$frequency<=p2,])[1]>0) df[p1<df$frequency & df$frequency<=p2,]$F_Score = i
  }
  if(dim(df[f[len]<df$frequency,])[1]>0) df[f[len]<df$frequency,]$F_Score = len+1
  
  ## scoring the Monetary      
  len = length(m)
  M_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,M_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=m[i-1]
    }
    p2=m[i]
    
    if(dim(df[p1<df$monetary & df$monetary<=p2,])[1]>0) df[p1<df$monetary & df$monetary<=p2,]$M_Score = i
  }
  if(dim(df[m[len]<df$monetary,])[1]>0) df[m[len]<df$monetary,]$M_Score = len+1
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return(df)
  
} # end of function of getScoreWithBreaks


# set the Recency ranges as 0-30 days, 30-90days, 90-365 days, 365-730days, and more than 730 days.
r <-c(30,90,365,730)
# set the Frequency ranges as 0 - 1times, 2-6 times,6-12 times, 12-20 times, and more than 20 times.
f <-c(1,3,6,12)
# set the Monetary ranges as 0-40 dollars, 40-180 dollars, 180-800,800-3600 and so on.
m <-c(70,180,800,3600) 
#Get the score
df2<-getScoreWithBreaks(df_rfm,r,f,m)



length(table(df2$Total_Score))

#Get the histogram
ggplot(df2,aes(R_Score,F_Score))+geom_col()+facet_grid(R_Score~M_Score)+facet_grid(F_Score~M_Score)# setwd("H:/ROBBC/olap")
# 
df2%>%ggplot(aes(x=recency,y=monetary,size=monetary,color=monetary))+geom_point(position="jitter")+theme_bw()

# #Balloon Plots
# library(gplots)
# rfmbp <- read.csv("RFM_BP.csv")
# n <- sum(rfmbp$Count)
# n
# rev <- sum(rfmbp$Monetary * rfmbp$Frequency)
# sum(rev)
# 
# #Revenue sum
# balloonplot(rfmbp$R_Score, rfmbp$F_Score, rfmbp$T24, xlab="Recency", ylab="Frequency",
#             zlab="Revenue", main="US VAR (-Misc) Set - Revenue", label.size=1,
#             scale.range="relative", show.margins=FALSE, sorted=FALSE)
# #Revenue mean
# balloonplot(rfmbp$R_Score, rfmbp$F_Score, rfmbp$T12, xlab="Recency", ylab="Frequency", 
#             zlab="Revenue", main="US VAR (-Misc) Set - Median Revenue", 
#             label.size=1, scale.range="relative", show.margins=FALSE, sorted=FALSE, fun=median)
# #Cust Count
# 


rfm2<-df2%>%mutate(one_time = ifelse(frequency==1,"One_timer","More_than_once"),avg_purchase=monetary/frequency)
df2%>%arrange(desc(monetary/frequency))
rfm2%>%ggplot(aes(x=one_time,y=monetary))+geom_violin()


segment <- c(
  "Champions", "Loyal Customers", "Potential Loyalist",
  "New Customers", "Promising", "Need Attention",
  "About To Sleep", "At Risk", "Can't Lose Them", "Hibernating",
  "Lost"
)
description <- c(
  "Bought recently, buy often and spend the most",
  "Spend good money . Responsive to promotions",
  "Recent customers, spent good amount, bought more than once",
  "Bought more recently, but not often",
  "Recent shoppers, but haven't spent much",
  "Above average recency, frequency & monetary values",
  "Below average recency, frequency & monetary values",
  "Spent big money, purchased often but long time ago",
  "Made big purchases and often, but long time ago",
  "Low spenders, low frequency, purchased long time ago",
  "Lowest recency, frequency & monetary scores"
)
recency <- c("4 - 5", "2 - 5", "3 - 5", "4 - 5", "3 - 4", "2 - 3", "2 - 3", "<= 2", "<= 1", "1 - 2", "<= 2")
frequency <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")
monetary <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")
segments <- tibble(
  Segment = segment, Description = description,
  R = recency, `F` = frequency, M = monetary
)
segments %>%
  kable() %>%
  kable_styling(full_width = TRUE, font_size = 12)
colnames(df2)[1]<-"customer_id"

rfm_segments <- df2  %>% group_by(customer_id)%>%mutate(
    segment = case_when(
      (R_Score %>% between(4, 5)) & (F_Score %>% between(4, 5)) &
        (M_Score %>% between(4, 5))  ~ "Champions",
      (R_Score <= 1) & (F_Score <= 2) &
        (M_Score <= 2) ~ "Lost",
      (R_Score %>% between(1, 2)) & (F_Score <= 2) &
        (M_Score <= 2) ~ "About To Sleep",
      (R_Score <= 2) & (F_Score %>% between(4, 5)) &
        (M_Score %>% between(4, 5)) ~ "Cant Lose Them",
      (R_Score %>% between(3, 5)) & (F_Score %>% between(3, 5)) &
        (M_Score %>% between(3, 5)) ~ "Loyal Customers",
      (R_Score %>% between(3, 5)) & (F_Score %>% between(1, 3)) &
        (M_Score %>% between(1, 3)) ~ "Potential Loyalist",
      (R_Score %>% between(3, 5)) & (F_Score <= 2)|(F_Score == 5) &
        (M_Score <= 2) ~ "New Customers",
      (R_Score %>% between(3, 4)) & (F_Score <= 2) &
        (M_Score <= 1) ~ "Promising",
      (R_Score %>% between(2, 3)) & (F_Score %>% between(2, 3)) &
        (M_Score %>% between(2, 3)) ~ "Needs Attention",
      (R_Score <= 2) & (F_Score %>% between(2, 5)) &
        (M_Score %>% between(2, 5)) ~ "At Risk",
      (R_Score %>% between(1, 2)) & (F_Score %>% between(1, 2)) &
        (M_Score %>% between(1, 2)) ~ "Hibernating",
      TRUE ~ "Others"
    )
  ) 

table(rfm_segments$segment)

summary(rfm_segments)
rfm_segments<-as.data.frame(rfm_segments)
rfm_segments$m<-log(rfm_segments$monetary)

cust<-unique(rfm_segments$customer_id)

customerflow<-Customer2%>%filter(Customer_ID %in% cust)


summary(customerflow)
colnames(customerflow)[5]<-"customer_id"

seg<-rfm_segments%>%select(customer_id,segment)
customerflow<-merge(customerflow,seg,all.x=TRUE)
colnames(customerflow)[3]<-"FY"

ggplot(customerflow, aes(x = Ordered_Date, y = segment,fill = segment)) +  
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")



# use datatable
rfm_segments %>%
  datatable(
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE),
    colnames = c(
      "Customer", "Segment", "RFM",
      "Orders", "Recency", "Total Spend"
    )
  )
setwd("H:/Customer Analytics")

write.csv(as.data.frame(rfm_segments),"CDWSegment17.csv")
write.csv(as.data.frame(segments),"segments dimension.csv")

D16<-rfm_segments

