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
library(lubridate)
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
cnnstr <- olap_lookup[olap_lookup$Friendly_Name =="SQLBI_SellThru_Cube2", "Connection_String"]
olapCnn <- OlapConnection(cnnstr)

C<-"SELECT NON EMPTY { [Measures].[Sales Amount], [Measures].[Qty Sold] } ON COLUMNS, NON EMPTY { ([Customer].[Group Name].[Group Name].ALLMEMBERS * 
[High Volume Trans].[High Volume].[High Volume].ALLMEMBERS * [Item].[Short SKU].[Short SKU].ALLMEMBERS * [Order Details].[Invoice Number].[Invoice Number].ALLMEMBERS * 
[Date].[Date].[Date].ALLMEMBERS * [Item].[How Is it Bought].[How Is it Bought].ALLMEMBERS * [Customer].[Customer Country].[Customer Country].ALLMEMBERS * 
[Customer].[Customer Region].[Customer Region].ALLMEMBERS ) } DIMENSION PROPERTIES MEMBER_CAPTION, MEMBER_UNIQUE_NAME ON ROWS 
FROM ( SELECT ( Filter( [Date].[Fiscal Calendar].ALLMEMBERS - [Date].[Fiscal Calendar].[(All)].ALLMEMBERS, 
Instr( [Date].[Fiscal Calendar].currentmember.Properties( 'Member_Caption' ), '2020' )  = 1  ) ) ON COLUMNS
FROM ( SELECT ( { [Transaction Type].[Transaction Type].&[1], [Transaction Type].[Transaction Type].&[3] } ) ON COLUMNS 
FROM [SellThru])) WHERE ( [Transaction Type].[Transaction Type].CurrentMember )
"

Customer <- execute2D(olapCnn, C)
View(Customer)
Customer <- Customer[c(1,3,5,7,9,11,13,15,17,18)]
colnames(Customer)<-c("CustomerName","HighVolume","SKU","Invoice_Number","Order_Date","HowIsItBought","Country","Region","Sales_Amount","Qty_Sold")
Customer$Order_Date<-as.Date(Customer$Order_Date,format="%m/%d/%y")



rfmdata2020<-Customer%>%filter(year(Order_Date)==2020|(year(Order_Date)==2019 & month(Order_Date)==12))%>%select(CustomerName,SKU,Order_Date,Sales_Amount,Qty_Sold,Invoice_Number)

length(unique(rfmdata2020$CustomerName)) #36613


detach("package:plyr", unload=TRUE) 
df_rfm<-rfmdata2020%>%filter(Sales_Amount>0)%>%group_by(CustomerName)%>%
  summarize(Avg_UnitsPerTrxn=sum(Qty_Sold)/n_distinct(Invoice_Number),
            InvPerCust=n_distinct(Invoice_Number),
            Avg_DolPerTrxn=sum(Sales_Amount)/n_distinct(Invoice_Number),
            Dist_SKUPerCust=n_distinct(SKU),
            monetary=sum(Sales_Amount))



#total

# df_rfm<-Customer%>%group_by(Customer_ID)%>%
#   summarise(Date=max(Order_Date),recency=as.numeric(Sys.Date()-max(Order_Date)),
#             frequency=n_distinct(Invoice_Number),
#             monetary=sum(Sales_Amount)/n_distinct(Invoice_Number))
summary(df_rfm)
#ggplot(df_rfm,aes(Avg_UnitsPerTrxn ))+geom_histogram()
ggplot(df_rfm,aes(InvPerCust))+geom_histogram(binwidth=1)
ggplot(df_rfm,aes(Avg_DolPerTrxn))+geom_histogram(binwidth=1)
ggplot(df_rfm,aes(Dist_SKUPerCust))+geom_histogram(binwidth=300)
df_rfm%>%filter(Dist_SKUPerCust<=100)
hist(log(df_rfm$Avg_UnitsPerTrxn))
hist(log(df_rfm$InvPerCust))
hist(log(df_rfm$Avg_DolPerTrxn))
hist(log(df_rfm$Dist_SKUPerCust))
a<-exp(log(5000)/3)
b<-exp(log(5000)/3*2)
c<-exp(log(94566)/3)
d<-exp(log(94566)/3*2)
e<-exp(log(62423)/3)
f<-exp(log(62423)/3*2)

g<-exp(log(3131)/3)
h<-exp(log(3131)/3*2)

au <-c(a,b)
ic <-c(c,d)
ad <-c(e,f)
ds <-c(g,h)

#d<-df_rfm$CustomerName<-NULL

################################################################################ # Function # getIndependentScore(df,r=5,f=5,m=5) 

getScoreWithBreaks <- function(df,au,ic,ad,ds) {
  
  ## scoring the Avg_UnitsPerTrxn
  len = length(au)
  AU_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,AU_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=au[i-1]
    }
    p2=au[i]
    
    if(dim(df[p1<df$Avg_UnitsPerTrxn & df$Avg_UnitsPerTrxn<=p2,])[1]>0) df[p1<df$Avg_UnitsPerTrxn & df$Avg_UnitsPerTrxn<=p2,]$AU_Score = i
  }
  if(dim(df[au[len]<df$Avg_UnitsPerTrxn,])[1]>0) df[au[len]<df$Avg_UnitsPerTrxn,]$AU_Score = len+1
  
  ## scoring the Invoice per customer     
  len = length(ic)
  IC_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,IC_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=ic[i-1]
    }
    p2=ic[i]
    
    if(dim(df[p1<df$InvPerCust & df$InvPerCust<=p2,])[1]>0) df[p1<df$InvPerCust & df$InvPerCust<=p2,]$IC_Score = i
  }
  if(dim(df[ic[len]<df$InvPerCust,])[1]>0) df[ic[len]<df$InvPerCust,]$IC_Score = len+1
  
  ## scoring the Average dollars per transaction      
  len = length(ad)
  AD_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,AD_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=ad[i-1]
    }
    p2=ad[i]
    
    if(dim(df[p1<df$Avg_DolPerTrxn & df$Avg_DolPerTrxn<=p2,])[1]>0) df[p1<df$Avg_DolPerTrxn & df$Avg_DolPerTrxn<=p2,]$AD_Score = i
  }
  if(dim(df[ad[len]<df$Avg_DolPerTrxn,])[1]>0) df[ad[len]<df$Avg_DolPerTrxn,]$AD_Score = len+1
  
  ## scoring the Distinct SKUs per Customer      
  len = length(ds)
  DS_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,DS_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=ds[i-1]
    }
    p2=ds[i]
    
    if(dim(df[p1<df$Dist_SKUPerCust & df$Dist_SKUPerCust<=p2,])[1]>0) df[p1<df$Dist_SKUPerCust & df$Dist_SKUPerCust<=p2,]$DS_Score = i
  }
  if(dim(df[ds[len]<df$Dist_SKUPerCust,])[1]>0) df[ds[len]<df$Dist_SKUPerCust,]$DS_Score = len+1
  
  #order the dataframe by AD_Score, IC_Score, AU_Score, DS_Score desc
  df <- df[order(-df$AD_Score,-df$IC_Score,-df$AU_Score,-df$DS_Score),]
  
  # caculate the total score
  Total_Score <- c(1000*df$AD_Score + 100*df$IC_Score+10*df$AU_Score+df$DS_Score)
  
  df <- cbind(df,Total_Score)
  
  return(df)
  
} # end of function of getScoreWithBreaks



#Get the score
df20<-getScoreWithBreaks(df_rfm,au,ic,ad,ds)


length(table(df20$Total_Score))

#AD_Score + 100*df$IC_Score+10*df$AU_Score+df$DS_Score

rfm_segments <- df20  %>% group_by(CustomerName)%>%mutate(
  segment = case_when(
    (AD_Score %>% between(2, 3)) & (IC_Score ==3) &
      (AU_Score %>% between(1,2)) & (DS_Score %>% between(2, 3))  ~ "Transactional Champion",
    (Total_Score=="3131") ~ "Bid Driven Player",
    (Total_Score=="3222") ~ "Promising NICHE Player",
    (AD_Score  %>% between(2, 3)) & (IC_Score  %>% between(2,3)) &
      (AU_Score  %>% between(1, 2))  ~ "Transactional Loyalist",
    (AD_Score  %>% between(2, 3)) & (IC_Score  %>% between(1, 2)) &
      (AU_Score  %>% between(2, 3)) & (DS_Score %>% between(2,3))  ~ "Promising NICHE Player",
    
    (AD_Score  %>% between(2, 3)) & (IC_Score  %>% between(1,2)) &
      (DS_Score  %>% between(1, 2))   ~ "Project Based Player",
    (AD_Score  %>% between(2, 3)) & (IC_Score  %>% between(1, 2))  ~ "Bid Driven player",
    
    TRUE ~ "Low Value Transactional Player"
  )
) 



table(rfm_segments$segment)
unique(rfm_segments[rfm_segments$segment=="Triers",]$Total_Score)
summary(rfm_segments)
rfm_segments<-as.data.frame(rfm_segments)
rfm_segments$m<-log(rfm_segments$monetary)

cust<-unique(rfm_segments$customer_id)

customerflow<-Customer%>%filter(Customer_ID %in% cust)


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
setwd("C:/Users/gliu/OneDrive - StarTech.com/H Drive Migration")
write.csv(as.data.frame(rfm_segments),"SellThru2020.csv")

write.csv(as.data.frame(rfm_segments),"CDWSegment17.csv")
write.csv(as.data.frame(segments),"segments dimension.csv")

D16<-rfm_segments



remotes::install_github("guangchuangyu/nCov2019")
library(nCov2019)
x<-get_nCove
