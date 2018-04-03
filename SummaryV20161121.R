####################################################################################################################
### Purpose: R code for Summary analysis
### Author: Nirupam
### Date: 21/11/2016
### Version: v01
#################################################################################################################


# creating a function to count number of NAs for specefic field

Count_NA <- function(df, field){
  temp_df <- df
  number_of_NAs <- sum(is.na(temp_df[,field]))
  return(number_of_NAs)
}

## Finding summary for each SKU per retailer at particuar General Hierarchy 1
Count_NA_SKU_Retailer_GH1 <- function(sel_df,sel_SKU,sel_Retailer_Market,sel_Generic_Hierarchy_1){  
  sel_df <- filter(sel_df, SKU==sel_SKU, Retailer_Market== sel_Retailer_Market,Generic_Hierarchy_1 == sel_Generic_Hierarchy_1)
  dfcount_NA_SKU <- NULL
  week_count <- counter <- length(sel_df$Week_ending)
  sales_total <- sum(sel_df$Sales_Total_USD,na.rm = FALSE)
  dfcount_NA_SKU<- cbind(sel_SKU,sel_Retailer_Market,week_count,sales_total)
  
  filter_input <- select(sel_df,-Week_starting,-Week_ending,-Period_ID,-Region,-Generic_Hierarchy_1,-Generic_Hierarchy_2
                         ,-Generic_Hierarchy_3, -Generic_Hierarchy_4,-Generic_Hierarchy_5,-Generic_Hierarchy_6,-Generic_Hierarchy_7
                         ,-Generic_Hierarchy_8,-Generic_Hierarchy_9,-Variable_Cost_Manufacturer_1,-Variable_Cost_Manufacturer_2
                         ,-Variable_Cost_Manufacturer_3,-Fixed_Cost_Manufacturer_1,-Fixed_Cost_Manufacturer_2,-Fixed_Cost_Manufacturer_3
                         ,-Other_Promo_Cost_Retailer,-Volume_Observed_EU,-Seasonality_Group,-Other_Retailer_Funding,-Promo_Pct_store
                         ,-B3_Hierarchy_1,-B3_Hierarchy_2,-B3_Hierarchy_3
  )
  
  for(i in names(filter_input)){
    x <- Count_NA(filter_input,i)
    dfcount_NA_SKU<- cbind(dfcount_NA_SKU,x)
  }
  dfcount_NA_SKU<- as.data.frame(dfcount_NA_SKU)
  colnames(dfcount_NA_SKU) <- c("SKU_Name","Retailer_Name","Total_week_Counts","sales_Total",names(filter_input))
  return(dfcount_NA_SKU)
}

# Creating a function to be called to find the summary 
Summary_NA_Final <- function(sel_df)
{
  
  #get unique combinations  
  df_Unique_SKUandRetailer_MarketandGH1 <- unique(sel_df[,c('SKU','Retailer_Market','Generic_Hierarchy_1')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_SKUandRetailer_MarketandGH1)
  for (i in 1:n)
  {  
    df_temp <- as.data.frame(Count_NA_SKU_Retailer_GH1(sel_df,df_Unique_SKUandRetailer_MarketandGH1$SKU[i],df_Unique_SKUandRetailer_MarketandGH1$Retailer_Market[i],df_Unique_SKUandRetailer_MarketandGH1$Generic_Hierarchy_1[i]))
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return[,-5])
}  


summary_execution(input)
{
## Finding summary of Total sales and weekly points per retailer per SKU For SKUs
## which are always on promotion and discounting those weeks which do not have sales
summary_SKU_AllwaysOnPromo_Data <- sqldf("SELECT Retailer_Market, SKU , SUM( CASE WHEN Sales_Total_USD > 0 THEN 1 ELSE 0 END )
                      as Week_counts,
                      SUM(Sales_Total_USD) as Total_sales from input 
                      GROUP BY Retailer_Market, SKU having  Week_counts = 52" )

## Storing the summary of SKUs always on promo in a CSV file
x <- getwd()
setwd("C:/MSP/Promo_Effectiveness_R_20151111/3_Output")
write.csv(x = summary_SKU_AllwaysOnPromo_Data,file = "Summary_SKU_Always_On_Promo.csv",row.names = FALSE)



# Calculating summary for SKU decribing number of NAs and total sales and number of weeks active. Data is aggregated over GH1, Retailer
summary_NA_all <- Summary_NA_Final(input)
write.csv(x = summary_NA_all,file = "Summary_File.csv",row.names = FALSE)
setwd(x)
}