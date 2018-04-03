#################################################################################################################
### Purpose: R code for B4
### Author: Nirupam
### Date: 2016/11/22
### Version: v12
#################################################################################################################

# clear environment
rm(list=ls())

#################################################################################################################
###                                          Execetuion                                                       ###
#################################################################################################################

### ====================================================================================================== ###
### file locations and date format
system.time({
  # set work directory
  
  Working_Directory <- "C:/MSP/Promo_Effectiveness_R_20151111/"   
  setwd(paste0(Working_Directory,"/2_RScript/"))   
  
  # load saved workspace
  # load(".RData")
  
  # loading functions 
  source("005_Promo_Effectiveness_GeneralFunctions_Source_v09_QNV_20161111.R")
  source("010_Promo_Effectiveness_B2Functions_Source_v05_QNV_20161109.R")
  source("011_Promo_Effectiveness_B3Functions_Source_v11_QNV_20161111.R")
  #source("mod2.R")
  #source("SummaryV20161121..R")
  # input file
  csv_RawDataLocation <- paste0(Working_Directory,"/1_Input/WWOM - Input - Main Dataset.csv")
  
  # complementary relationsships table
  csv_ComplementaryDataLocation <- paste0(Working_Directory,"/1_Input/WWOM - Input - Complementarity Relationships.csv")
  
  # Table with field names
  csv_OutputList <- paste0(Working_Directory,"/1_Input/OutputList.csv")
  # output folder
  Output_Directory <- paste0(Working_Directory,"/3_Output/")
  
  # format string of date in input file
  inp_DateFormatString <-  "%m/%d/%Y" 
  
})

### ====================================================================================================== ###
#### read input and transform dates
system.time({
  raw_input <- read.csv(csv_RawDataLocation)
  raw_input$Week_ending <-as.Date(raw_input$Week_ending,inp_DateFormatString)
  raw_input$Week_starting <-as.Date(raw_input$Week_starting,inp_DateFormatString)
  df_ComplementaryRelationships <- read.csv(csv_ComplementaryDataLocation)
  df_OutputList <- read.csv(csv_OutputList)
})

### ====================================================================================================== ###
### Calculation Steps and output
### Run_Promo_Effectiveness(InputData, ComplRelationsData, method)
### # Input arguments: InputData is the base data frame on SKU+Retailer+week level
###                    ComplRelationsData is the 3 column data frame for complementarity relations
###                    method is one of the implemented approaches "B1", "B2", "B3" ("B4" to be developed)
###                    OutputFields is a ordered list that shal be output 
###                    AdditionalFieds is a ordered list of additional fields that shall be output (new fields added to the basic input and shall be passed through algo)
### Note for B3:  you can set the Percentile thresholds as fourth (Pantry Loading) and fifth (Cannibalization)
###               argument. If there are noparticula percentiles given, the algo will use 100%.
###               See method description for details about these parameters. 
### Example calls:   > Run_Promo_Effectiveness(raw_input,df_ComplementaryRelationships,OutputFields,AdditionalFields,"B1")  
###                  > Run_Promo_Effectiveness(raw_input,df_ComplementaryRelationships,OutputFields,,AdditionalFields,"B2")  
###                  > Run_Promo_Effectiveness(raw_input,df_ComplementaryRelationships,OutputFields,AdditionalFields,"B3",0.9,0.9)  

# create an array of additional fields in raw data (unknown)
AdditionalFields <- setdiff(colnames(raw_input),df_OutputList$R_ColumnName)
# create an array of known fields that shal be output
OutputFields <- filter(df_OutputList, Output_Type %in% c("Input","Output"))$R_ColumnName

# run alorithm  using B2   
df_output <- Run_Promo_Effectiveness(raw_input,df_ComplementaryRelationships,OutputFields,AdditionalFields,"B2")   

# run alorithm using B3 to obtain a separate Tableau output file of B3 as well   
df_outputB3 <- Run_Promo_Effectiveness(raw_input,df_ComplementaryRelationships,OutputFields,AdditionalFields,"B3")   



# Adding columns for baseline raw and seasonality index using B2 to df_output
df_output<- B2_1_Baseline_append_to_df(df_output)
df_output <-B2_2_Seasonality_append_to_df(df_output)


# Adding columns for baseline raw and seasonality index using B2 to df_output3
df_outputB3<- B2_1_Baseline_append_to_df(df_outputB3)
df_outputB3 <-B2_2_Seasonality_append_to_df(df_outputB3)

## creating data for tableau for B2
{
df_output$Fixed_Cost_Manufacturer_1 <- raw_input$Fixed_Cost_Manufacturer_1
df_output$Fixed_Cost_Manufacturer_2 <- raw_input$Fixed_Cost_Manufacturer_2
df_output$Fixed_Cost_Manufacturer_3 <- raw_input$Fixed_Cost_Manufacturer_3
df_output$Variable_Cost_Manufacturer_1 <- raw_input$Variable_Cost_Manufacturer_1
df_output$Variable_Cost_Manufacturer_2 <- raw_input$Variable_Cost_Manufacturer_2
df_output$Variable_Cost_Manufacturer_3 <- raw_input$Variable_Cost_Manufacturer_3

df_output_Tableau <-select(df_output, SKU	,Retailer_Market	,Brand,Brand_ClientFlag,Generic_Hierarchy_1,Generic_Hierarchy_2,Manufacturer,SKU_Pack_Units,SKU_Pack_Type
                           ,Volume_Observed,Week_ending,Baseline_Raw_Volume,Seasonality_Index	,Baseline_all_factors_Volume,	Baseline_all_factors_Margin_Manufacturer,	Baseline_all_factors_Margin_Retail	,Baseline_all_factors_Sales_Manufacturer,
                         Price_Baseline_Retail,	Price_Baseline_Manufacturer,	Price_Promo_Retail	,Promo_Flag,	Unit_Costs
                         ,Observed_Sales_Manufacturer,	Observed_Sales_Retailer,	Observed_Sales_System	,Uplift_Volume	,Uplift_Margin_System	,Uplift_Margin_Manufacturer,	Uplift_Revenue_Manufacturer	,Net_Uplift_Margin_Manufacturer	
                         ,Net_Uplift_Margin_System	,Net_Uplift_Revenue_Manufacturer	,Net_Uplift_Revenue_System	,Incremental_Margin_Manufacturer,	Incremental_Margin_System	,Incremental_Margin_Retailer	,Incremental_Revenue_Manufacturer
                          ,Pantry_Loading_Volume	,Pantry_Loading_Margin_Manufacturer	,Pantry_Loading_Margin_System	,Pantry_Loading_Sales_Manufacturer,B3_Pantry_Loading_Volume	,B3_Pantry_Loading_Margin_Manufacturer	,B3_Pantry_Loading_Sales_Manufacturer
                          ,Cannibalization_Volume	,Cannibalization_Margin_System,	Cannibalization_Margin_Manufacturer	,Cannibalization_Sales_Manufacturer,Cannibalization_Group
                          ,Complementary_Margin_Manufacturer, Complementary_Margin_System,
                         Variable_Cost_Manufacturer_1	,Variable_Cost_Manufacturer_2	,Variable_Cost_Manufacturer_3	,Fixed_Cost_Manufacturer_1	,Fixed_Cost_Manufacturer_2	,Fixed_Cost_Manufacturer_3	,Other_Promo_Cost_Manufacturer	,Other_Retailer_Funding	,Invested_Margin_Discounts_Manufacturer	
                         ,Invested_Margin_Discounts_System	,Total_Variable_Costs	,Total_Fixed_Costs	,Total_Promo_Cost_Manufacturer	,Total_Promo_Cost_System
                         ,Flyer_Back_Manufacturer	,Flyer_Front_Manufacturer	,Flyer_Inside_Manufacturer	,Flyer_Total_Manufacturer
                        ,Baseline_Margin_Manufacturer	,Baseline_Margin_System	,Other_Promo_Cost_System
                        ,Event_ID_SKUlevel	,Promo_Depth	,Promo_Depth_Category	,Promo_Type	,Promo_Mechanism
)

}


## creating data for tableau for B3

df_outputB3$Fixed_Cost_Manufacturer_1 <- raw_input$Fixed_Cost_Manufacturer_1
df_outputB3$Fixed_Cost_Manufacturer_2 <- raw_input$Fixed_Cost_Manufacturer_2
df_outputB3$Fixed_Cost_Manufacturer_3 <- raw_input$Fixed_Cost_Manufacturer_3
df_outputB3$Variable_Cost_Manufacturer_1 <- raw_input$Variable_Cost_Manufacturer_1
df_outputB3$Variable_Cost_Manufacturer_2 <- raw_input$Variable_Cost_Manufacturer_2
df_outputB3$Variable_Cost_Manufacturer_3 <- raw_input$Variable_Cost_Manufacturer_3
df_output_TableauB3 <-select(df_outputB3, SKU	,Retailer_Market	,Brand,Brand_ClientFlag,Generic_Hierarchy_1,Generic_Hierarchy_2,Manufacturer,SKU_Pack_Units,SKU_Pack_Type
                           ,Volume_Observed,Week_ending,Baseline_Raw_Volume,Seasonality_Index	,Baseline_all_factors_Volume,	Baseline_all_factors_Margin_Manufacturer,	Baseline_all_factors_Margin_Retail	,Baseline_all_factors_Sales_Manufacturer,
                           Price_Baseline_Retail,	Price_Baseline_Manufacturer,	Price_Promo_Retail	,Promo_Flag,	Unit_Costs
                           ,Observed_Sales_Manufacturer,	Observed_Sales_Retailer,	Observed_Sales_System	,Uplift_Volume	,Uplift_Margin_System	,Uplift_Margin_Manufacturer,	Uplift_Revenue_Manufacturer	,Net_Uplift_Margin_Manufacturer	
                           ,Net_Uplift_Margin_System	,Net_Uplift_Revenue_Manufacturer	,Net_Uplift_Revenue_System	,Incremental_Margin_Manufacturer,	Incremental_Margin_System	,Incremental_Margin_Retailer	,Incremental_Revenue_Manufacturer
                           ,Pantry_Loading_Volume	,Pantry_Loading_Margin_Manufacturer	,Pantry_Loading_Margin_System	,Pantry_Loading_Sales_Manufacturer,B3_Pantry_Loading_Volume	,B3_Pantry_Loading_Margin_Manufacturer	,B3_Pantry_Loading_Sales_Manufacturer
                           ,Cannibalization_Volume	,Cannibalization_Margin_System,	Cannibalization_Margin_Manufacturer	,Cannibalization_Sales_Manufacturer,Cannibalization_Group
                           ,Complementary_Margin_Manufacturer, Complementary_Margin_System,
                           Variable_Cost_Manufacturer_1	,Variable_Cost_Manufacturer_2	,Variable_Cost_Manufacturer_3	,Fixed_Cost_Manufacturer_1	,Fixed_Cost_Manufacturer_2	,Fixed_Cost_Manufacturer_3	,Other_Promo_Cost_Manufacturer	,Other_Retailer_Funding	,Invested_Margin_Discounts_Manufacturer	
                           ,Invested_Margin_Discounts_System	,Total_Variable_Costs	,Total_Fixed_Costs	,Total_Promo_Cost_Manufacturer	,Total_Promo_Cost_System
                           ,Flyer_Back_Manufacturer	,Flyer_Front_Manufacturer	,Flyer_Inside_Manufacturer	,Flyer_Total_Manufacturer
                           ,Baseline_Margin_Manufacturer	,Baseline_Margin_System	,Other_Promo_Cost_System
                           ,Event_ID_SKUlevel	,Promo_Depth	,Promo_Depth_Category	,Promo_Type	,Promo_Mechanism
)

## Replacing NA values in tableau files with blank
df_output_TableauB3[is.na(df_output_TableauB3)] <-" " 
df_output_Tableau[is.na(df_output_Tableau)] <-" " 

### saving data for tableau in csv 
raw_location <- getwd()
setwd("C:/MSP/Promo_Effectiveness_R_20151111/3_Output")
write.csv(x = df_output_Tableau,file = "Output_Tableau_xx__20161114_14h58B2.csv",row.names = FALSE)
write.csv(x = df_output_TableauB3,file = "Output_Tableau_xx__20161114_14h58B3.csv",row.names = FALSE)
setwd(raw_location)





#################################################################################################################
###                       Calculating new variables for regression                                            ###
#################################################################################################################



### Calculating avg weekly quantity of non promo weeks of that SKU at that retailer
### Creating function which calculates avg_Weekly_nonpromo for each SKU per retailer 
### having inputs as raw data , one SKU and one Retailer

avg_Weekly_nonpromo <- function(sel_df,sel_SKU, sel_Retailer){  
  # initialize
  df_temp <- filter(sel_df, SKU==sel_SKU & Retailer_Market==sel_Retailer)
  summ = 0
  counter = 0
  for(i in 1:nrow(df_temp)){
    if(df_temp$Promo_Flag[i] == 0){
      
      summ = summ + df_temp[i,"Volume_Observed"]
      counter = counter +1
    }
  }
  average= summ/counter
  df_temp$avg_nonpromo <- average
  return(df_temp)
}

## Creating a function that calculates avg weekly quantity of non promo weeks
## for all retailers and SKU
avg_Weekly_nonpromo_main <- function(sel_df)
{
  
  #get unique combinations  
  df_Unique_SKUandRM <- unique(sel_df[,c('SKU','Retailer_Market')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_SKUandRM)
  for (i in 1:n)
  {  
    df_temp <- avg_Weekly_nonpromo(sel_df,df_Unique_SKUandRM$SKU[i],df_Unique_SKUandRM$Retailer_Market[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return)
}  

### Calling the avg_Weekly_nonpromo_main to claculate average weekly non-promo value
df_output<- avg_Weekly_nonpromo_main(df_output)


############################################################################################
## Creating a function that calculates Seasonality adjusted raw baseline per SKU per retailer

SeasonalitySKURETAIL <- function(sel_df,sel_SKU, sel_Retailer){  

    df_temp <- filter(sel_df, SKU==sel_SKU & Retailer_Market==sel_Retailer)
  for(i in 1:nrow(df_temp)){
    df_temp[i,"Seasonality_Adjusted_Raw_Baseline"] <- df_temp[i,"Seasonality_Index"] * df_temp[i,"avg_nonpromo"]
    
    }
  return(df_temp)
}

## Creating a function that calculates Seasonality adjusted raw baseline by calling SeasonalitySKURETAIL
SeasonalitySKURETAIL_main <- function(sel_df)
{
  #get unique combinations  
  df_Unique_SKUandRM <- unique(sel_df[,c('SKU','Retailer_Market')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_SKUandRM)
  for (i in 1:n)
  {  
    
    df_temp <- SeasonalitySKURETAIL(sel_df,df_Unique_SKUandRM$SKU[i],df_Unique_SKUandRM$Retailer_Market[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return)
}  

### calling the SeasonalitySKURETAIL_main function with df_output as argument to calculate seasonality adjusted raw baseline
df_output<- SeasonalitySKURETAIL_main(df_output)



#####Creating a function that Aggregates the seasonality adjusted raw baseline per pair of Group Hierarchy 2 and week

SeasRawBaselineGHWK <- function(sel_df, sel_Generic_Hierarchy_2, sel_Week){  
  
  df_temp <- filter(sel_df, Generic_Hierarchy_2==sel_Generic_Hierarchy_2 & Week_ending ==sel_Week)
  SARB_avg <- mean(df_temp$Seasonality_Adjusted_Raw_Baseline)
  for(i in 1:nrow(df_temp)){

    df_temp[i,"Seasonality_Adjusted_Raw_Baseline"] <-  SARB_avg

      }
  return(df_temp)
}

## Creating function to be called that aggregates the seasonality adjusted raw baseline
AdjSeasRawBaselineGHWK_main <- function(sel_df)
{
  
  #get unique combinations  
  df_Unique_GH2andWK <- unique(sel_df[,c('Generic_Hierarchy_2','Week_ending')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_GH2andWK)
  for (i in 1:n)
  {  
    df_temp <- SeasRawBaselineGHWK(sel_df,df_Unique_GH2andWK$Generic_Hierarchy_2[i],df_Unique_GH2andWK$Week_ending[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return)
}  


#Calling the function to aggregate seasonality adjusted raw baseline


df_output <- AdjSeasRawBaselineGHWK_main(df_output)




#############################################################################
##                        Creating Advertising Flag
#############################################################################
# Adding the column Advertising Flag AD for Advertising Impact
# Whether a promo is advertised in-store or online
# it has values 0 and 1 where 1=Ad, 0=No Ad 
# if Flyer_Total_Manufacturer >0, then 1 else 0 

df_output$AD <- (ifelse(df_output$Flyer_Total_Manufacturer != 0, 1, 0))

#############################################################################
##                     Calculating Price Index                                   
#############################################################################
# Price Index
# o   Description: Price elasticity variables interacted with retailer
# o   Calculation: 1-Discount% (Promo_Depth) (Base Price - Promo Price)/Base Price


## We calculate "Discount_Depth" which is equavalent to discount percentage
for(i in 1:nrow(df_output)){
  df_output[i,"Discount_Depth"] <- (df_output[i,"Price_Baseline_Retail"]- df_output[i,"Price_Promo_Retail"])/ df_output[i,"Price_Baseline_Retail"] 
}




## Creating a function that calculates Price Index at Retailer and week level
Price_index_SKUnRETnWK <- function(sel_df,sel_SKU, sel_Retailer, sel_Week){  

  df_temp <- filter(sel_df, SKU==sel_SKU & Retailer_Market==sel_Retailer & Week_ending==sel_Week)
  dic_avg <- mean(df_temp$Discount_Depth)
  for(i in 1:nrow(df_temp)){
    
    df_temp[i,"Price_index"] <- 1-dic_avg 
    
  }
  return(df_temp)
}

## Creating a function to be called to calculate Price Index
Price_index_SKUnRETnWK_main <- function(sel_df)
{
  
  #get unique combinations  
  df_Unique_SKUandRMandWK <- unique(sel_df[,c('SKU','Retailer_Market','Week_ending')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_SKUandRMandWK)
  for (i in 1:n)
  {  
    df_temp <- Price_index_SKUnRETnWK(sel_df,df_Unique_SKUandRMandWK$SKU[i],df_Unique_SKUandRMandWK$Retailer_Market[i],df_Unique_SKUandRMandWK$Week_ending[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return)
}  

# calling the function to create price index
df_output<- Price_index_SKUnRETnWK_main(df_output)





#####################################################################################################################3
############################################################################################
## Creating a function that calculates avg discount per SKU per retailer for advertising elasticity
## for promo weeks only
avg_DiscountSKURETAIL <- function(sel_df,sel_SKU, sel_Retailer){  
  # initialize
  df_temp <- filter(sel_df, SKU==sel_SKU & Retailer_Market==sel_Retailer)
  summ = 0
  counter = 1
  for(i in 1:nrow(df_temp)){
    if(df_temp$Promo_Flag[i] == 1){
      
      summ = summ + df_temp[i,"Discount_Depth"]
      counter = counter +1
      
    }
  }
  average= summ/counter
  df_temp$avg_Disc <- average
  return(df_temp)
}

## Creating a function to be called that calculates avg weekly discount of promo weeks only
## for all retailers and SKU
avg_Disc_promo_main <- function(sel_df)
{
  #get unique combinations  
  df_Unique_SKUandRM <- unique(sel_df[,c('SKU','Retailer_Market')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_SKUandRM)
  for (i in 1:n)
  {  
    
    df_temp <- avg_DiscountSKURETAIL(sel_df,df_Unique_SKUandRM$SKU[i],df_Unique_SKUandRM$Retailer_Market[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return)
}  

## Calling the avg_Disc_promo_main function on input data
## to calculate actual discounts
input <- avg_Disc_promo_main(df_output)

#################################################################################################
##                              Cannibalization Price Index
#################################################################################################
# o   Description: Avg discount% (including non-promo as 0%) within same group (Generic_Hierarchy_2) across all retailers.
# o   Calculation: Sum of discount% (including 0% for non promo weeks) of all Manufacturers SKUs (Brand_ClientFlag = TRUE) in same L1 (Generic_Hierarchy_2), 
#                  divided by count of Manufacturers SKUs (Brand_ClientFlag = TRUE) in same L1 (Generic_Hierarchy_2)
## Calling CannibalizationPriceIndex to find the Cannibalization Price Index

# Creating a function that finds  Cannibalization Price Index per manufacturer per 
# general hierarchy 2 which has inputs raw data , one general hierarchy 2 level value
# and one manufacturer value


CannibalizationPriceIndex_GH_MAN <- function(sel_df,sel_GH2, sel_Manufacturer){  
  df_temp <- filter(sel_df, Manufacturer==sel_Manufacturer & Generic_Hierarchy_2==sel_GH2)
  summ = 0
  counter = 0
  for(i in 1:nrow(df_temp)){
    if(df_temp$Promo_Flag[i] == 1){
      summ = summ + df_temp[i,"Discount_Depth"]
    }
    else{
      summ = summ + 0
    }
    
  }
  # Counting distinct number of SKUs
  counter <- length(unique(df_temp$SKU))
  average= summ/counter
  df_temp$CannibalisedPriceIndex <- average
  return(df_temp)
}


## Creating a function that calculates  Cannibalization Price Index
CannibalizationPriceIndex <- function(sel_df)
{
  
  #get unique combinations  
  df_Unique_GH2andMAN <- unique(sel_df[,c('Generic_Hierarchy_2','Manufacturer')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_GH2andMAN)
  for (i in 1:n)
  {  
    
    df_temp <- CannibalizationPriceIndex_GH_MAN(sel_df,df_Unique_GH2andMAN$Generic_Hierarchy_2[i],df_Unique_GH2andMAN$Manufacturer[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  return(df_return)
}  

## Cannibalization Price Index
input <- CannibalizationPriceIndex(df_output)


###################################################################################
##                              Competitor price index
###################################################################################
# o   Description: Avg discount% (including non-promo as 0%) across all retailers.
# o   Calculation: Sum of discount% (including 0% for non promo weeks) of all competitor SKUs in same group (Generic_Hierarchy_2), 
#                  divided by count of competitor SKUs in same group (Generic_Hierarchy_2)
# Creating a function that finds  competitor Price Index for a pair of competitor 
# and one level of general hierarchy 2 

CompetitorPriceIndex_GH_MAN <- function(sel_df,sel_GH2, sel_Manufacturer){  
  df_temp <- filter(sel_df, Manufacturer==sel_Manufacturer & Generic_Hierarchy_2==sel_GH2)
  summ = 0
  for(i in 1:nrow(df_temp)){
    if(df_temp$Promo_Flag[i] == 1){
      summ = summ + df_temp[i,"Discount_Depth"]
    }
    else{
      summ = summ + 0
    }
    
  }
  
  counter <- length(unique(df_temp$SKU))
  average= summ/counter
  df_temp$competitorPriceIndex <- average
  return(df_temp)
}



## Creating a function that calculates  competitor Price Index
CompetitorPriceIndex <- function(sel_df)
{
  
  #get unique combinations  
  df_Unique_GH2andMAN <- unique(sel_df[,c('Generic_Hierarchy_2','Manufacturer')])
  #loop through list
  df_return <- NULL
  n <- nrow(df_Unique_GH2andMAN)
  for (i in 1:n)
  {  
    
    df_temp <- CompetitorPriceIndex_GH_MAN(sel_df,df_Unique_GH2andMAN$Generic_Hierarchy_2[i],df_Unique_GH2andMAN$Manufacturer[i])
    df_return <- plyr::rbind.fill(df_return,df_temp)
  }
  
  return(df_return)
}  


## Calling the function CompetitorPriceIndex on input
df_output <- CompetitorPriceIndex(df_output)

n <- nrow(df_output)
for (i in 1:n)
{  
  if(df_output[i,"Manufacturer"]== "CPG Client"){
    df_output[i,"competitorPriceIndex"]<- NA
  }
}

####################################################################################
##                    Calculating advertising Elasticity AE
##          AD x [ Ln (1-Promo Discount%) - Ln (1-Avg Discount%) ]
####################################################################################
for(i in 1:nrow(df_output)){
  df_output[i,"Advertising_Elasticity"] <-df_output[i,"AD"]*(log(1-df_output[i,"Promo_Depth"])- log( 1-df_output[i,"avg_Disc"])) 
}



## Storing the output in a CSV file
setwd("C:/MSP/Promo_Effectiveness_R_20151111/3_Output")
write.csv(x = df_output,file = "FinalB4output.csv",row.names = FALSE)





###   Two Assumptions yet to be applied on data. Once done we can execute the below regression code 
###################################################################################################################
##################                 REGRESSION                                                                 #####
###################################################################################################################


# 
# ### storing the value of log(priceIndex) in another variable log_priceIndex
# df_output$log_priceIndex <- log(df_output$Price_index)
# 
# # since log_priceIndex contains contains extreme negative values 
# # so we replace them with 0 
# df_output[df_output$log_priceIndex <0 , "log_priceIndex"]<- 0
# 
# 
# 
# # Function that returns Root Mean Squared Error
# rmse <- function(error)
# {
#   sqrt(mean(error^2))
# }
# 
# 
# ##### 1. Running the regression model pred to predict sales
# 
# pred <- lm(formula = log(df_output$Volume_Observed) ~ log(df_output$Seasonality_Adjusted_Raw_Baseline) 
#            + df_output$log_priceIndex 
#            +df_output$log_priceIndex*df_output$AD 
#            + df_output$AD 
#            + df_output$competitorPriceIndex
#            + df_output$CannibalisedPriceIndex  )
# 
# print(summary(pred))
# print(rmse(pred$residuals))
# 
# 
# #full regression output
# stargazer(pred, type="text",
#           dep.var.labels=c("Volume predicted by model pred"),
#           out="model_pred.txt")
# 
# 
# 
# 
# ### 2. Running a model pred_no_promo where it Predict sales value with regression model 
# ### by turning off promo-related variables (discount variables)
# 
# pred_no_promo <- lm(formula = log(df_output$Volume_Observed) ~ log(df_output$Seasonality_Adjusted_Raw_Baseline) 
#                     #+ df_output$log_priceIndex 
#                     +df_output$log_priceIndex*df_output$AD 
#                     + df_output$AD 
#                     + df_output$competitorPriceIndex
#                     + df_output$CannibalisedPriceIndex  )
# 
# print(summary(pred_no_promo))
# print(rmse(pred_no_promo$residuals))
# 
# 
# #full regression output incl. PLs
# stargazer(pred_no_promo, type="text",
#           dep.var.labels=c("Volume predicted by model pred_no_promo"),
#           out="model_pred_no_promo.txt")
# 
# 
# ### 3. Running a model pred_no_cann where it Predict sales value with regression model 
# ### by turning off cannibalisation realated variables
# 
# pred_no_cann <- lm(formula = log(df_output$Volume_Observed) ~ log(df_output$Seasonality_Adjusted_Raw_Baseline) 
#                    + df_output$log_priceIndex 
#                    +df_output$log_priceIndex*df_output$AD 
#                    + df_output$AD 
#                    + df_output$competitorPriceIndex
#                    #+ df_output$CannibalisedPriceIndex  
# )
# 
# print(summary(pred_no_cann))
# print(rmse(pred_no_cann$residuals))
# 
# 
# #full regression output 
# stargazer(pred_no_cann, type="text",
#           dep.var.labels=c("Volume predicted by model_pred_no_cann"),
#           out="model_pred_no_cann.txt")
# 
# ### 4. Running a model pred_no_ad where it Predict sales value with regression model 
# ### by turning off ad realated variables
# 
# pred_no_ad <- lm(formula = log(df_output$Volume_Observed) ~ log(df_output$Seasonality_Adjusted_Raw_Baseline) 
#                  + df_output$log_priceIndex 
#                  #+df_output$log_priceIndex*df_output$AD 
#                  #+ df_output$AD 
#                  + df_output$competitorPriceIndex
#                  + df_output$CannibalisedPriceIndex  )
# 
# print(summary(pred_no_ad))
# print(rmse(pred_no_ad$residuals))
# 
# 
# #full regression output 
# stargazer(pred_no_ad, type="text",
#           dep.var.labels=c("Volume predicted by model pred_no_ad"),
#           out="model_pred_no_ad.txt")
# 