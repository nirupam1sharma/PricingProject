#################################################################################################################
### Purpose: R code for Catalyst Promo Effectiveness code (wip)
###          B1 and B2 functions
### Author: Nirupam Sharma
### Date: 2016/11/09
### Version: v05
#################################################################################################################


#################################################################################################################
###                                          Functions                                                        ###
#################################################################################################################
{
   
###### ====================================================================================================== ###
##### 0 user inputs and initialization ##########################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   ### diverse input values
   {
      inp_BaselineweeksNr <- 5
      inp_SeasonalityWeeks <- 3 # weeks prior and after actual week to consider
      inp_PantryLoadingWeeks <- 3 # weeks after a promo which are considered for pantry loading
   }
   
   
###### ====================================================================================================== ###
##### 1 Baseline calculation ####################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### B2_1_Baseline #################################################################################################
   ### inputs: df is data frame (MUST be sorted on week ending and needs SKU,Retailer_Market,Week_ending,Promo_Flag,Volume_Observed,Baseline_Raw_Volume
   ###         SKU
   ###         Retailer
   ###         respective week ending
   ### output: Basesline value for particular SKU, Retailer and Week_ending as selected in the function inputs
   B2_1_Baseline <- function(df,selSKU,selRetailer,selWeek_ending)
   {
      df_temp <- subset(df,SKU==selSKU & Retailer_Market==selRetailer & Week_ending<=selWeek_ending & Week_ending>=selWeek_ending-(inp_BaselineweeksNr-1)*7, 
                        select = c("SKU","Retailer_Market","Week_ending","Promo_Flag","Volume_Observed","Baseline_Raw_Volume"))
      
      if (nrow(df_temp)==1) #1st case: very first week of SKU at retailer
         c(mean(subset(df,SKU==selSKU & Retailer_Market==selRetailer & Promo_Flag==0)$Volume_Observed),"B2_1")
      else if(sum(!df_temp$Promo_Flag==1 | is.na(df_temp$Promo_Flag))>=2) #2nd case: average if at least 2 non-promo available
         c(mean(subset(df_temp,Promo_Flag==0, select=Volume_Observed)$Volume_Observed),"B2_2")
      else #3rd case: take the last available value, sometimes previoius week is not in data
         c(df_temp[nrow(df_temp)-1,]$Baseline_Raw_Volume,paste("B2_3 (", (selWeek_ending-df_temp[nrow(df_temp)-1,]$Week_ending)/7," weeks)" ,sep=""))
   }
   
   ### B2_1_Baseline_append_to_df #############################################################################################
   ### uses B2_1_Baseline to calculate relevant values for all SKU/Retailer/Weeks and outputs a list
   ### input: data frame with volumes per SKU, Retailer_Market and Week_ending
   ### output: original data frame with baseline_raw appended
   B2_1_Baseline_append_to_df <- function(sel_df,TF_ProgressPrinting=FALSE)
   {
      cat(paste0("\n","Baseline calculations started:",Sys.time()))   
      
      #get unique combinations  
      Unique_SKUs <- unique(sel_df[,c('SKU')])
      #loop through list
      df_baseline_raw <- NULL
      
      n <- length(Unique_SKUs)
      if (TF_ProgressPrinting)cat(paste(sep="","SKU...", 0,"/",n," ", Sys.time(),"\n"))
      
      for (j in 1:n)
      {#get relevant data for SKU j out of 1:n and sort it by week
         df_1SKU <- arrange(subset(sel_df, SKU==Unique_SKUs[j]),Week_ending)
         #loop through resulting data set and calculate Baseline_raw each
         df_1SKU$Baseline_Raw_Volume <- NA
         for (i in 1:nrow(df_1SKU)) {
            temp <- B2_1_Baseline(df_1SKU,df_1SKU$SKU[i],df_1SKU$Retailer_Market[i],df_1SKU$Week_ending[i])
            df_1SKU$Baseline_Raw_Volume[i]<- as.numeric(temp[1])
            df_1SKU$Baseline_Type[i] <- temp[2]
         }
         #union to results of other SKUs
         df_baseline_raw <- rbind(df_baseline_raw,df_1SKU)
         if (j%%50==0 & TF_ProgressPrinting) { cat(paste(sep="","SKU...", j,"/",n," ", Sys.time(),"\n")) }
      }
      cat(paste0("\n","Baseline calculations completed: ",Sys.time()),"\n")   
      return(df_baseline_raw)
   }
   
###### ====================================================================================================== ###
##### 2 Seasonability calculation #################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### B2_2_Seasonality_perGroup #####################################################################################
   ### calculates seasonality indexes for a seasonality group and retailer
   ### inputs: df: main data frame
   ###         selSeasonality_Group    
   ###         selRetailer
   ### Output: new data frame for the selected seasonality group + retailer on week_ending detail level
   B2_2_Seasonality_perGroup <- function(sel_df,selSeasonality_Group,selRetailer)
   {#get relevant numbers and aggregate all volumes
      df_season_volumes <- sel_df %>% 
         filter(Seasonality_Group==selSeasonality_Group)  %>% 
         filter(Retailer_Market==selRetailer)  %>% 
         group_by(Seasonality_Group,Retailer_Market,Week_ending) %>% summarise(wk_volume=sum(Volume_Observed))
      
      #calculate over all average per line
      df_season_volumes$Seasonality_OverAllAvg <- mean(df_season_volumes$wk_volume,na.rm = TRUE)
      
      #calculate rolling average per line
      df_season_volumes$Seasonality_RollingAvg <- NA
      for (dt_week in df_season_volumes$Week_ending)
         df_season_volumes[df_season_volumes$Week_ending==dt_week,]$Seasonality_RollingAvg <- mean(na.rm = TRUE,subset(df_season_volumes,dt_week-7*inp_SeasonalityWeeks<=Week_ending & Week_ending<=dt_week+7*inp_SeasonalityWeeks)$wk_volume)
      
      #calculate seasonality index per line  
      df_season_volumes$Seasonality_Index <- df_season_volumes$Seasonality_RollingAvg/df_season_volumes$Seasonality_OverAllAvg
      # order for normalization step by GdR
      df_season_volumes <- arrange(df_season_volumes, Week_ending)
      # @Ti, it normalizes it by the moving average window used to calculate the baseline
      df_season_volumes$Seasonality_Index <- df_season_volumes$Seasonality_Index / runmean(df_season_volumes$Seasonality_Index, inp_BaselineweeksNr, align = "right")
      
      df_season_volumes <- select(df_season_volumes,-c(wk_volume))
      #output
      return(df_season_volumes)
   }
   
   ### B2_2_Seasonality_for_df #############################################################################################
   ### uses B2_2_Seasonality_perGroup to calculate relevant values for all groups of seasonality+retailer
   ### input: data frame with volumes per SKU for Seasonality_Group, Retailer_Market and Week_ending
   ### output: data frame with seaonality index per Seasonality_Group, Retailer_Market and Week_ending (to be merged to the main data frame)
   B2_2_Seasonality_for_df <- function(sel_df,TF_ProgressPrinting=FALSE)
   {#get unique combinations  
      df_Unique_SGandR <- unique(sel_df[,c('Seasonality_Group','Retailer_Market')])
      #loop through list
      df_season_rows_combined <- NULL
      n <- nrow(df_Unique_SGandR)
      for (i in 1:n)
      {#get seasonality for each Seasonality group + retailer and union it to the rest
         df_temp <- B2_2_Seasonality_perGroup(sel_df,df_Unique_SGandR$Seasonality_Group[i],df_Unique_SGandR$Retailer_Market[i])
         df_season_rows_combined <- rbind(df_season_rows_combined,df_temp)
         if(TF_ProgressPrinting) {cat(paste(sep="","Seasonality group + Retailer...", i,"/",n,"\n"))}
      }
      return(df_season_rows_combined)
   }
   
   ### B2_2_Seasonality_append_to_df #######################################################################################
   ### uses B2_2_Seasonality_for_df and joins the result to raw data
   ### input: main data frame
   ### output: main data frame with additional seasonality columns and Unit_Costs_asUsed, Volume_Diff_Obs_BaseAll
   B2_2_Seasonality_append_to_df <- function(sel_df,TF_ProgressPrinting=FALSE)
   {
      cat(paste0("\n","Seasonality calculations started:",Sys.time()))  
      df_temp <- B2_2_Seasonality_for_df(sel_df,TF_ProgressPrinting)
      df_return <- merge(sel_df[, c("Seasonality_Group","Retailer_Market","Week_ending", setdiff(colnames(sel_df),colnames(df_temp)))],df_temp,by=c("Seasonality_Group","Retailer_Market","Week_ending"))
      
    
      ### calculate Volume_Diff_Obs_BaseAll and baseline all factor 
      df_return <- mutate(df_return, Baseline_all_factors_Volume = Baseline_Raw_Volume * Seasonality_Index)    
      df_return <- mutate(df_return, Volume_Diff_Obs_BaseAll = Volume_Observed - Baseline_all_factors_Volume)

      
      cat(paste0("\n","Seasonality calculations completed: ",Sys.time()),"\n")   
      
      return(df_return)
   }
   
###### ====================================================================================================== ###
##### 3 Pantry loading ##########################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### B2_3_PantryLoading ############################################################################################
   ### inputs: df is data frame with steps 2 completed 
   ###         SKU
   ###         Retailer
   ### output: data frame with Pantry loading values for the selected SKU+Retailer (to be joined back to main data frame)
   B2_3_PantryLoading <- function(df,selSKU,selRetailer)
   {
      df_temp <- arrange(subset(df,SKU==selSKU & Retailer_Market==selRetailer, 
                                select = c("SKU","Retailer_Market","Week_ending","Promo_Flag","Volume_Observed","Baseline_all_factors_Volume","Volume_Diff_Obs_BaseAll",
                                           "Price_Baseline_Manufacturer","Unit_Costs_asUsed","Price_Baseline_Retail")
      ),Week_ending)
      
      # Initialize
      j <- 0
      df_temp$Promo_Flag_group <- NA
      NonPromo_counter <- 0
      df_temp$Promo_Flag_group <- "none"
      j <- 0      
      
      #adjust missing Promo_Flag
      df_temp$Promo_Flag <- ifelse(is.na(df_temp$Promo_Flag),0,df_temp$Promo_Flag)    
      
      if (!all(df_temp$Promo_Flag!=1)) # i.e. there is a promo week
      {
         i_first_promo <- which(df_temp$Promo_Flag==1)[1] # first promo week
         for ( i in i_first_promo:nrow(df_temp))
         {#i.e increase counter if you have a new promo beginning after non-promo or there was a gap.
            if(i>1){
               j <- j+ifelse((df_temp[i-1,]$Promo_Flag!=1 | df_temp[i,]$Week_ending > df_temp[i-1,]$Week_ending+7) & df_temp[i,]$Promo_Flag==1,1,0)
            } else {j<-ifelse(df_temp[i,]$Promo_Flag==1,1,0)}
            
            if (df_temp[i,]$Promo_Flag==1)
            {df_temp[i,]$Promo_Flag_group <- paste("P_",j,sep="")
            i_last_promo <- i
            NonPromo_counter<- 0}
            else if(i_last_promo>0 & NonPromo_counter<= inp_PantryLoadingWeeks & df_temp[i,]$Week_ending<=df_temp[i_last_promo,]$Week_ending+7*inp_PantryLoadingWeeks )
            {# i.e. number smaller than number of Pantry loading week
               df_temp[i,]$Promo_Flag_group <- paste("P_",j,sep="")
               NonPromo_counter <-  NonPromo_counter+1}
            else # i.e. too far awway from promo to be considered as pantry loading
            {df_temp[i,]$Promo_Flag_group <- paste("none")}
         }
         
         # calculate pantry loading for each pantry loading group
         df_summary <- df_temp %>% group_by(Promo_Flag_group) %>%
            summarise(sum_uplift = sum(ifelse(Promo_Flag==1 & Promo_Flag_group!="none", pmax(Volume_Diff_Obs_BaseAll,0),0)),
                      sum_shortage = sum(ifelse(Promo_Flag==0 & Promo_Flag_group!="none", -pmin(Volume_Diff_Obs_BaseAll,0),0))  )
         # merge it back to the original df
         df_temp <- arrange(merge(df_temp,df_summary, by="Promo_Flag_group"),Week_ending)
         
      }
      else # i.e. no promo
      {df_temp$sum_uplift <- NA
      df_temp$sum_shortage <- NA}
      
      # calculate actual Pantry loading per row
      df_temp <- mutate(df_temp, Pantry_Loading_Volume = ifelse(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0,
                                                                (Volume_Diff_Obs_BaseAll/sum_uplift*ifelse(sum_uplift >= sum_shortage, sum_shortage,sum_uplift)),0))
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Volume = ifelse(Promo_Flag==0 & Volume_Diff_Obs_BaseAll<0,
                                                                   (-Volume_Diff_Obs_BaseAll/sum_shortage*ifelse(sum_uplift >= sum_shortage, sum_shortage,sum_uplift)),0))
      
      # with the B3_Pantry_Loading_Volume we can calculate row wise Margin and Sales of affected non-weeks, that has to be done because of actual prices in week
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Margin_Manufacturer = B3_Pantry_Loading_Volume * (Price_Baseline_Manufacturer - Unit_Costs_asUsed))
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Margin_System = B3_Pantry_Loading_Volume * (Price_Baseline_Retail - Unit_Costs_asUsed))
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Margin_Retailer = B3_Pantry_Loading_Margin_System - B3_Pantry_Loading_Margin_Manufacturer)
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Sales_Manufacturer = B3_Pantry_Loading_Volume * Price_Baseline_Manufacturer)
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Sales_System = B3_Pantry_Loading_Volume * Price_Baseline_Retail)
      
      # with this volume we can calculate the total margins and sales for the affecting promo groups of weeks
      df_summary <- df_temp %>% group_by(Promo_Flag_group) %>%
         summarise(sum_Pantry_Loading_Margin_Manufacturer = sum(B3_Pantry_Loading_Margin_Manufacturer),
                   sum_Pantry_Loading_Margin_System = sum(B3_Pantry_Loading_Margin_System),
                   sum_Pantry_Loading_Margin_Retailer = sum(B3_Pantry_Loading_Margin_Retailer),
                   sum_Pantry_Loading_Sales_Manufacturer = sum(B3_Pantry_Loading_Sales_Manufacturer),
                   sum_Pantry_Loading_Sales_System = sum(B3_Pantry_Loading_Sales_System))
      
      # these values are used to distribute the share to every single week
      df_temp <- arrange(merge(df_temp,df_summary, by="Promo_Flag_group"),Week_ending)
      
      df_temp <- mutate(df_temp, Pantry_Loading_Margin_Manufacturer = ifelse(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0,
                                                                             (Volume_Diff_Obs_BaseAll/sum_uplift*sum_Pantry_Loading_Margin_Manufacturer),0))
      df_temp <- mutate(df_temp, Pantry_Loading_Margin_System = ifelse(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0,
                                                                       (Volume_Diff_Obs_BaseAll/sum_uplift*sum_Pantry_Loading_Margin_System),0))
      df_temp <- mutate(df_temp, Pantry_Loading_Margin_Retailer = ifelse(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0,
                                                                         (Volume_Diff_Obs_BaseAll/sum_uplift*sum_Pantry_Loading_Margin_Retailer),0))
      df_temp <- mutate(df_temp, Pantry_Loading_Sales_Manufacturer = ifelse(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0,
                                                                            (Volume_Diff_Obs_BaseAll/sum_uplift*sum_Pantry_Loading_Sales_Manufacturer),0))
      df_temp <- mutate(df_temp, Pantry_Loading_Sales_System = ifelse(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0,
                                                                      (Volume_Diff_Obs_BaseAll/sum_uplift*sum_Pantry_Loading_Sales_System),0))
      
      #remove helper columns
      select(df_temp, -c(sum_uplift,sum_shortage,starts_with("sum_Pantry")))
   }
   
   {
      #test
      # View(B2_3_PantryLoading(df_2b_baseline_all_factors,"NU.011584","AHOUSACTA"),"test")
      # View(B2_3_PantryLoading(df_2b_baseline_all_factors,"COMP NU.011621","MEJTTLCTA"),"test")
      #test end
   }
   ### B2_3_PantryLoading_append_to_df #######################################################################################
   ### uses B2_3_PantryLoading and joins the result to input data frame
   ### input: main data frame
   ### output: main data frame with additional Pantry_Loading columns
   B2_3_PantryLoading_append_to_df <- function(sel_df,TF_ProgressPrinting=FALSE)
   {
      cat(paste0("\n","PantryLoading calculations started:",Sys.time()))   
      
      #get unique combinations  
      df_Unique_SGandR <- unique(sel_df[,c('SKU','Retailer_Market')])
      #loop through list
      df_temp <- NULL
      n <- nrow(df_Unique_SGandR)
      for (i in 1:n)
      {# calculate Pantry Loading for each combination union results to the rest
         df_temp <- rbind(df_temp,B2_3_PantryLoading(sel_df,df_Unique_SGandR$SKU[i],df_Unique_SGandR$Retailer_Market[i]))
         if (i%%100==0 & TF_ProgressPrinting) { cat(paste(sep="","Combination SKU+retailer...",i,"/",n," ", Sys.time(),"\n")) }
      }
      
      #merge results with original input data frame
      if(TF_ProgressPrinting)cat(paste(sep="","Merging...", Sys.time(),"\n"))
      df_temp <- merge(sel_df[, c("SKU","Retailer_Market","Week_ending", setdiff(colnames(sel_df),colnames(df_temp)))],df_temp,
                       by=c("SKU","Retailer_Market","Week_ending"))
      if(TF_ProgressPrinting) cat(paste(sep="","Merging...", Sys.time(),"\n"))
      
      cat(paste0("\n","PantryLoading calculations completed: ",Sys.time()),"\n")   
      return(df_temp)
   }
   
###### ====================================================================================================== ###
##### 4 Cannibalization #########################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### B2_4_Cannibalization_append_to_df ############################################################################################
   ### inputs: sel_df is data frame with steps 3 completed 
   ### output: data frame with cannibalization values for the selected week_ending+Retailer (to be joined back to main data frame)
   B2_4_Cannibalization_append_to_df <- function(sel_df)
   {
      cat(paste0("\n","Cannibalization calculations started:",Sys.time()))
      
      df_temp <- arrange(sel_df,Week_ending)
      
      #adjust missing promo flag
      df_temp$Promo_Flag <- ifelse(is.na(df_temp$Promo_Flag),FALSE,df_temp$Promo_Flag)
      #calculate uplift volume that remains after pantry loading is calculated per row
      df_temp$Remaining_Uplift <- ifelse(df_temp$Promo_Flag==1, pmax(0,pmax(df_temp$Volume_Diff_Obs_BaseAll,0) - df_temp$Pantry_Loading_Volume ,na.rm=TRUE), NA  )
      
      df_temp <- mutate(df_temp, Cannibalization_Potential_Volume = ifelse(Promo_Flag==0 & Volume_Diff_Obs_BaseAll<0,-Volume_Diff_Obs_BaseAll-pmax(0,B3_Pantry_Loading_Volume,na.rm = TRUE),0))
      
      #summarise to cannibalization group in selected week and calculate pool volume
      df_summary <- df_temp %>% group_by(Retailer_Market,Cannibalization_Group,Week_ending) %>%
         summarise(Remaining_Uplift_of_CanniGroup = sum(ifelse(Promo_Flag==1, pmax(Remaining_Uplift,0,na.rm = TRUE),0)),
                   Cannibalization_Pool_Volume = sum(ifelse(Promo_Flag==0, pmax(Cannibalization_Potential_Volume,0,na.rm = TRUE),0)) 
         )
      #merge to input data
      df_temp <- merge(df_temp[, c("Retailer_Market","Cannibalization_Group","Week_ending",setdiff(colnames(df_temp),colnames(df_summary)))],df_summary,
                       by=c("Retailer_Market","Cannibalization_Group","Week_ending"))
      
      #calculate share and volume per cannibalizing row, as well as cannibalized volume per cannibalized row
      df_temp <- mutate(df_temp, Cannibalization_Share = ifelse(Promo_Flag==1 & Remaining_Uplift>0, Remaining_Uplift/Remaining_Uplift_of_CanniGroup,0))
      df_temp <- mutate(df_temp, Cannibalization_Volume = ifelse(Remaining_Uplift_of_CanniGroup>=Cannibalization_Pool_Volume,Cannibalization_Share*Cannibalization_Pool_Volume,Remaining_Uplift))  
      df_temp <- mutate(df_temp, Cannibalized_Share_passive = ifelse(Promo_Flag==0 & Cannibalization_Potential_Volume>0, Cannibalization_Potential_Volume/Cannibalization_Pool_Volume,NA))
      df_temp <- mutate(df_temp, Cannibalized_Volume_passive = ifelse(Remaining_Uplift_of_CanniGroup>=Cannibalization_Pool_Volume,Cannibalization_Potential_Volume,Cannibalized_Share_passive*Remaining_Uplift_of_CanniGroup))  
      
      #summarise to cannibalization group again to calculate the aggregated margin and sales
      df_summary <- df_temp %>% group_by(Retailer_Market,Cannibalization_Group,Week_ending) %>%
         summarise(Cannibalization_Pool_Margin_System = sum(ifelse(Promo_Flag!=1,Cannibalized_Volume_passive*(Price_Baseline_Retail-Unit_Costs_asUsed),0),na.rm=TRUE),
                   Cannibalization_Pool_Margin_Manufacturer = sum(ifelse(Promo_Flag!=1,Cannibalized_Volume_passive*(Price_Baseline_Manufacturer-Unit_Costs_asUsed),0),na.rm=TRUE),
                   Cannibalization_Pool_Margin_Retailer = Cannibalization_Pool_Margin_System-Cannibalization_Pool_Margin_Manufacturer,
                   Cannibalization_Pool_Sales_Manufacturer = sum(ifelse(Promo_Flag!=1,Cannibalized_Volume_passive*Price_Baseline_Manufacturer,0),na.rm=TRUE),
                   Cannibalization_Pool_Sales_System = sum(ifelse(Promo_Flag!=1,Cannibalized_Volume_passive*Price_Baseline_Retail,0),na.rm=TRUE)
         )
      
      #merge to input data
      df_temp <- merge(df_temp[, c("Retailer_Market","Cannibalization_Group","Week_ending",setdiff(colnames(df_temp),colnames(df_summary)))],df_summary,
                       by=c("Retailer_Market","Cannibalization_Group","Week_ending"))
      
      #calculate margins and sales per cannibalizing promo row
      df_temp <- mutate(df_temp, Cannibalization_Margin_System = Cannibalization_Share * Cannibalization_Pool_Margin_System)
      df_temp <- mutate(df_temp, Cannibalization_Margin_Manufacturer = Cannibalization_Share * Cannibalization_Pool_Margin_Manufacturer)
      df_temp <- mutate(df_temp, Cannibalization_Margin_Retailer = Cannibalization_Share * Cannibalization_Pool_Margin_Retailer)
      df_temp <- mutate(df_temp, Cannibalization_Sales_Manufacturer = Cannibalization_Share * Cannibalization_Pool_Sales_Manufacturer)
      df_temp <- mutate(df_temp, Cannibalization_Sales_System = Cannibalization_Share * Cannibalization_Pool_Sales_System)
      
      cat(paste0("\n","Cannibalization calculations completed: ",Sys.time()),"\n")
      return(df_temp)
   }
   
   {##test
      #View(B2_4_Cannibalization(df_3_Pantry)[,-c(11:120)],"test")
      # View(B2_4_Cannibalization(df_3_Pantry,as.Date("2013-07-20"),"AHOUSACTA"),"2013-07-20")
   }
   
###### ====================================================================================================== ###
##### 5 Complementary ###########################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### B2_5_Complementary ############################################################################################
   ### calculates complementary effect for one particular SKU on the nonPromo site, i.e. the one that has a volume uplift due to the promo of a complementary SKU
   ### inputs: sel_df is data frame with step 4 completed 
   ###         sel_dfComplRelations table with complementary relations ships
   ###         sel_SKU selected SKU of which the additional volume shall be atrributed to complimentary promo SKUs
   ### output: data frame with complementary values for the selected week_ending+Retailer (to be joined back to main data frame)
   B2_5_Complementary <- function(sel_df,sel_SKU,sel_dfComplRelations=df_ComplementaryRelationships)
   {
      #get only relevant columns of complementary relationships for selected SKU
      df_temp_ComplRelations <- subset(sel_dfComplRelations, Complementary_NonPromo_SKU==sel_SKU)
      #check if there if there is no relevant data
      if(nrow(df_temp_ComplRelations)==0) return(sel_df)
      
      #adjust missing promo flag
      df_temp <- sel_df
      df_temp$Promo_Flag <- ifelse(is.na(df_temp$Promo_Flag),FALSE,df_temp$Promo_Flag)
      
      #get all rows from main data frame that are from selected SKU that has uplift due to complementary effects  
      df_temp1 <- subset(sel_df, Promo_Flag!=1 & Volume_Diff_Obs_BaseAll>0 & SKU ==sel_SKU)
      df_temp_rest <- subset(sel_df, !(Promo_Flag!=1 & Volume_Diff_Obs_BaseAll>0 & SKU ==sel_SKU))
      
      #get all rows from main data frame that may have affected the complementary sales in the selected SKU
      df_temp2 <- subset(df_temp_rest, Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0 & SKU %in% df_temp_ComplRelations$Complementary_Promo_SKU
                         & paste(Retailer_Market,Week_ending) %in% paste(df_temp1$Retailer_Market,df_temp1$Week_ending)) #i.e. only if SKU is is in relationship table, and there is the seleceted SKU for this week and retailer
      df_temp_rest <- subset(df_temp_rest, !(Promo_Flag==1 & Volume_Diff_Obs_BaseAll>0 & SKU %in% df_temp_ComplRelations$Complementary_Promo_SKU
                                             & paste(Retailer_Market,Week_ending) %in% paste(df_temp1$Retailer_Market,df_temp1$Week_ending)))
      
      #check if there if there is no relevant data
      if(nrow(df_temp2)==0) return(sel_df)
      
      #get the MaxRatios (they are temporary for the selected SKU)
      df_temp2 <- select(merge(df_temp2,df_temp_ComplRelations, by.x="SKU",by.y="Complementary_Promo_SKU"),-c(Complementary_NonPromo_SKU))
      
      #union the two relevant of them (rest will be unioned in the end for a complete set)
      df_temp <- plyr::rbind.fill(df_temp1,df_temp2)
      
      #calculate uplift volume that is possible complementary per row
      df_temp <- mutate(df_temp, Complementary_Promo_Volume = Remaining_Uplift * Complementary_MaxRatio)
      df_temp <- mutate(df_temp, Complementary_Potential_Volume = ifelse(SKU==sel_SKU & Volume_Diff_Obs_BaseAll>0,Volume_Diff_Obs_BaseAll,0))
      
      #summarise to weekly level and calculate total possible complementary volume
      df_summary <- df_temp %>% group_by(Retailer_Market,Week_ending) %>%
         summarise(Complementary_Promo_Volume_forComplGroup = sum(ifelse(Promo_Flag==1, pmax(Complementary_Promo_Volume,0,na.rm = TRUE),0)))
      
      #merge to input data
      df_temp <- merge(df_temp[, c("Retailer_Market","Week_ending",setdiff(colnames(df_temp),colnames(df_summary)))],df_summary,
                       by=c("Retailer_Market","Week_ending"))
      
      #calculate volume, margin and sales for the affected SKU sel_SKU
      df_temp <- mutate(df_temp, Complementary_Pool_Volume = ifelse(SKU==sel_SKU, pmin(Complementary_Promo_Volume_forComplGroup,Complementary_Potential_Volume),NA ) ) # i.e. only maximum possible value (either limited to non SKU uplift, or promo Maxration indicated)
      df_temp <- mutate(df_temp, Complementary_Pool_Margin_Manufacturer = ifelse(SKU==sel_SKU,Complementary_Pool_Volume * (Price_Baseline_Manufacturer - Unit_Costs_asUsed),0 ) )
      df_temp <- mutate(df_temp, Complementary_Pool_Margin_System = ifelse(SKU==sel_SKU,Complementary_Pool_Volume * (Price_Baseline_Retail - Unit_Costs_asUsed),0 ) )
      df_temp <- mutate(df_temp, Complementary_Pool_Margin_Retailer = ifelse(SKU==sel_SKU,Complementary_Pool_Margin_System - Complementary_Pool_Margin_Manufacturer,0 ) )
      df_temp <- mutate(df_temp, Complementary_Pool_Sales_Manufacturer = ifelse(SKU==sel_SKU,Complementary_Pool_Volume * Price_Baseline_Manufacturer,0 ) )
      df_temp <- mutate(df_temp, Complementary_Pool_Sales_System = ifelse(SKU==sel_SKU,Complementary_Pool_Volume * Price_Baseline_Retail,0 ) )
      
      #create helping table in oder to append the calculated fields to each promo SKU
      df_summary <- df_temp[df_temp$SKU==sel_SKU,] %>% group_by(Retailer_Market,Week_ending) %>%
         summarise(Complementary_TempSum_Volume = sum(pmax(Complementary_Pool_Volume,0,na.rm = TRUE),0),
                   Complementary_TempSum_Margin_Manufacturer = sum(pmax(Complementary_Pool_Margin_Manufacturer,0,na.rm = TRUE),0),
                   Complementary_TempSum_Margin_System = sum(pmax(Complementary_Pool_Margin_System,0,na.rm = TRUE),0),
                   Complementary_TempSum_Margin_Retailer = sum(pmax(Complementary_Pool_Margin_Retailer,0,na.rm = TRUE),0),
                   Complementary_TempSum_Sales_Manufacturer = sum(pmax(Complementary_Pool_Sales_Manufacturer,0,na.rm = TRUE),0),
                   Complementary_TempSum_Sales_System = sum(pmax(Complementary_Pool_Sales_System,0,na.rm = TRUE),0))
      
      #merge to input data
      df_temp <- merge(df_temp[, c("Retailer_Market","Week_ending",setdiff(colnames(df_temp),colnames(df_summary)))],df_summary,
                       by=c("Retailer_Market","Week_ending"))
      
      #calculate share for promo SKUs as well as the resulting shares of margin and sales (this is recursive, so it should add tp existing values from other complimentary SKUs)
      df_temp <- mutate(df_temp, Complementary_Temp_Share = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0, Complementary_Promo_Volume/Complementary_Promo_Volume_forComplGroup,NA))
      df_temp <- mutate(df_temp, Complementary_Volume = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0,Complementary_Volume + (Complementary_Temp_Share * Complementary_TempSum_Volume),0 ) )
      df_temp <- mutate(df_temp, Complementary_Margin_Manufacturer = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0,Complementary_Margin_Manufacturer + (Complementary_Temp_Share * Complementary_TempSum_Margin_Manufacturer),0 ) )
      df_temp <- mutate(df_temp, Complementary_Margin_System = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0,Complementary_Margin_System + (Complementary_Temp_Share * Complementary_TempSum_Margin_System),0 ) )
      df_temp <- mutate(df_temp, Complementary_Margin_Retailer = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0,Complementary_Margin_Retailer + (Complementary_Temp_Share * Complementary_TempSum_Margin_Retailer),0 ) )
      df_temp <- mutate(df_temp, Complementary_Sales_Manufacturer = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0,Complementary_Sales_Manufacturer + (Complementary_Temp_Share * Complementary_TempSum_Sales_Manufacturer),0 ) )
      df_temp <- mutate(df_temp, Complementary_Sales_System = ifelse(Promo_Flag==1 & Complementary_Promo_Volume>0,Complementary_Sales_System + (Complementary_Temp_Share * Complementary_TempSum_Sales_System),0 ) )
      
      df_temp <- select(df_temp, -c(Complementary_MaxRatio,contains("_Temp")))
      
      df_temp <- plyr::rbind.fill(df_temp,df_temp_rest)
   }
   
   # undebug(B2_5_Complementary)
   # df_test <- B2_5_Complementary(df_4_Cannibalization,"NU.015089" )
   #df_test <- B2_5_Complementary(df_4_Cannibalization,"NU.011659" )
   #df_test <- B2_5_Complementary(df_4_Cannibalization,"COMP NU.004069" )
   
   ### B2_5_Complementary_append_to_df ######################################################################################
   ### covers table of complementary relationships and calculates the resulting values for whole data frame sel_df
   ### inputs: sel_df is data frame with step 4 completed 
   ###         sel_dfComplRelations table with complementary relations ships
   ### output: data frame with complementary values for the selected week_ending+Retailer (to be joined back to main data frame)
   B2_5_Complementary_append_to_df <- function(sel_df,sel_dfComplRelations,TF_printprogress=FALSE)
   {
      cat(paste0("\n","Complementary calculations started: ",Sys.time()))
      
      df_temp <- sel_df
      #initialize values so that in each iteration of the for loop can be added on the existing value (recursively) starting from zero
      df_temp$Complementary_Temp_Share  <- 0
      df_temp$Complementary_Volume  <- 0
      df_temp$Complementary_Margin_Manufacturer  <- 0
      df_temp$Complementary_Margin_System  <- 0
      df_temp$Complementary_Margin_Retailer <- 0
      df_temp$Complementary_Sales_Manufacturer <- 0
      df_temp$Complementary_Sales_System <- 0
      iCount <- 0
      
      #get unique list of NonPromo SKUs in complementarity relationship table
      arComplementary_NonPromo_SKU <- unique(sel_dfComplRelations$Complementary_NonPromo_SKU)
      #and loop throught this list calculating complementary effects for each of them
      for (iSKU in arComplementary_NonPromo_SKU)
      {
         df_temp <- B2_5_Complementary(df_temp,iSKU,sel_dfComplRelations)
         if(TF_printprogress) # optional print out to see how quick things go
         {
            iCount <- iCount+1
            cat(paste0("\n",iCount,"/",length(arComplementary_NonPromo_SKU)," SKUs handled"))
         }
      }
      
      cat(paste0("\n","Complementary calculations completed: ",Sys.time()),"\n")   
      return(df_temp)
   }
   

   
###### ====================================================================================================== ###
##### B1 All Steps ##############################################################################################
###### ------------------------------------------------------------------------------------------------------ ###

   
   ### B1_DoAllSteps ############################################################################################
   ### does all B1 steps
   ### input: data frame 
   ### output: data frame with additional row wise calulations   
   B1_DoAllSteps <- function(sel_df,sel_dfComplementaryRelations)
   {
      df_temp <- sel_df
      ### A
      #1 Initial data check
      df_temp <- A_Initial_Data_Check(df_temp)
      
      ### B1 
      ### calculate Volume_Diff_Obs_BaseAll and baseline all factor 
      df_temp <- mutate(df_temp, Baseline_all_factors_Volume = Volume_Nielsen_Baseline)    
      df_temp <- mutate(df_temp, Volume_Diff_Obs_BaseAll = Volume_Observed - Baseline_all_factors_Volume)
      #3 Pantry loading
      df_temp <- B2_3_PantryLoading_append_to_df(df_temp)
      #4 Cannibalization
      df_temp <- B2_4_Cannibalization_append_to_df(df_temp)
      #5 Complementary
      df_temp <- B2_5_Complementary_append_to_df(df_temp,sel_dfComplementaryRelations,FALSE)

      ### B5
      #1 Row calcs
      df_temp <- B5_RowCalcs(df_temp)
      return(df_temp)
   }

###### ====================================================================================================== ###
##### B2 All Steps ##############################################################################################
###### ------------------------------------------------------------------------------------------------------ ###

   ### B2_DoAllSteps ############################################################################################
   ### does all B2 steps
   ### input: data frame 
   ### output: data frame with additional row wise calulations   
   B2_DoAllSteps <- function(sel_df,sel_dfComplementaryRelations)
   {
      df_temp <- sel_df
      ### A
      #1 Initial data check
      df_temp <- A_Initial_Data_Check(df_temp)
      
      ### B2 
      #1 Baseline
      df_temp <- B2_1_Baseline_append_to_df(df_temp)
      #2 Seasonality
      df_temp <- B2_2_Seasonality_append_to_df(df_temp)
      #3 Pantry loading
      df_temp <- B2_3_PantryLoading_append_to_df(df_temp)
      #4 Cannibalization
      df_temp <- B2_4_Cannibalization_append_to_df(df_temp)
      #5 Complementary
      df_temp <- B2_5_Complementary_append_to_df(df_temp,sel_dfComplementaryRelations,FALSE)

      ### B5
      #1 Row calcs
      df_temp <- B5_RowCalcs(df_temp)
      return(df_temp)
   }   
   
}#<-- end Functions section

