#################################################################################################################
### Purpose: R code for Catalyst Promo Effectiveness code (wip)
###          General functions
### Author: QNV
### Date: 2016/11/11
### Version: v09
#################################################################################################################


###### ====================================================================================================== ###
##### 0 user inputs and initialization ##########################################################################
###### ------------------------------------------------------------------------------------------------------ ###

   ### packages
   {
      if(!require("plyr")){
         install.packages("tidyverse")
         library(tidyverse)
      }
      detach(package:plyr) #remove it because it yields issues with tidyverse; however, still need to make sure it's installed for rbind.fill
      
      if(!require("tidyverse")){
         install.packages("tidyverse")
         library(tidyverse)
      }
     # @Ti I added my package here
     if(!require("caTools")){
       install.packages("caTools")
       library(caTools)
     }     
     if(!require("sqldf")){
       install.packages("sqldf")
       library(sqldf)
     }     
     if(!require("dplyr")){
       install.packages("dplyr")
       library(dplyr)
     }     
     
     if(!require("stargazer")){
       install.packages("stargazer")
       library(stargazer)
     }     
     
   }

###### ====================================================================================================== ###
##### data check and imputing  (WIP) ############################################################################
###### ------------------------------------------------------------------------------------------------------ ###

   ### A_Helper_MissingNumericValues ############################################################################
   ### helper function to convert fields into numeric and impute zero for NA 
   ### input:  sel_df: data frame, 
   ###         sel_String: field name as string
   ###         TF_OnlyPromo: explains if it shall be applied to a promo weeks only or to all fields
   ### output: data frame with corrected field
   A_Helper_MissingNumericValues <- function(sel_df, sel_String, TF_OnlyPromo=TRUE){
      df_return <- sel_df
      df_return[,sel_String] <- as.numeric(df_return[,sel_String])
      if(TF_OnlyPromo)
         number_of_NAs <- sum(is.na(df_return[df_return$Promo_Flag==1,sel_String]))
      else
         number_of_NAs <- sum(is.na(df_return[,sel_String]))
      
      
      if(number_of_NAs>0){
         message(paste0("There are missing or non-numeric values in field ", sel_String, ". These (", number_of_NAs, ") values will be replaced by 0."))
         df_return[is.na(df_return[sel_String]),sel_String] <- 0
         count_warnings <<- count_warnings+1
         }
      invisible(df_return)
   }
   
   ### A_Initial_Data_Check #################################################################################################
   ### checks raw input for flaws and corrects them or returns an error if user action is required. (to be finished)
   A_Initial_Data_Check <- function(sel_df){
      cat(paste0("\n","Data check (work in progress) started: ",Sys.time()))
      
      count_warnings <<- 0
      
      df_temp <- sel_df
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Flyer_Front_Manufacturer")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Flyer_Back_Manufacturer")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Flyer_Inside_Manufacturer")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Other_Promo_Cost_Retailer")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Other_Promo_Cost_Manufacturer")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Other_Retailer_Funding")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Variable_Cost_Manufacturer_1")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Variable_Cost_Manufacturer_2")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Variable_Cost_Manufacturer_3")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Fixed_Cost_Manufacturer_1")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Fixed_Cost_Manufacturer_2")
      df_temp <- A_Helper_MissingNumericValues(df_temp,"Fixed_Cost_Manufacturer_3")
      
            
      if(sum(is.na(df_temp[,"Unit_Costs"])>0)){
         message(paste0("\nThere are ", sum(is.na(df_temp[df_temp$Promo_Flag==1,"Unit_Costs"])), " records with missing Unit Costs. Procedures will use SKU average if available."))
         count_warnings <<- count_warnings+1
      }
      ### Caclculate Avg_SKU_Unit_Costs
      df_helper <- df_temp %>% group_by(SKU) %>%
         summarise(Avg_SKU_Unit_Costs=mean(Unit_Costs,na.rm=TRUE))
      df_temp <- merge(df_temp,df_helper,by=c("SKU"))
      df_temp <- mutate(df_temp, Unit_Costs_asUsed = ifelse(is.na(Unit_Costs),Avg_SKU_Unit_Costs,Unit_Costs))  
      
      int_Missing <- length(unique(df_temp$SKU[is.na(df_temp$Unit_Costs_asUsed)]))
      if(int_Missing>0){
         #stop(paste0("\nThere are records with missing Unit Costs and no SKU average available (see Missing_UnitCosts.csv) Procedure aborted!"))

         write.csv(unique(df_temp$SKU[is.na(df_temp$Unit_Costs_asUsed)]), paste0(Output_Directory,"Missing_UnitCosts",format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%Hh%M.csv")))
         df_temp$Unit_Costs_asUsed[is.na(df_temp[,"Unit_Costs_asUsed"])] <- 0
         message(paste0("\n\n+++++++++++++++++++++++++++++++ Warning +++++++++++++++++++++++++++++++\n","\nThere are ", int_Missing,
                        " SKUs with no Unit Costs and no average available. \nThese missing values are replaced by 0 => Margins cannot be calculated correctly for these SKUs.\n",
                        "Check Missing_UnitCosts",format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%Hh%M.csv")," for a list of SKUs.\n",
                        "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"))
         count_warnings <<- count_warnings+1
      }
      
      if(!("RecordID" %in% colnames(df_temp))) df_temp$RecordID <-  1:nrow(df_temp)
      if(count_warnings>0)
         cat(paste0("\n","Data check (work in progress) completed (with ", count_warnings, " WARNINGS! Check messages above.): ",Sys.time(),"\n"))
      else
         cat(paste0("\n","Data check (work in progress) completed (no warnings): ",Sys.time(),"\n"))         
      invisible(select(df_temp,-Avg_SKU_Unit_Costs))
   }


###### ====================================================================================================== ###
##### B5 Row wise calcs  ########################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### B5_RowCalcs #################################################################################################
   ### does row wise calculations
   ### input: data frame after step 2
   ### output: data frame with additional row wise calulations
   B5_RowCalcs <- function(sel_df)
   {
      cat(paste0("\n","Additional row wise calculations started: ",Sys.time()),"\n")
      
      df_return <- sel_df
      
      ## baseline related measures     
      df_return <- mutate(df_return, Baseline_all_factors_Margin_Manufacturer = Baseline_all_factors_Volume * (Price_Baseline_Manufacturer - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Baseline_all_factors_Margin_Retail = Baseline_all_factors_Volume * (Price_Baseline_Retail - Price_Baseline_Manufacturer))
      df_return <- mutate(df_return, Baseline_all_factors_Margin_System = Baseline_all_factors_Volume * (Price_Baseline_Retail - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Baseline_all_factors_Sales_Manufacturer = Baseline_all_factors_Volume * Price_Baseline_Manufacturer)
      df_return <- mutate(df_return, Baseline_all_factors_Sales_System = Baseline_all_factors_Volume * Price_Baseline_Retail)
      
      ##uplifts
      df_return <- mutate(df_return, Observed_Sales_Manufacturer = Volume_Observed * ifelse(Promo_Flag !=1 | is.na(Promo_Flag),Price_Baseline_Manufacturer,Price_Promo_Manufacturer))
      df_return <- mutate(df_return, Observed_Sales_Retailer = Volume_Observed * ifelse(Promo_Flag !=1 | is.na(Promo_Flag),Price_Baseline_Retail,Price_Promo_Retail))
      df_return <- mutate(df_return, Observed_Sales_System = Observed_Sales_Retailer)
      df_return <- mutate(df_return, Uplift_Volume = ifelse(Promo_Flag == 1,Volume_Observed - Baseline_all_factors_Volume,NA))
      
      df_return <- mutate(df_return, Uplift_Margin_Retailer = Uplift_Volume * (Price_Baseline_Retail - Price_Baseline_Manufacturer))
      df_return <- mutate(df_return, Uplift_Margin_System = Uplift_Volume * (Price_Baseline_Retail - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Uplift_Margin_Manufacturer = Uplift_Volume * (Price_Baseline_Manufacturer - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Uplift_Revenue_Manufacturer = Uplift_Volume * Price_Baseline_Manufacturer)
      df_return <- mutate(df_return, Uplift_Revenue_System = Uplift_Volume * Price_Baseline_Retail)
      df_return <- mutate(df_return, Net_Uplift_Margin_Manufacturer = Uplift_Volume * (Price_Promo_Manufacturer - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Net_Uplift_Margin_Retailer = Uplift_Volume * (Price_Promo_Retail - Price_Promo_Manufacturer))
      df_return <- mutate(df_return, Net_Uplift_Margin_System = Uplift_Volume * (Price_Promo_Retail - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Net_Uplift_Revenue_Manufacturer = Uplift_Volume * Price_Promo_Manufacturer)
      df_return <- mutate(df_return, Net_Uplift_Revenue_System = Uplift_Volume * Price_Promo_Retail)
      
      #Waterfall Calcs
      df_return <- mutate(df_return, Baseline_Margin_Manufacturer = Baseline_all_factors_Volume * (Price_Baseline_Manufacturer - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Baseline_Margin_System = Baseline_all_factors_Volume * (Price_Baseline_Retail - Unit_Costs_asUsed))
      df_return <- mutate(df_return, Baseline_Margin_Retailer = Baseline_all_factors_Volume * (Price_Baseline_Retail - Price_Baseline_Manufacturer))
      
      #Invested margins, and costs
      df_return <- mutate(df_return, Invested_Margin_Discounts_Manufacturer = ifelse(Promo_Flag==1, Volume_Observed * (Price_Baseline_Manufacturer - Price_Promo_Manufacturer), NA))
      df_return <- mutate(df_return, Invested_Margin_Discounts_System = ifelse(Promo_Flag==1, Volume_Observed * (Price_Baseline_Retail - Price_Promo_Retail), NA))
      df_return <- mutate(df_return, Invested_Margin_Discounts_Retailer = ifelse(Promo_Flag==1, Invested_Margin_Discounts_System - Invested_Margin_Discounts_Manufacturer, NA))
      df_return <- mutate(df_return, Other_Promo_Cost_System = Other_Promo_Cost_Manufacturer + Other_Promo_Cost_Retailer)
      df_return <- mutate(df_return, Flyer_Total_Manufacturer = Flyer_Front_Manufacturer + Flyer_Back_Manufacturer + Flyer_Inside_Manufacturer)
      df_return <- mutate(df_return, Total_Variable_Costs = (Variable_Cost_Manufacturer_1 + Variable_Cost_Manufacturer_2 + Variable_Cost_Manufacturer_3) * Volume_Observed)
      df_return <- mutate(df_return, Total_Fixed_Costs = Fixed_Cost_Manufacturer_1 + Fixed_Cost_Manufacturer_2 + Fixed_Cost_Manufacturer_3)

      
      df_return <- mutate(df_return, Total_Promo_Cost_Retailer = Invested_Margin_Discounts_Retailer + Other_Promo_Cost_Retailer + Other_Retailer_Funding)
      df_return <- mutate(df_return, Total_Promo_Cost_Manufacturer = Invested_Margin_Discounts_Manufacturer + Total_Variable_Costs + Total_Fixed_Costs + Flyer_Total_Manufacturer + Other_Promo_Cost_Manufacturer - Other_Retailer_Funding)
      df_return <- mutate(df_return, Total_Promo_Cost_System = Total_Promo_Cost_Manufacturer + Total_Promo_Cost_Retailer)
      
      #incremental margins/sales
      df_return <- mutate(df_return, Incremental_Margin_Manufacturer = Uplift_Margin_Manufacturer - Cannibalization_Margin_Manufacturer - Pantry_Loading_Margin_Manufacturer + Complementary_Margin_Manufacturer - Total_Promo_Cost_Manufacturer)
      df_return <- mutate(df_return, Incremental_Margin_System = Uplift_Margin_System - Cannibalization_Margin_System - Pantry_Loading_Margin_System + Complementary_Margin_System - Total_Promo_Cost_System)
      df_return <- mutate(df_return, Incremental_Margin_Retailer = Incremental_Margin_System - Incremental_Margin_Manufacturer)
      df_return <- mutate(df_return, Incremental_Revenue_System = Uplift_Revenue_System - Cannibalization_Sales_System - Pantry_Loading_Sales_System + Complementary_Sales_System)
      df_return <- mutate(df_return, Incremental_Revenue_Manufacturer = Uplift_Revenue_Manufacturer - Cannibalization_Sales_Manufacturer - Pantry_Loading_Sales_Manufacturer + Complementary_Sales_Manufacturer)
      
      #EU conversions
      #df_return <- mutate(df_return, Baseline_Raw_Volume_EU = Baseline_Raw_Volume * EU_conversion_factor)
      # df_return <- mutate(df_return, Baseline_all_factors_Volume_EU = Baseline_all_factors_Volume * EU_conversion_factor)
      # df_return <- mutate(df_return, Uplift_Volume_EU = Uplift_Volume * EU_conversion_factor)
      # df_return <- mutate(df_return, Pantry_Loading_Volume_EU = Pantry_Loading_Volume * EU_conversion_factor)
      # df_return <- mutate(df_return, Cannibalization_Pool_Volume_EU = Cannibalization_Pool_Volume * EU_conversion_factor)
      # df_return <- mutate(df_return, Cannibalization_Volume_EU = Cannibalization_Volume * EU_conversion_factor)
      # 
      cat(paste0("Additional row wise calculations completed:",Sys.time()),"\n")  
      
      return(df_return)
   }
   
###### ====================================================================================================== ###
##### Output ####################################################################################################
###### ------------------------------------------------------------------------------------------------------ ###
   
   ### C_Output #################################################################################################
   ### does row wise calculations
   ### input: data frame 
   ### output: writes csv to output folder
   
   C_Output <- function(sel_df, sel_filename_beginning = "output"){
      chrOutputFileName <- paste0(sel_filename_beginning,"_",format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%Hh%M.csv"))
      sel_df$Promo_Flag <- ifelse(sel_df$Promo_Flag==1, 1, 0)
      write.csv(sel_df, file = chrOutputFileName, row.names=FALSE, na="")
      cat(paste0("\n","Output was written to file:\n",chrOutputFileName,"\n"))
   }
   
###### ====================================================================================================== ###
##### Run analyses ##############################################################################################
###### ------------------------------------------------------------------------------------------------------ ###

   ### Run_Promo_Effectiveness ##################################################################################
   ### runs analyses and outputs result df in a csv
   ### input: raw input
   ### output: data frame with additional row wise calulations 
   Run_Promo_Effectiveness <- function(inputData, ComplRelationsData, arrOutputFields, arrAdditionalFields, method ="(missing)", ...)
   {
      if(method == "B1"){
         ### B1 ###
         df_temp <- B1_DoAllSteps(inputData,df_ComplementaryRelationships)
      }
      else if(method == "B2"){
         ### B2 ###
         df_temp <- B2_DoAllSteps(inputData,df_ComplementaryRelationships)
      }
      else if(method == "B3"){
         ### B3 ###
         df_temp <- B3_DoAllSteps(inputData,df_ComplementaryRelationships, ...)
      }
      else
         stop(paste0("Method ", method, " not found. Try \"B1\", \"B2\" or \"B3\" (\"B4\" to be developed)"))
      
      df_temp <- df_temp[,c(intersect(arrOutputFields,colnames(df_temp)), arrAdditionalFields)]
      
      # output result
      C_Output(df_temp,paste0(Output_Directory,"Output_",method,"_"))
         
      invisible(df_temp)
   }   
      # output in a Tableau friendly form
      # df_test <- df_output
      # for(j in 1:length(df_test))
      # {
      #    if(is.numeric(df_test[,j]))
      #       {
      #       #cat(paste0("Numeric: ",colnames(df_test[,j]),j,"\n"))
      #          df_test[,j] <- ifelse(is.na(df_test[,j]),0, df_test[,j])
      #          df_test[,j] <- format(round(df_test[,j],4), scientific = FALSE)
      #          df_test[,j] <- trimws(df_test[,j])
      #       }
      #    
      # }
      # chrOutputFileName <- paste0(Output_Directory,"df_output_Impute0","_",format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%Hh%M.csv"))
      # write.csv(df_test, file = chrOutputFileName, row.names=FALSE, na="")