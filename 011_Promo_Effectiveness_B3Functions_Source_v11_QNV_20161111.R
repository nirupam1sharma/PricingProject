#################################################################################################################
### Purpose: R code for Catalyst Promo Effectiveness code
###          B3 functions
### Author: Nirupam Sharma
### Date: 2016/11/11
### Version: v11
#################################################################################################################

#################################################################################################################
###                                          B3 Calculations                                                  ###
### ========================================================================================================= ###


### ========================================================================================================= ###
###  0 user inputs and initialization  ###
### --------------------------------------------------------------------------------------------------------- ###
   ### diverse input values
   {
      inp_B3StockUpWeeks <- 2 # weeks after a promo which are considered for pantry loading in B3
      inp_B3_StockUpAverageRangeWeeks <- 3  # number of weeks that are considered for the average (+- in both directions)
      inp_B3_StockUpThreshold_Hierarchy3 <- 10  # number of minimum sample size on stock up hierarchy level 3 to use the corresponding stock up factor
      inp_B3_StockUpThreshold_Hierarchy2 <- 15  # number of minimum sample size on stock up hierarchy level 2 to use the corresponding stock up factor
      inp_B3_StockUpThreshold_Hierarchy1 <- 25  # number of minimum sample size on stock up hierarchy level 1 to use the corresponding stock up factor
      
      inp_B3_CanniAverageRangeWeeks <- 2 # range of weeks for cannibalization base average (+-)
      inp_B3_CanniThreshold_Hierarchy3 <- 50  # number of minimum sample size on cannibalization hierarchy level 3 to use the corresponding stock up factor
      inp_B3_CanniThreshold_Hierarchy2 <- 50  # number of minimum sample size on cannibalization hierarchy level 2 to use the corresponding stock up factor
      inp_B3_CanniThreshold_Hierarchy1 <- 50  # number of minimum sample size on cannibalization hierarchy level 1 to use the corresponding stock up factor

   }
### ========================================================================================================= ###
###                                0 Week flagging                                                            ###
### --------------------------------------------------------------------------------------------------------- ###

### B3_0_WeekIndicator -------------------------------------------------------------------------------------- ###
### takes the raw input and calculates the flags for each week:
###   C: clean week-no stock-up or cannibalization
###   D: dirty week-another SKU in the same cannibalization group is on promo
###   P: promo week-the SKU in question is on promo
###   S1: stock-up week one-the week immediately after a promo period ends
###   S2: stock-up week two-the second week immediately after a promo period ends
###   NOTE: weeks with sotck-up and cannibalization are marked as S1 or S2 with Promo_Flag_inCanniGroup = 1
   
   B3_0_WeekIndicator <- function(sel_df){
      cat(paste0("\n","Week Indicator calculations started: ",Sys.time()))
      
      df_temp <- arrange(sel_df, Retailer_Market, SKU, Week_ending)
     
      # adjust promo flag
      df_temp$Promo_Flag <- ifelse(is.na(df_temp$Promo_Flag), 0, df_temp$Promo_Flag)
       
      # initialize week flag
      df_temp$B3_WeekIndicator <- ifelse(df_temp$Promo_Flag == 1, "P", NA)
      df_temp$Promo_Flag_group <- NA
      
      # flag stock up weeks and use same loop to flag which stock-up weeks belong to which promotions
      iCounter <- 0 # counter for stock up weeks
      iPromo_Flag_Group_Flag_counter <- 0
      df_temp$Promo_Flag_group[1] <- ifelse(df_temp$Promo_Flag[1]==1,"P_0",NA)
             
      for(i in 2:nrow(df_temp)){
         # Stock up flag
         if( is.na(df_temp$B3_WeekIndicator[i])                         # no week indicator yet
             & df_temp$SKU[i-1] == df_temp$SKU[i]                       # still the same SKU
             & df_temp$Retailer_Market[i-1] == df_temp$Retailer_Market[i] # still same Retailer_Market
             & !is.na(df_temp$B3_WeekIndicator[i-1]))                     # the previous week has an indicator (either promo or S1,S2...)
         {
            if (df_temp$B3_WeekIndicator[i-1] == "P"){ 
               iCounter <- 0                                              # reset counter 
            }
            iCounter <- iCounter + 1  
            if(iCounter<=inp_B3StockUpWeeks & df_temp$Week_ending[i-1]+7*inp_B3StockUpWeeks >= df_temp$Week_ending[i])
               df_temp$B3_WeekIndicator[i] <- paste0("S",iCounter)        # set to stock up week
            else
               iCounter <- 0                                              # start over 
         }
         
         #Promo_Flag_group flag
         if( !is.na(df_temp$B3_WeekIndicator[i]))                         # the actual week has an indicator (either promo or S1,S2...)
         {
            if((df_temp$Promo_Flag[i-1]!=1 & df_temp$Promo_Flag[i]==1) | # new promo
               (df_temp$Week_ending[i-1]+14< df_temp$Week_ending[i])  |  # or breaker larger than 2 weeks
                df_temp$SKU[i-1] != df_temp$SKU[i] |                     # or new SKU
                df_temp$Retailer_Market[i-1] != df_temp$Retailer_Market[i]) # or new retailer Retailer_Market
               iPromo_Flag_Group_Flag_counter <- iPromo_Flag_Group_Flag_counter+1   

            df_temp$Promo_Flag_group[i] <- paste0("P_",iPromo_Flag_Group_Flag_counter)
         }
      }
            
      # flag dirty weeks due to canni
      df_tempCanni <- df_temp %>% group_by(Retailer_Market, Cannibalization_Group, Week_ending) %>%
                               summarise(Promo_Flag_inCanniGroup=max(Promo_Flag))
      df_temp <- merge(df_temp,df_tempCanni, by=c("Retailer_Market", "Cannibalization_Group", "Week_ending"))
      df_temp$B3_WeekIndicator <- ifelse(df_temp$Promo_Flag_inCanniGroup == 1 & is.na(df_temp$B3_WeekIndicator), "D",df_temp$B3_WeekIndicator )
      df_temp$Promo_Flag_group <- ifelse(df_temp$B3_WeekIndicator == "D", NA , df_temp$Promo_Flag_group)
      
      
      # flag others remaining weeks as clean ones
      df_temp$B3_WeekIndicator <- ifelse(is.na(df_temp$B3_WeekIndicator) == 1, "C", df_temp$B3_WeekIndicator)
      
      cat(paste0("\n","Week Indicator calculations completed: ",Sys.time(),"\n"))
      return(df_temp)
   }
   
### ========================================================================================================= ###
###                                Pantry Loading or Stock-up correction                                      ###
### --------------------------------------------------------------------------------------------------------- ###

### B3_1_PantryLoading_Correction --------------------------------------------------------------------------- ###
### requires Week_indicators from B3_0_WeekIndicator
### calculates and append pantry loading/stock up values for input data frame
### input:  sel_df: data frame with Weekindicator
###         inp_B3_FactorQuantileThreshold: at this quantile the Su factors are cut off
   
   B3_1_PantryLoading_Correction <- function(sel_df, inp_B3_FactorQuantileThreshold = 0.9 ){
      
      cat(paste0("\n","Pantry Loading correction calculations started: ",Sys.time()))
      
      df_temp <- arrange(sel_df, Retailer_Market, SKU, Week_ending)
     
      # adjust promo flag
      df_temp$Promo_Flag <- ifelse(is.na(df_temp$Promo_Flag), 0, df_temp$Promo_Flag)
 
      # initialize Stock_Up average
      df_temp$StockUp_BaseAverage <- NA

      # calculate individual averages for each stock-up week (also dirty ones) with enough (2 or more) clean (C) weeks around it
      # weeks with not enough clean weeks are not considered (neither calculation nor application of correction factors) in this analysis
      for(i in 2:nrow(df_temp)){
         if( substr(df_temp$B3_WeekIndicator[i],1,1)=="S")   # i.e. is flagged as stock up weeks
         {
            df_tmpHelper <- filter(df_temp, Week_ending <= df_temp$Week_ending[i]+inp_B3_StockUpAverageRangeWeeks*7 & 
                           Week_ending >= df_temp$Week_ending[i]-inp_B3_StockUpAverageRangeWeeks*7 &
                           B3_WeekIndicator == "C" & SKU == df_temp$SKU[i] & Retailer_Market == df_temp$Retailer_Market[i] )
            if(nrow(df_tmpHelper)>1){ # i.e. 2 or more clean weeks at least
               df_temp$StockUp_BaseAverage[i] <- mean(df_tmpHelper$Volume_Observed,na.rm=TRUE)
            }
         }
      }
      
      # stock up deviation for each stock-up week (also dirty ones, but only if base average exists, i.e. # clean weeks>1)
      df_temp$StockUp_Deviation <- pmax(df_temp$StockUp_BaseAverage - df_temp$Volume_Observed,0)
      
      # determine which stock up weeks are for factor calculation and application
      df_temp$StockUp_CorrectionFactorCalculation <- ifelse(substr(df_temp$B3_WeekIndicator,1,1)=="S" & !is.na(df_temp$StockUp_Deviation) & df_temp$StockUp_Deviation>0 & df_temp$Promo_Flag_inCanniGroup==0 ,TRUE,FALSE)
      df_temp$StockUp_CorrectionFactorApplication <- ifelse(substr(df_temp$B3_WeekIndicator,1,1)=="S" & !is.na(df_temp$StockUp_Deviation) & df_temp$StockUp_Deviation>0,TRUE,FALSE)
      # remove deviation for weeks that are not used for calculation of factors
      df_temp$StockUp_Deviation <- ifelse(df_temp$StockUp_CorrectionFactorCalculation, df_temp$StockUp_Deviation, NA)
      
      # stock up factors 
      #- B3_Hierarchy_3
      df_tmpSUF1 <- df_temp %>% group_by(B3_Hierarchy_3,Retailer_Market,B3_WeekIndicator) %>%
                                summarise(StockUp_SuFactor_Hier_3 = sum(StockUp_Deviation,na.rm=TRUE)/sum(ifelse(!is.na(StockUp_Deviation) & StockUp_Deviation>0, Volume_Observed,0)),
                                          StockUp_SuFactor_Hier_3_count = sum(!is.na(StockUp_Deviation) & StockUp_Deviation>0))
      df_temp <-  merge(df_temp,df_tmpSUF1, by=c("B3_Hierarchy_3", "Retailer_Market", "B3_WeekIndicator"))
      #- B3_Hierarchy_2
      df_tmpSUF1 <- df_temp %>% group_by(B3_Hierarchy_2,Retailer_Market,B3_WeekIndicator) %>%
                                summarise(StockUp_SuFactor_Hier_2 = sum(StockUp_Deviation,na.rm=TRUE)/sum(ifelse(!is.na(StockUp_Deviation) & StockUp_Deviation>0, Volume_Observed,0)),
                                          StockUp_SuFactor_Hier_2_count = sum(!is.na(StockUp_Deviation) & StockUp_Deviation>0))
      df_temp <-  merge(df_temp,df_tmpSUF1, by=c("B3_Hierarchy_2","Retailer_Market", "B3_WeekIndicator"))
      #- B3_Hierarchy_1
      df_tmpSUF1 <- df_temp %>% group_by(B3_Hierarchy_1,Retailer_Market,B3_WeekIndicator) %>%
                                summarise(StockUp_SuFactor_Hier_1 = sum(StockUp_Deviation,na.rm=TRUE)/sum(ifelse(!is.na(StockUp_Deviation) & StockUp_Deviation>0, Volume_Observed,0)),
                                          StockUp_SuFactor_Hier_1_count = sum(!is.na(StockUp_Deviation) & StockUp_Deviation>0))
      df_temp <-  merge(df_temp,df_tmpSUF1, by=c("B3_Hierarchy_1","Retailer_Market", "B3_WeekIndicator"))

      
      # determine used stock up factors 
      df_temp <- mutate(df_temp, StockUp_SuFactor_Used = ifelse(StockUp_CorrectionFactorApplication, # apply factors only for deviation > 0
                                                            ifelse(StockUp_SuFactor_Hier_3_count>= inp_B3_StockUpThreshold_Hierarchy3,
                                                                  StockUp_SuFactor_Hier_3,
                                                            ifelse(StockUp_SuFactor_Hier_2_count>= inp_B3_StockUpThreshold_Hierarchy2,
                                                                  StockUp_SuFactor_Hier_2,
                                                            ifelse(StockUp_SuFactor_Hier_1_count>= inp_B3_StockUpThreshold_Hierarchy1,
                                                                  StockUp_SuFactor_Hier_1,
                                                                  NA))) # last else case, i.e. no stock up correction
                                                            ,NA) 
                        )
  
      # take the 90% percentile of StockUp_SuFactos_Used and cut off at this value, and cap the used stock-up factors
      df_temp$StockUp_SuFactor_Used_90thPercentile <- quantile(df_temp$StockUp_SuFactor_Used, probs=inp_B3_FactorQuantileThreshold, na.rm=TRUE, type=2)
      df_temp$StockUp_SuFactor_Used <- pmin(df_temp$StockUp_SuFactor_Used,df_temp$StockUp_SuFactor_Used_90thPercentile)
      
      # calculate correction volume for SKU+weeks that eventually have a Stock-up factor (i.e. valid for application and meet one of sample size thresholds)
      df_temp <- mutate(df_temp, StockUp_CorrectionVolume = ifelse(is.na(StockUp_SuFactor_Used), 0,StockUp_SuFactor_Used * Volume_Observed))
 
      cat(paste0("\n","Pantry Loading correction calculations completed: ",Sys.time(),"\n"))
      
      return(df_temp)
   }

### ========================================================================================================= ###
###                                Cannibalization correction                                                 ###
### --------------------------------------------------------------------------------------------------------- ###

### B3_3_Cannibalization_Correction ------------------------------------------------------------------------- ###
### requires Week_indicators from B3_0_WeekIndicator
### calculates and append cannibalization values for input data frame
   
   B3_2_Cannibalization_Correction <- function(sel_df, inp_B3_FactorQuantileThreshold = 0.9 ){
      cat(paste0("\n","Cannibalization correction calculations started: ",Sys.time()))
      
      df_temp <- arrange(sel_df, Retailer_Market, Week_ending,Cannibalization_Group, SKU)
      
      # adjust promo flag
      df_temp$Promo_Flag <- ifelse(is.na(df_temp$Promo_Flag), 0, df_temp$Promo_Flag)
      
      # get per CG+Retailer+Week: number of SKUs in Promo, total number of SKUs 
      df_summary <- df_temp %>% group_by(Retailer_Market, Cannibalization_Group, Week_ending) %>% 
                                 summarise( number_of_SKUs_in_CG_in_Promo = sum(Promo_Flag),
                                            number_of_SKUs_in_CG_total = n())
      
      # calculate Share of SKUs in promo in CG
      df_summary$Pct_of_CG_in_Promo <- df_summary$number_of_SKUs_in_CG_in_Promo/df_summary$number_of_SKUs_in_CG_total
      
      # join values back to df_temp
      df_temp <- merge(df_temp,df_summary, by=c("Retailer_Market", "Cannibalization_Group", "Week_ending"))
      df_temp$Canni_BaseAverage <- NA

      # calculate individual averages for each cannibalized week
      for(i in 1:nrow(df_temp)){
         if( df_temp$Promo_Flag[i]==0 & df_temp$Promo_Flag_inCanniGroup[i]==1 & df_temp$Pct_of_CG_in_Promo[i]>=0.1 )   
            # i.e. SKU not in promo, but some other in that week (dirty + relevant S1, S2) which meet the requirement of at least 10% in promo
         {
            df_tmpHelper <- filter(df_temp, Week_ending <= df_temp$Week_ending[i]+inp_B3_CanniAverageRangeWeeks*7 & 
                                    Week_ending >= df_temp$Week_ending[i]-inp_B3_CanniAverageRangeWeeks*7 &
                                    B3_WeekIndicator == "C" & SKU == df_temp$SKU[i] & Retailer_Market == df_temp$Retailer_Market[i] )
            if(nrow(df_tmpHelper)>1){ # at least 2 clean weeks
               df_temp$Canni_BaseAverage[i] <- mean(df_tmpHelper$Volume_Observed,na.rm=TRUE)
            }
         }
      }
      
      # Canni deviation per canni week
      df_temp$Canni_Deviation <- (pmax(df_temp$Canni_BaseAverage - df_temp$Volume_Observed,0))/df_temp$number_of_SKUs_in_CG_in_Promo
      
      # determine which canni weeks are for factor calculation and application
      df_temp$Canni_CorrectionFactorCalculation <- ifelse(df_temp$B3_WeekIndicator == "D" & !is.na(df_temp$Canni_Deviation) & df_temp$Canni_Deviation>0  ,TRUE,FALSE)
      df_temp$Canni_CorrectionFactorApplication <- ifelse(df_temp$B3_WeekIndicator == "D" & !is.na(df_temp$Canni_Deviation) & df_temp$Canni_Deviation>0  ,TRUE,FALSE)
      
      # remove deviation for weeks that are not used for calculation of factors
      df_temp$Canni_Deviation <- ifelse(df_temp$Canni_CorrectionFactorCalculation, df_temp$Canni_Deviation, NA)
      
      ### Canni factors
      #- B3_Hierarchy_3
      df_tmpSUF1 <- df_temp %>% group_by(B3_Hierarchy_3, Retailer_Market, Promo_Flag, Promo_Flag_inCanniGroup) %>%
                                summarise(Canni_Factor_Hier_3 =  sum(Canni_Deviation,na.rm=TRUE)/sum(ifelse(!is.na(Canni_Deviation) & Canni_Deviation>0, Volume_Observed,0)),
                                          Canni_Factor_Hier_3_count = sum(!is.na(Canni_Deviation) & Canni_Deviation>0))
      df_temp <-  merge(df_temp,df_tmpSUF1, by=c("B3_Hierarchy_3", "Retailer_Market", "Promo_Flag", "Promo_Flag_inCanniGroup"))
      #- B3_Hierarchy_2
      df_tmpSUF1 <- df_temp %>% group_by(B3_Hierarchy_2, Retailer_Market, Promo_Flag, Promo_Flag_inCanniGroup) %>%
                                summarise(Canni_Factor_Hier_2 =  sum(Canni_Deviation,na.rm=TRUE)/sum(ifelse(!is.na(Canni_Deviation) & Canni_Deviation>0, Volume_Observed,0)),
                                          Canni_Factor_Hier_2_count = sum(!is.na(Canni_Deviation) & Canni_Deviation>0))
      df_temp <-  merge(df_temp,df_tmpSUF1,  by=c("B3_Hierarchy_2", "Retailer_Market", "Promo_Flag", "Promo_Flag_inCanniGroup"))
      #- B3_Hierarchy_1
      df_tmpSUF1 <- df_temp %>% group_by(B3_Hierarchy_1, Retailer_Market, Promo_Flag, Promo_Flag_inCanniGroup) %>%
                                summarise(Canni_Factor_Hier_1 =  sum(Canni_Deviation,na.rm=TRUE)/sum(ifelse(!is.na(Canni_Deviation) & Canni_Deviation>0, Volume_Observed,0)),
                                          Canni_Factor_Hier_1_count = sum(!is.na(Canni_Deviation) & Canni_Deviation>0))
      df_temp <-  merge(df_temp,df_tmpSUF1,  by=c("B3_Hierarchy_1", "Retailer_Market", "Promo_Flag", "Promo_Flag_inCanniGroup"))


      # determine used canni factors
      df_temp <- mutate(df_temp, Canni_Factor_Used = ifelse(Canni_CorrectionFactorApplication, # apply factors only for deviation > 0
                                                       ifelse(Canni_Factor_Hier_3_count >= inp_B3_CanniThreshold_Hierarchy3,
                                                               Canni_Factor_Hier_3,
                                                       ifelse(Canni_Factor_Hier_2_count >= inp_B3_CanniThreshold_Hierarchy2,
                                                               Canni_Factor_Hier_2,
                                                       ifelse(Canni_Factor_Hier_1_count >= inp_B3_CanniThreshold_Hierarchy1,
                                                               Canni_Factor_Hier_1,
                                                               NA))), # last else case, i.e. canni up correction
                                                            NA)
                        )

      
      # take the 90% percentile of Canni_Factors_Used and cut off at this value, and cap the used stock-up factors
      df_temp$Canni_Factor_Used_90thPercentile <- quantile(df_temp$Canni_Factor_Used, probs=inp_B3_FactorQuantileThreshold, na.rm=TRUE, type=2)
      df_temp$Canni_Factor_Used <- pmin(df_temp$Canni_Factor_Used,df_temp$Canni_Factor_Used_90thPercentile)
      
      # calculate adjusted volume
      df_temp <- mutate(df_temp, Canni_CorrectionVolume = ifelse(is.na(Canni_Factor_Used),0, Canni_Factor_Used * number_of_SKUs_in_CG_in_Promo * Volume_Observed))

      cat(paste0("\n","Cannibalization correction calculations completed: ",Sys.time(),"\n"))
      return(df_temp)
   }

   
### ========================================================================================================= ###
###                                B3 Baseline                                                                ###
### --------------------------------------------------------------------------------------------------------- ### 
### B3_3_Baseline_perGroup ---------------------------------------------------------------------------------- ###
### requires data frame with correction factors 
### calculates and appends baseline for/to input data frame
### input:  sel_df: data frame with all correction values
    B3_3_Baseline_perGroup <- function(sel_df,sel_SKU, sel_Retailer){  
      
      # initialize
      df_temp <- filter(sel_df, SKU==sel_SKU & Retailer_Market==sel_Retailer)
      df_temp$HelperB3_ForwardMovingAvg <- NA
      df_temp$HelperB3_BackwardMovingAvg <- NA
      
      # separate promo weeks
      df_tempNonPromo <-  arrange(filter(df_temp,(Promo_Flag!=1 | is.na(Promo_Flag))), Week_ending)
      df_tempPromo <-  filter(df_temp, Promo_Flag==1 )
      
      # calculate adjusted volumes for baseline
      df_tempNonPromo <- mutate(df_tempNonPromo,B3_Adjusted_Volume = Volume_Observed + StockUp_CorrectionVolume + Canni_CorrectionVolume)

      
      # calculate moving averages
      n <- nrow(df_tempNonPromo)
      if (n>1){
         df_tempNonPromo$HelperB3_ForwardMovingAvg[1] <-  df_tempNonPromo$B3_Adjusted_Volume[1]
         df_tempNonPromo$HelperB3_BackwardMovingAvg[n] <-  df_tempNonPromo$B3_Adjusted_Volume[n]
         for (i in 2:n){
            df_tempNonPromo[i,]$HelperB3_ForwardMovingAvg <- (df_tempNonPromo$HelperB3_ForwardMovingAvg[i-1]+df_tempNonPromo$B3_Adjusted_Volume[i])/2
            df_tempNonPromo[n+1-i,]$HelperB3_BackwardMovingAvg <- (df_tempNonPromo$B3_Adjusted_Volume[n+1-i]+df_tempNonPromo$HelperB3_BackwardMovingAvg[n+2-i])/2
         }
         df_tempNonPromo$HelperB3_OverallMovingAvg <- (df_tempNonPromo$HelperB3_ForwardMovingAvg + df_tempNonPromo$HelperB3_BackwardMovingAvg)/2
      }
      else{
         df_tempNonPromo$HelperB3_OverallMovingAvg <- df_tempNonPromo$B3_Adjusted_Volume
      }
      
      # bring back the promo weeks
      df_temp <- arrange(plyr::rbind.fill(df_tempNonPromo,df_tempPromo), Week_ending)
      
      
      ### calculate baseline for promo
      df_temp$Baseline_all_factors_Volume <- NA
      i <- 1
      
      if(sum(df_temp$Promo_Flag,na.rm=TRUE)>0 & sum(df_temp$Promo_Flag!=1,na.rm=TRUE)>0){
         # 1. if promo at the beginning
         if(df_temp$Promo_Flag[i] == 1){
            # get index of first non-promo week (it has an OverallMovingAverage)
            tmp_FollowingIndex <- i + min(which(!is.na(df_temp$HelperB3_OverallMovingAvg[i+1:nrow(df_temp)])))
            tmp_FollowingValue <- df_temp$HelperB3_OverallMovingAvg[tmp_FollowingIndex]
            # assign following non-NA value to all weeks before that week (they must be NA)
            df_temp$Baseline_all_factors_Volume[1:(tmp_FollowingIndex-1)] <- tmp_FollowingValue
            i <- tmp_FollowingIndex
         }

         # usual weeks
         while (i <= nrow(df_temp)){
            if(df_temp$Promo_Flag[i] == 1){
               # get index of previous and following non-promo week (it has an OverallMovingAverage)               
               tmp_previousIndex <- max(which(!is.na(df_temp$HelperB3_OverallMovingAvg[1:i-1])))
               tmp_PreviousValue <- df_temp$HelperB3_OverallMovingAvg[tmp_previousIndex]
               if(any(!is.na(df_temp$HelperB3_OverallMovingAvg[i+1:nrow(df_temp)])))
               { # i.e. there is a following non-NA value 
                  tmp_FollowingIndex <- i + min(which(!is.na(df_temp$HelperB3_OverallMovingAvg[i+1:nrow(df_temp)])))
                  tmp_FollowingValue <- df_temp$HelperB3_OverallMovingAvg[tmp_FollowingIndex]
                  # assign average of previous and following non NA value to all weeks between (they must be NA)
                  df_temp$Baseline_all_factors_Volume[(tmp_previousIndex+1):(tmp_FollowingIndex-1)] <- (tmp_PreviousValue + tmp_FollowingValue)/2
                  i <- (tmp_FollowingIndex-1)
                  }
               else{ # i.e. there is no following non-NA value, hence, set all to the previous value
                  df_temp$Baseline_all_factors_Volume[(tmp_previousIndex+1):nrow(df_temp)] <- tmp_PreviousValue
               }
            }
            i <- i+1
         }
      }
      
      # fill values for non-promo weeks (for these values it is simply Baseline_all_factors_Volume = OverallMovingAvg)
      df_temp$Baseline_all_factors_Volume <- ifelse(is.na(df_temp$Baseline_all_factors_Volume), df_temp$HelperB3_OverallMovingAvg, df_temp$Baseline_all_factors_Volume)
      
      return(df_temp) 
    }
    
### B3_3_Baseline ------------------------------------------------------------------------------------------- ###
### requires data frame with correction factors 
### calculates and appends baseline for/to input data frame
### input:  sel_df: data frame with all correction values
    B3_3_Baseline <- function(sel_df,TF_ProgressPrinting=FALSE)
   {
      cat(paste0("\n","Baseline calculations started: ",Sys.time()))
       
      #get unique combinations  
      df_Unique_SKUandRM <- unique(sel_df[,c('SKU','Retailer_Market')])
      #loop through list
      df_return <- NULL
      n <- nrow(df_Unique_SKUandRM)
      for (i in 1:n)
      {  
         #cat(paste(sep="","SKU ", df_Unique_SKUandRM$SKU[i],", \n"))
         df_temp <- B3_3_Baseline_perGroup(sel_df,df_Unique_SKUandRM$SKU[i],df_Unique_SKUandRM$Retailer_Market[i])
         df_return <- plyr::rbind.fill(df_return,df_temp)
         if(TF_ProgressPrinting & i %% 50 == 0 ) {cat(paste(sep="","\n...SKU ", df_Unique_SKUandRM$SKU[i]," + Retailer ", df_Unique_SKUandRM$Retailer_Market[i], "...", i,"/",n))}
      }
      
      df_return$Baseline_Type <- "B3"
      
      cat(paste0("\n","Baseline calculations completed: ",Sys.time(),"\n"))
      
      return(df_return)
   }
    
    
    
    
### ========================================================================================================= ###
###                                Pantry Loading or Stock-up allocation                                      ###
### --------------------------------------------------------------------------------------------------------- ###

### B3_4_Volume_Allocations ----------------------------------------------------------------------------- ###
### requires Week_indicators from B3_0_WeekIndicator
### calculates and append pantry loading/stock up values for input data frame
### input:  sel_df: data frame with all coorection values and baseline calculated
    B3_4_Volume_Allocations <- function(sel_df){  
      cat(paste0("\n","Volume allocation started: ",Sys.time()))
       
      df_temp <- sel_df
      
      df_temp <- mutate(df_temp, Volume_Diff_Obs_BaseAll = Volume_Observed - Baseline_all_factors_Volume)
      df_temp <- mutate(df_temp, B3_PotentialPantryOrCanniVolume = ifelse(Promo_Flag != 1, pmax(-Volume_Observed + Baseline_all_factors_Volume,0), NA))

      
      ### calculate pantry loading in affected non promo weeks before cap
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Volume_beforeCap = 
                           ifelse(substr(df_temp$B3_WeekIndicator,1,1)=="S" & df_temp$Promo_Flag_inCanniGroup==0, # i.e. pure stock up week
                               B3_PotentialPantryOrCanniVolume, 
                           ifelse(substr(df_temp$B3_WeekIndicator,1,1)=="S" & df_temp$Promo_Flag_inCanniGroup==1, # i.e. canni & stock up
                               ifelse(is.na(StockUp_SuFactor_Used) & is.na(Canni_Factor_Used),  # i.e. both factors available
                                      B3_PotentialPantryOrCanniVolume * StockUp_SuFactor_Used / 
                                       (StockUp_SuFactor_Used +(Canni_Factor_Used*number_of_SKUs_in_CG_in_Promo)),
                               ifelse(is.na(StockUp_SuFactor_Used) & !is.na(Canni_Factor_Used),0,      # i.e. no stock-up factor available
                               ifelse(!is.na(StockUp_SuFactor_Used) & is.na(Canni_Factor_Used),B3_PotentialPantryOrCanniVolume, # i.e. no canni factor available
                               ifelse(!is.na(StockUp_SuFactor_Used) & !is.na(Canni_Factor_Used),0, # no factors available at all
                                 NA)))),NA)))
      
      ### calculate cannibalization in affected non promo weeks before cap
      df_temp <- mutate(df_temp, B3_Cannibalization_Volume_beforeCap = 
                           ifelse(substr(df_temp$B3_WeekIndicator,1,1)!="S" & df_temp$Promo_Flag_inCanniGroup==1, # i.e. pure cannibalization
                               B3_PotentialPantryOrCanniVolume, 
                           ifelse(substr(df_temp$B3_WeekIndicator,1,1)=="S" & df_temp$Promo_Flag_inCanniGroup==1, # i.e. canni & stock up
                                  ifelse(is.na(StockUp_SuFactor_Used) & is.na(Canni_Factor_Used), # i.e. both factors available
                                     B3_PotentialPantryOrCanniVolume * (Canni_Factor_Used*number_of_SKUs_in_CG_in_Promo) / 
                                       (StockUp_SuFactor_Used +(Canni_Factor_Used*number_of_SKUs_in_CG_in_Promo)),
                                  ifelse(is.na(StockUp_SuFactor_Used) & !is.na(Canni_Factor_Used),B3_PotentialPantryOrCanniVolume,      # i.e. no stock-up factor available
                                  ifelse(!is.na(StockUp_SuFactor_Used) & is.na(Canni_Factor_Used),0, # i.e. no canni factor available
                                  ifelse(!is.na(StockUp_SuFactor_Used) & !is.na(Canni_Factor_Used),0, # no factors available at all
                                    NA)))),NA)))


      ### distribute Pantry loading volume before cap to the promo weeks by VOLUME
      df_summary <- df_temp %>% group_by(Promo_Flag_group) %>% summarize(tempSum_PantryLoading_beforeCap = sum(B3_Pantry_Loading_Volume_beforeCap, na.rm=TRUE),
                                                                         tempSum_Volume_inPantryGroup = sum(ifelse(Promo_Flag == 1, pmax(Volume_Observed, 0), 0), na.rm=TRUE))
      df_temp <- merge(df_temp,df_summary, by=c("Promo_Flag_group"))
      df_temp <- mutate(df_temp, Pantry_Loading_Volume_beforeCap = (ifelse(Promo_Flag == 1,pmax(Volume_Observed, 0),NA) / tempSum_Volume_inPantryGroup) * tempSum_PantryLoading_beforeCap )

      ### distribute cannibalization volume before cap to the promo weeks by UPLIFT
      df_summary <- df_temp %>% group_by(Week_ending, Retailer_Market, Cannibalization_Group) %>% summarize(tempSum_CanniVol_beforeCap = sum(B3_Cannibalization_Volume_beforeCap, na.rm=TRUE),
                                                                                    tempSum_Uplift_inCanniGroup = sum(ifelse(Promo_Flag == 1, pmax(Volume_Diff_Obs_BaseAll, 0), 0), na.rm=TRUE))
      df_temp <- merge(df_temp,df_summary, by=c("Week_ending", "Retailer_Market", "Cannibalization_Group"))
      df_temp <- mutate(df_temp, Cannibalization_Volume_beforeCap = (ifelse(Promo_Flag == 1,pmax(Volume_Diff_Obs_BaseAll, 0),NA) / tempSum_Uplift_inCanniGroup) * tempSum_CanniVol_beforeCap )      
      df_temp <- mutate(df_temp, Cannibalization_Volume_beforeCap = (ifelse(Promo_Flag == 1 & is.na(Cannibalization_Volume_beforeCap), 0, Cannibalization_Volume_beforeCap)))

            
      ### Apply cap for Pantry loading and Cannibalization
      df_temp <- mutate(df_temp, tempTF_ApplyCap = (Pantry_Loading_Volume_beforeCap + Cannibalization_Volume_beforeCap > pmax(Volume_Diff_Obs_BaseAll,0)))
      df_temp <- mutate(df_temp, Pantry_Loading_Volume = ifelse(Promo_Flag == 1, ifelse(!tempTF_ApplyCap, 
                                                                                    Pantry_Loading_Volume_beforeCap, 
                                                                                    pmax(Volume_Diff_Obs_BaseAll,0) * Pantry_Loading_Volume_beforeCap / 
                                                                                       (Pantry_Loading_Volume_beforeCap + Cannibalization_Volume_beforeCap)),NA))
      df_temp <- mutate(df_temp, Cannibalization_Volume = ifelse(Promo_Flag == 1, ifelse(!tempTF_ApplyCap, 
                                                                                         Cannibalization_Volume_beforeCap, 
                                                                                         pmax(Volume_Diff_Obs_BaseAll,0) * Cannibalization_Volume_beforeCap / 
                                                                                            (Pantry_Loading_Volume_beforeCap + Cannibalization_Volume_beforeCap)),NA))
     
      ### distribute the actual allocated volumes (after cap) to promo week back to the affected non-promo weeks by there before cap pantry loading
      df_summary <- df_temp %>% group_by(Promo_Flag_group) %>% summarize(tempSum_PantryLoading_afterCap = sum(Pantry_Loading_Volume, na.rm=TRUE))
      df_temp <- merge(df_temp,df_summary, by=c("Promo_Flag_group"))
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Volume = (B3_Pantry_Loading_Volume_beforeCap / tempSum_PantryLoading_beforeCap) * tempSum_PantryLoading_afterCap )      
      
      ### distribute the actual allocated volumes (after cap) to promo week back to the affected non-promo weeks by there before cap cannibalization
      df_summary <- df_temp %>% group_by(Week_ending, Retailer_Market, Cannibalization_Group) %>% summarize(tempSum_CanniVol_afterCap = sum(Cannibalization_Volume, na.rm=TRUE))
      df_temp <- merge(df_temp,df_summary, by=c("Week_ending", "Retailer_Market", "Cannibalization_Group"))
      df_temp <- mutate(df_temp, B3_Cannibalization_Volume = (B3_Cannibalization_Volume_beforeCap / tempSum_CanniVol_beforeCap) * tempSum_CanniVol_afterCap ) 


      ### Distribute B3 stock-up week values to the causing promo weeks based on actual Pantry loading volumes (after cap)
      # calcualte margins and sales for affected non promo weeks / non promo weeks
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Margin_Manufacturer = B3_Pantry_Loading_Volume * (Price_Baseline_Manufacturer - Unit_Costs_asUsed))
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Margin_System = B3_Pantry_Loading_Volume * (Price_Baseline_Retail - Unit_Costs_asUsed))
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Margin_Retailer = B3_Pantry_Loading_Margin_System - B3_Pantry_Loading_Margin_Manufacturer)
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Sales_Manufacturer = B3_Pantry_Loading_Volume * Price_Baseline_Manufacturer)
      df_temp <- mutate(df_temp, B3_Pantry_Loading_Sales_System = B3_Pantry_Loading_Volume * Price_Baseline_Retail)

      # get aggregated values per Promo_Flag_group
      df_summary <- df_temp %>% group_by(Promo_Flag_group) %>% # Promo_Flag_group is determined in B3_0_Weekindicator
                              summarise( tempSum_Pantry_Loading_Volume     = sum(ifelse(!is.na(Pantry_Loading_Volume), Pantry_Loading_Volume, 0),na.rm=TRUE),
                                         tempSumStockUp_Volume             = sum(B3_Pantry_Loading_Volume,na.rm=TRUE),
                                         tempSumStockUp_Margin_Manufacture = sum(B3_Pantry_Loading_Margin_Manufacturer,na.rm=TRUE),
                                         tempSumStockUp_Margin_System      = sum(B3_Pantry_Loading_Margin_System,na.rm=TRUE),
                                         tempSumStockUp_Margin_Retailer    = sum(B3_Pantry_Loading_Margin_Retailer,na.rm=TRUE),
                                         tempSumStockUp_Sales_Manufacturer = sum(B3_Pantry_Loading_Sales_Manufacturer,na.rm=TRUE),
                                         tempSumStockUp_Sales_System       = sum(B3_Pantry_Loading_Sales_System,na.rm=TRUE)
                                       )
      # merge them to data frame
      df_temp <- merge(df_temp,df_summary, by="Promo_Flag_group")

      # calculate resulting shares (note: distribution key different to B2)
      df_temp <- mutate(df_temp, Pantry_Loading_Margin_Manufacturer = ifelse(Promo_Flag==1 & tempSum_Pantry_Loading_Volume>0, Pantry_Loading_Volume/tempSum_Pantry_Loading_Volume * tempSumStockUp_Margin_Manufacture,0))
      df_temp <- mutate(df_temp, Pantry_Loading_Margin_System       = ifelse(Promo_Flag==1 & tempSum_Pantry_Loading_Volume>0, Pantry_Loading_Volume/tempSum_Pantry_Loading_Volume * tempSumStockUp_Margin_System,0))
      df_temp <- mutate(df_temp, Pantry_Loading_Margin_Retailer     = ifelse(Promo_Flag==1 & tempSum_Pantry_Loading_Volume>0, Pantry_Loading_Volume/tempSum_Pantry_Loading_Volume * tempSumStockUp_Margin_Retailer,0))
      df_temp <- mutate(df_temp, Pantry_Loading_Sales_Manufacturer  = ifelse(Promo_Flag==1 & tempSum_Pantry_Loading_Volume>0, Pantry_Loading_Volume/tempSum_Pantry_Loading_Volume * tempSumStockUp_Sales_Manufacturer,0))
      df_temp <- mutate(df_temp, Pantry_Loading_Sales_System        = ifelse(Promo_Flag==1 & tempSum_Pantry_Loading_Volume>0, Pantry_Loading_Volume/tempSum_Pantry_Loading_Volume * tempSumStockUp_Sales_System,0))
      
  
      ### Distribute B3 canni values to the causing promo SKUs based on actual canni volumes (after cap)
      #summarise to cannibalization group to calculate the aggregated margin and sales
      df_summary <- df_temp %>% group_by(Retailer_Market,Cannibalization_Group,Week_ending) %>%
         summarise(Cannibalization_Pool_Volume = sum(ifelse(Promo_Flag!=1,B3_Cannibalization_Volume,0),na.rm=TRUE),
                   Cannibalization_Pool_Margin_System = sum(ifelse(Promo_Flag!=1,B3_Cannibalization_Volume*(Price_Baseline_Retail-Unit_Costs_asUsed),0),na.rm=TRUE),
                   Cannibalization_Pool_Margin_Manufacturer = sum(ifelse(Promo_Flag!=1,B3_Cannibalization_Volume*(Price_Baseline_Manufacturer-Unit_Costs_asUsed),0),na.rm=TRUE),
                   Cannibalization_Pool_Margin_Retailer = Cannibalization_Pool_Margin_System-Cannibalization_Pool_Margin_Manufacturer,
                   Cannibalization_Pool_Sales_Manufacturer = sum(ifelse(Promo_Flag!=1,B3_Cannibalization_Volume*Price_Baseline_Manufacturer,0),na.rm=TRUE),
                   Cannibalization_Pool_Sales_System = sum(ifelse(Promo_Flag!=1,B3_Cannibalization_Volume*Price_Baseline_Retail,0),na.rm=TRUE)
         )
      iFactor <- sapply(df_summary, is.factor)
      df_summary[iFactor] <- lapply(df_summary[iFactor], as.character) # do so to remove columns in merging step
      
      #merge to input data
      df_temp <- merge(df_temp[, c("Retailer_Market","Cannibalization_Group","Week_ending", setdiff(colnames(df_temp),colnames(df_summary)))],df_summary,
                       by=c("Retailer_Market","Cannibalization_Group","Week_ending"))
      
      #calculate margins and sales per cannibalizing promo row
      df_temp <- mutate(df_temp, Cannibalization_Margin_System = ifelse(Cannibalization_Pool_Volume>0, Cannibalization_Volume/Cannibalization_Pool_Volume * Cannibalization_Pool_Margin_System,0))
      df_temp <- mutate(df_temp, Cannibalization_Margin_Manufacturer = ifelse(Cannibalization_Pool_Volume>0, Cannibalization_Volume/Cannibalization_Pool_Volume * Cannibalization_Pool_Margin_Manufacturer,0))
      df_temp <- mutate(df_temp, Cannibalization_Margin_Retailer = ifelse(Cannibalization_Pool_Volume>0, Cannibalization_Volume/Cannibalization_Pool_Volume * Cannibalization_Pool_Margin_Retailer,0))
      df_temp <- mutate(df_temp, Cannibalization_Sales_Manufacturer = ifelse(Cannibalization_Pool_Volume>0, Cannibalization_Volume/Cannibalization_Pool_Volume * Cannibalization_Pool_Sales_Manufacturer,0))
      df_temp <- mutate(df_temp, Cannibalization_Sales_System = ifelse(Cannibalization_Pool_Volume>0, Cannibalization_Volume/Cannibalization_Pool_Volume * Cannibalization_Pool_Sales_System,0))
      
      cat(paste0("\n","Volume allocation completed: ",Sys.time(),"\n"))
      
      invisible(df_temp)

    }
    
### ========================================================================================================= ###
###                                       Complementary                                                       ###
### --------------------------------------------------------------------------------------------------------- ###

### B3_Complementary_append_to_df ----------------------------------------------------------------------------- ###
### uses same function as B2 function, needs to calculate Remaining Uptlift beforehand   
   
   B3_5_Complementary_append_to_df <- function(sel_df,...){
      df_temp <- sel_df
      df_temp <- mutate(df_temp, Remaining_Uplift = ifelse(Promo_Flag == 1, pmax(Volume_Diff_Obs_BaseAll - Pantry_Loading_Volume,0), NA))
      B2_5_Complementary_append_to_df(df_temp,...)
   }
   
   ### B3_DoAllSteps ############################################################################################
   ### does row wise calculations
   ### input:  sel_df: data frame with week+retailer+SKU data
   ###         sel_dfComplementaryRelations: data frame with Complementary relations
   ###         sel_PantryPercentile: cut off threshold for pantry loading correction factors
   ###         sel_CanniPercentile: cut off threshold for cannibalization correction factors
   ### output: data frame with calculated fields from B3   
   B3_DoAllSteps <- function(sel_df, sel_dfComplementaryRelations, sel_PantryPercentile=0.9, sel_CanniPercentile=0.9)
   {
      df_temp <- sel_df
      ### A
      #1 Initial data check
      df_temp <- A_Initial_Data_Check(df_temp)
      
      ### B3 
      #0 Week indicator
      df_temp <- B3_0_WeekIndicator(df_temp)
      #1 Stock-up correction
      df_temp <- B3_1_PantryLoading_Correction(df_temp)
      #3 Pantry loading
      df_temp <- B3_2_Cannibalization_Correction(df_temp, sel_PantryPercentile)
      #4 Cannibalization
      df_temp <- B3_3_Baseline(df_temp, sel_CanniPercentile)
      #4 Volumme Allocation
      df_temp <- B3_4_Volume_Allocations(df_temp)
      #5 Complementary
      df_temp <- B3_5_Complementary_append_to_df(df_temp,sel_dfComplementaryRelations,FALSE)

      ### B5
      #1 Row calcs
      df_temp <- B5_RowCalcs(df_temp)
      return(df_temp)
   }    
   

   
