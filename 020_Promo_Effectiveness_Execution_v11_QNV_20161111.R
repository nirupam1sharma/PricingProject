#################################################################################################################
### Purpose: R code for Catalyst Promo Effectiveness code (wip)
### Author: Nirupam Sharma
### Date: 2016/11/11
### Version: v11
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
      Working_Directory <- "C:\\Users\\Vuong Quan Nhon\\Desktop\\in progress\\34 - Michele Casertano"   
      setwd(Working_Directory)   
      
      # load saved workspace
      # load(".RData")
      
      # loading functions 
      source(paste0(Working_Directory,"\\2_RScript\\", "005_Promo_Effectiveness_GeneralFunctions_Source_v09_QNV_20161111.R"))
      source(paste0(Working_Directory,"\\2_RScript\\", "010_Promo_Effectiveness_B2Functions_Source_v05_QNV_20161109.R"))
      source(paste0(Working_Directory,"\\2_RScript\\", "011_Promo_Effectiveness_B3Functions_Source_v11_QNV_20161111.R"))
      
      # input file
      csv_RawDataLocation <- paste0(Working_Directory,"\\1_Input\\WWOM - Input - Main Dataset.csv")
      
      # complementary relationsships table
      csv_ComplementaryDataLocation <- paste0(Working_Directory,"\\1_Input\\WWOM - Input - Complementarity Relationships.csv")
      
      # Table with field names
      csv_OutputList <- paste0(Working_Directory,"\\1_Input\\OutputList.csv")
      
      # output folder
      Output_Directory <- paste0(Working_Directory,"\\3_Output\\")
      
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
   
   # run alorithm and output   
   df_output <- Run_Promo_Effectiveness(raw_input,df_ComplementaryRelationships,OutputFields,AdditionalFields,"B2")   

#################################################################################################################
###                                             END                                                           ###
#################################################################################################################

   df_start <- filter(raw_input,Retailer_Market == "Retailer 3")

   
