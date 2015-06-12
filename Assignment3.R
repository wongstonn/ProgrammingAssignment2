best <- function (state_str_v, outcome_str_v)
{
        
##        hospconnames <- c("Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
##                         ,"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
##                         ,"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
        hospconnames <- c(25,13,19)
        conditions   <- c("pneumonia","heart attack","heart failure")
        condition_lookup_df <- data.frame(conditions, hospconnames)
        hospital_name_const <- "Hospital.Name"

        outcome_df          <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##################### validate inputs ########################
                
        look_up_result      <- match(outcome_str_v,condition_lookup_df[,"conditions"],nomatch=-1)
        if (look_up_result == -1)
        {
                stop ("invalid outcome")
        } 
        col_2B_looked_up = condition_lookup_df[look_up_result,"hospconnames"]
        ## print(paste("Col to be looked up=",toString(col_2B_looked_up)))
        
        if (match(state_str_v,outcome_df[,"State"],nomatch=-1) == -1)
        {
                stop ("invalid state")
        } 
        
        ##################### create data frame for the state requested in functional call ########################
        ##################### and ensure unwanted values are filtered out                  ########################
        ########## and finally because data comes in as all strings, convert to numbers    ######################## 
                
        state_df = subset (outcome_df,outcome_df[,"State"] == state_str_v & outcome_df[,col_2B_looked_up] != "Not Available")
        state_df$measurements = as.numeric(state_df[,col_2B_looked_up])

        ## Take the min measurements, then order by hospital name        
        state_df = subset (state_df, state_df$measurements == min(state_df$measurements) )
        state_df = state_df[order(state_df[,"Hospital.Name"]),] 
        print(paste("Hospital with the minimum in state",state_str_v, "is:", state_df[1,"Hospital.Name"], "with", min(state_df$measurements)))
        
        return("hello")
          
} ## end of function best

stateMean <- function (input_state_df)
{
        ##print( colnames (input_state_df))
        ##y = nrow(input_state_df)
        ##y = mean(input_state_df[,"Upper.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"],na.rm=TRUE)
        ##print(y)
        ##return(y)
}

best("NY","heart failure")





##split_outcome_by_state_df  <- split(outcome_df,outcome_df[,"State"])
##state_df = split_outcome_by_state_df[state_str_v]
##state_df = subset (state_df, )
##        if (outcome_str_v == 'pneumonia'){
##
##        results       = state_df[,look_up_result]
##        print(results)
##        hospital_name = state_df[,hospital_name_const] 
##        print(hospital_name) 
##        
##        results_v     = data.frame(hospital_name,results)
##        
##        print(results_v)

##print(state_df)
##print(dimnames(state_df[$1[,]]))
##print(state_df$look_up_result)
##state_df <- subset(state_df,!is.na(state_df[,look_up_result]))
##state_df <- subset(state_df,!is.na(state_df$look_up_result))
##print("After")