best <- function (state_str_v, outcome_str_v)
{
        hospconnames <- c(25,13,19) ## These are the column numbers in the input file for the conditions outlined on the next line
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
        
        return(state_df[1,"Hospital.Name"])
          
} ## end of function best


