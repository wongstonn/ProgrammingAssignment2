rankall <- function (input_cond_str_v, input_rank_str_v="best"){
        output_df           <- data.frame(hospital=character(0), state=character(0))  
        colnames(output_df) <- c("hospital","state")
        
        infile_df          <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        unique_state_list  <- unique(infile_df[,"State"])
        unique_state_list  <- sort(unique_state_list)

        split_df           <- split(infile_df,infile_df[,"State"])  

        for (input_state in unique_state_list){
                temp =rankhospital(infile_df,input_state, input_cond_str_v,input_rank_str_v)
                temp2 = as.data.frame(temp)
                output_df = rbind(output_df,temp2)
        }
        return (output_df)
} # end rankall

rankhospital <- function (input_df, state_str_v, outcome_str_v, rank_str_v){
      
        hospconnames <- c(23,11,17) ## These are the column numbers in the input file for the conditions outlined on the next line
        conditions   <- c("pneumonia","heart attack","heart failure")
        condition_lookup_df <- data.frame(conditions, hospconnames)
        
        ##################### validate inputs ########################
                
        look_up_result      <- match(outcome_str_v,condition_lookup_df[,"conditions"],nomatch=-1)
        if (look_up_result == -1)
        {
                stop ("invalid outcome")
        } 
        col_2B_looked_up = condition_lookup_df[look_up_result,"hospconnames"]
        
        if (rank_str_v == "best"){
                rank_str_v = "1"
        } 
        else
        {
                if (rank_str_v == "worst")
                { 
                        # this is a temporary value, once we know the number of hospitals we will adjust this
                        rank_str_v = "-1"
                } 
                else
                {
                        if (!is.numeric(as.numeric(rank_str_v)))
                        {
                                ## error situation
                                stop("invalid rank")
                        }
                }
                                
        }
        
        rank_num_v = as.numeric(rank_str_v)
        
        ##################### create data frame for the state requested in functional call ########################
        ##################### and ensure unwanted values are filtered out                  ########################
        ########## and finally because data comes in as all strings, convert to numbers    ######################## 
                
        state_df = subset (input_df,input_df[,"State"] == state_str_v & input_df[,col_2B_looked_up] != "Not Available")
        
        state_df$measurements = as.numeric(state_df[,col_2B_looked_up])

        if (rank_num_v == -1)
        {
                rank_num_v = nrow(state_df)
        }
        else
        {
                if (rank_num_v > nrow(state_df))
                {
                        ##return_l = list(NA,state_str_v)
                        return_l = list(hospital=state_df[rank_num_v,"Hospital.Name"], state=state_str_v)
                        return (return_l)
                }
        }

        ## Order by hospital name        
        state_df = state_df[order(state_df[,"measurements"],state_df[,"Hospital.Name"]),] 
        return_l = list(hospital=toString(state_df[rank_num_v,"Hospital.Name"]), state=state_str_v)

        return(return_l)  
        
} ## end of function rankhospital

