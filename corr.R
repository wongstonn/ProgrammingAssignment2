
complete <- function(directory, id = 1:332) {

  ## initialise local variables
  
  id2                   <- 1:332 # a placeholder to store all values 1 to 332.  Used in filename generation
  id_as_string          <- ""    # we use this to generate file path names
  number_complete_cases <- c(length(id))
  filenames             <- c(length(id))
  
  ## loop through all files in range specified in the function call, read in input file
  ## strip out NA navlues then update running totals used in our mean pollutant calc
  
  for (i in id)
  {
    ## the next section of code creates a string containing the path and filename
    ## of the input files.  As part of this we loop through the id values we need to turn this
    ## into a string with the correct number of leading zeroes
    id_as_string [i < 10]           <- paste("00",toString(id2[i]),sep="")
    id_as_string [i > 9 & i < 100]  <- paste("0",toString(id2[i]),sep="")
    id_as_string [i > 99]           <- toString(id2[i])
    path_string                     <- paste("c:/users/Neil/datasciencecoursera/",directory,"/",id_as_string,".csv",sep="")
    
    ## now that we have generated the file and pathname, read in the file, determine
    ## which values for our pollutant are "NA" (no reading given by the meter)
    ## then create a data frame called valid_values containing reading when complete (neither are NA)
    
    current_file_values <- read.csv(path_string,header = TRUE)
    
    valid_values        <- subset(current_file_values,!is.na(current_file_values["sulfate"]) & !is.na(current_file_values["nitrate"]))

    number_complete_cases [match(i,id)]     <- nrow(valid_values)
    filenames [match(i,id)]                 <- id[match(i,id)]
    
  }  ## end of the "for i" loop
  
  return_data_frame <- data.frame(filenames,number_complete_cases)  
  return (return_data_frame)
}  ## end of complete function definition

####################################################################################################

corr <- function(directory, threshold = 0) {
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  ## initialise local variables
  rm()
  id2                   <- 1:332 # a placeholder to store all values 1 to 332.  Used in filename generation
  id_as_string          <- ""    # we use this to generate file path names
  corr_answer_vector    <- vector()  # this is the return value from this function 

  ## Get a data.frame variable containing an inventory of files and the number of complete results
  ## per file
  
  x <- complete(directory,1:332)

  ## create a small data frame containing an inventory of files where the number of complete results
  ## exceeds the threshold
  
  x <- subset (x, (number_complete_cases >= threshold) )

  ## return zero is no files meet the threshold criteria
  
  if (nrow(x) == 0){ 
    return (corr_answer_vector)
    } 

  ## If we process here we must have some monitors meeting the requirement, so now we parse
  ## through them and create vectors of complete results

  
  filenames_loop_vector <- x[,"filenames"]

  for (i in filenames_loop_vector)
  {
    ## the next section of code creates a string containing the path and filename
    ## of the input files.  As part of this we loop through the id values we need to turn this
    ## into a string with the correct number of leading zeroes

    id_as_string [i < 10]           <- paste("00",toString(i),sep="")
    id_as_string [i > 9 & i < 100]  <- paste("0",toString(i),sep="")
    id_as_string [i > 99]           <- toString(i)
    path_string                     <- paste("c:/users/Neil/datasciencecoursera/",directory,"/",id_as_string,".csv",sep="")
    
    ## now that we have generated the file and pathname, read in the file, determine
    ## and create separate vectors for sulfate and nitrates for where a reading exists for both
    ## We keep them in separate vectors as this is format required later on to use the correlation
    ## function cor()
    
    current_file_values <- read.csv(path_string,header=TRUE)
    reading_taken       <- !is.na(current_file_values["sulfate"]) & !is.na(current_file_values["nitrate"])
    current_sulfate_readings_vector  <- current_file_values [reading_taken,"sulfate" ]
    current_nitrate_readings_vector  <- current_file_values [reading_taken,"nitrate" ]
    
    temp_cor <- cor (current_sulfate_readings_vector,current_nitrate_readings_vector )
    if (!is.na(temp_cor)){
        corr_answer_vector              <- rbind(corr_answer_vector, swirlcor (current_sulfate_readings_vector,current_nitrate_readings_vector ))
      }
    } ## end of for loop
  
  return (corr_answer_vector)
}  ## end of complete function definition

directory <- "specdata"

cr <- vector()
cr <- corr(directory,150)
head(cr)
summary(cr)

cr <- corr(directory,400)
head(cr)
summary(cr)

cr <- corr(directory,5000)
summary(cr)
length(cr)

cr <- corr(directory)
summary(cr)
length(cr)

