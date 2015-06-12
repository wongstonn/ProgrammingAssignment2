
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
    path_string                     <- paste(directory,id_as_string,".csv",sep="")
    
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

directory <- "c:/users/Neil/datasciencecoursera/specdata/"

x <- complete(directory, 1)
print(x)
x <- complete(directory, c(2,4,8,10,12))
print(x)
x <- complete(directory, 30:25)
print(x)
x <- complete(directory, 3)
print(x)
