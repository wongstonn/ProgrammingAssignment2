rm()

pollutantmean <- function(directory, pollutant, id=1:332) 
{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!

  ## initialise local variables
  directory <- "c:/users/Neil/datasciencecoursera/specdata/"
  id2 <- 1:332                   # a placeholder to store all values 1 to 332.  Used in filename generation
  id_as_string <- ""             # we'll use this to generate file path names
  running_total_pollution <- 0   # tally of all pollutant measurements
  number_of_readings <- 0        # number of pollutant measurements read in  
  
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
	  ## then create a vector called valid_values containing only actual readings for our chosen pollutant
	  current_file_values <- read.csv(path_string,header = TRUE)
	  reading_taken <- !is.na(current_file_values[pollutant])
	  valid_values <- current_file_values [reading_taken,pollutant]
	  
    ## update running totals
	  running_total_pollution <- running_total_pollution + sum(valid_values)
	  number_of_readings <- number_of_readings + length(valid_values)
	  
	 }  ## end of the "for i" loop
  
    ## And here is the answer
    ##message <- paste("Mean of ",pollutant,"pollutant is",toString(running_total_pollution/number_of_readings))
    ##print(message)
    return (running_total_pollution/number_of_readings)
}  ## end of pollutantmean function definition

pollutantmean(directory,"sulfate",1:10)
pollutantmean(directory,"nitrate",70:72)
pollutantmean(directory,"nitrate",23)
