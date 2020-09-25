range_to_mean <- function(num_range){
  # This function takes a character type number range and converts it into
  # the numeric type mean of leftmost and rightmost numbers of the range.
  # If the input character is not a range, then just return it as numeric
  
  # Ex. "60-69" should turn into numeric 64.5
  
  # Factors should be converted into string first
  if(class(num_range) == "factor"){
    num_range <- as.character(num_range)
  }
  
  library(stringr)
  library(dplyr)
  
  # If there is a hyphen
  if( str_detect(num_range, "-") == T ){
    # Store the leftmost and rightmost values of the range
    left_num <- str_replace(
      num_range, "[:space:]*-[:space:]*[:digit:]+$", "") %>%
      as.numeric()
    
    right_num <- str_replace(
      num_range, "^[:digit:]+[:space:]*-[:space:]*", "") %>%
      as.numeric()
    
    # Return the mean of the two numbers
    return(mean(c(left_num, right_num)))
  }
  
  # If there is no hyphen and it can't be converted into numeric
  # use NA instead
  else if( is.na(as.numeric(num_range)) == T ){
    return(NA)
  }
  
  # Otherwise, convert the number into numeric type
  else{
    return(as.numeric(num_range))
  }
}

# Range
range_to_mean("60-69")
range_to_mean("6")
range_to_mean(as.factor("6"))
range_to_mean("")