# Take a series of lists with some common "column" names between them, and puts all those lists together into a single unified dataframe; note that the lists in question should be provided with differentiating columns beforehand, or else discerning which original list a dataframe row came from is not possible.
collideListsIntoDataFrame <- function(listOfLists) {
  # Get intersection of column names to determine which column names to use.
  listOfListsLength <- length(listOfLists)
  # Ensure that issues with column structure cause errors early
  try(columnVectorsList <- lapply(listOfLists, colnames))
  columnNamesToUse <- columnVectorsList[[1]]
  if (listOfListsLength > 1) {
    for(index in 2:listOfListsLength) {
      columnNamesToUse <- intersect(columnNamesToUse, columnVectorsList[[index]])
    }
  }
  
  # Put the input lists into data frame form, using the standardized column set above.
  arrangedValues <- lapply(listOfLists, function(inputValue) {
    valueSet <- inputValue[, columnNamesToUse]
    valueSet <- data.frame(valueSet)
    return(valueSet)
  })
  
  # Smash the data frames together to form one collated corpus.
  organizedValues <- arrangedValues[[1]]
  if (listOfListsLength > 1) {
    for(index in 2:listOfListsLength) {
      organizedValues <- rbind(organizedValues, arrangedValues[[index]])
    }
  }

  return(organizedValues)
}