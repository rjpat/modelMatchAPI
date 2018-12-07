# plumber.R

#Injest the known model information in a dataframe and clean so they are ready for the match
known <- readr::read_csv('data/models2.csv')
known$cleanName <- stringr::str_to_lower(known$Name)
known$cleanName <- gsub("[^[:alnum:][:space:]']", "", known$cleanName)
known$cleanName <- stringr::str_split(known$cleanName, " ")

#' Match the model and provide the resulting id and confidence rating
#' @param name The name to match
#' @get /modelMatch

function(name) {
  
  #' Clean the incoming names from the client. 
  #' This removes all non-needed characters and splits the data into its seperate words
  nameClean <- stringr::str_to_lower(name)
  nameClean <- gsub("[^[:alnum:][:space:]']", "", nameClean)
  nameClean <- gsub("\\s+", " ", nameClean)
  nameClean <- stringr::str_split(nameClean, " ")
  unlistedMatch <- unlist(nameClean)
  lengthMatch <- length(unlistedMatch)
  
  #' Take the dataframe of known information and compare it to the client data.
  #' This then provides a resulting 'confidence' of the match by comparing the 
  #' number of words matched from both sides
  known$resultant <- purrr::map_dbl(known$cleanName, function(x) {
    unlistedName <- unlist(x) 
    
    resultInt <- sum(unlistedName %in% unlistedMatch)
    
    (resultInt * 2 / (lengthMatch + length(unlistedName)))
  })
  
  matched <- subset(known, known$resultant == max(known$resultant))
  matched <- subset(matched, matched$priority == min(matched$priority), 
                    select = c(id, Name, resultant))
  matched <- head(subset(matched, matched$id == min(matched$id)),1)
  
  #' Return the original name provided, matched information and confidence
  list(Name = paste0(name),
    knownName = paste0(matched$Name),
    knownId = paste0(matched$id),
    confidence = paste0(matched$resultant))
}

