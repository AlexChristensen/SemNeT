#' Extract Groups from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description Extract groups from a \code{\link[SemNetCleaner]{textcleaner}} object
#' 
#' @param object Matrix, data frame, or object from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param groups Vector.
#' Specify groups using character or numeric values
#' 
#' @return Returns a list containing the cleaned data split by groups
#' 
#' @examples
#' # Openness to experience data
#' 
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Extract groups
# Updated 21.01.2022
extract_groups <- function(object, groups, type = c("fluency", "free"))
{
  
  # Check if object is textcleaner
  if(any(class(object) == "textcleaner")){
    
    # Obtain response matrix
    if(any(class(object) == "fluency")){
      response_matrix <- object$data$clean
    }else if(any(class(object) == "free")){
      response_matrix <- object$data$clean
    }
    
    # Check if type is missing
    if(missing(type)){
      
      # Obtain class
      if("fluency" %in% class(object)){
        type <- "fluency"
      }else if("free" %in% class(object)){
        type <- "free"
      }
      
    }

  }else{# Assume input is response matrix
    response_matrix <- object
  }
  
  # Obtain grouped data
  if(type == "fluency"){
    
    # Check if groups match cases
    if(length(groups) != nrow(response_matrix)){
      stop("The length of group membership does not match the number of cases in the response matrix")
    }
    
    # Assign groups to list
    group_list <- lapply(unique(groups), function(g){
      response_matrix[which(groups == g),]
    })
    names(group_list) <- unique(groups)
    
    # Result list
    result_list <- group_list
    
  }else if(type == "free"){
    
    # Obtain unique IDs
    unique_ids <- na.omit(unique(response_matrix[,"ID"]))
    
    # Check if groups match IDs
    if(length(groups) != length(unique_ids)){
      stop("The length of group membership does not match the number of unique IDs in the response matrix")
    }
    
    # Obtain unique groups
    unique_groups <- na.omit(unique(groups))
    
    # Assign groups to list
    group_list <- lapply(seq_along(unique_groups), function(i){
      
      # Obtain group ID indices
      group_ID <- which(groups == unique_groups[i])
      
      # Obtain IDs
      IDs <- unique_ids[group_ID]
      
      # Obtain response matrix
      response_matrix <- response_matrix[!is.na(match(response_matrix[,"ID"], IDs)),]
      
      # Convert to data frame
      as.data.frame(response_matrix)
      
    })
    names(group_list) <- unique_groups
    
    # Create frequency matrices
    frequency_list <- lapply(seq_along(group_list), function(i){
    
      # Obtain unique responses
      unique.responses <- na.omit(unique(group_list[[i]]$Response))
      
      # Obtain unique cues
      unique.cues <- na.omit(unique(group_list[[i]]$Cue))
      
      # Obtain cleaned list
      cleaned.list <- group_list[[i]]
      
      ## Initialize cleaned matrix
      cleaned.matrix <- matrix(
        0, nrow = length(unique.responses), ncol = length(unique.cues) 
      )
      row.names(cleaned.matrix) <- unique.responses
      colnames(cleaned.matrix) <- unique.cues
      
      # Loop through for frequencies
      for(i in 1:length(unique.cues)){
        
        frequency <- table(cleaned.list$Response[cleaned.list$Cue == unique.cues[i]])
        
        cleaned.matrix[names(frequency),i] <- frequency
        
      }
      
      return(cleaned.matrix)
      
    })
    names(frequency_list) <- unique_groups
    
    # Set up result list
    result_list <- list()
    result_list$responses <- group_list
    result_list$frequency <- frequency_list
    
  }
  
  # Return grouped list
  return(result_list)
  
}
