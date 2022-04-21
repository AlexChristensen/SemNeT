#' Extract Groups from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @description Extract groups from a \code{\link[SemNetCleaner]{textcleaner}} object
#' 
#' @param object Matrix, data frame, or object from \code{\link[SemNetCleaner]{textcleaner}}
#' 
#' @param groups Vector.
#' Specify groups using character or numeric values
#' 
#' @param type Character.
#' Whether data are from a \code{"fluency"} task or
#' \code{"free"} association task
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
# Updated 21.04.2022
extract_groups <- function(object, groups, type = c("fluency", "free"))
{
  
  # Check if object is textcleaner
  if(any(class(object) == "textcleaner")){
    
    # Obtain response matrix
    if(any(class(object) == "fluency")){
      response_matrix <- object$responses$clean
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
    if(length(groups) %% length(unique_ids) != 0){
      stop("The length of group membership does not match the number of unique IDs in the response matrix")
    }else{
      
      # Obtain number of times
      times <- length(groups) / length(unique_ids)
      
      if(times > 1){
        
        # Re-obtain unique IDs
        ids <- response_matrix[,"ID"]
        
        # Obtain sequences
        id_sequences <- lapply(unique_ids, function(x){
          
          # Obtain target ID
          target_id <- which(ids == x)
          
          # Return minimum
          return(seq_min_max(target_id))
          
        })
        
        # Create new IDs
        new_ids <- vector(length = nrow(response_matrix))
        
        # Obtain minimums to obtain ordering
        id_min_matrix <- simplify2array(lapply(id_sequences, function(x){
          x$min
        }), higher = FALSE)
        
        # Order across rows
        id_orders <- apply(id_min_matrix, 1, order)
        
        # Loop through for new IDs
        for(i in seq_along(id_sequences)){
          
          # Obtain sequences
          target_sequence <- id_sequences[[i]]
          
          # Loop through number of times
          for(j in seq_along(target_sequence$min)){
            
            # Sequence
            sequence <- target_sequence$min[j]:target_sequence$max[j]
            
            new_ids[ # start and end of sequence
              target_sequence$min[j]:target_sequence$max[j]
            ] <- paste( # get ID from orderings
              unique_ids[id_orders[i,j]], j, sep = "_"
            )
            
          }
          
        }
        
        # Create new unique_ids
        new_unique_ids <- na.omit(unique(new_ids))
        
        # Assign new IDs to previous IDs
        response_matrix[,"ID"] <- new_ids
        unique_ids <- new_unique_ids
        
      }
      
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
