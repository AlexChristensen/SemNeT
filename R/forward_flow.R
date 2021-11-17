#' Forward Flow
#'
#' @description This function calculates the forward flow of a list of words per Gray et. al, 2019. Forward flow is a way to quantify the forward motion of naturalistic thought. Research suggests that it is correlated with creativity.  See forwardflow.org for more information.
#' 
#' @param response_matrix A data frame containing the set of responses to calculate forward flow on.  Must have id column, followed by all responses for that observation in wide format.
#' 
#' @param semantic_space The semantic space used to compute the distances between words. 
#' 
#' @param min_response A numeric indicating the minimum number of valid responses needed to calculate forward flow on.
#' 
#' @param task Type of semantic task
#' 
#' @param prompt_word If prompt_included = FALSE, the prompt word given to participants.  Will be used as the first word in the forward flow string. 
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} / 2 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' (see \code{parellel::detectCores()})
#' 
#' @return A tibble with row ID, original ID, prompt word, and dynamic forward flow values.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> & Brendan Baker <bsb5477@psu.edu>
#' 
#' @export
#' 
# Forward Flow
# Updated 11/17/2021
forward_flow <- function(
  response_matrix,
  semantic_space = c(
    "baroni", "cbow", "cbow_ukwac",
    "en100", "glove", "tasa", "all"
  ), min_response = 10,
  task = c("free", "fluency"),
  prompt_word = NULL,
  cores
)
{
  
  # Check for missing arguments
  ## Semantic Space
  if(missing(semantic_space)){
    message("No semantic space selected. Using default: GloVe")
    semantic_space <- "glove"
  }else{
    semantic_space <- match.arg(semantic_space, several.ok = TRUE)
  }
  
  # Check for all semantic spaces
  if("all" %in% semantic_space){
    semantic_space <- c("baroni", "cbow", "cbow_ukwac", "en100", "glove", "tasa")
  }
  
  ## Parallel processing cores
  if(missing(cores)){
    cores <- round(parallel::detectCores() / 2, 0)
  }else{cores <- cores}
    
  # Check for all semantic spaces
  if(length(semantic_space) > 1){
    
    # Initialize results matrix
    ff_values <- matrix(
      nrow = nrow(response_matrix),
      ncol = length(semantic_space)
    )
    
    # Change column names
    colnames(ff_values) <- semantic_space
    
    for(i in 1:length(semantic_space)){
      
      # Let user know which semantic space
      message(
        paste("Computing forward flow with ", semantic_space[i], "...", sep = "")
      )
      
      # Compute forward flow
      ff_values[,i] <- ff_function(
        response_matrix = response_matrix,
        semantic_space = semantic_space[i],
        min_response = min_response,
        task = task,
        prompt_word = prompt_word,
        cores = cores
      )
      
    }
    
    
  }else{
    
    # Initialize results matrix
    ff_values <- matrix(
      nrow = nrow(response_matrix),
      ncol = length(semantic_space)
    )
    
    # Change column names
    colnames(ff_values) <- semantic_space
    
    # Compute forward flow
    ff_values[,1] <- ff_function(
      response_matrix = response_matrix,
      semantic_space = semantic_space,
      min_response = min_response,
      task = task,
      prompt_word = prompt_word,
      cores = cores
    )
    
  }
  
  return(as.data.frame(ff_values))
  
}