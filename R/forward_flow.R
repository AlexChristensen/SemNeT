#' Forward Flow
#'
#' @description This function calculates the
#' forward flow of a list of words (Gray et. al, 2019).
#' Forward flow is a way to quantify the forward motion of
#' naturalistic thought. See \href{https://forwardflow.org}{forwardflow.org} for more information.
#'
#' @param response_matrix Matrix, data frame, or
#' \code{\link[SemNetCleaner]{textcleaner}} object.
#' For \code{type = "fluency"}, data are expected to
#' follow wide formatting (IDs are the row names
#' and are \strong{not} a column in the matrix
#' or data frame):
#' 
#' \tabular{cccc}{
#' 
#' \code{row.names} \tab Response 1 \tab Response 2 \tab Response n \cr
#' ID_1 \tab 1 \tab 2 \tab n \cr
#' ID_2 \tab 1 \tab 2 \tab n \cr
#' ID_n \tab 1 \tab 2 \tab n 
#' }
#' 
#' For \code{type = "free"}, data are expected to
#' follow long formatting:
#' 
#' \tabular{ccc}{
#' 
#' ID \tab Cue \tab Response \cr
#' 1 \tab 1 \tab 1 \cr
#' 1 \tab 1 \tab 2 \cr
#' 1 \tab 1 \tab n \cr
#' 1 \tab 2 \tab 1 \cr
#' 1 \tab 2 \tab 2 \cr
#' 1 \tab 2 \tab n \cr
#' 1 \tab n \tab 1 \cr
#' 1 \tab n \tab 2 \cr
#' 1 \tab n \tab n \cr
#' 2 \tab 1 \tab 1 \cr
#' 2 \tab 1 \tab 2 \cr
#' 2 \tab 1 \tab n \cr
#' 2 \tab 2 \tab 1 \cr
#' 2 \tab 2 \tab 2 \cr
#' 2 \tab 2 \tab n \cr
#' 2 \tab n \tab 1 \cr
#' 2 \tab n \tab 2 \cr
#' 2 \tab n \tab n \cr
#' n \tab 1 \tab 1 \cr
#' n \tab 1 \tab 2 \cr
#' n \tab 1 \tab n \cr
#' n \tab 2 \tab 1 \cr
#' n \tab 2 \tab 2 \cr
#' n \tab 2 \tab n \cr
#' n \tab n \tab 1 \cr
#' n \tab n \tab 2 \cr
#' n \tab n \tab n
#' 
#' }
#' 
#' @param semantic_space Character vector.
#' The semantic space used to compute the distances between words
#' (more than one allowed). Here's a list of the semantic spaces:
#' 
#' \itemize{
#' 
#' \item{\code{"baroni"}}
#' {Combination of British National Corpus, ukWaC corpus, and a 2009
#' Wikipedia dump. Space created using continuous bag of words algorithm
#' using a context window size of 11 words (5 left and right)
#' and 400 dimensions. Best word2vec model according to
#' Baroni, Dinu, & Kruszewski (2014)}
#' 
#' \item{\code{"cbow"}}
#' {Combination of British National Corpus, ukWaC corpus, and a 2009
#' Wikipedia dump. Space created using continuous bag of words algorithm with
#' a context window size of 5 (2 left and right) and 300 dimensions}
#' 
#' \item{\code{"cbow_ukwac"}}
#' {ukWaC corpus with the continuous bag of words algorithm with
#' a context window size of 5 (2 left and right) and 400 dimensions}
#' 
#' \item{\code{"en100"}}
#' {Combination of British National Corpus, ukWaC corpus, and a 2009
#' Wikipedia dump. 100,000 most frequent words. Uses moving window model
#' with a size of 5 (2 to the left and right). Positive pointwise mutual
#' information and singular value decomposition was used to reduce the
#' space to 300 dimensions}
#' 
#' \item{\code{"glove"}}
#' {\href{https://dumps.wikimedia.org/}{Wikipedia 2014 dump} and \href{https://catalog.ldc.upenn.edu/LDC2011T07}{Gigaword 5} with 400,000
#' words (300 dimensions). Uses co-occurrence of words in text
#' documents (uses cosine similarity)}
#' 
#' \item{\code{"tasa"}}
#' {Latent Semantic Analysis space from TASA corpus all (300 dimensions).
#' Uses co-occurrence of words in text documents (uses cosine similarity)}
#' 
#' \item{\code{"all"}}
#' {Computes semantic distance using all semantic spaces}
#' 
#' }
#' 
#' All semantic spaces are available for download on our
#' \href{https://osf.io/4fmnd/}{OSF}. Semantic spaces are downloaded
#' into R using the \code{\link[googledrive]{drive_download}}. For more information
#' on these spaces, see 
#' \href{https://sites.google.com/site/fritzgntr/software-resources/semantic_spaces}{Gunther, Dudschig, & Kaup (2015)}
#' and \href{https://nlp.stanford.edu/projects/glove/}{Pennington, Socher, & Manning (2014).}
#' 
#' User-defined semantic spaces can also be used. The data object
#' should be input instead of a character 
#' 
#' @param min_cue Numeric.
#' Minimum number of cues participant must have provided responses
#' to compute forward flow (given \code{NA} if criterion not met)
#' 
#' @param min_response Numeric.
#' Minimum number of responses to compute forward flow.
#' Defaults to \code{3}, which ensures flow and derivatives can be computed
#' 
#' @param max_response Numeric.
#' Maximum number of responses to compute forward flow.
#' Useful for avoid confounding fluency (number of responses generated
#' by a participant) when computing forward flow.
#' Defaults to \code{NULL}, which uses all possible responses.
#' When set, only the first \code{n} responses will be used
#' 
#' @param type Character.
#' Type of semantic task.
#' Automatically determined when a
#' \code{\link[SemNetCleaner]{textcleaner}} object is
#' input as the \code{response_matrix}
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} / 2 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' (see \code{parellel::detectCores()})
#' 
#' @return A list labeled with each semantic space used. Values in 
#' each list correspond to dynamic forward flow values (Gray et al., 2019).
#' For \code{"fluency"} \strong{and} \code{"free"} data, each list will contain:
#' 
#' \item{mean_flow}{A data frame of the average forward flow over all responses (in all cues)
#' for each participant}
#' 
#' \item{response_flow}{A list corresponding to each participants forward flow
#' for each of their responses (for each cue)}
#' 
#' In addition, \code{"free"} has a data frame containing:
#' 
#' \item{mean_response_flow}{A data frame storing the mean forward flow
#' for each cue for each participant}
#' 
#' @examples
#' # Load data
#' response_matrix <- open.clean
#' 
#' \dontrun{# Forward flow on fluency
#' animals_ff <- forward_flow(
#'  response_matrix = response_matrix,
#'  semantic_space = "glove",
#'  type = "fluency",
#'  cores = 2 # for CRAN checks
#' )}
#' 
#' @references 
#' # \code{"baroni"} \cr
#' Baroni, M., Dinu, G., & Kruszewski, G. (2014).
#' Don't count, predict! a systematic comparison of context-counting vs. context-predicting semantic vectors.
#' In \emph{Proceedings of the 52nd annual meting of the association for computational linguistics} (pp. 238-247).
#' 
#' Beaty, R. E., Zeitlen, D. C., Baker, B. S., & Kenett, Y. N. (2021).
#' Forward flow and creative thought: Assessing associative cognition and its role in divergent thinking.
#' \emph{Thinking Skills and Creativity}, 100859.
#' 
#' Gray, K., Anderson, S., Chen, E. E., Kelly, J. M., Christian, M. S., Patrick, J., ... & Lewis, K. (2019).
#' "Forward flow": A new measure to quantify free thought and predict creativity.
#' \emph{American Psychologist}, \emph{74}(5), 539-554.
#' 
#' # \code{"glove"} \cr
#' Pennington, J., Socher, R., & Manning, C. D. (2014).
#' GloVe: Global vectors for word representation.
#' In \emph{Proceedings of the 2014 conference on empirical methods in natural language processing} (pp. 1532-1543).
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> & Brendan Baker <bsb5477@psu.edu>
#' 
#' @export
#' 
# Forward Flow
# Updated 22.09.2022
forward_flow <- function(
  response_matrix,
  semantic_space = c(
    "baroni", "cbow", "cbow_ukwac",
    "en100", "glove", "tasa", "all"
  ),
  min_cue = NULL,
  min_response = 3,
  max_response = NULL,
  type = c("free", "fluency"),
  cores
)
{
  # Check response matrix for textcleaner
  if("textcleaner" %in% class(response_matrix)){
    
    if("free" %in% class(response_matrix)){
      
      type <- "free"
      response_matrix <- response_matrix$data$clean
      
    }else if("fluency" %in% class(response_matrix)){
      
      type <- "fluency"
      response_matrix <- response_matrix$responses$clean
      
    }
    
  }
  
  # Check for missing arguments
  ## Semantic Space
  if(missing(semantic_space)){
    message("No semantic space selected. Using default: GloVe")
    semantic_space <- "glove"
  }
  
  ## Parallel processing cores
  if(missing(cores)){
    cores <- round(parallel::detectCores() / 2, 0)
  }else{cores <- cores}
  
  # Determine whether semantic space is pre-defined
  predefined <- is(semantic_space, "character")
  
  # Check for pre-defined semantic space
  if(isTRUE(predefined)){
    
    # Convert semantic space to lower
    semantic_space <- tolower(semantic_space)
    
    # Check for all semantic spaces
    if("all" %in% semantic_space){
      semantic_space <- c("baroni", "cbow", "cbow_ukwac", "en100", "glove", "tasa")
    }
    
    # Initialize results list
    results <- vector("list", length(semantic_space))
    names(results) <- semantic_space
    
  }else{
    
    # Initialize results list
    results <- vector("list", 1)
    names(results) <- deparse(substitute(semantic_space))
    
  }
    
  # Loop through semantic spaces
  for(i in seq_along(results)){
    
    # Let user know which semantic space
    message(
      paste("Computing forward flow with ", names(results)[i], "...", sep = "")
    )
    
    # Check for predefined semantic space
    if(isTRUE(predefined)){
      input_space <- semantic_space[i]
    }else{
      input_space <- semantic_space
    }
    
    
    # Compute forward flow
    results[[i]] <- ff_function(
      response_matrix = response_matrix,
      semantic_space = input_space,
      min_cue = min_cue,
      min_response = min_response,
      max_response = max_response,
      task = type,
      cores = cores
    )
    
  }
  
  # Let user know about the rounding to three of the semantic space
  message("Dimension vectors of the semantic space are rounded to three decimal places to ensure efficient analysis")
  
  return(results)
  
}
