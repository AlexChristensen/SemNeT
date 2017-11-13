#' Semantic Network Cleaner
#' @description An automated cleaning function for semantic network data
#' @param data A dataset of verbal fluency or linguistic data
#' @return A binary matrix of responses (rows = participants, columns = responses)
#' @examples
#' \dontrun{
#' 
#' data<-read.csv(file.choose(),header=FALSE,sep=",",as.is=TRUE)
#' 
#' rmat<-semnetcleaner(data)
#' }
#' @references 
#' Hornik, K., & Murdoch, D. (2010).
#' Watch Your Spelling!.
#' \emph{The R Journal}, 3(2), 22-28.
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Semantic Network Cleaner----
semnetcleaner<-function(data)
{
  #install/load packages
  if (!require("pluralize"))
  {
    devtools::install_github("hrbrmstr/pluralize")
    library(pluralize)
  }else library(pluralize)
  
  #perform spell check
  v<-apply(data,c(2),qdap::check_spelling_interactive)
  
  #transform data into a writeable format
  y<-as.data.frame(do.call(cbind,ifelse(v=="NULL",data,v)))
  
  
  #singularize data
  w<-apply(y,c(2),pluralize::singularize)
  
  #grab unique responses only
  uni<-rbind(sort(unique(tolower(unlist(apply(w,c(2),unique))))))
  uni[uni==""]<-NA
  uni[uni==" "]<-NA
  uni[uni=="  "]<-NA
  while(any(is.na(uni)))
    for (i in 1:length(uni))
      if(is.na(uni[i])){uni<-uni[-i]}
  #attach unique responses to response matrix
  resp<-t(w) #transpose response
  z<-matrix(nrow=nrow(resp),ncol=length(uni)) #initialize matrix
  for (i in 1:ncol(resp)) #populate response matrix
  {
    z[,i]<-resp[,i]
  }
  o<-rbind(uni,z) #add unique response to top
  
  #binarize responses
  k<-matrix(nrow=nrow(o),ncol=ncol(o))
  for (i in 1:ncol(o))
    for (j in 2:nrow(o))
      k[j,]<-match(o[1,],o[j,])
  k[is.na(k)]<-0
  for (i in 1:ncol(o))
    for (j in 2:nrow(o))
      if (k[j,i]>0){k[j,i]<-1}
  k<-k[-1,]
  colnames(k)<-o[1,]
  k<-as.data.frame(k)
  return(k)
}
#----
#' Convergence Function
#' @description Merge a column of binarized response data with another
#' @param word The column name that will incoporate the \strong{replace} column's binarized responses (must be characters)
#' @param replace The column name that should be merged with the \strong{word} column (must be characters)
#' @return The response matrix with the \strong{word} column merged and the \strong{replace} column removed
#' @examples
#' \dontrun{
#' 
#' rmat <- converge("cat","abyssinian")
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Converge Function----
converge <- function (word, replace)
{
    if(any(colnames(k)==replace))
    {
        if(any(colnames(k)==word))
        {
            for(i in 1:nrow(k))
                if(k[i,which(colnames(k)==replace)]==1)
                {
                    k[i,which(colnames(k)==word)] <- 1
                }
            
            #make sure appropriate response converged
            k[which(colnames(k)==word)]
            #remove column with spelling difference
            k<-k[-which(colnames(k)==replace)]
            
            return(k)
        }else{stop("word not found")}
    }else{stop("word to replace not found")}
}
#----
#' Finalize Function
#' @description Finalizes the response matrix by keeping responses that are given by two or more people
#' @param rmat A semnetcleaner and converge filtered response matrix
#' @return A matrix with responses given by two or more people
#' @examples
#' \dontrun{
#' 
#' finalRmat <- finalize(rmat)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Finalize Function----
finalize <- function (rmat)
{rmat <- rmat[which(colSums(rmat)>=2)]
return(rmat)}
#----