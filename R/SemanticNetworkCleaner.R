#' Semantic Network Cleaner
#' @description An automated cleaning function for semantic network data
#' @param data A dataset of verbal fluency or linguistic data (rows = responses, columns = participants)
#' @return A binary matrix of responses (rows = participants, columns = responses) and unique responses of the response matrix
#' @examples
#' \dontrun{
#' 
#' data<-read.csv(file.choose(),header=FALSE,sep=",",as.is=TRUE)
#' 
#' rmat<-semnetcleaner(trial)
#' }
#' @references 
#' Hornik, K., & Murdoch, D. (2010).
#' Watch Your Spelling!.
#' \emph{The R Journal}, 3(2), 22-28.
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#' @importFrom stats na.omit
#Semantic Network Cleaner----
semnetcleaner<-function(data)
{
  
    if(!require(pluralize))
    {devtools::install_github("hrbrmstr/pluralize")}
    
  for(i in 1:ncol(data))
  data[,i]<-trimws(data[,i])
  #perform spell check
  v<-apply(data,c(2),qdap::check_spelling_interactive)
  
  #transform data into a writeable format
  y<-as.data.frame(do.call(cbind,ifelse(v=="NULL",data,v)))
  if(any(is.na(y)))
  {for(i in 1:ncol(y))
      if(all(is.na(y[,i])))
      {y[,i]<-data[,i]}
  y<-na.omit(y)}
  
  
  #singularize data
  w<-apply(y,c(2),pluralize::singularize)
  
  #grab unique responses only and make them all lowercase
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
  {z[,i]<-resp[,i]}
  
  #binarize responses
  k<-matrix(nrow=nrow(z),ncol=ncol(z))
  for (i in 1:ncol(k))
    for (j in 2:nrow(k))
      k[j,]<-match(uni,z[j,])
  k[is.na(k)]<-0
  #fill out other half of matrix
  for (i in 1:ncol(k))
    for (j in 2:nrow(k))
      if (k[j,i]>0){k[j,i]<-1}
  colnames(k)<- uni
  k<-as.data.frame(k)
  return(k)
}
#----
#' Converge Responses
#' @description Merge a column of binarized response data with another
#' @param rmat A semnetcleaner filtered response matrix
#' @param word The column name that will incoporate the \strong{replace} column's binarized responses (must be characters)
#' @param replace The column name that should be merged with the \strong{word} column (must be characters)
#' @return The response matrix with the \strong{word} column merged and the \strong{replace} column removed
#' @examples
#' rmat <- converge(rmat,"cat","kitten")
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Converge Function----
converge <- function (rmat, word, replace)
{
    if(any(colnames(rmat)==replace))
    {
        if(any(colnames(rmat)==word))
        {
            for(i in 1:nrow(rmat))
                if(rmat[i,which(colnames(rmat)==replace)]==1)
                {rmat[i,which(colnames(rmat)==word)] <- 1}
            
            #coverge word to be replaced with correct word
            rmat[which(colnames(rmat)==word)]
            #remove column with spelling difference
            rmat<-rmat[-which(colnames(rmat)==replace)]
            
            return(rmat)
        }else{stop("word not found")} #produce error if word does not exist
    }else{stop("word to replace not found")} #produce error if word to replace does not exist
}
#----
#' Finalize Response Matrix
#' @description Finalizes the response matrix by keeping responses that are given by two or more people
#' @param rmat A semnetcleaner filtered response matrix
#' @return A matrix with responses given by two or more people
#' @examples
#' finalRmat <- finalize(rmat)
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Finalize Function----
finalize <- function (rmat)
{fmat <- rmat[which(colSums(rmat)>=2)]
return(fmat)}
#----
#' Equate Group Responses
#' @description An automated cleaning function for matching groups' responses
#' @param rmatA A semnetcleaner filtered response matrix for group 1
#' @param rmatB A semnetcleaner filtered response matrix for group 2
#' @return A list of responses matched for group 1 (rmatA) and group 2 (rmatB)
#' @examples
#' \dontrun{
#' 
#' groups_resp_match<-equate(rmatA,rmatB)
#' 
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
# Equate----
equate<-function(rmatA,rmatB)
{
    if(length(colnames(rmatA))>length(colnames(rmatB)))
    {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
    }else if(length(colnames(rmatB))>length(colnames(rmatA)))
    {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
    }else if(all(match(colnames(rmatA),colnames(rmatB))))
    {print("Responses match")}
    if(length(colnames(rmatA))>length(colnames(rmatB)))
    {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
    }else if(length(colnames(rmatB))>length(colnames(rmatA)))
    {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
    }else if(all(match(colnames(rmatA),colnames(rmatB))))
    {print("Responses match")}
    return(list(rmatA=rmatA,rmatB=rmatB))
}
#----
#Trial data of verbal fluency responses----
#' Trial data of verbal fluency responses
#' 
#' Trial data of animal verbal fluency responses. The columns are participants and the
#' rows are their responses.
#' 
#' @docType data
#' 
#' @usage data(trial)
#' 
#' @format A 49x15 response matrix
#' 
#' @keywords datasets
#' 
#' @examples 
#' 
#' data(trial)
"trial"
#----
#Cleaned trial data of verbal fluency responses----
#' Cleaned trial data of verbal fluency responses
#' 
#' Cleaned trial data of animal verbal fluency responses. The columns are responses and the
#' rows are the participants.
#' 
#' @docType data
#' 
#' @usage data(rmat)
#' 
#' @format A 15x95 response matrix
#' 
#' @keywords datasets
#' 
#' @examples 
#' 
#' data(rmat)
"rmat"
#----