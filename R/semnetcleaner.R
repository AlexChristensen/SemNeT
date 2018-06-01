#' Semantic Network Cleaner
#' @description An automated cleaning function for semantic network data
#' @param data A dataset of verbal fluency or linguistic data
#' @param miss Value for missing data. Defaults to 99
#' @return A list of a binary matrix of responses (binary; rows = participants, columns = responses) and cleaned response matrix (responses)
#' @examples
#' \donttest{
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
semnetcleaner <- function(data, miss = 99)
{
  for(i in 1:ncol(data))
        data[,i]<-trimws(data[,i])    
      
  for(i in 1:nrow(data))
      for(j in 1:ncol(data))
          if(is.na(data[i,j])){next
          }else if(data[i,j]==miss){data[i,j]<-NA} 
  
  if(nrow(data)<ncol(data))
  {data<-t(data)}
    
  #perform spell check
  v<-apply(data,c(2),qdap::check_spelling_interactive)
  
  y<-matrix(NA,nrow=nrow(data),ncol=ncol(data))
  #transform data into a writeable format
  if(is.list(v))
  {for(i in 1:length(v))
        {
         if(is.null(v[[i]]))
           {v[[i]]<-data[,i]}
          y[,i]<-v[[i]]
        }
    }else(y<-v)
  if(any(is.na(y)))
  for(i in 1:ncol(y))
      if(all(is.na(y[,i])))
      {y[,i]<-data[,i]}
  for(i in 1:nrow(y))
      for(j in 1:ncol(y))
          if(is.na(y[i,j]))
          {y[i,j]<-""}

  #singularize data
  singularize <- function(x)
  {
      x <- as.character(x)
      
      sing <- vector(length=length(x))
      
      if(length(x)>1)
      {
          for(i in 1:length(x))
          {
              Splural <- substring(x[i],nchar(x[i]),nchar(x[i]))
              ESplural <- substring(x[i],nchar(x[i])-1,nchar(x[i]))
              
              if(ESplural=="es")
              {sing[i] <- substring(x[i],1,nchar(x[i])-2)
              }else{sing[i] <- x[i]}
              if(Splural=="s")
              {sing[i] <- substring(x[i],1,nchar(x[i])-1)
              }else{sing[i] <- x[i]}
          }
      }else{
          Splural <- substring(x,nchar(x),nchar(x))
          ESplural <- substring(x,nchar(x)-1,nchar(x))
      
          if(ESplural=="es")
          {sing <- substring(x,1,nchar(x)-2)
          }else{sing <- x}
          if(Splural=="s")
          {sing <- substring(x,1,nchar(x)-1)
          }else{sing <- x}
      }
      
      return(sing)
  }
      
  w<-apply(y,c(2),singularize)
  w<-tolower(w)
  
  #grab unique responses only and make them all lowercase
  uni<-rbind(sort(unique(tolower(unlist(apply(w,c(2),unique))))))
  
  uni[uni==""]<-NA
  uni[uni==" "]<-NA
  uni[uni=="  "]<-NA
  while(any(is.na(uni)))
    for (i in 1:length(uni))
      if(is.na(uni[i])){uni<-uni[-i]}
  #attach unique responses to response matrix
  if(nrow(w)<ncol(w))
  {resp<-t(w)
  }else{resp<-w} #transpose response
  z<-matrix(nrow=nrow(resp),ncol=length(uni)) #initialize matrix
  for (i in 1:ncol(resp)) #populate response matrix
  {z[,i]<-resp[,i]}
  
  z<-tolower(z)
  
  #binarize responses
  k<-matrix(nrow=nrow(z),ncol=ncol(z))
      for (j in 1:nrow(k))
          if(any(!is.na(match(uni,z[j,]))))
          {k[j,]<-match(uni,z[j,])}
  k[is.na(k)]<-0
  #fill out other half of matrix
  for (i in 1:ncol(k))
    for (j in 1:nrow(k))
      if (k[j,i]!=0){k[j,i]<-1}
  colnames(k)<- uni
  if(any(rowSums(k)==0))
  {warning(paste(length(which(rowSums(k)==0))),
  " rows were removed for zero responses\nrow(s): ",paste(which(rowSums(k)==0),collapse = ", "),
  "\nsubject id(s): ",paste(colnames(data)[which(rowSums(k)==0)],collapse = ", "))
  k<-k[-which(rowSums(k)==0),]}
  k<-as.data.frame(k)
  return(list(binary=k,responses=w))
}
#----