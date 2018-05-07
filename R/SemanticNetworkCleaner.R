#' Semantic Network Cleaner
#' @description An automated cleaning function for semantic network data
#' @param data A dataset of verbal fluency or linguistic data
#' @param miss Value for missing data. Defaults to 99
#' @return A list of a binary matrix of responses (binary; rows = participants, columns = responses) and cleaned response matrix (responses)
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
semnetcleaner<-function(data, miss = 99)
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
  w<-apply(y,c(2),pluralize::singularize)
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
#' Autmated Converge Responses
#' @description Automated \link[SemNetCleaner]{converge} function merging of columns of binarized response data with another
#' @param rmat A semnetcleaner filtered response matrix
#' @return The response matrix with the \strong{word} column merged and the \strong{replace}
#' column removed for all variables
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @importFrom utils menu
#' @export
#Automated Converge Function----
autoConverge <- function (rmat)
{
    is.letter <- function(x) 
    {grepl("[[:alpha:]]", x)}
    
    n <- ncol(rmat)
    
    name <- colnames(rmat)
    
    repmat <- rmat
    
    for(i in n:1)
    {
        #check for converge
        check <- colnames(rmat)[i]
        
        first <- substring(check,1,1)
        
        if(!is.letter(first))
        {
            for(j in 2:nchar(check))
            {
                let <- is.letter(substring(check,j,j))
                
                if(let)
                {break}
            }
            first <- substring(check,j,j)
        }
        
        start <- as.character(paste("^",first,sep=""))
        
        print(check)
        
        ans <- menu(c("Yes","No","RENAME","REMOVE","TYPE MY OWN"),title="Converge response?")
        
        if(ans==1)
        {
            #potential converges
            pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
            
            ans <- menu(c(pot,"STARTS WITH A DIFFERENT LETTER","TYPE MY OWN"),title="Converge response?\nPotential responses:")
            
            if(ans==length(pot)+2)
            {
                resp <- readline("Response: ")
                
                orresp <- resp
                
                noresp <- !any(colnames(repmat)==resp)
                
                while(noresp)
                {
                    resp <- readline("No response with that name. Type a new response or press 1 to view options: ")
                    
                    if(resp==1)
                    {
                        first <- substring(orresp,1,1)
                        start <- as.character(paste("^",first,sep=""))
                        pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
                        print(pot)
                        resp <- readline("Type response: ")
                    }
                    
                    noresp <- !any(colnames(repmat)==resp)
                }
                
                repmat <- converge(repmat,as.character(resp),check)
            }else if(ans!=(length(pot)+1))
            {repmat <- converge(repmat,as.character(pot[ans]),check)}
            
            while(ans==length(pot)+1)
            {
                ans2 <- menu(letters,title="Which letter?")
                
                if(!is.letter(ans2))
                {start <- as.character(paste("^",letters[ans2],sep=""))
                }else if(is.letter(ans2))
                {start <- as.character(paste("^",ans2,sep=""))}
                
                #potential converges
                pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
                
                ans <- menu(c(pot,"STARTS WITH A DIFFERENT LETTER","REMOVE","TYPE MY OWN"),title="Converge response?\nPotential responses:")
                
                if(ans==length(pot)+3)
                {
                    resp <- readline("Type response: ")
                    
                    orresp <- resp
                    
                    noresp <- !any(colnames(repmat)==resp)
                    
                    while(noresp)
                    {
                        resp <- readline("No response with that name. Type a new response or press 1 to view options: ")
                        
                        if(resp==1)
                        {
                            first <- substring(orresp,1,1)
                            start <- as.character(paste("^",first,sep=""))
                            pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
                            print(pot)
                            resp <- readline("Type response: ")
                        }
                        
                        noresp <- !any(colnames(repmat)==resp)
                    }
                    
                    repmat <- converge(repmat,as.character(resp),check)
                }else if(ans==(length(pot)+2))
                {repmat <- repmat[,-i]
                }else if(ans!=(length(pot)+1))
                {repmat <- converge(repmat,as.character(pot[ans]),check)}
            }
        }else if(ans==3)
        {
            newname <- readline("New name for response: ")
            colnames(repmat)[i] <- newname
        }else if(ans==4)
        {repmat <- repmat[,-i]
        }else if(ans==5)
        {
            resp <- readline("Type response: ")
            
            orresp <- resp
            
            noresp <- !any(colnames(repmat)==resp)
            
            while(noresp)
            {
                resp <- readline("No response with that name. Type a new response or press 1 to view options: ")
                
                if(resp==1)
                {
                    first <- substring(orresp,1,1)
                    start <- as.character(paste("^",first,sep=""))
                    pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
                    print(pot)
                    resp <- readline("Type response: ")
                }
                
                noresp <- !any(colnames(repmat)==resp)
            }
            
            repmat <- converge(repmat,as.character(resp),check)
        }
    }
    
    return(repmat)
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
    if(length(colnames(rmatA))>=length(colnames(rmatB)))
    {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
    }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
    {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
    }else if(all(match(colnames(rmatA),colnames(rmatB))))
    {print("Responses match")}
    
    if(length(colnames(rmatA))>=length(colnames(rmatB)))
    {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
    }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
    {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
    }else if(all(match(colnames(rmatA),colnames(rmatB))))
    {print("Responses match")}
    return(list(rmatA=rmatA,rmatB=rmatB))
}
#----
#' De-string Responses
#' @description De-string responses after performing semnetcleaner
#' @param rmat A semnetcleaner filtered response matrix
#' @param rm.str The column number of the stringed response
#' @return The response matrix with the string column merged into appropriate response columns and the string response removed
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#De-string Function----
destr <- function (rmat, rm.str)
{
    
    replace<-unlist(strsplit(colnames(rmat[rm.str]),"[.]"))
    
    if(any(is.na(match(replace,colnames(rmat)))))
    {warning(paste(replace[which(is.na(match(replace,colnames(rmat))))],"is not a current response for subject",
                   which(rmat[colnames(rmat[rm.str])]!=0),"additional manipulation is required",sep=" ",collapse=", "))
        replace<-replace[-which(is.na(match(replace,colnames(rmat))))]}
    
    rmat[match(replace,colnames(rmat))][which(rmat[rm.str]!=0),]<-1
    rmat<-rmat[,-rm.str]
}
#----
#' Automated De-string Responses
#' @description Automated de-string responses after performing semnetcleaner
#' @param rmat A semnetcleaner filtered response matrix
#' @param char Minimum number of characters in a string to be checked for \link[SemNetCleaner]{destr}.
#' Defaults to 10
#' @return A question asking whether the response should be de-string-ed.
#' If yes, \link[SemNetCleaner]{destr} will be applied.
#' If no, the next response will be checked
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Automated De-string Function----
autoDeStr <- function (rmat, char = 10)
{
    #potential de-string candidates
    pot <- which(nchar(colnames(rmat))>char)
    
    for(i in length(pot):1)
    {
        print(colnames(rmat[pot[i]]))
        ans <- menu(c("Yes","No"),title="De-string response?")
        
        if(ans==1)
        {rmat <- destr(rmat, pot[i])}
    }
    
    return(rmat)
}
#----
#' Partial Bootstrapped Semantic Network Analysis
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' @param data Cleaned response matrix
#' @param nodes Number of nodes for partial bootstrap.
#' Defaults to half
#' @param iter Number of iterations in bootstrap.
#' Defaults to 1000
#' @param corr Association method to use.
#' Defaults to "cosine"
#' @param cores Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maxmimum amount of cores on your computer
#' @param seeds Seeds used in previous run.
#' Defaults to NULL.
#' Input a vector from previous run to replicate analyses
#' @return Returns a list that includes the original semantic network measures (origmeas; ASPL, CC, Q, S),
#' the bootstrapped semantic network measures (bootmeas),
#' and Seeds that can be used to replicate analysis
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @importFrom stats cor runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach %dopar%
#' @export
#Partial Bootstrapped Semantic Network Analysis----
semnetboot <- function (data, nodes,
                        iter = 1000, corr = c("cor","cosine"),
                        cores, seeds = NULL)
{
    if(missing(nodes))
    {nodes<-round((ncol(data)/2),0)
    }else(nodes<-round(nodes,0))
    
    if(missing(corr))
    {corr <- "cosine"
    }else{corr <- match.arg(corr)}
    
    if(corr=="cor")
    {cormat <- cor(data)
    }else{cormat <- lsa::cosine(as.matrix(data))}
    
    if(is.null(seeds))
    {Seeds <- vector(mode="numeric",length=iter)
    }else{
        seeds<-as.vector(seeds)
        Seeds <- seeds
        iter <- length(seeds)
        }
    
    full <- ncol(data)
    
    sampslist<-list() #initialize sample list
    
    #Parallel processing
    if(missing(cores))
    {
        cores <- parallel::detectCores()
        cores <- cores - 1 #not to overload your computer 
    }else{cores <- cores}
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    
        pb <- txtProgressBar(max=iter, style = 3) #progress bar
        progress <- function(num) setTxtProgressBar(pb, num)
        opts <- list(progress = progress)
    
    sampslist<-foreach::foreach(i=1:iter,
                                .packages = c("NetworkToolbox","lsa"),
                                .options.snow = opts)%dopar%
                                {
                                    f<-round(runif(i,min=1,max=1000000),0)
                                    if(is.null(seeds))
                                    {
                                        Seed <- sample(f,1)
                                        set.seed(Seed)
                                    }else{Seed <- seeds[i]
                                    set.seed(seeds[i])
                                    }
                                    
                                    mat <- data[,sample(1:full,nodes,replace=FALSE)]
                                    
                                    if(corr=="cor")
                                    {cormat <- cor(mat)
                                    }else{cormat <- lsa::cosine(as.matrix(mat))}
                                    
                                    net <- NetworkToolbox::TMFG(cormat)$A
                                    
                                    samps<-c(suppressWarnings(NetworkToolbox::semnetmeas(net,iter=10)),NetworkToolbox::conn(net)$total,Seed)
                                    return(samps)
                                }
    close(pb)
    parallel::stopCluster(cl)
    
    tru<-c(suppressWarnings(NetworkToolbox::semnetmeas(NetworkToolbox::TMFG(cormat)$A)),NetworkToolbox::conn(NetworkToolbox::TMFG(cormat)$A)$total)
    
    metrics <- matrix(0,nrow=iter,ncol=7)
    
    for(i in 1:length(sampslist))
    {
        metrics[i,] <- sampslist[[i]][1:7]
        if(is.null(seeds))
        {Seeds[i] <- sampslist[[i]][8]}
    }
    
    metrics<-as.data.frame(metrics)
    
    colnames(metrics)<-c("ASPL","CC","Q","S","randASPL","randCC","Mean Network Strength")
    
    return(list(origmeas=tru,bootmeas=metrics,Seeds=Seeds))
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