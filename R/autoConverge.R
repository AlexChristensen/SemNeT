#' Autmated Converge Responses
#' @description Automated \link[SemNetCleaner]{converge} function merging of columns of binarized response data with another
#' @param rmat A semnetcleaner filtered response matrix
#' @return The response matrix with the \strong{word} column merged and the \strong{replace}
#' column removed for all variables
#' @examples
#' \donttest{
#' convmat <- autoConverge(rmat)
#' }
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
    
    output <- list()
    output$converged <- matrix(NA,nrow=n,ncol=2)
    colnames(output$converged) <- c("from","to")
    
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
        
        ans <- menu(c("Yes","No","RENAME","REMOVE","TYPE MY OWN"),title="Converge response with another response?")
        
        while(ans==1)
        {
            #potential converges
            pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
            
            ans2 <- menu(c(pot,"STARTS WITH A DIFFERENT LETTER","TYPE MY OWN"),title="Potential responses:")
            
            if(ans2==length(pot)+2)
            {
                ans <- 5
            }else if(ans2<(length(pot)+1))
            {
                output$converged[i,] <- cbind(as.character(check),as.character(pot[ans2]))
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
                repmat <- converge(repmat,as.character(pot[ans2]),check)
                break
            }
            
            while(ans2==length(pot)+1)
            {
                ans3 <- menu(letters,title="Which letter?")
                
                if(!is.letter(ans3))
                {start <- as.character(paste("^",letters[ans3],sep=""))
                }else if(is.letter(ans3))
                {start <- as.character(paste("^",ans3,sep=""))}
                
                #potential converges
                pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
                
                ans4 <- menu(c(pot,"STARTS WITH A DIFFERENT LETTER","REMOVE","TYPE MY OWN"),title="Potential responses:")
                
                if(ans4==length(pot)+3)
                {
                    ans <- 5
                }else if(ans4==(length(pot)+2))
                {
                    ans <- 4
                }else if(ans4<(length(pot)+1))
                {
                    output$converged[i,] <- cbind(as.character(check),as.character(pot[ans4]))
                    if(sum(rmat[,i]==1)!=0)
                    {output$participant[[check]] <- which(rmat[,i]==1)}
                    repmat <- converge(repmat,as.character(pot[ans4]),check)
                }else if(ans4==length(pot)+1)
                {
                    ans2 <- length(pot)
                }
            }
            if(ans4<(length(pot)+1)){break}
        }
        
        if(ans==2)
        {
            output$converged[i,] <- cbind(as.character(check),as.character(check))
            if(sum(rmat[,i]==1)!=0)
            {output$participant[[check]] <- which(rmat[,i]==1)}
        }
        
        if(ans==3)
        {
            newname <- readline("New name for response: ")
            
            if(any(newname==colnames(repmat)))
            {
                conv <- colnames(repmat)[which(newname==colnames(repmat))]
                message(paste("Response already exists. Converged with response:",conv))
                output$converged[i,] <- cbind(as.character(check),as.character(conv))
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
                repmat <- converge(repmat,as.character(conv),as.character(check))
             }else{
                colnames(repmat)[i] <- newname
                message(paste(check)," changed to ",newname)
                output$converged[i,] <- cbind(colnames(repmat)[i],newname)
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
                }
        }else if(ans==4)
        {
            message(paste("Reponse removed:",check))
            output$converged[i,] <- cbind(as.character(check),NA)
            if(sum(rmat[,i]==1)!=0)
            {output$participant[[check]] <- which(rmat[,i]==1)}
            repmat <- repmat[,-i]
        }else if(ans==5)
        {
            chn <- 0
            
            resp <- readline("Type response: ")
            
            noresp <- !any(colnames(repmat)==resp)
            
            while(noresp)
            {
                orresp <- resp
                
                message("No response with that name")
                resp <- readline("Type a new response or press 1 to view options: ")
                
                noresp <- !any(colnames(repmat)==resp)
                
                if(resp==1)
                {
                    first <- substring(orresp,1,1)
                    start <- as.character(paste("^",first,sep=""))
                    pot <- colnames(repmat[grep(start,colnames(repmat),ignore.case=TRUE)])
                    print(pot)
                    resp <- readline("Type response: ")
                }
                
                if(noresp)
                {
                    repla <- menu(c("Yes","No"),title="Replace response's name with typed response?")
                    
                    if(repla==1)
                    {
                        conv <- colnames(repmat)[which(resp==colnames(repmat))]
                        if(length(conv)!=0)
                        {
                            resp <- conv
                            message(paste("Response already exists. Converged with response:",resp))
                            noresp <- FALSE
                        }else{
                            chn <- menu(c("Yes","No"),title="Response not in response list. Change name?")
                            
                            if(chn==1)
                            {
                                output$converged[i,] <- cbind(as.character(check),as.character(resp))
                                if(sum(rmat[,i]==1)!=0)
                                {output$participant[[check]] <- which(rmat[,i]==1)}
                                colnames(repmat)[i] <- resp
                                message(paste(check)," changed to ",resp)
                                noresp <- FALSE
                            }else{noresp <- TRUE}
                        }
                    }
                }
            }
            
            if(chn!=1)
            {
                output$converged[i,] <- cbind(as.character(check),as.character(resp))
                if(sum(rmat[,i]==1)!=0)
                {output$participant[[check]] <- which(rmat[,i]==1)}
                repmat <- converge(repmat,as.character(resp),check)
            }
        }
    }
    
    output$respmat <- as.data.frame(repmat)
    output$converged <- as.data.frame(output$converged)
    
    chnResp <- which(is.na(match(output$converged$from,output$converged$to)))
    
    output$changed <- output$converged[chnResp,]
        
    output$participant <- output$participant[order(names(output$participant))]
    
    return(output)
}
#----