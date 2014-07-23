myMultiStrFind <- function(mainString,searchString){
        
        
        have_a_match=list()
        
        mainString=strsplit(mainString,"")
        
        for(nPattern in seq_along(searchString)){
                
                aSearchString=strsplit(searchString[nPattern],"")
                
                matches=list()
                for(i in 1:length(aSearchString[[1]])){
                        matches[[i]] <- grep(aSearchString[[1]][i],mainString[[1]],fixed=TRUE)
                        
                }
                
                #   print(have_a_match)
                
                have_a_match[[nPattern]] <- NULL
                for(i in matches[[1]]){
                        #     print(i)
                        if (nchar(aSearchString)>1){
                                for(j in 1:(length(matches)-1)){
                                        #       print(j)
                                        if((i+j) %in% matches[[1+j]]){
                                                if(j==(length(matches)-1)){
                                                        if(nPattern<=length(have_a_match)){
                                                                have_a_match[[nPattern]]=c(have_a_match[[nPattern]],i)
                                                        }else{
                                                                have_a_match[[nPattern]]=i
                                                                
                                                        }
                                                        #           print(have_a_match)
                                                }
                                        }
                                        else{
                                                break
                                        }
                                        
                                }
                        }else{
                                #                                 print(nPattern)
                                have_a_match[[nPattern]]=c(have_a_match[[nPattern]],i)
                        }
                        
                }
                
                #   if(length(have_a_match)>1){
                #     have_a_match=have_a_match[-1]
                #   }
                
        }
        return(have_a_match)
        
        
}

myStrFind <- function(mainString,searchString){
        
        searchString=strsplit(searchString,"")
        mainString=strsplit(mainString,"")
        
        matches=list()
        for(i in 1:length(searchString[[1]])){
                matches[[i]] <- grep(searchString[[1]][i],mainString[[1]],fixed=TRUE)
                
        }
        
        have_a_match <- NULL
        for(i in matches[[1]]){
                if (nchar(searchString)>1){
                        for(j in 1:(length(matches)-1)){
                                if((i+j) %in% matches[[1+j]]){
                                        if(j==(length(matches)-1)){
                                                have_a_match=c(have_a_match,i)
                                        }
                                }
                                else{
                                        break
                                }
                                
                        }
                }else{
                        have_a_match=c(have_a_match,i)
                }
                
        }
        
        return(have_a_match)
        
        
}

newStrFind <- function(mainString,pattern){
        # main string is length N
        # pattern is length M
        
        
        
        mainString=strsplit(mainString,"")
        results=list()
        
        for(p in seq_along(pattern)){
                
                searchString=strsplit(pattern[p],"")
                matches=list()
                for(i in 1:length(searchString[[1]])){
                        matches[[i]] <- grep(searchString[[1]][i],mainString[[1]],fixed=TRUE)
                        
                }
                
                have_a_match <- NA
                for(i in matches[[1]]){
                        if (nchar(searchString)>1){
                                for(j in 1:(length(matches)-1)){
                                        if((i+j) %in% matches[[1+j]]){
                                                if(j==(length(matches)-1)){
                                                        have_a_match=c(have_a_match,i)
                                                }
                                        }
                                        else{
                                                break
                                        }
                                        
                                }
                        }else{
                                have_a_match=c(have_a_match,i)
                        }
                        
                }
                
                # Clean up leading NA if there were results
                if(length(have_a_match)>1){
                        have_a_match <- have_a_match[-1]
                }
                results[[p]] <- have_a_match
                
                
        }
        
        return(results)
        
        
}