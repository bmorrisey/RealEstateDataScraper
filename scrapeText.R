scrapeText <- function(htmlLines,startPattern,endPattern){
        
        pageOffset=nchar(startPattern)
        result <- NA
        
        for(l in 1:length(htmlLines)){
                # Search through each line of HTML looking for startPattern
                
                if(length(startPattern)>1){
                        patternLocation=myMultiStrFind(htmlLines[l],startPattern)
                        
                }else{
                        patternLocation=myStrFind(htmlLines[l],startPattern)
                }
                
                if(!is.null(patternLocation)){
                        # If we found the pattern
                        
                        # Look for the end pattern in this line
                        if(length(endPattern)>1){
                                endMatch=myMultiStrFind(htmlLines[l],endPattern)
                        }else{
                                endMatch=myStrFind(htmlLines[l],endPattern)
                        }
                        
                        if(is.null(endMatch)|max(endMatch)<min(patternLocation)){
                                next
                        }
                        
                        # Set up vector that will be 1:1 end location for each pattern location
                        endLocation=numeric(length(patternLocation))
                        
                        # Trim all matches for the end pattern to just the first match following each patternLocation
                        # Do this because the end pattern may be generic and frequently found in the HTML
                        for(i in 1:length(patternLocation)){
                                # Store the location of the first occurrance of the end pattern after each start pattern
                                endLocation[i] <- endMatch[endMatch>patternLocation[i]][1] 
                                # print(substring(htmlLines[l],patternLocation[i]+pageOffset,endLocation[i]-1))
                                
                        }
                        
                        # Collect the number of pages of search results
                        result=c(result, substring(htmlLines[l],patternLocation+pageOffset,endLocation-1))
                        
                }
        }
        if (length(result)>1){
                result=result[-1] #remove the leading NA if it found stuff
        }
        return(unique(result))       
}

newScrapeText <- function(htmlLines,pattern){
        #pattern is list first element is "start" second is "end"
        startPattern=pattern[[1]]
        endPattern=pattern[[2]]
        
        offset=nchar(startPattern)
        # len offset=len pattern[[1]]
        result <- as.list(rep(NA,length(startPattern)))
        
        for(l in 1:length(htmlLines)){
                # Search through each line of HTML looking for startPattern
                
                patternLocation=newStrFind(htmlLines[l],startPattern)
                
                
                # If we found the pattern
                
                # Look for the end pattern in this line
                
                endMatch=newStrFind(htmlLines[l],endPattern)
                
                
                #                         if(is.null(endMatch)|max(endMatch)<min(patternLocation)){
                #                                 next
                #                         }
                
                # Set up vector that will be 1:1 end location for each pattern location
                #                         endLocation=numeric(length(patternLocation))
                endLocation=list()
                # Trim all matches for the end pattern to just the first match following each patternLocation
                # Do this because the end pattern may be generic and frequently found in the HTML
                for(p in 1:length(startPattern)){
                        endLocation[[p]]=NA
                        for(i in 1:length(patternLocation[[p]])){
                                # Store the location of the first occurrance of the end pattern after each start pattern
                                endLocation[[p]][i] <- endMatch[[p]][endMatch[[p]]>patternLocation[[p]][i]][1] 
                                # print(substring(htmlLines[l],patternLocation[i]+offset,endLocation[i]-1))
                                
                        }
                        
                        # patternLocation is a list where each list element is a vector of starting locations for the start string
                        # endLocations is the same format as patternLocation
                        # offset is the lengths of each startPattern
                        # 
                        # Collect the actual text between locations
                        result[[p]]=unique(c(result[[p]],substring(htmlLines[l],patternLocation[[p]]+offset[p],endLocation[[p]]-1)))
                }
                
                
                
                
                
        }
        for(p in 1:length(startPattern)){
                if (length(result[[p]])>1){
                        result[[p]]=result[[p]][-1] #remove the leading NA if it found stuff
                }
        }
        return(result)       
}