library(pracma)
source("myStrFind.R")
source("scrapeText.R")
source("HousingDataPlot.R")
# Initial tests look like my strfind function is noticeably faster than the pracma function

city="San Diego"
state="CA"
min_price=150000
max_price=800000
min_beds=1

# file_name="search_results.csv"
file_name=paste(gsub("[ ]","_",city),"_",as.Date(Sys.Date()),".csv",sep="")

city=gsub("[ ]","-",city)
loc_str=paste(city,"-",state,sep="")
price_str=paste(format(min_price,scientific=FALSE),"-",format(max_price,scientific=FALSE),"_price",sep="")


search_url=paste("http://www.zillow.com/homes/for_sale/",
                 loc_str,
                 "/fsba,fsbo,new_lt/",
                 min_beds,
                 "-_beds/",
                 price_str,
                 "/days_sort/12_zm/0_mmm/",sep="")

# goalURL="http://www.zillow.com/homes/for_sale/Ventura-CA/fsba,fsbo,new_lt/2-_beds/300000-450000_price/days_sort/12_zm/0_mmm/"

# Get search pages from search url
cat("Getting Search Result Page URLS","\n")
con <- url(search_url,"r")
htmlLines=readLines(con)
close(con)

# Define the search patterns that show the search page results that we want
startPattern="SearchMain.changePage("
linkPattern=");return false;\" href=\""
endPattern=");return"
linkEndPattern="\">"
linkOffset=nchar(linkPattern)
pageOffset=nchar(startPattern)

links=NULL

for(l in 1:length(htmlLines)){
        # Search through each line of HTML looking for startPattern
        
        #   patternLocation=strfind(htmlLines[l],startPattern)
        patternLocation=myStrFind(htmlLines[l],startPattern)
        
        if(!is.null(patternLocation)){
                # If we found the pattern
                
                # Look for the end pattern in this line
                endMatch=myStrFind(htmlLines[l],endPattern)
                linkEndMatch=myStrFind(htmlLines[l],linkEndPattern)
                
                # Set up vector that will be 1:1 end location for each pattern location
                endLocation=numeric(length(patternLocation))
                linkEndLocation=numeric(length(patternLocation))
                
                # Trim all matches for the end pattern to just the first match following each patternLocation
                # Do this because the end pattern may be generic and frequently found in the HTML
                for(i in 1:length(patternLocation)){
                        # Store the location of the first occurrance of the end pattern after each start pattern
                        endLocation[i] <- endMatch[endMatch>patternLocation[i]][1] 
                        linkEndLocation[i] <- linkEndMatch[linkEndMatch>patternLocation[i]][1] 
                        # print(substring(htmlLines[l],patternLocation[i]+pageOffset,endLocation[i]-1))
                        
                }
                
                # Collect the number of pages of search results
                pageNumbers=substring(htmlLines[l],patternLocation+pageOffset,endLocation-1)
                pageNums=as.numeric(pageNumbers)
                
                # Collect the links for shown pages of search results (this might be a subset of total search result pages)
                link=substring(htmlLines[l],patternLocation+pageOffset+nchar(pageNumbers)+linkOffset,linkEndLocation-1)
                
                # Divide the link text around the page number parameter
                pageParameterLocation=myStrFind(link[1],paste(pageNumbers[1],"_p",sep=""))
                part0="http://www.zillow.com"
                part1=substring(link[1],1,pageParameterLocation-1)
                part2=substring(link[1],pageParameterLocation+nchar(pageNumbers[1]),)
                
                # Make a vector of links for ALL the pages of search results
                links=c(links, paste(part0,part1,seq(pageNums[1],max(pageNums)),part2,sep=""))
                
        }
        
        
        
}

links=c(search_url,links)
# Use substring(x,start,stop)


# For each search result page, scrape it for the individual home links
cat("Getting Home URLS","\n")

startPattern="jpg\"><a href=\"/homedetails/"
endPattern="\" class"
prefix="http://www.zillow.com/homedetails/"
homeLinks=NULL

for(l in seq_along(links)){
        
        # Get search pages from search url
        con <- url(links[l],"r")
        htmlLines=readLines(con)
        close(con) 
        
        homeLinks <- c(homeLinks,paste(prefix,scrapeText(htmlLines,startPattern,endPattern),sep=""))
        
}


# For each home link, scrape it for data
cat("Getting Home Data","\n")
num_houses=length(homeLinks)

pricePattern="\"Price\", \"$"
priceEndPattern="\"],"
priceStr=character(num_houses)

bedPattern="data['zbed'] = '"
bedEndPattern="';"
bedStr=character(num_houses)

bathPattern="data['zbath'] = '"
bathEndPattern="';"
bathStr=character(num_houses)

sqftPattern="name=\"bath\"></input><input value=\""
sqftEndPattern="\" id=\"sqft\""
sqftStr=character(num_houses)

typePattern="\"PropertyType\", \""
typeEndPattern="\"],"
typeStr=character(num_houses)

locationPattern="itemprop=\"addressLocality\">"
locationEndPattern="</span>"
locationStr=character(num_houses)

rentPattern="<div class=\"zest-value\">$"
rentEndPattern="/mo"
rentStr=character(num_houses)

zpidPattern="name=\"zpid\" value=\""
zpidEndPattern="\" />"
zpidStr=character(num_houses)

patterns=list(start=c(pricePattern,bedPattern,bathPattern,sqftPattern,typePattern,locationPattern,rentPattern,zpidPattern),
                    end=c(priceEndPattern,bedEndPattern,bathEndPattern,sqftEndPattern,typeEndPattern,locationEndPattern,rentEndPattern,zpidEndPattern))


for(h in seq_along(homeLinks)){
        
        cat("\014")
        pct=100*h/num_houses
        cat("Progress",round(pct,digits=1),"%")
        # Get home pages from home link
        con <- url(homeLinks[h],"r")
        htmlLines=readLines(con)
        close(con)
        
        #         res=as.numeric(gsub(",","",scrapeText(htmlLines,pricePattern,priceEndPattern)))
        #         print(res)
        
        
        scraped_data=newScrapeText(htmlLines,patterns)
        
#         price=scrapeText(htmlLines,pricePattern,priceEndPattern)
#         bed=scrapeText(htmlLines,bedPattern,bedEndPattern)
#         bath=scrapeText(htmlLines,bathPattern,bathEndPattern)
#         sqft=scrapeText(htmlLines,sqftPattern,sqftEndPattern)
#         type=scrapeText(htmlLines,typePattern,typeEndPattern)
#         location=scrapeText(htmlLines,locationPattern,locationEndPattern)
#         rent=scrapeText(htmlLines,rentPattern,rentEndPattern)
#         zpid=scrapeText(htmlLines,zpidPattern,zpidEndPattern)
        priceStr[h]=scraped_data[[1]]
        bedStr[h]=scraped_data[[2]]
        bathStr[h]=scraped_data[[3]]
        sqftStr[h]=scraped_data[[4]]
        typeStr[h]=scraped_data[[5]]
        locationStr[h]=scraped_data[[6]]
        rentStr[h]=scraped_data[[7]]
        zpidStr[h]=scraped_data[[8]]
        
}

date=rep(as.Date(Sys.Date()),num_houses)
price=as.numeric(gsub(",","",priceStr))
bed=as.numeric(bedStr)
bath=as.numeric(bathStr)
sqft=as.numeric(gsub(",","",sqftStr))
type=gsub(" \\/ "," ",typeStr,fixed=TRUE)
location=locationStr
rent=as.numeric(gsub(",","",rentStr))
zpid=as.numeric(zpidStr)
url=homeLinks

allData=data.frame(date,price,bed,bath,sqft,location,type,rent,zpid,url)

# Write data to file
if(file.exists(file_name)){
        file.remove(file_name)
}

outFile=file(file_name,"a")

# write.csv(allData,outFile,row.names=FALSE,col.names=FALSE)
write.table(allData,outFile,sep=",",row.names=FALSE,col.names=FALSE)

close(outFile)

newHousingDataPlot(file_name)