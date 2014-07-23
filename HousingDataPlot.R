HousingDataPlot <- function(city1,city2){
        
        # city1="san_jose.csv"
        # city2="portland.csv"
        
        cnames=c("date","price","bed","bath","sqft","location","type","rent","zpid","url")
        
        data1=read.table(city1,header=FALSE,sep=",",col.names=cnames)
        data2=read.table(city2,header=FALSE,sep=",",col.names=cnames)
        
        # CLean up data
        # Remove Zeros and NAs
        data1=data1[data1$price>0 & data1$sqft>0 & complete.cases(data1),]
        data2=data2[data2$price>0 & data2$sqft>0 & complete.cases(data2),]
        
        mean_rent_per_sqft1=mean(data1$rent/data1$sqft)
        mean_rent_per_sqft2=mean(data2$rent/data2$sqft)
        
        c1_rp=100*data1$rent/data1$price
        c2_rp=100*data2$rent/data2$price
        #   print(max(c1_rp,c2_rp))
        bins=seq(0,max(c1_rp,c2_rp)+0.05,0.05)
        
        hist(c1_rp,breaks=bins,col=rgb(0,0,1,1/4),xlim=c(0,2),
             xlab="Rent : Price Ratio (%)",
             main="Comparison of Rent Price Ratio")
        hist(c2_rp,breaks=bins,col=rgb(1,0,0,1/4),xlim=c(0,2),add=T)
        grid()
        
        plot(data1$price,data1$rent/data1$sqft,
             xlab="Price($)",
             ylab="Rent/Sq Ft")
        lines(data2$price,data2$rent/data2$sqft,type="p",col="red")
        lines(c(1.5e5,5e5),c(mean_rent_per_sqft1,mean_rent_per_sqft1),type="l",col="black",lwd=3)
        lines(c(1.5e5,5e5),c(mean_rent_per_sqft2,mean_rent_per_sqft2),type="l",col="red",lwd=3)
        
        desiredRentRatio=0.0050;
        xLine=c(1.5e5,5e5)
        yLine=desiredRentRatio*xLine
        plot(data1$price,data1$rent,
             xlab="Price($)",
             ylab="Rent($)")
        lines(data2$price,data2$rent,type="p",col="red")
        lines(xLine,yLine,type="l",col="purple",lwd=3)
}

newHousingDataPlot <- function(cityFile){
        
        # city1="san_jose.csv"
        # city2="portland.csv"
        
        cnames=c("date","price","bed","bath","sqft","location","type","rent","zpid","url")
        
        for(f in seq_along(cityFile)){
                
                data=read.table(cityFile[f],header=FALSE,sep=",",col.names=cnames)
                
                # CLean up data
                # Remove Zeros and NAs
                data=data[data$price>0 & data$sqft>0 & complete.cases(data),]
                
                mean_rent_per_sqft=mean(data$rent/data$sqft)
                
                c_rp=100*data$rent/data$price
                #   print(max(c1_rp,c2_rp))
                bins=seq(0,max(c_rp)+0.05,0.05)
                
                hist(c_rp,breaks=bins,col=rgb(0,0,1,1/4),xlim=c(0,2),
                     xlab="Rent : Price Ratio (%)",
                     main="Comparison of Rent Price Ratio")
                grid()
                
                plot(data$price,data$rent/data$sqft,
                     xlab="Price($)",
                     ylab="Rent/Sq Ft")
                lines(c(1.5e5,5e5),c(mean_rent_per_sqft,mean_rent_per_sqft),type="l",col="black",lwd=3)
                
                desiredRentRatio=0.0050;
                xLine=c(1.5e5,5e5)
                yLine=desiredRentRatio*xLine
                plot(data$price,data$rent,
                     xlab="Price($)",
                     ylab="Rent($)")
                lines(xLine,yLine,type="l",col="purple",lwd=3)
        }
}