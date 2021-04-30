#Loading data processing package
library(dplyr)
library(grDevices)
library(RColorBrewer)
#Loading R object
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Check if there's any missing value issue
sum(is.na(NEI))
#return 0, meaning there's no NA　value

#Assignment: Have total emissions from PM2.5 decreased in the United States from 
#1999 to 2008? Using the base plotting system, make a plot showing the 
#total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


#1st Approach
NEIByYear <- group_by(NEI,year)
anw <- summarize(NEIByYear,sum = sum(Emissions))


marks <- c(0,1000000,2000000,3000000,4000000,5000000,6000000,7000000)
cols <- brewer.pal(1,"Blues")
pal <- colorRampPalette(cols)



png(filename = "plot1.png")
plot <- barplot(anw$sum,names.arg = anw$year, ylab = "Total PM2.5 Emission", 
        xlab = "Year",yaxt = 'n',col = pal(1),ylim =c(0, 7500000),
        main = "Have total emissions from PM2.5 decreased from 1999 to 2008?")


axis(2,at = marks,cex.axis = 0.8,labels = format(marks,scientific = F,big.mark = ','))
text(plot,c(anw$sum - 1000000),labels = format(round(anw$sum,0),scientific = F,big.mark = ','),cex = 1,pos = 3)


dev.off()

