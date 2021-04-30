#Loading data processing package
library(dplyr)
library(grDevices)
library(RColorBrewer)
#Loading R object
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Assignment2 : Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland *fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
png(filename = "plot2.png")
NEI_BY_Year_City <- group_by(NEI, fips, year, .add=T)
data_bar <- NEI_BY_Year_City %>% summarize(sum = sum(Emissions)) %>%
        filter(fips == "24510")

#Setting color for bar plot
col <- brewer.pal(1,"Reds")
pal <- colorRampPalette(col,1)

#Start plotting bar chart

bars <- barplot(data_bar$sum, main = "Baltimore Maryland total PM2.5 Emission",
        xlab = "Year", names.arg = data_bar$year,
        ylim = c(0,max(data_bar$sum)+500),
        col = pal(1))
text(x = c(0.7,1.9,3.1,4.3),y = data_bar$sum-500 , labels = format(round(data_bar$sum,0),scientific = F,big.mark = ","), pos = 3)
dev.off()

