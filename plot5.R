#Loading necessary libraries for data processing and plotting
library(dplyr)
library(ggplot2)
library(grDevices)
library(RColorBrewer)

#Reading data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Checking missing value of data.
for (i in 1:dim(NEI)[2]){
    print(sum(grepl("NA",NEI[,i])))
}
# Result of above code let us know 473 out of 6497651 (0.007%) observation missed fips code.

#Convert string to N.A 
NEI[grepl("NA",NEI$fips),1] <- NA

#From below document, we know that motor vehicle sources SCC code start with 220, and Baltimore city fips code
#is 24510.
# SCC Document: https://ofmpub.epa.gov/sccwebservices/sccsearch/docs/SCC-IntroToSCCs_2021.pdf
# Wikipedia for fips code: https://en.wikipedia.org/wiki/List_of_counties_in_Maryland

# Filtering data we need

df_plot <- NEI %>%
    filter(fips == 24510 & grepl("^220",SCC)) %>%
    group_by(year) %>%
    summarise(total_pm25 = sum(Emissions))

# Using simple bar chart to represent the data:
plot5 <- ggplot(data = df_plot,aes(as.factor(year),total_pm25),group = 1)+ 
    geom_point()+
    geom_text(aes(label = format(round(total_pm25,2),big.mark = ",")),vjust = -0.5, color = "black", size = 3)
