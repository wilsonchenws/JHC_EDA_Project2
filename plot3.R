#Loading data processing package
library(dplyr)
library(grDevices)
library(RColorBrewer)
library(ggplot2)
#Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df_for_plot3 <- NEI %>%
    filter(fips == "24510") %>%
    group_by(type,year,.add = F) %>%
    summarise(sum = sum(Emissions)) %>%
    transform(year = as.factor(year), type = as.factor(type))


#Using ggplot


png(filename = "plot3.png", width = 1200, height = 800)
graph <- ggplot(data = df_for_plot3, aes(x = year, y= sum, fill = year)) 
graph + geom_bar(stat = "identity") + 
    scale_fill_brewer(palette = "Blues") +
    facet_grid(.~type) + 
    geom_text(aes(label = format(round(sum,0),big.mark = ",")), 
              vjust = -0.3, color = "black",position = position_dodge(0.9), size = 4) +
    xlab("Year") +
    ylab("Total PM2.5 Emissions") +
    theme_minimal()
dev.off()
