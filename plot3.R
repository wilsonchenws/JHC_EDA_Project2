#Loading data processing package
library(dplyr)
library(grDevices)
library(RColorBrewer)
library(ggplot2)
#Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI_By_Year_Type <- group_by(NEI,type,year,.add = F)

df_for_plot3 <- summarise(NEI_By_Year_Type, sum = sum(Emissions))
df_for_plot3 <- transform(df_for_plot3, year = as.factor(year), type = as.factor(type))

col <- brewer.pal(4,"Blues")
pal <- colorRampPalette(col,4)

#Using qplot
qplot(year,sum,data = df_for_plot3,facets = ~type)

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
