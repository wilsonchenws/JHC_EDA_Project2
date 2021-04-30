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

qplot(year,sum,data = df_for_plot3,facets = ~type)

