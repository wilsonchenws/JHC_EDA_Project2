library(dplyr)
library(ggplot2)
library(grDevices)
library(RColorBrewer)
library(scales)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Missing value processed
NEI[grepl("NA",NEI$fips),1] <- NA

#Based on analysis done in plot4, we know Baltimore City fips code (24510) and SCC code for 
#motore vehicle generating source started with 220, and Los Angeles County fips code are 
#given (06037).

#Use filter to form dataframe we need.

df_plot <- NEI %>%
    filter((fips == "24510"|fips == "06037")&(grepl("^220",SCC))) %>%
    group_by(fips,year) %>%
    summarise(PM2.5Emission = sum(Emissions)) %>%
    mutate(fips = recode_factor(fips,"06037" = "Los Angeles", "24510" = "Baltimore City"))

percent_change <- c()
for(i in 1:8){
    if(i %in% c(1,5)){
        percent_change[i] <- 1
    }
    else if (i %in% c(2,3,4)){
        percent_change[i] <- df_plot$PM2.5Emission[i]/df_plot$PM2.5Emission[1]
    }
    else{
        percent_change[i] <- df_plot$PM2.5Emission[i]/df_plot$PM2.5Emission[5]
    }
}
percent_change <- scales::percent(percent_change,prefix = "(",suffix = "%)")

png(filename = "plot6.png",width = 1440,height = 1080)

ggplot(data = df_plot, aes(x = as.factor(year), y = PM2.5Emission))+
    geom_bar(stat = "identity",aes(fill = fips))+
    geom_text(aes(label = paste(format(round(PM2.5Emission,2),big.mark = ","),percent_change)), color = "black", vjust = -0.5,size = 7)+
    facet_wrap(.~fips, ncol = 2)+
    labs(fill = "City",
         title = "Comparisosn of PM2.5 from motor vehicle source in 2 City",
         caption = "% of emission of 1999")+
    xlab("Year")+
    ylab("PM2.5 Emission (μg/m3)")+
    theme(plot.title = element_text(size = 30),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 30),
          plot.caption = element_text(size = 20))s
dev.off()
    
 
