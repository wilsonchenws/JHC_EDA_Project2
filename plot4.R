# summarize total emission by state
# merging dataset, states, 
# Color by emission



#Loading library for data processing and plotting
library(dplyr)
library(ggplot2)
library(grDevices)
library(RColorBrewer)
library(usmap)
library(maps)

#loading data
SCC <- readRDS("Source_Classification_Code.rds",na.string = "   NA")
NEI <- readRDS("summarySCC_PM25.rds")
row.names(NEI) <- seq(1:dim(NEI)[1])


#check missing value in NEI file:
NEI <- NEI %>%
    mutate(fips = gsub(" ","",fips)) 
NEI[NEI == "NA"] <- NA

#473 observation missed fips code. Roughly 0.00007%)



#check if any SCC code used in NEI is not included in SCC code table.
unique_SCC <- unique(SCC[,2])
Missing_SCC_Num <- sum( NEI[,2] %in% unique_SCC)

#0 SCC code is not in SCC code table.

# Based on the documentation in this PDF file: https://ofmpub.epa.gov/sccwebservices/sccsearch/docs/SCC-IntroToSCCs_2021.pdf
# We know that we can use SCC.level.1 to capture external and internal combustion;
# We can also know that fuel being used in the combustion process is stored in level 3 or 4.



# Due to the complexity involved in the SCC coding convention, I decided to simplify the assignment by
# filtering string with key word "Coal". 

SCC_Code_list <- SCC %>%
    filter(grepl("Combustion",SCC.Level.One)) %>%
    filter(grepl("[Cc]oal",SCC.Level.Three)| grepl("[Cc]oal",SCC.Level.Four)) %>%
    select(SCC)

df_for_plot4 <- NEI %>%
    filter(SCC %in% SCC_Code_list[,1])


# Start Plotting the Beautiful States!

MainStates <- map_data("state")
Merged_df <- merge(merge(MainStates))

ggplot()+
    geom_polygon(data = MainStates,aes(x=long,y = lat,group = group),color="black", fill = "lightblue")
