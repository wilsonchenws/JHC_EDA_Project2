#Loading necessary library for data processing and plotting
library(dplyr)
library(ggplot2)
library(grDevices)
library(RColorBrewer)
library(tigris)
library(usmap)
library(maps)

#loading data
SCC <- readRDS("Source_Classification_Code.rds")
NEI <- readRDS("summarySCC_PM25.rds")
row.names(NEI) <- seq(1:dim(NEI)[1])
data("fips_codes")

#check missing value in NEI file:
NEI <- NEI %>%
    mutate(fips = gsub(" ","",fips))

NEI[NEI == "NA"] <- NA

# Calculate how many missing values
sum(is.na(NEI))

#(473 observation missed fips code. Roughly 0.00007%)


#check if any SCC code used in NEI is not included in SCC code table.
unique_SCC <- unique(SCC[,1])
Missing_SCC_Num <- dim(NEI)[1] - sum( NEI[,2] %in% unique_SCC)

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

df_int <- NEI %>%
    filter(SCC %in% SCC_Code_list[,1]) %>%
    mutate(fips_2 = (substr(fips,1,2)))


df_state <- data.frame(unique(fips_codes[c("state_code","state_name")]))
names(df_state) <- c("fips","state_name")


# Merge 2 dataset to transfer fips code into real states name 
df_int <- merge(df_int,df_state, by.x = "fips_2",by.y = "fips",all.x = T)


df_grouped <- group_by(df_int,state_name,year)
df_plot <- data.frame(summarize(df_grouped,sum(Emissions)))
df_plot[,1] <- tolower(df_plot[,1])


# Start Plotting the Beautiful States!

MainStates <- map_data("state")
#plotting 1999 first

#Preparation for multi-facet plot
Merged.states <- left_join(MainStates,df_plot[df_plot$year==1999,],by = c("region" = "state_name"))
for (years in c(2002,2005,2008)){
    temp_states <- left_join(MainStates,df_plot[df_plot$year == years,],by = c("region" = "state_name"))
    Merged.states <- rbind(Merged.states,temp_states)
}
#Eliminate NA value(Roughly XXX % of total data number)
Merged.states <- filter(Merged.states,!is.na(year))

state.lab <- MainStates %>%
            group_by(region) %>%
            summarise(long = mean(long),lat = mean(lat))


png(filename = "plot4.png", width = 1444, height = 1080)

ggplot()+
    geom_polygon(data = Merged.states ,aes(x=long,y = lat, group = group,fill = sum.Emissions.),
                 color="white", size = 0.2) +
    scale_fill_viridis_c("Total PM2.5 Emission",option ="A")+
    labs(title = "PM2.5 Emissions by State of different years")+
    facet_wrap(.~year)+
    geom_text(aes(label = region, x = long, y = lat), data = state.lab, size = 2, hjust = 0.5,color = "white")

dev.off()
