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
SCC <- readRDS("Source_Classification_Code.rds")
NEI <- readRDS("summarySCC_PM25.rds")
row.names(NEI) <- seq(1:dim(NEI)[1])
data("state.fips")

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
    mutate(fips_2 = as.numeric(substr(fips,1,2)))


sum(is.na(df_int$fips_2))
# 15 data missing fips code, roughly 0.03%


temp_state <- c()
counter <- 1
for (i in 1:nrow(state.fips)){
    ind_target <- unlist(regexpr(":",state.fips$polyname[i]))
    if(ind_target != -1){
        temp_state[counter] <- substr(state.fips$polyname[i],1,ind_target[1]-1)
        
    }
    else{
        temp_state[counter] <- state.fips$polyname[i]
    }
    counter <- counter + 1
}
df_state <- unique(data.frame(state.fips$fips,temp_state))
names(df_state) <- c("fips","state_name")



df_int <- merge(df_int,df_state, by.x = "fips_2",by.y = "fips")

df_grouped <- group_by(df_int,fips)
df_plot <- summarize(df_grouped,sum(Emissions))


# Start Plotting the Beautiful States!

MainStates <- map_data("state")

ggplot()+
    geom_polygon(data = MainStates,aes(x=long,y = lat,group = group),color="black", fill = "lightblue")
