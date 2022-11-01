# Does the data on owls in BOLD indicate that owls follow latitudinal gradients in diversity?
# https://pubmed.ncbi.nlm.nih.gov/14970922/

# First, we need to install and open the libraries required for this project
install.packages("tidyverse")
install.packages("vegan")
install.packages("dplyr")
install.packages("plyr")
install.packages("leaflet")
library(tidyverse)
library(vegan)
library(dplyr)
library(plyr)
library(leaflet)

# We first grab data on the order Strigiformes, which is the taxonomic group that contains owls
strigiformes <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Strigiformes&format=tsv")

# We can save this data to our hard drive if we want (but first we need to make sure we are in the desired directory)
# getwd()
# setwd()
# write_tsv(strigiformes, "Strigiformes_BOLD_data.tsv")

# Next, we can drop the columns in the dataframe that won't be of any real use to us...
# First, determine which columns are relevant to our research question:
names(strigiformes)
# Next, we can create a new data frame by taking a subset of the original dataset:
strigiformes_2 <- strigiformes[, c(1, 6, 8, 10, 12, 14, 16, 20, 22, 27, 47, 48)]

# We need to remove records that lack latitude/longitude data to properly investigate our question...
strigiformes_2 <- subset(strigiformes_2, !is.na(lat))
strigiformes_2 <- subset(strigiformes_2, !is.na(lon))
# And we need to remove records that don't have a species assigned to them...
strigiformes_2 <- subset(strigiformes_2, !is.na(species_name))

#We can start by taking a look at the the locations of the remaining records on a map...
plot_coordinates <- leaflet() # get our base map
plot_coordinates <- addTiles(plot_coordinates)
# We are adding markers of each record using the longitude/latitude provided
plot_coordinates <- addMarkers(plot_coordinates, lng = strigiformes_2$lon, lat = strigiformes_2$lat)
# Print the result
plot_coordinates

# From this preliminary mapping, it seems that most of the records are from north of the equator,
# but we haven't filtered for unique species yet, so we should see how many species exist at various latitudes...

# Find the min and max latitude values so that we can create bins that span the entire dataset's latitude values
max(strigiformes_2$lat)
min(strigiformes_2$lat)
# We will create bins of latitude at these values
breaks <- c(-55, -45, -35, -25, -15, -5, 5, 15, 25, 35, 45, 55, 65, 75)
strigiformes_binned <- mutate(strigiformes_2, lat_bins = cut(lat, breaks))

# Now we will create the x and y vectors for our eventual bar plot where the lat bins are the x and the 
# species counts are y...
lat_bins <- c(unique(strigiformes_binned$lat_bins))
# We want to make sure that the latitude bin and species count correspond to each other, so we order the bins
lat_bins <- sort(lat_bins, decreasing = FALSE)
# This vector will contain the number of unique species in each bin
species_counts <- vector(mode="numeric", length(lat_bins))

# Next we will determine the number of unique species within each latitude bin and add them to the vector we made
i = 1
# Loop through each bin
while(i <= length(lat_bins)){
  # We will extract every record that falls within the current latitude bin
  temp = strigiformes_binned[strigiformes_binned$lat_bins == lat_bins[i],]
  # Then count the number of distinct species in the resulting data frame, and add it to our vector
  species_counts[i] <- n_distinct(temp$species_name)
  i = i + 1
}

# Now we can plot our findings to see how many unique species are in each latitude range
barplot(species_counts ~ lat_bins,
        main = "Number of Unique Species within Latitudinal Ranges",
        xlab = "Latitude (in degrees)",
        ylab = "Number of Species",
        ylim = c(0,30),
        col = "lightpink",
        las=2)

# We quickly find that the number of species is not greatest at the equator(-5, 5], and actually 
# seems to be greatest in the northern hemisphere. Let's look at which species the hemisphere's contain/share:

# Get only records from the southern hemisphere
southern_hem = strigiformes_binned[strigiformes_binned$lat < 0,]

# Get only records from the nothern hemisphere
northern_hem = strigiformes_binned[strigiformes_binned$lat > 0,]

# Find total list of complete species and make it into a dataframe
unique_species = unique(strigiformes_binned$species_name)
df1 <- data.frame(unique_species)

# Get a list of the unique species present in each hemisphere
unique_southern <- unique(southern_hem$species_name)
unique_northern <- unique(northern_hem$species_name)

# Iterate through each record of the unique_species column in the data frame and check to see if that record
# exists in the respective unique_northern or unique_southern lists. If it does, then set the respective column
# at that index to 1:
df1$N <- 0
df1$N[df1$unique_species %in% unique_northern] <- 1
df1$S <- 0
df1$S[df1$unique_species %in% unique_southern] <- 1
# Now let's take a look at the table:
df1

# Now lets perform a chi-square test to see if the difference in species between the two is statistically
# significant:

# Count the total number of unique species in each hemisphere
num_unique_southern <- length(unique_southern)
num_unique_northern <- length(unique_northern)

# Perform statistical analysis
df2 <- data.frame(N = num_unique_northern, S = num_unique_southern, row.names = "Species")
chisq <- chisq.test(df2)
chisq

# We can conclude that the difference in species number between the hemispheres is statistically significant.



