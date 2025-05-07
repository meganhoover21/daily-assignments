
library(readr)
library(tidyverse)
library(sf) #needed for spatial objects
library(AOI)
library(ggrepel)#labels maps with overlapping points


#read in cities csv.
cities <- read_csv("data/uscities.csv")

glimpse(cities)
#values ini decimal degrees and have lat/lon-->wgs84 or 4326

# Convert to an sf object using longitude (lng) and latitude (lat)
# Convert to sf and set original CRS to 4326, then reproject to 5070
cities_sf <- st_as_sf(cities, coords = c("lng", "lat"), crs = 4326)
cities_sf <- st_transform(cities_sf, 5070)# EPSG:5070 = NAD83 / Conus Albers

# Check the result
plot(st_geometry(cities_sf))

# Get Larimer County boundary
larimer <- aoi_get(state = "CO", county = "Larimer")
# Reproject to EPSG:5070
larimer <- st_transform(larimer, 5070)

#filter cities within Larimer county
cities_larimer <- st_filter(cities_sf, larimer)

#find top 3 cities in larimer using slice. Make ggplot with it
top3 <- cities_larimer %>%
arrange(desc(population)) %>%
  slice_head(n = 3)


ggplot() +
  geom_sf(data = larimer, fill = NA, color = "black", size = 1) +
  geom_sf(data = cities_larimer, color = "blue", size = 1) +
  geom_sf(data = top3, color = "red", size = 3) +
  ggrepel::geom_label_repel(
    data = top3,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3) +
  coord_sf(datum = NA) +
  ggtitle("Top 3 cities in Larimer County")
  theme_minimal()


#geom_label_repel() is similar to geom_label
#hint: specify the label argument to the name of the city &the geometry argument to the geometry column of you sf object.
#hint:set the stat argument to sf_coordinates so the function knows to use the coordinates of the geometry column for the label positions.

# Add labels to the cities
#ggrepel::geom_label_repel(
 # data = top3,
  #aes(label = city, geometry = geometry),
  #stat = "sf_coordinates",
  #size = 3) +
  #coord_sf(datum = NA) +
 # ggtitle("Cities in Larimer County with Top 3 Labeled")


#this shows the labels along witht he other points, the other graph cut off the labels
final_plot <- ggplot() +
  geom_sf(data = larimer, fill = NA, color = "black") +
  geom_sf(data = cities_larimer, color = "blue", size = 1) +   # all cities
  geom_sf(data = top3, color = "red", size = 3) +              # highlight top 3
  ggrepel::geom_label_repel(
    data = top3,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 2,
  ) +
  coord_sf(datum = NA) +  # Keeps full extent without clipping labels
  ggtitle("Top 3 populated cities in Larimer County") +
  theme_minimal()

print(final_plot)


#Save the plot as a PNG
ggsave("imgs/larimer_county_map.png", plot = final_plot, width = 8, height = 6, dpi = 300)
