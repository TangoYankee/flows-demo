library(ggmap)
library(rgdal)

maps_key = Sys.getenv("GOOGLE_MAPS_API_KEY")
register_google(maps_key, "standard")

load("./data/tx_od_main_JT00_2014.Rdata")
XWalk <- read.csv("./data/tx_xwalk.csv")
options(scipen = 999)

# Merge onto the home block code
OD <- merge(OD,XWalk, by.x = "h_geocode", by.y = "tabblk2010", all.x= TRUE)
# Change column names
colnames(OD) <- c("h_geocode","w_geocode","S000","h_geocode_trct")

# Merge onto the work block code
OD <- merge(OD,XWalk, by.x = "w_geocode", by.y = "tabblk2010", all.x= TRUE)
# Change column names
colnames(OD) <- c("h_geocode","w_geocode","S000","h_geocode_trct","w_geocode_trct")

# Aggregate flows into Tracts
OD_Tract <- aggregate(data=OD, S000 ~ h_geocode_trct + w_geocode_trct, sum)

# Import spatial data
TX_SP <- readOGR('./data/Texas_Tract.geojson', 'Texas_Tract')

# Convert to WGS84
TX_SP <- spTransform(TX_SP, CRS("+init=epsg:4326"))

# Create lookup table
TX_tract_centroids <- data.frame(TX_SP@data$GEOID,coordinates(TX_SP))
# Change column names
colnames(TX_tract_centroids) <- c("Tract","lon","lat")

# Add home lat lon
OD_Tract <- merge(OD_Tract,TX_tract_centroids,by.x="h_geocode_trct", by.y="Tract",all.x=TRUE)
# Fix column names
colnames(OD_Tract) <- c("h_geocode_trct","w_geocode_trct","S000","h_lon","h_lat")

# Add work lat lon
OD_Tract <- merge(OD_Tract,TX_tract_centroids,by.x="w_geocode_trct", by.y="Tract",all.x=TRUE)
# Fix column names
colnames(OD_Tract) <- c("w_geocode_trct","h_geocode_trct","S000","h_lon","h_lat","w_lon","w_lat")

# Get base map
texas <- get_map(location = "Texas", zoom = 6,color = 'bw')
  
# Create plot
ggmap(texas,darken = 0.8) + 
  geom_segment(data=OD_Tract[OD_Tract$S000 > 70,],aes(y = h_lat, x = h_lon, yend = w_lat, xend = w_lon),colour= "white", alpha= 0.1, size=0.2)

# Get base map
Houston <- get_map(location = "Houston, TX", zoom = 10,color = 'bw')
ggmap(Houston,darken = 0.8) + 
  geom_segment(data=OD_Tract[OD_Tract$S000 > 5,],aes(y = h_lat, x = h_lon, yend = w_lat, xend = w_lon, alpha= S000), size=0.3, colour = "white") +
  scale_alpha_continuous(range = c(0.004, 0.3))  +
  theme ( legend.position="none",
          axis.text = element_blank (), 
          axis.title = element_blank (),
          axis.ticks = element_blank ()
  )
