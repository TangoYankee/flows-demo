library("ggmap")
library("googleway")

maps_key = Sys.getenv("GOOGLE_MAPS_API_KEY")
register_google(maps_key, "standard")

stations <- read.csv("./data/201608_station_data.csv")
trips <- read.csv("./data/201608_trip_data.csv")

stations <- stations[stations$landmark == "San Francisco",]

SanFran <- get_map(location = c(-122.405,37.79), zoom = 14,color = 'bw')

ggmap(SanFran) + 
  geom_point(
    data = stations,
    aes(x = long, y = lat, color="red")
  ) +
  theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position="none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

s_SF <- unique(stations$station_id)
trips_SF <- trips[(trips$Start.Terminal %in% s_SF) & (trips$End.Terminal %in% s_SF),]
OD_trips_SF <- table(trips$Start.Terminal, trips$End.Terminal)
OD_trips_SF_Narrow <- data.frame(OD_trips_SF)
colnames(OD_trips_SF_Narrow) <- c("Origin", "Destination", "Trips")

OD_trips_SF_Narrow <- OD_trips_SF_Narrow[order(OD_trips_SF_Narrow$Trips, decreasing = TRUE),]
top10 <- OD_trips_SF_Narrow[1:10,]

top10 <- merge(top10, stations, by.x="Origin",by.y="station_id", all.x = TRUE)
top10 <- subset(top10, select=c("Origin", "Destination", "Trips", "lat", "long"))
colnames(top10) <- c("Origin", "Destination", "Trips", "O_lat", "O_long")

top10 <- merge(top10, stations, by.x="Destination", by.y="station_id", all.x = TRUE)
top10 <- subset(top10, select=c("Origin", "Destination", "Trips", "O_lat", "O_long", "lat", "long"))
colnames(top10) <- c("Origin", "Destination", "Trips", "O_lat", "O_long", "D_lat", "D_long")

tmp <- data.frame(lat = numeric(0), lon = numeric(0), ID = numeric(0), Trips = numeric(0))

for (x in 1:nrow(top10)) {
  
  # Get origins and destinations
  origin <- c(top10[x,"O_lat"],top10[x,"O_long"])
  destination <- c(top10[x,"D_lat"],top10[x,"D_long"])
  
  # get the directions from Google Maps API
  res <- google_directions(origin = origin,destination = destination,key = maps_key, mode= "bicycling")
  
  # Convert the results to co-ordinates
  df_polyline <- decode_pl(res$routes$overview_polyline$points)
  
  # Add a route ID and Trips to the data frame
  df_polyline$ID <- x
  df_polyline$Trips <- top10[x,"Trips"]
  
  # Append the results to the tmp object
  tmp <- rbind(tmp,df_polyline)
  
}

ggmap(SanFran) +
  geom_path(aes(x = lon, y = lat, color = as.factor(ID)), size = 0.5, data = tmp, lineend = "round")

load("./data/All_Flows.Rdata")
ggmap(SanFran,darken = 0.8) +
  geom_path(aes(x = lon, y = lat, group = ID,colour = All_Flows$Trips), data = All_Flows, size = All_Flows$Trips/1000) +
  scale_colour_gradient(low="#900C3F", high="#FFC300",name="Trips") +
  geom_point(data=stations, aes(long, lat),colour="red") +
  geom_text(data = stations,aes(x = long, y = lat, label = station_id), check_overlap = TRUE, colour="#FFFFFF",hjust=-0.6) +
  theme (
    axis.text = element_blank (), 
    axis.title = element_blank (),
    axis.ticks = element_blank ()
  )
