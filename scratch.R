library("ggmap")

register_google(Sys.getenv("GOOGLE_MAPS_API_KEY"), "standard")

stations <- read.csv("./data/201608_station_data.csv")
trips <- read.csv("./data/201608_trip_data.csv")

head(stations)
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
