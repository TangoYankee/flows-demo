library("ggmap")

register_google(Sys.getenv("GOOGLE_MAPS_API_KEY"), "standard")
k <- Sys.getenv("GOOGLE_MAPS_API_KEY")
SanFran <- get_map(location = c(-122.405, 37.79), zoom = 14, color = 'bw')
ggmap(SanFran, darken=0.75)
