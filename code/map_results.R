# Read map data
mapdata <- readOGR("data/Voting_Precincts") # this is a shapefile
mapdata2 <- merge(mapdata, results_mayor2 %>% filter(metric == "Total Votes") %>% 
                    select(-metric) %>% spread(candidate, value) %>% 
                    rename(VoterDist = County), by = "VoterDist")

# Color Palette for total votes
pal <- colorBin("viridis", bins = 7, domain = c(0, 1200))
pal <- colorBin("RdYlGn", bins = 9, domain = c(0, 1200))
# Create leaflet map
leaflet(mapdata2) %>% addTiles() %>% 
  addPolygons(weight = 1, fillOpacity = .7, fillColor = ~pal(`PETER AMAN`),
              stroke = FALSE, smoothFactor = .3) %>%
  addLegend(pal = pal, values = mapdata2@data$`PETER AMAN`, opacity = 1.0)