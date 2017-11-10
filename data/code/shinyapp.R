library(shiny)
library(leaflet)
library(RColorBrewer)

mapdata <- readOGR("data/Voting_Precincts") # this is a shapefile

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "75%"),
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "candidate_name", label = "Candidate",
                            choices = results_mayor2 %>% filter(metric == "Total Votes") %>% 
                              group_by(candidate) %>% summarise(votes = sum(value)) %>% 
                              arrange(desc(votes)) %>% .$candidate
                )
  )
)

server <- function(input, output, session){
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
      merge(mapdata, 
            results_mayor2 %>% 
              filter(metric == "Total Votes" & candidate == input$candidate_name) %>% 
              select(VoterDist = County, candidate, votes = value) %>%
              mutate(pct.of.candidate = votes / sum(votes)),
            by = "VoterDist")
    })
    pal <- colorBin("RdYlGn", bins = 9, domain = c(0, 1200))
    output$map <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(mapdata) %>% addTiles() %>%
        fitBounds(-84.5518, 33.6480, -84.2897, 33.8872)
    })
    
    observe({
        leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>%
          addPolygons(weight = 1, fillOpacity = .7, fillColor = ~pal(votes),
                      stroke = FALSE, smoothFactor = .3,
                      label = ~paste(paste0(VoterDist, ": ", votes, " Votes"),
                                     paste0(" (", scales::percent(pct.of.candidate), " of Candidate's Total", ")")))
        })
    observe({
      proxy <- leafletProxy("map", data = mapdata)

      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~mapdata$votes)
      })
}

shinyApp(ui, server)