con_status <- factor(c('acquired', 'ipo', 'operating'))
ui <-
  fluidPage(
    # App title
    titlePanel("Companies"),
    
    # sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        radioButtons("status",
                     "Choose status", 
                     choices = con_status),
        sliderInput("round", "Rounds",
                    min = 0,
                    max = range(as.numeric(us_companies$funding_rounds))[2],
                    value = c(3,19),
                    animate = animationOptions(interval = 300))
      ),
      
      
      
      # Main panel for displaying outputs
      mainPanel(
        plotlyOutput(outputId = "companies")
      )
    ))
server <- function(input, output) {
  output$companies <-
    renderPlotly({
      prep <- us_companies %>% 
        filter(status %in% input$status) %>%
        filter(funding_rounds >= input$round[1]) %>%
        filter(funding_rounds <= input$round[2])
      
      prep <- prep %>%
        group_by(location, category, status, lat, long, state_code) %>%
        select(funding_total_usd, funding_rounds) %>% 
        summarise(x = median(as.numeric(funding_total_usd)), 
                  mean_funding_rounds = mean(as.numeric(funding_rounds)),
                  n = n())
      "
      for(i in 1:nrow(prep)) {
        prep[i,'lat'] <- prep[i,'lat'] + runif(1, -0.15, 0.15)
        prep[i,'long'] <- prep[i,'long'] + runif(1, -0.15, 0.15)
      }
      "
      
      
      fig <- prep
      fig %>%
        plot_mapbox() %>%
        add_markers(x = ~long,
                    y = ~lat,
                    color = ~category,
                    size = ~x * n,
                    text = ~paste0(location, "<br>",
                                   "<b>Median salary:</b> $", round(x, 2), "<br>",
                                   "<b>Number of companies:</b> ", round(n, 2), "<br>",
                                   "<b>Category:</b> ", category, "<br>",
                                   "<b>Average Funding Rounds:</b> ", mean_funding_rounds, "<br>")) %>%
        layout(
          legend = list(orientation = "h", x = 0.5),
          mapbox = list(
            style = 'dark',
            center = list(lon = -97, lat = 38),
            zoom = 2.5)) 
    })
}
shinyApp(ui, server)