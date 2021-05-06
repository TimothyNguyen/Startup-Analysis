investor <- investor%>%
  ungroup()%>%
  filter(country_code == "USA")%>%
  select(investor_name, state_code, category, funded_Year, raised_amount_usd)

investor$category <- as.factor(investor$category)
investor$investor_name <- as.factor(investor$investor_name)


ui <- fluidPage(
  titlePanel('Highest Funding total per Industry'),
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      sliderInput("year", "Year",
                  min = 2013,
                  max = range(as.numeric(investor$funded_Year))[2],
                  value = 2013,
                  sep = "",
                  step = 1,
                  animate = animationOptions(interval = 1000)),
      pickerInput(
        inputId = "state",
        label = "Select a state",
        choices = unique(investor$state_code),
        options = list(`actions-box` = TRUE),
        selected = "CA",
        multiple = TRUE)
    ),
    
    
    
    # Main panel for displaying outputs
    mainPanel(
      plotlyOutput("bars"),
      highchartOutput("treemap")
    ))
  
  
)


server <- function(input, output, session) {
  
  output$bars <- renderPlotly({
    inv <- investor%>%
      filter(funded_Year %in% input$year)%>%
      filter(state_code %in% input$state)
    inv <- aggregate(as.numeric(inv$raised_amount_usd), by = list(category = inv$category),
                     FUN = sum, na.rm = TRUE)
    names(inv)[2] <- "raised_total"
    
    hi <- inv[order(inv$raised_total, decreasing = TRUE), ]
    
    hi<- hi %>%
      mutate(category = fct_reorder(category, raised_total))
    
    hi <- head(hi, 20)
    hi%>%
      plot_ly(source = "bar_plot",
              hoverlabel  = list(bgcolor = "yellow",
                                 font = list(color = "red")),
              type = "bar")%>%
      add_bars(y = ~category, x = ~raised_total, color = ~category, showlegend = FALSE, hoverinfo = "x+y")%>%
      layout(title = list(text = "Top 20 industry with highest Funding total"))
    
  })
  
  output$treemap <- renderHighchart({
    clickbar <- event_data("plotly_click", source = "bar_plot")
    if (is.null(clickbar)) { return(NULL) }
    inv <- investor%>%
      filter(funded_Year %in% input$year)%>%
      filter(state_code %in% input$state)%>%
      filter(category == clickbar$y)
    inv <- aggregate(as.numeric(inv$raised_amount_usd), by = list(investor_name = inv$investor_name),
                     FUN = sum, na.rm = TRUE)
    names(inv)[2] <- "raised_total"
    
    hi <- inv[order(inv$raised_total, decreasing = TRUE), ]
    
    hi<- hi %>%
      mutate(category = fct_reorder(investor_name, raised_total))
    
    hi <- head(hi, 20)
    hi%>%
      hchart(
        "treemap",
        hcaes(x = investor_name, value = raised_total, color = raised_total))%>%
      hc_title(text = paste0("Top 20 investors of ", clickbar$y, " funding (in $)"))%>%
      hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))  
    
  })
  
}

shinyApp(ui, server)

