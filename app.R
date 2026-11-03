#########################################################
# Shiny app by Celia Kucker
# Nov 2, 2026
#
# Shows vehicle type, weather, and location of fatal accidents in 2023 and 2024 in the US
#
# Deployed at https://oy3dh7-celia-kucker.shinyapps.io/Assignment_10_shiny/
# Source code at GitHub: https://github.com/celiakucker/Shiny-App-Assignment-10
#########################################################

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(arrow)
library(plotly)
library(usmap)

# load data
crash_data <- read_parquet("crashdata.parquet")


crash_data$YEAR <- as.factor(crash_data$YEAR)

# make filter choices
year_choices <- sort(unique(crash_data$YEAR))

state_choices <- sort(unique(
  na.omit(crash_data$STATENAME)
))

weather_choices <- sort(unique(
  na.omit(crash_data$WEATHERNAME)
))


# user interface

ui <- page_sidebar(
  
  title = "Fatal Traffic Crashes: 2023-2024",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#276678"
  ),
  
  # sidebar filters
  sidebar = sidebar(
    
    selectInput(
      inputId = "year",
      label = "Select Year:",
      choices = c("Both Years", as.character(year_choices)),
      selected = "Both Years"
    ),
    
    selectInput(
      inputId = "state",
      label = "Select State:",
      choices = c("All States", state_choices),
      selected = "All States"
    ),
    
    selectInput(
      inputId = "weather",
      label = "Select Weather Condition:",
      choices = c("All Conditions", weather_choices),
      selected = "All Conditions"
    )
  ),
  
  # create tabs
  navset_card_tab(
    
    # vehicle types tab
    nav_panel(
      title = "Vehicle Types",
      
      card(
        card_header("Vehicles Involved in Fatal Crashes by Type"),
        
        plotlyOutput(
          outputId = "vehicle_plot",
          height = "550px"
        )
      )
    ),
    
    # weather and road conditions tab
    nav_panel(
      title = "Weather & Road Conditions",
      
      card(
        card_header("Fatal Crashes by Weather and Road Surface Conditions"),
        
        plotlyOutput(
          outputId = "heatmap",
          height = "500px"
        )
      )
    ),
    
    # state comparison tab
    nav_panel(
      title = "State Comparison",
      
      card(
        card_header("Fatal Crashes by State"),
        
        plotlyOutput(
          outputId = "state_plot",
          height = "650px"
        )
      )
    )
  )
)


# server

server <- function(input, output) {
  
  # filter dataset by sidebar selections
  filtered_data <- reactive({
    
    data <- crash_data
    
    # filter by year
    if (input$year != "Both Years") {
      
      data <- data |> 
        filter(YEAR == input$year)
      
    }
    
    # filter by state
    if (input$state != "All States") {
      
      data <- data |> 
        filter(STATENAME == input$state)
      
    }
    
    # filter by weather
    if (input$weather != "All Conditions") {
      
      data <- data |> 
        filter(WEATHERNAME == input$weather)
      
    }
    
    return(data)
    
  })
  
  
  # tab 1 vehicles
  
  output$vehicle_plot <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available for these filters.")
    )
    
    # vehicles by year and vehicle type
    vehicle_counts <- data |>
      filter(!is.na(VPICBODYCLASSNAME)) |>
      group_by(YEAR, VPICBODYCLASSNAME) |>
      summarise(
        vehicle_count = n(),
        .groups = "drop"
      )
    
    vehicle_counts <- vehicle_counts |>
      group_by(VPICBODYCLASSNAME) |>
      mutate(
        total_count = sum(vehicle_count)
      ) |>
      ungroup()
    
    # top 10 most common vehicle
    top_vehicles <- vehicle_counts |>
      distinct(VPICBODYCLASSNAME, total_count) |>
      slice_max(
        order_by = total_count,
        n = 10,
        with_ties = FALSE
      )
    
    # include only 10 types
    vehicle_counts <- vehicle_counts |>
      filter(VPICBODYCLASSNAME %in% top_vehicles$VPICBODYCLASSNAME)
    
    # order vehicle types
    vehicle_counts <- vehicle_counts |>
      mutate(
        VPICBODYCLASSNAME = reorder(
          VPICBODYCLASSNAME,
          total_count
        )
      )
    
    # make bar chart
    p <- ggplot(
      vehicle_counts,
      aes(
        x = VPICBODYCLASSNAME,
        y = vehicle_count,
        fill = YEAR,
        text = paste(
          "Vehicle Type:", VPICBODYCLASSNAME,
          "<br>Vehicle Count:", vehicle_count,
          "<br>Year:", YEAR
        )
      )
    ) +
      
      geom_col(position = "dodge") +
      
      coord_flip() +
      
      labs(
        x = NULL,
        y = "Number of Vehicles",
        fill = "Year"
      ) +
      
      scale_fill_manual(
        values = c(
          "2023" = "#276678",
          "2024" = "#F4A261"
        )
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 14
        ),
        legend.position = "bottom"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # tab 2 road conditions
  
  output$heatmap <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available for these filters.")
    )
    
    # vehicles for each weather and road condition
    heatmap_data <- data |> 
      filter(
        !is.na(WEATHERNAME),
        !is.na(VSURCONDNAME)
      ) |> 
      group_by(WEATHERNAME, VSURCONDNAME) |> 
      summarise(
        vehicle_count = n(),
        .groups = "drop"
      )
    
    # create heatmap
    p <- ggplot(
      heatmap_data,
      aes(
        x = VSURCONDNAME,
        y = WEATHERNAME,
        fill = vehicle_count,
        text = paste(
          "Weather:", WEATHERNAME,
          "<br>Road Condition:", VSURCONDNAME,
          "<br>Vehicles:", vehicle_count
        )
      )
    ) +
      
      geom_tile(color = "white") +
      
      geom_text(
        aes(label = vehicle_count),
        size = 3
      ) +
      
      scale_fill_gradient(
        low = "#DCEEF2",
        high = "#145374",
        name = "Vehicles"
      ) +
      
      labs(
        x = "Road Surface Condition",
        y = "Weather Condition"
      ) +
      
      theme_classic() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 14
        ),
        axis.text.x = element_text(
          angle = 35,
          hjust = 1
        )
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # tab 3 state comparison
  
  output$state_plot <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available for these filters.")
    )
    
    # count crash only once
    state_counts <- data |>
      group_by(STATENAME) |>
      summarise(
        crash_count = n_distinct(ST_CASE),
        .groups = "drop"
      )
    
    # load state map data
    map_data <- us_map(regions = "states") |>
      distinct(full, abbr) |>
      left_join(
        state_counts,
        by = c("full" = "STATENAME")
      )
    
    # create map
    plot_ly(
      data = map_data,
      type = "choropleth",
      locations = ~abbr,
      locationmode = "USA-states",
      z = ~crash_count,
      text = ~paste(
        "State:", full,
        "<br>Fatal Crashes:",
        ifelse(
          is.na(crash_count),
          "No data",
          format(crash_count, big.mark = ",")
        )
      ),
      hoverinfo = "text",
      colorscale = list(
        list(0, "#DCEEF2"),
        list(1, "#145374")
      ),
      colorbar = list(
        title = "Fatal Crashes"
      ),
      marker = list(
        line = list(
          color = "white",
          width = 1
        )
      )
    ) |>
      layout(
        title = "Fatal Crashes by State",
        geo = list(
          scope = "usa",
          projection = list(
            type = "albers usa"
          ),
          showlakes = TRUE,
          lakecolor = "white"
        )
      )
    
  })
}


shinyApp(
  ui = ui,
  server = server
)
