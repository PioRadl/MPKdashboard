library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(purrr)
library(arrow)
library(sf)
library(concaveman)
library(shinyTime)
library(paletteer)

stops_data <- read.csv("stops.csv")
trip_data <- read_parquet("trips.parquet")

get_departures_df <- function(stop_id) {
  # url <- paste0("http://www.poznan.pl/mim/komunikacja/service.html?stop_id=", stop_id)
  # response <- POST(url)
  # 
  # if (status_code(response) != 200) {
  #   stop("Request failed with status: ", status_code(response))
  # }
  # 
  # result <- content(response, as = "text", encoding = "UTF-8")
  # data <- fromJSON(result, simplifyVector = FALSE)
  # 
  # departures_df <- map_dfr(data$routes, function(route) {
  #   route_name <- route$name
  #   
  #   map_dfr(route$variants, function(variant) {
  #     headsign <- variant$headsign
  #     
  #     map_dfr(variant$services, function(service) {
  #       when <- service$when
  #       
  #       map_dfr(service$departures, function(dep) {
  #         tibble(
  #           route = route_name,
  #           headsign = headsign,
  #           when = when,
  #           hours = dep$hours,
  #           minutes = dep$minutes,
  #           trip_id = dep$trip_id
  #         )
  #       })
  #     })
  #   })
  # })
  departures_df <- trip_data %>% filter(Id == stop_id)
  return(departures_df)
}

get_points_df <- function(trip_id) {
  url <- paste0("https://www.poznan.pl/mim/plan/map_service.html?co=feature&mtype=pub_transport&id=", trip_id)
  response <- POST(url)
  if (status_code(response) != 200) {
    stop("Request failed with status: ", status_code(response))
  }
  
  result <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(result, simplifyVector = FALSE)
  
  points_df <- map_dfr(data$features, function(feature) {
    coords <- feature$geometry$coordinates
    
    map_dfr(seq_along(coords), function(i) {
      tibble(
        Coord1 = coords[[i]][1],
        Coord2 = coords[[i]][2],
        Order = i,
      )
    })
  })
  points_df <- points_df %>% mutate(Coord1 = as.numeric(Coord1), Coord2 = as.numeric(Coord2))
  return(points_df)
}

############
#### UI ####
############

ui <- fluidPage(
    titlePanel(
      tags$div(
        style = "display: flex; align-items: center; gap: 20px;",
        tags$img(src = "mpk.png", height = "80px"),
        tags$h1("Miejskie Przedsiębiorstwo Komunikacyjne Poznań", style = "font-weight: bold; font-size: 36px; margin: 0; color: darkgreen")
      )
    ),
    navbarPage("Navigation",
      tabPanel("Tab1",
        sidebarLayout(
          sidebarPanel(
            tags$div("Departures schedule", style = "font-weight: bold; font-size: 20px;"),
            tags$div("(Click on the departure to show it on the map)", style = "font-size: 16px; margin-bottom: 10px;"),
            selectInput("stop", "Stop name:", 
                        choices = unique(stops_data$Name), selected = "Politechnika"),
            splitLayout(
              selectInput("stop_id", "Stop code:", 
                          choices = stops_data$Id[stops_data$Name == "Politechnika"],
                          selected = stops_data$Id[stops_data$Name == "Politechnika"][1]),
              shinyTime::timeInput("time", "Select time:", value = Sys.time(), seconds = FALSE)
            ),
            DTOutput("table"),
          ),
          mainPanel(
            splitLayout(
              wellPanel(
                style = "height: 700px; overflow-y: auto;",
                div(
                  uiOutput("text"),
                  style = "padding-bottom: 15px;"
                ),
                leafletOutput("map", height = 600),
              ),
              wellPanel(
                tags$div("Reachable stops in a given amount of time", style = "font-weight: bold; font-size: 20px;"),
                style = "height: 700px; overflow-y: auto;",
                splitLayout(
                  sliderInput("exchanges", HTML("Choose max<br> number of exchanges:"), 
                              min = 0, max = 2, value = 0),
                  sliderInput("time_available", HTML("Adjust available time <br>(in minutes):"), 
                              min = 0, max = 60, value = 15)
                ),
                leafletOutput("reachable")
              )
            )
          )
        )
      ),
      tabPanel("Tab2",
        splitLayout(
          wellPanel(
            style = "height: 600px; overflow-y: auto;",
            verticalLayout(
              tags$div("Top stops by number of departures", style = "font-weight: bold; font-size: 20px;"),
              splitLayout(
                verticalLayout(
                  checkboxGroupInput("count_vehicles", "Calculate for vehicle types:",
                                     choices = c("Bus", "Tram"), selected = c("Bus", "Tram"), 
                                     inline = TRUE),
                  checkboxGroupInput("count_zones", "Zones:",
                                     choices = c("A", "B", "C", "D"), selected = c("A", "B", "C", "D"), 
                                     inline = TRUE)
                ),
                sliderInput("rank_range", label = "Show places:", min = 1, max = 100, value = c(1, 10))
              )
            ),
            plotlyOutput("biggest_stops")
          ),
          wellPanel(
            style = "height: 600px; overflow-y: auto;",
            verticalLayout(
              tags$div("Total number of departures based on the time", style = "font-weight: bold; font-size: 20px;"),
              splitLayout(
                sliderInput("bins", "Choose the number of bins: ", min = 6, max = 96, value = 24),
                radioButtons("group_by", "Group by:", 
                             choices = c("LineType", "Zone"), selected = "LineType")
              ),
              plotlyOutput("plot")
            )
          ),
          wellPanel(
            style = "height: 600px; overflow-y: auto;",
            verticalLayout(
              tags$div("Ticket zones on the map", style = "font-weight: bold; font-size: 20px;"),
              splitLayout(
                sliderInput("concavity", "Adjust concavity:", min = 0, max = 5, value = 2, step = 0.1),
                checkboxGroupInput("enabled_zones", "Show zones:", 
                                   c("A", "B", "C", "D"), selected = c("A", "B", "C", "D"),
                                   inline = TRUE)
              ),
              leafletOutput("zones")
            )
          )
        )
      )
    )
  )
    

################
#### Server ####
################


server <- function(input, output, session) {
  
  departures_data <- reactive({
    req(input$stop_id)
    df <- get_departures_df(input$stop_id)
    
    df <- df %>%
      mutate(DepartureTime = paste0(Hour, ":", Minute)) %>%
      filter(as.POSIXct(DepartureTime, format = "%H:%M", tz = Sys.timezone()) > input$time) %>%
      arrange(as.POSIXct(DepartureTime, format = "%H:%M", tz = Sys.timezone()))
    
    return(df)
  })
  
  selected_trip_id <- reactive({
    req(input$table_rows_selected)
    row_index <- input$table_rows_selected
    departures_data()$TripID[row_index]
  })
  
  
  observeEvent(input$stop, {
    filtered <- stops_data %>%
      filter(Name == input$stop) %>%
      pull(Id)
    
    current_selection <- isolate(input$stop_id)
    new_selection <- if (current_selection %in% filtered) current_selection else filtered[1]
    
    updateSelectInput(session, "stop_id", choices = filtered, selected = new_selection)
  })
  
  observeEvent(input$rank_range, {
    max_range <- 24
    current_range <- input$rank_range
    
    if ((current_range[2] - current_range[1]) > max_range) {
      new_upper <- current_range[1] + max_range
      updateSliderInput(
        session,
        "rank_range",
        value = c(current_range[1], new_upper)
      )
    }
  })
  
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles(options = tileOptions(minZoom = 11))
    
    req(input$stop_id)
    selected <- stops_data %>% filter(Id == input$stop_id)
    
    m <- m %>%
      addMarkers(
        data = selected,
        ~Coord1, ~Coord2,
        icon = makeIcon(
          iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-icon.png",
          iconWidth = 15,  # smaller width
          iconHeight = 25, # smaller height
          iconAnchorX = 7, # adjust anchor to keep position centered
          iconAnchorY = 25
        ),
        label = ~paste0(Name, " (", Id, ")")
      )%>%
      setView(lng = selected$Coord1[1], lat = selected$Coord2[1], zoom = 14)
    
    trip_id_safe <- tryCatch(selected_trip_id(), error = function(e) NULL)
    
    if (!is.null(trip_id_safe)) {
      points_df <- tryCatch(get_points_df(trip_id_safe), error = function(e) NULL)
      stops_on_route <- trip_data %>%
        filter(TripID == trip_id_safe)
      m <- m %>%
        addPolylines(
          data = points_df,
          lng = ~Coord1,
          lat = ~Coord2,
          color = "blue",
          weight = 4,
          opacity = 0.8
        ) %>%
        setView(lng = selected$Coord1[1], lat = selected$Coord2[1], zoom = 12) %>%
        addCircleMarkers(
          data = stops_on_route,
          ~Coord1, ~Coord2,
          radius = 6,
          color = "blue",
          fillColor = "white",
          fillOpacity = 1,
          stroke = TRUE,
          weight = 2,
          label = ~lapply(paste0(Name, " (", Id, ")<br>Time of arrival: ", Hour, ":", Minute), htmltools::HTML)
        )
    }
    
    return(m)
  })
  
  output$table <- renderDT({
    req(input$stop_id)
    departures_to_show <- departures_data() %>% select(Line, Direction, DepartureTime) %>%
      distinct()
    
    datatable(departures_to_show, rownames = FALSE, selection = "single")
  })
  
  output$text <- renderUI({
    if (length(input$table_rows_selected) == 0)
      line_text = "None"
    else
      line_text <- departures_data()$Line[input$table_rows_selected]
    HTML(paste0("<b style='font-size: 20px; padding-bottom: 50px;'>Showing Line: ", line_text, "</b>"))
  })
  
  output$zones <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 9))
    if (length(input$enabled_zones) > 0) {
      zones_data <- stops_data %>%
        select(Name, Id, Coord1, Coord2, Zone) %>%
        distinct()
      
      zones_sf <- st_as_sf(zones_data, coords = c("Coord1", "Coord2"), crs = 4326)
      
      zone_levels <- c("A", "B", "C", "D")
      zone_colors <- paletteer_d("lisa::RoyLichtenstein")[c(1, 2, 4, 5)]
      names(zone_colors) <- zone_levels
      zones_sf$fillColor <- zone_colors[zones_sf$Zone]
      
      zone_polygons_list <- lapply(rev(input$enabled_zones), function(z) {
        zone_points <- zones_sf %>% filter(Zone == z)
        
        # concaveman needs a POINT sf object
        poly <- concaveman(zone_points, concavity = input$concavity)
        poly$Zone <- z
        poly$fillColor <- zone_colors[z]
        return(poly)
      })
      
      zone_polygons <- do.call(rbind, zone_polygons_list)
      
      # Create leaflet map
      m <- m %>%
        addPolygons(data = zone_polygons,
                    fillColor = ~fillColor,
                    fillOpacity = 0.5,
                    color = "black",
                    weight = 1,
                    label = ~paste("Zone:", Zone)) %>%
        addCircleMarkers(
          data = zones_sf %>% filter(Zone %in% input$enabled_zones),
          radius = 4,
          fillColor = ~fillColor,
          fillOpacity = 0.2,
          stroke = FALSE,
          fill = TRUE,
          label = ~paste0(Name, " (Zone: ", Zone, ")")
        )
    }
    else {
      m <- m %>% setView(lng = 16.9252, lat = 52.4064, zoom = 10)
    }
    return(m)
  })
  
  output$plot <- renderPlotly({
    histogram_data <- trip_data %>%
      mutate(Time = as.numeric(Hour) + as.numeric(Minute) / 60)
  
    
    p <- ggplot(histogram_data, aes(x = Time, fill = .data[[input$group_by]])) +
      geom_histogram(bins = input$bins, position = "stack") +
      scale_x_continuous(
        breaks = seq(0, 24, by = 4),
        labels = function(x) sprintf("%02d:00", x)
      ) +
      xlab("Time of Day (hours)") +
      ylab("Count") +
      scale_fill_manual(values = paletteer_d("lisa::RoyLichtenstein")[c(1, 2, 4, 5)])
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$reachable <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 9))
    
    chosen_time <- as.POSIXlt(input$time)$hour * 60 + as.POSIXlt(input$time)$min
    stop <- input$stop
    
    df <- data.frame(Name = stop, Time = chosen_time, Path = stop)
    
    find_reachable_stops <- function(Name, Time, Path) {
      stop <- Name
      time <- Time
      path <- Path
      possible_trips <- trip_data %>%
        mutate(Time = 60*as.numeric(Hour) + as.numeric(Minute)) %>%
        select(Name, Id, TripID, Time, Line) %>%
        filter(Name == stop, Time - chosen_time < 60, Time > time) %>%
        mutate(Path = paste(path, "->", Line))
      
      possible_trip_IDs <- possible_trips$TripID
      
      dep_times <- possible_trips %>%
        select(TripID, Time, Path) %>%
        rename (DepTime = Time)
      
      reachable_stops <- trip_data %>%
        mutate(Time = 60*as.numeric(Hour) + as.numeric(Minute)) %>%
        select(Name, Id, TripID, Time) %>%
        filter(TripID %in% possible_trip_IDs, Time - chosen_time < 60, Time > time)
      
      resulting_stops <- reachable_stops %>%
        left_join(dep_times, by = "TripID", relationship = "many-to-many") %>%
        filter(Time >= DepTime) %>%
        filter(Time - chosen_time <= input$time_available)
      
      return(resulting_stops)
    }
    
    for (i in 0:input$exchanges) {
      new_df <- pmap_dfr(df, find_reachable_stops) %>%
        select(Name, Time, Path) %>%
        mutate(Path = paste(Path, "->", Name)) %>%
        distinct()
      df <- df %>% rbind(new_df) %>%
        group_by(Name) %>%
        slice_min(order_by = Time)
    }
    
    
      stops_to_draw <- df %>%
        left_join(stops_data, by = "Name") %>% 
        select(-Line) %>%
        distinct() %>%
        mutate(TimeToReach = Time - chosen_time) %>%
        filter(TimeToReach <= input$time_available)
    
      if (nrow(stops_to_draw) == 0) {
        m <- m %>% setView(lng = 16.9252, lat = 52.4064, zoom = 10)
        return(m)
      }

      stops_sf <- st_as_sf(stops_to_draw, coords = c("Coord1", "Coord2"), crs = 4326)
      
      # poly <- concaveman(stops_sf, concavity = input$concavity2)
      
      pal <- colorNumeric(
        palette = viridisLite::viridis(256, option = "C"),
        domain = stops_sf$TimeToReach
      )
      
      m <- m %>%
        # addPolygons(data = poly,
        #             # fillColor = ~fillColor,
        #             fillOpacity = 0.5,
        #             color = "lightgray",
        #             weight = 1 )%>%
        #             # label = ~paste("Zone:", Zone)) %>%
        addCircleMarkers(
          data = stops_sf,
          # radius = ~(input$time_available - TimeToReach),
          radius = 6,
          fillColor = ~pal(TimeToReach),
          fillOpacity = 0.2,
          stroke = FALSE,
          fill = TRUE,
          label = ~lapply(paste0(Name, " (reachable in ", TimeToReach, " min)<br>",Path), htmltools::HTML)
        )
    # }
    # else {
    #   m <- m %>% setView(lng = 16.9252, lat = 52.4064, zoom = 10)
    # }
    return(m)
  })
  
  output$biggest_stops <- renderPlotly({
    top_stops <- trip_data %>%
      filter(Zone %in% input$count_zones, LineType %in% input$count_vehicles) %>%
      distinct(Name, TripID) %>%
      count(Name, name = "DeparturesCount") %>%
      arrange(desc(DeparturesCount)) %>%
      mutate(Rank = row_number()) %>%
      slice_head(n = input$rank_range[2]) %>%
      slice_tail(n = input$rank_range[2] - input$rank_range[1] + 1)
    
    p <- ggplot(top_stops, aes(x = reorder(Name, -Rank),
                               y = DeparturesCount,
                               fill = Rank,
                                text = paste(Name, 
                                             "<br>Departures:", DeparturesCount, 
                                             "<br>Rank:", Rank))) +
      geom_col() +
      scale_fill_viridis_c(option = "C", limits = c(1, 100)) + 
      coord_flip() +
      geom_text(aes(label = paste0("#", Rank), y = DeparturesCount + max(DeparturesCount)/20), size = 3.5) + 
      labs(
        x = "Stop Name",
        y = "Number of Departures"
      ) +
      theme_minimal() + 
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
