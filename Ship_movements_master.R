library(tidyverse)
library(leaflet)
library(shiny)
library(shiny.semantic)
library(lubridate)
library(geosphere)

# Reading the CSV file
Ships_raw <- read.csv("ships.csv")

# Creating a ship type list for the first drop-down menu
Ship_type_list <- Ships_raw %>%
  select(ship_type) %>%
  unique() %>%
  arrange_all()

# Converting DATETIME into POSIXct/POSIXt format
Ships_raw$DATETIME <- ymd_hms(Ships_raw$DATETIME)

#### Defining the app visual layout ####
# Creating the main grid
Big_grid <- grid_template(
  default = list(
    areas = rbind(
      c("Header", "Header"),
      c("Selector", "Map"),
      c("Info", "Map"),
      c("Info", "Map"),
      c("Info", "Map"),
      c("Info", "Map"),
      c("Footer", "Footer")
    ),
    rows_height = c("7%",
                    "30%",
                    "15%",
                    "15%",
                    "15%",
                    "15%",
                    "3%"),
    cols_width = c("20%", "80%"),
    container_style = "background: dimgray"
  ))

# Creating the subgrid for the info section
Small_grid <- grid_template(default = list(
  areas = rbind(
    c("Info_1"), 
    c("Info_2"), 
    c("Info_3"), 
    c("Info_4")
  ),
  rows_height = c("25%", "25%", "25%", "25%"),
  cols_width = "100%",
  container_style = "background: dimgray;"
))

# Creating the grid styles
Grid_styles <- list(
  Header = "background: dimgray;
            color: white;
            margin-top: 10px;
            text-align: center;",
  Selector = "background: dimgray;
              font-size: 2vh;
              font-family: Lato,'Helvetica Neue', Arial, Helvetica, sans-serif;
              text-transform: uppercase;
              font-weight: 700;
              color: white; 
              text-align: center;
              padding: 15px;",
  Info = "background: dimgray;",
  Map = "background: dimgray;",
  Footer = "background: dimgray;
            color: white;"
)

Small_grid_styles = list(
  Info_1 = "background: gray; font-size: 1.5vh; margin: 15px;",
  Info_2 = "background: gray; font-size: 1.5vh; margin: 15px;",
  Info_3 = "background: gray; font-size: 1.5vh; margin: 15px;",
  Info_4 = "background: gray; font-size: 1.5vh; margin: 15px;"
)

# Creating the header
Header_body <- div("SHIP MOVEMENTS APP",
                   style = "font-size: 3vh;
                   margin-top: 2vh;"
)

# Creating a module UI for the drop-down menus
DD_ModuleUI <- function(id) {
  ns <- NS(id)
  div(
    class = "content",
    div(class = "selector", "Vessel type"),
    div(
      class = "sel_1",
      dropdown_input("Sel_type",
                     default_text = "Select vessel type",
                     choices = Ship_type_list)
    ),
    div(class = "selector",
        style = "margin-top: 4vh;",
        "Vessel name"),
    div(
      class = "sel_2",
      dropdown_input(
        "Sel_vessel",
        default_text = "Select vessel name",
        choices = NULL,
        value = NULL
      )
    )
  )
}

# Creating the info blocks
Info_body <- grid(
  Small_grid,
  area_styles = Small_grid_styles,
  Info_1 = div(class = "ui one statistics",
               div(
                 class = "ui inverted statistic",
                 div(
                   class = "value",
                   style = "margin-top:1vh;",
                   textOutput("Max_dist_sailed")
                 ),
                 div(class = "label",
                     "Longest distance sailed, m")
               )),
  Info_2 = div(class = "ui one statistics",
               div(
                 class = "ui inverted teal statistic",
                 div(class = "value",
                     style = "margin-top:1vh;",
                     textOutput("Speed")),
                 div(class = "label", "Speed, kt")
               ),),
  Info_3 = div(class = "ui one statistics",
               div(
                 class = "ui inverted yellow statistic",
                 div(class = "value",
                     style = "margin-top:1vh;",
                     textOutput("Course")),
                 div(class = "label", "Course, deg")
               ),),
  Info_4 = div(
    class = "ui one tiny statistics",
    div(
      class = "ui inverted olive statistic",
      div(class = "label",
          style = "color: darkgreen;
                margin-top:0.7vh;",
          "Start time:"),
      div(class = "value",
          textOutput("Date_start")),
      div(
        class = "ui inverted olive statistic",
        div(class = "label",
            style = "color: darkred;
                  margin-top:0.7vh;",
            "End time:"),
        div(class = "value",
            textOutput("Date_end"))
      )
    )
  )
)

# Creating the footer
Footer_body <- div(shiny::tags$h5("Coding and design (c) ",
  a("Maxim Kobzev", href = "https://www.linkedin.com/in/maxim-kobzev/"),
  style = "font-family: Segoe Script !important;",
                                  align = "center")
)

# Creating the base leaflet map
Map_body <- leafletOutput(
  "Ship_map",
  width = "100%",
  height = "90vh")


#### Creating the UI ####
ui <- shinyUI(
  semanticPage(
    tags$head(includeCSS("styles.css")),
    grid(
      Big_grid,
      area_styles = Grid_styles,
      Header = Header_body,
      Selector = DD_ModuleUI("Dropdown_mod"), # Using the module to run the drop-down functionality
      Info = Info_body,
      Map = Map_body,
      Footer = Footer_body
    )
))


#### Creating the server part ####
##################################

server <- function (input, output, session) {
  
  # Selecting a vessel type
  S_type <- reactive({
    req(Ship_type_list)
    filter(Ships_raw,
           ship_type == input$Sel_type) %>%
      arrange(SHIPNAME)
  })

  # Selecting a vessel name
  Vessel <- reactive({
    req(input$Sel_vessel)
    filter(S_type(),
           SHIPNAME == input$Sel_vessel) %>%
      arrange(DATETIME)
  })
  
  
  # Creating the LON/LAT & DATETIME pairs for consecutive observations
  Vessel_legs <- reactive({
    Vessel() %>%
      mutate(
        LAT_prev = lag(LAT),
        LON_prev = lag(LON),
        DT_prev = lag(DATETIME)
      ) %>% 
      filter(LAT_prev != "NA")
  })
  
  # Defining the coordinates for the distances' lat/long pairs
  Coord_start <- reactive({
    Vessel_legs()[, c("LON_prev", "LAT_prev")]
  })
  Coord_end <- reactive({
    Vessel_legs()[, c("LON", "LAT")]
  })
  
  # Calculating the distances between the observations (in meters)
  Vessel_dist <- reactive({
    Vessel_legs() %>% 
      mutate(
        distance = distHaversine(Coord_start(),
                                 Coord_end())
      )
  })
  
  # Selecting the max distance observation for plotting the map & showing the info
  # Also filtering out the most recent observation in case of the several equal distances
  Max_dist_info <- reactive({
    Vessel_dist() %>%
      filter(distance == max(distance)) %>% 
      filter(DATETIME == max(DATETIME))
  })
  
  # Defining the maximum distance
  Max_distance <- reactive({
    Max_dist_info()$distance
  })
  
  # Defining the zero distance
  Zero_dist <- reactive({
    Max_distance() == 0
  })
  
  # Creating the base leaflet map for a moving vessel
  basemap_moving <- reactive({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      fitBounds(
        Max_dist_info()$LON_prev,
        Max_dist_info()$LAT_prev,
        Max_dist_info()$LON,
        Max_dist_info()$LAT
      ) %>%
      addMiniMap() %>% 
      addScaleBar("bottomleft")
  })
  
  # Creating the base leaflet map for a parked vessel
  basemap_parked <- reactive({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(Max_dist_info()$LON,
              Max_dist_info()$LAT,
              11) %>%
      addMiniMap() %>% 
      addScaleBar("bottomleft")
  })
  
  # Initializing the base map
  output$Ship_map <- renderLeaflet({
    if(Zero_dist()) {
      basemap_parked()
    } else {
      basemap_moving()
    }
  })
  
  # Creating a "Start"/"No movement observed" marker
  Moved_label <- reactive({
    if (Zero_dist()) {
      "NO MOVEMENT OBSERVED"
    } else {
      "START"
    }
  })
  
  # Defining the marker styles
  Marker_style <- reactive({
    if (Zero_dist()) {
      list(
        "color" = "white",
        "font-style" = "bold",
        "font-family" = "Lato,'Helvetica Neue', Arial, Helvetica, sans-serif",
        "background-color" = "orange",
        "border-color" = "orange"
      )
    } else {
      list(
        "color" = "white",
        "font-style" = "bold",
        "font-family" = "Lato,'Helvetica Neue', Arial, Helvetica, sans-serif",
        "background-color" = "darkgreen",
        "border-color" = "darkgreen"
      )
    }
  })
  
  # Creating a start marker for the maximum distance
  Start_marker <- function(map) {
    map %>%
      addLabelOnlyMarkers(
        label = Moved_label(),
        labelOptions = labelOptions(
          noHide = T,
          textsize = "32px",
          style = Marker_style(),
          opacity = 1
        ),
        lng = Max_dist_info()$LON_prev,
        lat = Max_dist_info()$LAT_prev,
        group = "markers"
      ) %>% 
      addCircles(lng = Max_dist_info()$LON_prev,
                 lat = Max_dist_info()$LAT_prev,
                 color = "darkgreen",
                 fillColor = "darkgreen",
                 radius = Max_distance()/100,
                 fillOpacity = 1
                 )
  }
  
  # Hiding an end marker if a vessel has not moved
  Hide_selector <- reactive({
    if (Zero_dist()) 
    {F} else {T}
  })
  
  
  End_radius <- reactive({
    if (Zero_dist()) {
      100
    } else {
      Max_distance() / 100
    }
  })
  
  End_color <- reactive({
    if (Zero_dist()) {
      "orange"
    } else {
      "firebrick"
    }
  })
  
  End_fill_color <- reactive({
    if (Zero_dist()) {
      "orange"
    } else {
      "firebrick"
    }
  })
  
  # Creating an end marker for the maximum distance
  End_marker <- function(map) {
    map %>% 
      addLabelOnlyMarkers(
        label = "END",
        labelOptions = labelOptions(
          noHide = Hide_selector(),
          textsize = "32px",
          style = list("color" = "white",
                       "font-style" = "bold",
                       "background-color" = "firebrick",
                       "border-color" = "firebrick"),
          opacity = 1,
        ),
        lng = Max_dist_info()$LON,
        lat = Max_dist_info()$LAT,
        group = "markers"
      ) %>% 
      addCircles(lng = Max_dist_info()$LON,
                 lat = Max_dist_info()$LAT,
                 radius = End_radius(),
                 color = End_color(),
                 fillColor = End_fill_color(),
                 fillOpacity = 1
                 )
  }
  
  # Displaying the distance data
  output$Max_dist_sailed <- renderText(paste(
    prettyNum(round(Max_distance(), 0), big.mark = ",")))
  
  # Displaying the speed data
  output$Speed <- renderText(paste(
    prettyNum(round(Max_dist_info()$SPEED, 1), big.mark = ",")))
  
  # Displaying the course data
  output$Course <- renderText(paste(
    prettyNum(round(Max_dist_info()$COURSE, 0), big.mark = ",")))
  
  # Displaying the date/time
  output$Date_start <- renderText(paste(Max_dist_info()$DT_prev))
  output$Date_end <- renderText(paste(Max_dist_info()$DATETIME))
  
  # DD_ModuleServer("Dropdown_mod")
  
  # Observing the changes of a vessel type and clearing the map
  observeEvent(input$Sel_type, {
    leafletProxy("Ship_map", session) %>%
      clearGroup(group = "markers") %>%
      clearShapes()
  })
  
  # Observing the changes of a vessel name and re-rendering the maps/markers
  observeEvent(input$Sel_vessel, {
    leafletProxy("Ship_map", session) %>%
      clearGroup(group = "markers") %>%
      clearShapes() %>%
      Start_marker %>%
      End_marker
  })
  
  
  # Observing the vessel name selection
  observeEvent(S_type(), {
    update_dropdown_input(session, "Sel_vessel",
                          choices = unique(S_type()$SHIPNAME)
    )
  })
  
}

shinyApp(ui, server)