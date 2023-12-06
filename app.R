#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Application developed by Samuel Jens, CNTI

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)
library(fresh)

library(dplyr)
library(tidyr)
library(leaflet)
library(sp)
library(rworldmap)
library(RColorBrewer)
library(readr)
library(ggplot2)


# Helpful resource: https://stackoverflow.com/questions/56319618/implement-select-all-option-in-reactive-shiny
# Helpful resource: https://rstudio.github.io/shinydashboard/appearance.html
# Helpful resource: https://educationshinyappteam.github.io/Style_Guide/staticImages.html


# UI ----
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = ""
                    ),
                    
                    # Create Tabs on Left Side of Page
                    dashboardSidebar(collapsed = TRUE,
                      sidebarMenu(
                        menuItem(
                          tabName = "map",
                          text = "Map",
                          icon = icon("globe")
                        ),
                        menuItem(
                          tabName = "source_code",
                          text = "Code",
                          icon = icon("code")
                        )
                      )
                    ),
                    
                    
                    dashboardBody(tags$head(tags$meta(name = "viewport", content = "width=1600")),
                                  tags$style(HTML('
      .main-header .logo {
        font-family: "Montserrat", sans-serif;
        font-weight: 500;
        font-size: 20px;
      }
       body {
    font-family: "Montserrat", sans-serif;
    font-weight: 400;
    overflow-x: hidden;
    overflow-y: auto;
       }
       .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {
    font-family: "Montserrat", sans-serif;
    font-weight: 400;
      }
    ')),
                                  tags$style(HTML('
        .skin-black .main-header .navbar {
    background-color: #A6A6A6;
        }
        .skin-black .main-header .logo {
    background-color: #A6A6A6;
    color: #333;
    border-bottom: 0 solid transparent;
    border-right: 1px solid #eee;
        }
.box-header.with-border {
    border-bottom: 1px solid #53A5DC;
}
      .col-sm-8 {
    width: 80%;
}
  ')),
                                  tabItems(
                                    # Map tab
                                    tabItem("map",
                                            fillPage(
                                              fluidRow(
                                                box(width = 12, solidHeader = TRUE, title = "User Input",
                                                    uiOutput("map_filter")
                                                )
                                              ),
                                              fluidRow(box(width = 12, title = "Interactive Map",
                                                           leafletOutput("worldmap", height = "65vh"))
                                              )
                                            )
                                    ),
                                    
                                    # Source tab
                                    tabItem(
                                      tabName = "source_code",
                                      fluidPage(
                                        fluidRow(
                                          box(width = 12, solidHeader = TRUE, title = "Source Code",
                                              mainPanel(
                                                p("The source code for this Shiny app may be found",
                                                  a(href = "https://github.com/cnti-global/aggregated_country_data_Shiny/blob/main/app.R", 
                                                    "here.", target = "_blank")))
                                          )
                                        )
                                      )
                                    )
                                  )
                    ),
                    title = "Aggregated Data by CNTI", 
)


# Server -----
# Define server logic required to draw a histogram
server <- function(input, output) {
  country_dat <-  read.csv("CNTI_app_data.csv")
  
  
  # Map tab ----
  output$map_filter <- renderUI({
    map_choices <- c("Internet penetration (%)", "V-Dem government attempts at internet censorship (0-4 scale)", 
                     "V-Dem degree of journalist harassment (0-4 scale)", "V-Dem Freedom of Expression Index (0-1 scale)")
    
    pickerInput("map_filter", "Select Data Column to Update Shading in Map",
                choices = map_choices)
  })
  
  
  # Create map object
  map <- joinCountryData2Map(country_dat, joinCode = "NAME", nameJoinColumn = "Country_Map", nameCountryColumn = "Country_Map", verbose = F)
  
  # Add in Palestine & Gaza information manually -- some reason does not merge correctly
  map@data$Country[172] <- "Palestine - West Bank"
  map@data$Country[196] <- "Palestine - Gaza"
  map@data$Internet_penetration2[196] <- 75
  map@data$V.Dem_gov_attempts_internet_censorship[196] <- 2.03
  map@data$V.Dem_degree_journalist_harassment[196] <- 0.86
  map@data$V.Dem_Freedom_Expression_Alt_Sources_Info_Index[196] <- 0.29
  
  # Alter name of Morocco / Western Sahara to Morocco (keeps original combined label for W. Sahara)
  map@data$Country[100] <- "Morocco"
  
  
  
  # Render reactive filter for map; selected by user
  user_decision <- reactive({
    switch(input$map_filter,
           "Internet penetration (%)" = map$Internet_penetration2,
           "V-Dem government attempts at internet censorship (0-4 scale)" = map$V.Dem_gov_attempts_internet_censorship,
           "V-Dem degree of journalist harassment (0-4 scale)" = map$V.Dem_degree_journalist_harassment,
           "V-Dem Freedom of Expression Index (0-1 scale)" = map$V.Dem_Freedom_Expression_Alt_Sources_Info_Index)
  })
  
  
  # Render leaflet map
  output$worldmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2.5)) #%>%
    #addMiniMap()
  })
  
  
  # Define color palette (https://rstudio.github.io/leaflet/colors.html)
  pal <- colorBin("Greens", domain = NULL, bins = 9) 
  
  
  # Generate map; color fill by user input
  observe({
    if(!is.null(input$map_filter)){
      leafletProxy("worldmap", data = map) %>%
        addTiles() %>% 
        clearShapes() %>%
        setView(lng = 10.00, lat = 22.00, zoom = 2) %>%
        addPolygons(fill = TRUE,
                    fillColor = ~pal(user_decision()), # Key
                    weight = 1,
                    opacity = .5,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = .8,
                      bringToFront = TRUE),
                    label = ~paste(as.character(map$Country), # Consider altering country label here
                                   " Score: ", as.character(user_decision()))
        )
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

