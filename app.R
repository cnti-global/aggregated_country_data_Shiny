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

library(DT)
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
    title = "Aggregated Data"
  ),
  
  
  # Create Tabs on Left Side of Page
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        tabName = "data",
        text = "Data",
        icon = icon("table")
      ),
      menuItem(
        tabName = "map",
        text = "Map",
        icon = icon("globe")
      ),
      menuItem(
        tabName = "about",
        text = "About",
        icon = icon("circle-info")
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
      tabItem(
        tabName = "data",
        fluidPage(
          fluidRow(
            box(width = 4, style = "height:175px;", solidHeader = TRUE, title = "Country Filter",
                uiOutput("country_filter"),
                uiOutput("region_filter")),
            box(width = 3, style = "height:175px;", solidHeader = TRUE, title = "Overview",
                tags$style(".small-box.bg-blu { background-color: #D9DFDB; color: #000000; }"),
                uiOutput("vb_table_count")),
            box(width = 5, style = "height:175px;", solidHeader = TRUE, title = "Created by CNTI",
                tags$figure(
                  style="text-align: center;",
                  tags$img(src = "CNTI_logo.png",
                           width = 175)))),
          
          fluidRow(
            box(width = 12, solidHeader = TRUE, title = "Table",
                DTOutput("aggregate_table")) 
      )
    )
  ),
  
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
  
    # About tab
    tabItem(
      tabName = "about",
      fluidPage(
        fluidRow(
          box(width = 12, solidHeader = TRUE, title = "About the Data",
          mainPanel(
            p("The data on this Shiny app are acquired from several sources. 
              More information about the sources may be found on CNTI's website",
              a(href = "https://innovating.news/article/aggregated-country-data/", 
                "here.", target = "_blank"),
              "Our aim in producing this tool is to allow users to interact with the data."),
            p("The first 'Data' tab allows users to select specific countries or world regions to view in the table.
              The value box updates to show how many countries are shown in the table."),
            p("The 'Map' tab presents an interactive world map where users may select from a variety of variables.
              The map updates and shades countries depending on the variable/column selected in the input window.
              Users may scroll over countries to view specific values."),
            p("The source code for creating this",
              a(href = "https://shiny.posit.co", "Shiny app", target = "_blank"),
              "may be found in the 'Code' section on the left-hand menu.
            We welcome any questions, and we thank you for visiting.")))
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
  
  
  # Remove cases I had to recode in Excel for interactive mapping 
  country_dat2 <- country_dat
  country_dat2 <- subset(country_dat, Country_Map != "Puerto Rico")
  country_dat2 <- subset(country_dat2, Country_Map != "Western Sahara") 
  

  # Data tab ----
  output$country_filter <- renderUI({
    country_choices <- c("All", sort(unique(country_dat2$Country))) # Add condition to view all
    pickerInput("country_filter", "Select Country",
                choices = country_choices)
  })
  
  
  # Reactive data ----
  reactive_country_data <- reactive({
    req(input$country_filter)
    filtered <- (input$country_filter == "All" | country_dat2$Country == input$country_filter) &
      (input$region_filter == "All" | country_dat2$Region == input$region_filter)
    country_dat2[filtered, , drop = FALSE]
  })
  
  
  # Country table ----
  output$aggregate_table <- renderDT({
    data <- reactive_country_data() %>%
      select(
        Country,
        Region,
        Population,
        "World Bank income group" = WorldBank_income_group,
        "V-Dem regime type" = `V.Dem_regime_type`,
        "WJP Rule of Law Index ranking (1-140)" = WJP_Rule_Law_Index_ranking,
        "Internet Penetration" = Internet_penetration,
        "Freedom House internet freedom status" = FreedomHouse_internet_freedom_status,
        "V-Dem government attempts at internet censorship (0-4 scale)" = V.Dem_gov_attempts_internet_censorship,
        "Tortoise Global AI Index ranking (1-62)" = Tortoise_Global_AI_Index_ranking,
        "V-Dem Freedom of Expression and Alt. Sources of Information Index (0-1 scale)" = V.Dem_Freedom_Expression_Alt_Sources_Info_Index,
        "RSF Global Press Freedom Index ranking (1-180)" = RSF_Global_Press_Freedom_Index_ranking,
        "RISJ overall trust in the news percentage" = RISJ_overall_trust_news_percentage,
        "V-Dem degree of journalist harassment (0-4 scale)" = V.Dem_degree_journalist_harassment,
        "CPJ count of journalists imprisoned or killed (yearly)" = CPJ_count_journalists_imprisoned_killed
      )
    
    # Modify column names with hyperlinks
    col_names <- colnames(data)
    col_names[which(col_names == "Region")] <- '<a href="https://unstats.un.org/unsd/methodology/m49/" target="_blank">Region</a>'
    col_names[which(col_names == "Population")] <- '<a href="https://data.worldbank.org/indicator/SP.POP.TOTL" target="_blank">Population</a>'
    col_names[which(col_names == "World Bank income group")] <- '<a href="https://datatopics.worldbank.org/world-development-indicators/the-world-by-income-and-region.html" target="_blank">World Bank income group¹</a>'
    col_names[which(col_names == "V-Dem regime type")] <- '<a href="https://v-dem.net/documents/29/V-dem_democracyreport2023_lowres.pdf" target="_blank">V-Dem regime type²</a>'
    col_names[which(col_names == "WJP Rule of Law Index ranking (1-140)")] <- '<a href="https://worldjusticeproject.org/rule-of-law-index/global" target="_blank">WJP Rule of Law Index ranking (1-140)³</a>'
    col_names[which(col_names == "Internet Penetration")] <- '<a href="https://www.internetworldstats.com/stats1.htm" target="_blank">Internet Penetration</a>'
    col_names[which(col_names == "Freedom House internet freedom status")] <- '<a href="https://freedomhouse.org/reports/freedom-world/freedom-world-research-methodology" target="_blank">Freedom House internet freedom status⁴<a/>'
    col_names[which(col_names == "V-Dem government attempts at internet censorship (0-4 scale)")] <- '<a href="https://v-dem.net/data/the-v-dem-dataset/" target="_blank">V-Dem government attempts at internet censorship (0-4 scale)⁵<a/>'
    col_names[which(col_names == "Tortoise Global AI Index ranking (1-62)")] <- '<a href="https://www.tortoisemedia.com/intelligence/global-ai/" target="_blank">Tortoise Global AI Index ranking (1-62)⁶</a>'
    col_names[which(col_names == "V-Dem Freedom of Expression and Alt. Sources of Information Index (0-1 scale)")] <- '<a href="https://v-dem.net/data/the-v-dem-dataset/" target="_blank">V-Dem Freedom of Expression and Alt. Sources of Information Index (0-1 scale)⁷<a/>'
    col_names[which(col_names == "RSF Global Press Freedom Index ranking (1-180)")] <- '<a href="https://rsf.org/en/methodology-used-compiling-world-press-freedom-index-2023?year=2023&data_type=general" target="_blank">RSF Global Press Freedom Index ranking (1-180)⁸<a/>'
    col_names[which(col_names == "RISJ overall trust in the news percentage")] <- '<a href="https://reutersinstitute.politics.ox.ac.uk/digital-news-report/2023" target="_blank">RISJ overall trust in the news percentatge⁹<a/>'
    col_names[which(col_names == "V-Dem degree of journalist harassment (0-4 scale)")] <- '<a href="https://v-dem.net/data/the-v-dem-dataset/" target="_blank">V-Dem degree of journalist harassment (0-4 scale)¹⁰<a/>'
    col_names[which(col_names == "CPJ count of journalists imprisoned or killed (yearly)")] <- '<a href="https://cpj.org/2022/12/attacks-on-the-press-in-2022/#interact" target="_blank">CPJ count of journalists imprisoned or killed (yearly)¹¹<a/>'
    
    datatable(
      data,
      rownames = FALSE,
      escape = FALSE, # Allow HTML
      options = list(paging = TRUE,
                     scrollY = "30vh",
                     autoWidth = FALSE,
                     scrollX = TRUE,
                     pageLength = 10),
      colnames = col_names
    )
  })
  

  
  # Region filter
  output$region_filter <- renderUI({
    region_choices <- c("All", sort(unique(country_dat2$Region)))
    pickerInput("region_filter", "Select Region",
                choices = region_choices)
  })
  
  
  # Value box
  output$vb_table_count <- renderValueBox({
    country_count <- reactive_country_data() %>%
      nrow()
    
    valueBox(value = format(country_count, big.mark = ","), subtitle = "Number of Countries Selected", color = "black") #icon = icon("table")
  })
  
  
  # Map tab ----
  output$map_filter <- renderUI({
    map_choices <- c("Internet penetration (%)", "V-Dem gov attempts internet censorship (0-4 scale)", 
                     "V-Dem degree journalist harassment (0-4 scale)", "V-Dem Freedom of Expression Index (0-1 scale)")
    
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
           "V-Dem gov attempts internet censorship (0-4 scale)" = map$V.Dem_gov_attempts_internet_censorship,
           "V-Dem degree journalist harassment (0-4 scale)" = map$V.Dem_degree_journalist_harassment,
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
