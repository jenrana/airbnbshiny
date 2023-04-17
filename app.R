library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(leaflet)
library(DT)
library(scales)
library(dplyr)
library(stringr)
library(shinydashboard)
library(flexdashboard)

#import data
airbnb <- read.csv("https://raw.githubusercontent.com/jenrana/airbnbshiny/main/airbnb.csv", stringsAsFactors=FALSE)
airbnb$city <- as.factor(str_to_title(airbnb$city))
airbnb_dt <- airbnb %>%
  select(-X,-room_shared,-room_private,-multi,-biz,-dist,-attr_index,-attr_index_norm,-rest_index,-rest_index_norm,-lat,-lng) %>%
  mutate(realSum=round(realSum),
         metro_dist=round(metro_dist, digits=2),
         ) %>%
         rename("City" = "city",
               "Availability" = "type",
               "Rate" = "realSum",
               "Type" = "room_type",
               "Capacity" = "person_capacity",
               "Superhost" = "host_is_superhost",
               "Clean Rating" = "cleanliness_rating",
               "Guest Rating" = "guest_satisfaction_overall",
               "Bedrooms" = "bedrooms",
               "Distance" = "metro_dist")

# UI section
ui <- fluidPage(
  
  tags$head(
    HTML("<title>Airbnb European Snapshot</title>"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,300;0,400;0,600;1,400&display=swap');
      body{  
        background: #f7f7f7;
      }
      .container-fluid {
        padding-right: 0;
        padding-left: 0;
      }
      .well {
        background-color: #484848;
      }
      .well h4{
        font-weight: 300;
        color: #fff;
      }
      .help-block {
        color: #fff;
      }
      .shiny-input-container {
        color: #FFF;
      }
      h2 {
        font-family: 'Montserrat', sans-serif;
        padding: 30px;
        color: #fff;
        background-color: #484848;
        margin-top: 0;  
        font-size: 40px;
        font-weight: 300;
      }
      h2 img{
        padding-right: 10px;
        margin-top: -5px;
      }
      .row {
        margin-right: 15px;
        margin-left: 15px;
      }
      .tab-content .row {
          margin-right: -15px;
          margin-left: -15px;
      }
      .small-box {
        padding: 10px 20px;
        color: #fff;
        background-color: #FF5A5F;
      }
      .small-box h3 {
          font-size: 16px;  
          margin-top: 10px;
          margin-bottom: 0;
      }
      .small-box p {
        font-size: 38px;
        font-weight: bold;
        margin: 0 0 3px 0;
          line-height: 48px;
      }
      .icon-large{
        text-align: right;
        font-size: 30px;
      }
      span.tabhead {
        border-bottom: 1px solid #eceeef;
        font-size: 16px;
        font-weight: 300;
        padding: 7px 10px 4px;
        background: #fff;
        display: block;
        margin: 10px 0 3px;
      }
      .irs--shiny .irs-bar {
        border-top: 1px solid #5AB2FF;
        border-bottom: 1px solid #5AB2FF;
        background: #5AB2FF;
      }
      .irs--shiny .irs-grid-pol {
        background-color: #fff;
      }
      .irs--shiny .irs-grid-pol.small,
      .irs--shiny .irs-min{
        display: none;
      }
      .shiny-input-container {
        color: #fff;
      }"))
  ),

    # Title
    titlePanel(title=span(img(src="logo.png", height=50),"Airbnb European Snapshot")),

    sidebarLayout(
        sidebarPanel(
          selectInput("city", 
                      h4("Choose your city:"), 
                      choices = levels(airbnb$city), 
                      selected = "NULL", 
                      multiple = FALSE),
          
          sliderInput("rooms", h4("Number of bedrooms:"),
                      min=0, 
                      max=10,
                      value = 10,
                      step = 1),
          
          checkboxGroupInput("listtype", h4("Availability:"),
                        choices = c("weekdays", "weekends"),
                        selected = c("weekdays", "weekends")
                        ),

          width = 2),

        mainPanel(
          tabsetPanel(
            tabPanel("Summary", 
                     fluidRow(
                       valueBoxOutput("numbox", width = 3),
                       valueBoxOutput("pricebox", width = 3),
                       valueBoxOutput("superbox", width = 3),
                       valueBoxOutput("guestbox", width = 3)
                     ) , 
                     fluidRow(
                       column(4, span(class="tabhead", "Number of Bedrooms"), plotOutput("bedplot")
                       ),
                       column(4, span(class="tabhead", "Cleanliness"), plotOutput("cleanplot")
                       ),
                       column(4, span(class="tabhead", "Price & Distance from City Center"), plotOutput("distplot")
                       )
                     ) 
            ), 
            tabPanel("Map View", leafletOutput("citymap")), 
            tabPanel("List View", DT::dataTableOutput(outputId = "table"))
          ),
          width=10)
    )
)

# Server section
server <- function(input, output) {
  cbPalette <- c("#5AB2FF", "#00A699", "#FFE25A", "#845AFF", "#FC642D", "#FF5A5F")

  filtered_data <- reactive({
    subset(airbnb_dt, (City %in% input$city) & (Availability == input$listtype[1] | Availability == input$listtype[2])) %>%
    filter(Bedrooms <= input$rooms)
  })
  
  filtered_plot_data <- reactive({
    subset(airbnb, (city %in% input$city) & (type == input$listtype[1] | type == input$listtype[2]))  %>%
    filter(bedrooms <= input$rooms)
  })   
  
  count_plot_data <- reactive({
    subset(airbnb, (city %in% input$city) & (type == input$listtype[1] | type == input$listtype[2]))
  })
  
  super_plot_data <- reactive({
    subset(airbnb_dt, (City %in% input$city) & (Availability == input$listtype[1] | Availability == input$listtype[2])) %>%
    filter(Superhost == "True")
  })

  output$table <- DT::renderDataTable(
    filtered_data(),
    #filter = 'top',
    rownames = FALSE
  )
  
  output$bedplot <- renderPlot({
    filtered_plot_data() %>%
      ggplot(aes(bedrooms, fill=type)) +
      geom_bar(stat="count",position="dodge")+
      theme_minimal()+
      theme(legend.position="bottom")+ 
      labs(fill="", x = "Number of Bedrooms", y = "")  +
      scale_fill_manual(values=cbPalette)
  })
  output$cleanplot <- renderPlot({
    filtered_plot_data() %>%
      ggplot(aes(cleanliness_rating, fill=room_type)) +
      geom_bar(stat="count",position="dodge")+
      theme_minimal()+
      theme(legend.position="bottom")+ 
      labs(fill="", x = "Cleanliness Rating", y = "")  +
      scale_fill_manual(values=cbPalette)
  })
  output$distplot <- renderPlot({
    filtered_plot_data() %>%
      filter(realSum<4000) %>%
      ggplot(aes(x=metro_dist, y=realSum, color=type)) +
      geom_point(alpha=.5, size=2)+
      theme_minimal()+
      theme(legend.position="bottom")+ 
      labs(color="", x = "Distance from City Center", y = "Total Price") +
      scale_color_manual(values=cbPalette)
  })
  output$numbox <- renderValueBox({
    shinydashboard::valueBox(
      "Number of rentals",
      nrow(count_plot_data() %>%
             filter(bedrooms <= input$rooms)),
      icon = icon("home"),
      width = 3
    )
  })
  output$pricebox <- renderValueBox({
    y <-round(mean(filtered_plot_data()$realSum), digits = 0)
    if(is.nan(y)) y <- 0
    shinydashboard::valueBox(
      "Average price per night",
      y,
      icon = icon("coins"),
      width = 3
    )
  })
  output$superbox <- renderValueBox({
    shinydashboard::valueBox(
      "Number of super hosts",
      nrow(super_plot_data() %>%
             filter(Bedrooms <= input$rooms)),
      icon = icon("fas fa-user"),
      width = 3
    )
  })
  output$guestbox <- renderValueBox({
    x <-round(mean(filtered_plot_data()$guest_satisfaction_overall), digits = 0)
    if(is.nan(x)) x <- 0
    shinydashboard::valueBox(
      "Average guest rating",
      x,
      icon = icon("fas fa-star"),
      width = 3
    )
  })
  
  output$citymap <- renderLeaflet({
    airbnbpopup = paste0( "<strong>Bedrooms:</strong> "
                          , filtered_plot_data()$bedrooms 
                          , "<br>"
                          , "<strong>Capacity:</strong> "
                          , filtered_plot_data()$person_capacity
                          , "<br>"
                          , "<strong>Room Type:</strong> "
                          , filtered_plot_data()$room_type
                          , "<br>"
                          , "<strong>Guest Rating:</strong> "
                          , filtered_plot_data()$guest_satisfaction_overall
                          , "<br>"
                          , "<strong>Clean Rating:</strong> "
                          , filtered_plot_data()$cleanliness_rating
    )
    startlat = mean(filtered_plot_data()$lat)
    startlon = mean(filtered_plot_data()$lng)
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(startlon,startlat, zoom = 12) %>% 
      addCircles(data=filtered_plot_data(),~lng, ~lat, popup=airbnbpopup, weight = 3, radius=100, 
                 color="#FF5A5F", stroke = FALSE, fillOpacity = 0.6)  
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
