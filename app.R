library(shiny)
library(shinyvalidate)
library(tidyverse)
library(shinydisconnect)
library(dplyr)
library(fontawesome)
library(DT)

id <- "1aS6l4KAXX7iuvSSfA6OlkYIlC_eF3POd"
dataset <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
dataset <- dataset[-1]
dataset$maxcrew <- round(625 - dataset$weight, digits = 2)

ui <- fluidPage(
  disconnectMessage(
    text = "App Disconnected - Please refresh the page and try again. If the error persists, contact Brad.",
    refresh = "Refresh",
    background = "#FFFFFF",
    colour = "#444444",
    refreshColour = "#337AB7",
    overlayColour = "#000000",
    overlayOpacity = 0.6,
    width = "full",
    top = "center",
    size = 22,
    css = ""
  ),
  
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      style = "text-align: center;",
      uiOutput("images"),
      br(),
      selectInput("aircraft", "Aircraft", sort(unique(dataset$aircraft))),
      numericInput("commander", HTML(
        paste0(
          "Aircraft Commander ",
          "<span style=\"text-decoration:underline\">(WITH parachute)</span>"
        )
      ), 0, 0, 120, 1),
      numericInput("passenger", HTML(
        paste0(
          "Passenger ",
          "<span style=\"text-decoration:underline\">(WITH parachute)</span>"
        )
      ), 0, 0, 120, 1),
      selectInput("ballast", "Ballast Weights", c(
        "None" = 0,
        "One" = 7,
        "Two" = 15
      )),
      br(),
      img(
        src = "632logo.png",
        height = 60,
        width = 120
      ),
      br(),
      p(
        "Created by ",
        a("Brad Kennedy", href = "mailto:bradley.kennedy100@rafac.mod.gov.uk", target =
            "_blank"),
        "for ",
        a("632 VGS", href = "https://632vgs.co.uk", target = "_blank"),
        br(),
        br(),
        a("Source Code", href = "https://github.com/b-kennedy0/632vgs/blob/master/app.R/", target =
            "_blank"),
        "|" ,
        a("Add new aircraft", href = "https://docs.google.com/forms/d/e/1FAIpQLSdZUL2xQoC6--gYshqy-mRN6uogpsxnZMvVtqh0qOgCmbNavg/viewform?usp=sf_link", target = "_blank")
      )
    ),
    
    mainPanel(
      tags$style(".fa-check-circle {color:#008000}"),
      tags$style(".fa-times-circle {color:#FF0000}"),
      tags$style(".fa-exclamation-circle {color:#FFA500}"),
      tags$style(
        "@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"
      ),
      tags$h3(HTML(as.character(icon(
        "lightbulb"
      )), " Assumptions")),
      HTML(
        "<ul><li>Aircraft Commander in the rear seat</li><li>Passenger in the front seat</li><li>Nil carry-on items</li></ul>"
      ),
      HTML("<hr>"),
      column(
        5,
        tags$h3(HTML(as.character(icon(
          "calculator"
        )), " Calculations")),
        htmlOutput("commander"),
        htmlOutput("passenger"),
        textOutput("ballast_weight"),
        "------",
        textOutput("combined_crew"),
        "------",
        textOutput("air_weight"),
        textOutput("totalaum"),
      ),
      column(
        5,
        tags$h3(HTML(as.character(
          icon("clipboard-check")
        ), " Output")),
        htmlOutput("aumlimit"),
        htmlOutput("frontseat"),
        htmlOutput("ballast"),
        br(),
        htmlOutput("approachspeed")
      ),
      column(10, HTML("<hr>"), tags$h3(
        HTML(as.character(icon("plane")), " Alternative aircraft")
      ), DTOutput("alt_aircraft"))
    )
  )
)

server <- function(input, output) {
  iv <- InputValidator$new()
  iv$add_rule("commander", sv_between(0, 120))
  iv$add_rule("passenger", sv_between(0, 120))
  iv$enable()
  
  output$images <- renderUI({
    tags$div(
      img(
        src = "632crest.png",
        width = 80,
        height = 100,
        style = "float:left;"
      ),
      br(),
      h4("Ballast Weight App"),
      helpText(
        "For information only.",
        br(),
        "Responsibility remains with the Aircraft Commander"
      )
    )
  })
  
  output$commander <- renderText({
    commander <- input$commander
    
    if (commander > 110) {
      return(
        paste(
          "<span style=\"color:red\">AIRCRAFT COMMANDER OVERWEIGHT = ",
          commander,
          "kg ",
          "</span>",
          tags$p(fa("times-circle", fill = "#FF0000"))
        )
      )
    }  else{
      print(paste0("Commander (with para) = ", commander, "kg"))
    }
  })
  
  output$passenger <- renderText({
    passenger <- input$passenger
    
    if (passenger > 110) {
      return(
        paste(
          "<span style=\"color:red\">PASSENGER OVERWEIGHT = ",
          passenger,
          "kg ",
          "</span>",
          tags$p(fa("times-circle", fill = "#FF0000"))
        )
      )
    } else{
      print(paste0("Passenger (with para) = ", passenger, "kg"))
    }
  })
  
  output$combined_crew <- renderText({
    commander <- input$commander
    passenger <- input$passenger
    ballast <- as.numeric(input$ballast)
    combined <- commander + passenger + ballast
    print(paste0("Total Payload = ", combined, "kg"))
  })
  
  output$ballast_weight <- renderText({
    ballast <- input$ballast
    print(paste0("Ballast weight = ", ballast, "kg"))
  })
  
  output$air_weight <- renderText({
    aircraft <- dataset$weight[dataset$aircraft == input$aircraft]
    print(paste0("Aircraft weight = ", round(aircraft, digits = 2), "kg"))
  })
  
  output$totalaum <- renderText({
    commander <- input$commander
    passenger <- input$passenger
    aircraft <- dataset$weight[dataset$aircraft == input$aircraft]
    ballast <- as.numeric(input$ballast)
    
    AUM <- commander + passenger + aircraft + ballast
    
    print(paste0("Aircraft All-Up-Mass = ", round(AUM, digits = 2), "kg"))
  })
  
  output$aumlimit <- renderText({
    commander <- input$commander
    passenger <- input$passenger
    aircraft <- dataset$weight[dataset$aircraft == input$aircraft]
    ballast <- as.numeric(input$ballast)
    
    AUM <- commander + passenger + aircraft + ballast
    
    aumlimit <- if (AUM < 625) {
      HTML(paste0(
        fa("check-circle", fill = "#008000"),
        "Aircraft All-Up-Mass Limits OK"
      ))
    }  else {
      return(
        paste0((fa("times-circle", fill = "#FF0000")),
               "<span style=\"color:red\">Aircraft All-Up-Mass Limits EXCEEDED </span>"
        )
      )
    }
  })
  
  output$frontseat <- renderText({
    passenger <- input$passenger
    
    min_front <- if (passenger < 55) {
      return(paste0((fa("times-circle", fill = "#FF0000")),
                    "<span style=\"color:red\">Front seat minimum weight NOT met </span>"
      ))
    } else {
      HTML(paste0((
        fa("check-circle", fill = "#008000")
      ), "Front seat minimum weight OK"))
    }
  })
  
  output$ballast <- renderText({
    passenger <- input$passenger
    
    ballast_weights <- if (passenger < 42) {
      return(paste0((fa("times-circle", fill = "#FF0000")),
                    "<span style=\"color:red\">PASSENGER TOO LIGHT TO FLY </span>"
      ))
    } else if (passenger < 55) {
      return(paste0((fa(
        "exclamation-circle", fill = "#FFA500"
      )),
      "<span style=\"color:orange\">PASSENGER IN REAR SEAT ONLY </span>"
      ))
    } else if (passenger < 64) {
      return(
        paste0((fa(
          "exclamation-circle", fill = "#FFA500"
        )),
        "<span style=\"color:orange\">TWO Ballast weights MUST be fitted </span>"
        )
      )
    } else if (passenger < 70) {
      return(
        paste0((fa(
          "exclamation-circle", fill = "#FFA500"
        )),
        "<span style=\"color:orange\">at least ONE Ballast weight MUST be fitted </span>"
        )
      )
    } else if (passenger < 96) {
      print(paste0((
        fa("check-circle", fill = "#008000")
      ), "Ballast not required, but 1 or 2 may be fitted"))
    } else if (passenger < 104) {
      print(paste0((
        fa("check-circle", fill = "#008000")
      ), "Ballast not required, but 1 may be fitted"))
    } else if (passenger < 111) {
      print(
        paste0((fa("check-circle", fill = "#FF0000")),
               "<span style=\"color:red\">Ballast weights are NOT PERMITTED</span>"
        )
      )
    } else
      return(paste0((fa("times-circle", fill = "#FF0000")),
                    "<span style=\"color:red\">PASSENGER TOO HEAVY TO FLY</span>"
      ))
  })
  
  output$approachspeed <- renderText({
    commander <- input$commander
    passenger <- input$passenger
    aircraft <- dataset$weight[dataset$aircraft == input$aircraft]
    ballast <- as.numeric(input$ballast)
    
    AUM <- commander + passenger + aircraft + ballast
    
    if (AUM < 580) {
      HTML(fa("info-circle"), "Approach speed 55kts")
    } else {
      HTML(fa("info-circle"), "Approach speed 60kts")
    }
  })
  
  output$alt_aircraft <- renderDT({
    commander <- input$commander
    passenger <- input$passenger
    ballast <- as.numeric(input$ballast)
    
    combined <- commander + passenger + ballast
    dataset <- mutate(dataset, overweight = if_else(maxcrew < combined, "TRUE", "FALSE"))
    colnames(dataset) = c("Tail No", "A/C Weight", "Max Payload", "Overweight?")
    datatable(dataset[order(dataset$"Tail No"), ]) %>% 
      formatStyle(
        'Overweight?', target = 'cell', 
        backgroundColor = styleEqual(c("TRUE", "FALSE"), c('lightcoral', 'lightgreen'))
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
