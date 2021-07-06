library(shiny)
library(shinyvalidate)
library(ggplot2)
library(tidyverse)
library(shinydisconnect)
library(dplyr)

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

    titlePanel(" "),

    sidebarLayout(
        sidebarPanel(
            style="text-align: center;",
            uiOutput("images"),
            br(),
            selectInput("aircraft", "Aircraft", sort(unique(dataset$aircraft))),
            numericInput("commander", HTML(paste0("Aircraft Commander ","<span style=\"text-decoration:underline\">(no parachute)</span>")), 0, 0, 120, 1),
            numericInput("passenger", HTML(paste0("Passenger ","<span style=\"text-decoration:underline\">(no parachute)</span>")), 0, 0, 120, 1),
            br(),
            img(src = "632logo.png", height = 60, width = 250),
            br(),
            p("Created by ",
                a("Brad Kennedy", href = "mailto:bradley.kennedy100@rafac.mod.gov.uk", target="_blank"),
                "for ",
                a("632 VGS", href = "https://632vgs.co.uk", target="_blank"),
              br(),
              br(),
                a("Source Code", href = "https://github.com/b-kennedy0/632vgs/blob/master/app.R/", target="_blank"),
                "|" , 
                a("Add new aircraft", href = "https://docs.google.com/forms/d/e/1FAIpQLSdZUL2xQoC6--gYshqy-mRN6uogpsxnZMvVtqh0qOgCmbNavg/viewform?usp=sf_link", target = "_blank")
            )),

        mainPanel(
            tags$style(".fa-check-circle {color:#008000}"),
            tags$style(".fa-times-circle {color:#FF0000}"),
            tags$style(".fa-exclamation-circle {color:#FFA500}"),
            tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
            tags$h3(HTML(as.character(icon("lightbulb")), " Assumptions")),
            HTML("<ul><li>Aircraft Commander in the rear seat</li><li>Passenger in the front seat</li>
                 <li>Nil ballast weights fitted</li><li>Nil carry-on items</li></ul>"),
            HTML("<hr>"),
            column(4,
            tags$h3(HTML(as.character(icon("calculator")), " Calculations")),
            htmlOutput("commander"),
            htmlOutput("passenger"),
            textOutput("combined_crew"),
            textOutput("air_weight"),
            textOutput("totalaum"),
            ),
            column(4,
            tags$h3(HTML(as.character(icon("clipboard-check")), " Output")),
            htmlOutput("aumlimit"),
            htmlOutput("frontseat"),
            htmlOutput("ballast"),
            br(),
            htmlOutput("approachspeed")),
            column(10,
            HTML("<hr>"),
            tags$h3(HTML(as.character(icon("plane")), " Alternative aircraft")),
            tableOutput("alt_aircraft"))
            # HTML("<hr>"),
            # plotOutput("stackedbar")
            )
        )
)

server <- function(input, output) {
    
    iv <- InputValidator$new()
    iv$add_rule("commander", sv_between(0, 120))
    iv$add_rule("passenger", sv_between(0, 120))
    iv$enable()
    
    output$images <- renderUI({
        tags$div(img(src = "632crest.png", width = 80, height = 100, style = "float:left;"), br(), h4("632 VGS Weight App"), helpText("This tool is for information only.",
                                                                                                                                br(),
                                                                                                                                "Responsibility remains with the Aircraft Commander"))
    })

    output$commander <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        if (commander_para > 110){
        return(paste("<span style=\"color:red\">AIRCRAFT COMMANDER OVERWEIGHT = ", commander_para,"kg ","</span>", as.character(icon("times-circle"))))
        }  else{
        print(paste0("Aircraft commander with parachute = ", commander_para,"kg"))
        }
    })

    output$passenger <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        if (passenger_para > 110){
            return(paste("<span style=\"color:red\">PASSENGER OVERWEIGHT = ", passenger_para, "kg ","</span>", as.character(icon("times-circle"))))
        } else{
            print(paste0("Passenger with parachute = ", passenger_para,"kg"))
        }
    })
    
    output$combined_crew <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        combined <- commander_para + passenger_para
        print(paste0("Combined crew = ", combined, "kg"))
        })
        
    output$air_weight <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
    print(paste0("Aircraft weight = ", round(aircraft, digits = 2), "kg"))
    })
    
    output$totalaum <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
    print(paste0("Aircraft All-Up-Mass = ", round(AUM, digits = 2), "kg"))
    })
    
    output$aumlimit <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        aumlimit <- if(AUM < 625){
            HTML("Aircraft All-Up-Mass Limits OK", as.character(icon("check-circle")))
        }  else {
            return(paste0("<span style=\"color:red\">Aircraft All-Up-Mass Limits EXCEEDED </span>", as.character(icon("times-circle"))))
        }
    })
    
    output$frontseat <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        min_front <- if(passenger_para < 70){
            return(paste0("<span style=\"color:red\">Front seat minimum weight NOT met </span>", as.character(icon("times-circle"))))
        } else {
            HTML("Front seat minimum weight OK", as.character(icon("check-circle")))
        }
    })
        
    output$ballast <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        ballast_weights <- if(passenger_para < 42){
            return(paste0("<span style=\"color:red\">PASSENGER TOO LIGHT TO FLY </span>", as.character(icon("times-circle"))))
        } else if (passenger_para < 55) {
            return(paste0("<span style=\"color:orange\">PASSENGER IN REAR SEAT ONLY </span>", as.character(icon("exclamation-circle"))))
        } else if (passenger_para < 63) {
            return(paste0("<span style=\"color:orange\">TWO Ballast weights to be fitted </span>", as.character(icon("exclamation-circle"))))
        } else if (passenger_para < 71) {
            return(paste0("<span style=\"color:orange\">ONE Ballast weight to be fitted </span>", as.character(icon("exclamation-circle"))))
        } else if (passenger_para < 111) {
            print(paste0("No Ballast Required ", as.character(icon("check-circle"))))
        } else
            return(paste0("<span style=\"color:red\">PASSENGER TOO HEAVY TO FLY</span>", as.character(icon("times-circle"))))
    })
    
    output$approachspeed <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        if(AUM < 580){
            HTML(as.character(icon("info-circle")), "Approach speed 55kts")
        } else {
            HTML(as.character(icon("info-circle")), "Approach speed 60kts")
        }
    })
    
    output$alt_aircraft <- renderTable({
        commander <- input$commander
        passenger <- input$passenger
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        combined <- commander_para + passenger_para
        dataset <- mutate(dataset, 
                          overweight = if_else(maxcrew < combined, "TRUE", "FALSE"))
        colnames(dataset) = c("Tail No", "A/C Weight", "Max Crew Weight", "Overweight?")
        dataset
        }, striped = TRUE, spacing = "s")
    
    output$stackedbar <- renderPlot({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        plotdata <- read.csv("plotdata.csv", fileEncoding="UTF-8-BOM")
        plotdata$Weight[plotdata$Item=="Aircraft"] <- aircraft
        plotdata$Weight[plotdata$Item=="Commander"] <- commander
        plotdata$Weight[plotdata$Item=="Passenger"] <- passenger
        
        p1 <- ggplot(data = plotdata, aes(x = Ballast, y = Weight, fill = Item)) + labs(title = paste0("Summary of Weight Components for ",input$aircraft), subtitle = "Red line indicates MAX weight") + geom_bar(stat="identity") + scale_fill_brewer(palette="Paired") + theme_minimal() + geom_hline(yintercept=625, color = "red", size=2) + theme(axis.text = element_text(size = 12), axis.title=element_text(size=14,face="bold"), plot.title = element_text(size = 18, face = "bold"), legend.title = element_text(size=12, face="bold"),legend.text = element_text(size=12, face="bold"))
            
            print(p1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
