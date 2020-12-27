library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydisconnect)

id <- "1aS6l4KAXX7iuvSSfA6OlkYIlC_eF3POd"
dataset <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
dataset <- dataset[-1]

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

    titlePanel("632 VGS Weight Limit App"),

    sidebarLayout(
        sidebarPanel(
            img(src = "632crest.png", height = 120, width = 100), style="text-align: center;",
            helpText("Select the Aircraft from the dropdown",
                     "box. Then enter weight for each",
                     "person, WITHOUT a parachute."),
            selectInput("aircraft", "Aircraft Registration", sort(unique(dataset$aircraft))),
            numericInput("commander", "Aircraft Commander (no parachute)", 0, 0, 120, 1),
            numericInput("passenger", "Passenger (no parachute)", 0, 0, 120, 1),
            br(),
            img(src = "632logo.png", height = 60, width = 250),
            br(),
            p("Created by Bradley Kennedy for ",
              a("632 VGS", href = "https://632vgs.co.uk", target="_blank"),
              br(),
              br(),
              a("Source Code", href = "https://github.com/b-kennedy0/632vgs/blob/master/app.R/", target="_blank"),
              br(),
              br(),
              a("Add new aircraft", href = "https://docs.google.com/forms/d/e/1FAIpQLSdZUL2xQoC6--gYshqy-mRN6uogpsxnZMvVtqh0qOgCmbNavg/viewform?usp=sf_link", target = "_blank")
            )),

        mainPanel(
            tags$h3("Calculations:"),
            textOutput("commander"),
            textOutput("passenger"),
            textOutput("air_weight"),
            textOutput("totalaum"),
            HTML("<hr>"),
            tags$h3("Output:"),
            textOutput("aumlimit"),
           textOutput("frontseat"),
           textOutput("ballast"),
           textOutput("approachspeed"),
           HTML("<hr>"),
           plotOutput("stackedbar")
        )
    )
)

server <- function(input, output) {

    output$commander <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        if (commander_para > 110){
        print(paste0("AIRCRAFT COMMANDER OVERWEIGHT = ", commander_para,"kg"))
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
            print(paste0("PASSENGER OVERWEIGHT = ", passenger_para, "kg"))
        } else{
            print(paste0("Passenger with parachute = ", passenger_para,"kg"))
        }
    })
        
    output$air_weight <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
    print(paste0("Aircraft weight = ", aircraft,"kg"))
    })
    
    output$totalaum <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
    print(paste0("Aircraft All-Up-Mass = ", AUM,"kg"))
    })
    
    output$aumlimit <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        aumlimit <- if(AUM < 625){
            print("Aircraft All-Up-Mass Limits OK")
        }  else {
            print("Aircraft All-Up-Mass Limits EXCEEDED")  
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
            print("Front seat minimum weight NOT met")
        } else {
            print("Front seat minimum weight OK")
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
            print("TOO LIGHT TO FLY")
        } else if (passenger_para < 55) {
            print("REAR SEAT ONLY")
        } else if (passenger_para < 63) {
            print("TWO Ballast weights MUST be fitted")
        } else if (passenger_para < 71) {
            print("ONE Ballast weight MUST be fitted")
        } else if (passenger_para < 111) {
            print("No Ballast Required")
        } else
            print("TOO HEAVY TO FLY")
    })
    
    output$approachspeed <- renderText({
        commander <- input$commander
        passenger <- input$passenger
        aircraft <- dataset$weight[dataset$aircraft==input$aircraft]
        
        commander_para <- commander + 7
        passenger_para <- passenger + 7
        AUM <- commander_para + passenger_para + aircraft
        
        if(AUM < 580){
            print("Approach speed 55kts")
        } else {
            print("Approach speed 60kts")
        }
    })
    
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
