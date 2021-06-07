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

    titlePanel(" "),

    sidebarLayout(
        sidebarPanel(
            style="text-align: center;",
            h3("632 VGS Weight Limit App"),
            img(src = "632crest.png", height = 120, width = 100),
            br(),
            br(),
            helpText("(1) Select the Aircraft from the dropdown box."),
            selectInput("aircraft", "Aircraft Registration", sort(unique(dataset$aircraft))),
            hr(),
            helpText("(2) Enter weight for each person, WITHOUT a parachute."),
            numericInput("commander", HTML(paste0("Aircraft Commander ","<span style=\"text-decoration:underline\">(no parachute)</span>")), 0, 0, 120, 1),
            numericInput("passenger", HTML(paste0("Passenger ","<span style=\"text-decoration:underline\">(no parachute)</span>")), 0, 0, 120, 1),
            br(),
            img(src = "632logo.png", height = 60, width = 250),
            br(),
            p("Created by ",
                a("Bradley Kennedy", href = "mailto:bradley.kennedy100@rafac.mod.gov.uk", target="_blank"),
                "for ",
                a("632 VGS", href = "https://632vgs.co.uk", target="_blank"),
              br(),
              br(),
                a("Source Code", href = "https://github.com/b-kennedy0/632vgs/blob/master/app.R/", target="_blank"),
                "|" , 
                a("Add new aircraft", href = "https://docs.google.com/forms/d/e/1FAIpQLSdZUL2xQoC6--gYshqy-mRN6uogpsxnZMvVtqh0qOgCmbNavg/viewform?usp=sf_link", target = "_blank")
            )),

        mainPanel(
            tags$h3("Assumptions"),
            HTML("<ul><li>Aircraft Commander in the rear seat</li><li>Passenger in the front seat</li>
                 <li>Nil ballast weights currently fitted</li></ul>"),
            HTML("<hr>"),
            tags$h3("Calculations:"),
            htmlOutput("commander"),
            htmlOutput("passenger"),
            textOutput("combined_crew"),
            textOutput("air_weight"),
            textOutput("totalaum"),
            HTML("<hr>"),
            tags$h3("Output:"),
            htmlOutput("aumlimit"),
           htmlOutput("frontseat"),
           htmlOutput("ballast"),
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
        return(paste("<span style=\"color:red\">AIRCRAFT COMMANDER OVERWEIGHT = ", commander_para,"kg","</span>"))
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
            return(paste("<span style=\"color:red\">PASSENGER OVERWEIGHT = ", passenger_para, "kg","</span>"))
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
            return("<span style=\"color:red\">Aircraft All-Up-Mass Limits EXCEEDED</span>")
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
            return("<span style=\"color:red\">Front seat minimum weight NOT met</span>")
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
            return("<span style=\"color:red\">TOO LIGHT TO FLY</span>")
        } else if (passenger_para < 55) {
            return("<span style=\"color:orange\">PASSENGER IN REAR SEAT ONLY</span>")
        } else if (passenger_para < 63) {
            return("<span style=\"color:orange\">TWO Ballast weights to be fitted</span>")
        } else if (passenger_para < 71) {
            return("<span style=\"color:orange\">ONE Ballast weight to be fitted</span>")
        } else if (passenger_para < 111) {
            print("No Ballast Required")
        } else
            return("<span style=\"color:red\">TOO HEAVY TO FLY</span>")
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
