source("anomaly.R")
source("breakout.R")

library(shinydashboard)

ui <- dashboardPage(

    ## start it
    dashboardHeader(title = "COVID-19 Surveillance Dashboard"),

    ## Sidebar content
    dashboardSidebar(
            br(),
            selectInput('countrydropdown', "Choose your country of interest", choices=sort(countrylist), selected="US"),
            br(),
            p("Want to learn more about anomaly detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series.html"),
            br(),
            br(),
            p("Want to learn more about breakout detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2014/breakout-detection-in-the-wild.html"),
            br(), 
            hr(),
            HTML(paste("Created by:", "Timothy Wiemken, PhD and", "Samson Niemotka, BS", sep="<br/>")),
            p(a("Email Us", target="_blank", href="mailto:timothy.wiemken@health.slu.edu")),
            p(img(src="medicine.png", align="center", height="70px", width="200px"))
),
    #### main dashboard contanet
    dashboardBody(
            tabsetPanel(type = "tabs",
                tabPanel("Anomaly Detection",
                         splitLayout(
                         sliderInput('alphaslider', "Choose the width of the normal range",min=0.01, max=0.5, value=0.05, step=0.01),
                         sliderInput('anomslider', "Choose the maximum proportion of anomalies",min=0.05, max=1, value=.2, step=.05)),
                         br(),
                         plotOutput("plot_anom"), height = 700),
            
                tabPanel("Breakout Detection",
                     sliderInput('breakslider', "Choose the minimum number of points for a breakout",min=3, max=30, value=7, step=1),
                     p("Dotted lines represent the mean of the breakout - a shift in mean has occured during these timeframes."),
                     plotOutput("plot_break")))
            

    ) #### dashboardBody
) #### dashboardPage


# Define server logic required to draw a histogram
source("anomaly.R")
source("breakout.R")
library(shinydashboard)

server <- function(input, output) {
    set.seed(122)

    output$plot_anom <- renderPlot({
        anomaly.covid.mort(alpha = input$alphaslider, max_anoms = input$anomslider, country = input$countrydropdown)
    })
    
    output$plot_break <- renderPlot({
        breakout(n.break= input$breakslider, country = input$countrydropdown)
    })

}
shinyApp(ui = ui, server = server)
