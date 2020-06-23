source("both.R")
source("both2.R")
source("maphelp.R")
source("both3.R")
source("both4.R")
library(shinydashboard)
library(leaflet)
library(tidyverse)

    ui<-dashboardPage(
        dashboardHeader(title = "COVID-19 Surveillance Dashboard", titleWidth = 300,
                        tags$li(a(target = "_blank", href = 'https://www.slu.edu/slucor/index.php', img(src = 'slu.jpg',
                                title = "Saint Louis University", height = "40px"),
                                style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown"),
                        tags$li(a(target = "_blank", href = 'https://www.slu.edu/slucor/index.php', img(src = 'ahead.jpg',
                                title = "AHEAD Institute", height = "40px"),
                                style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown")),

        dashboardSidebar(width = 300,
            sidebarMenu(
                br(),
                menuItem("Mortality by Country", tabName = "countrymort"),
                menuItem("Mortality in the US by State and County", tabName = "usmortality"),
                menuItem("MO Healthcare Utilization & Infection Rate Maps", tabName = "moutil"), 
                menuItem("MO New Cases and Mortality Rate Maps", tabName = "monew"), 
                menuItem("Infection Rate by St. Louis ZIP Code", tabName = "mozip"), 
                menuItem("Saint Louis COVID-19 Task Force", tabName = "stltf"), 
                menuItem("Analyze your own data", tabName = "template"),
                br(),
                br(),
                p("Want to learn more about breakout detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2014/breakout-detection-in-the-wild.html"),
                br(),
                hr(),
                HTML(paste("Created by:", "Samson Niemotka, BS", "Christopher Prener, PhD and", "Timothy Wiemken, PhD", sep="<br/>")),
                p(a("Email Us", target="_blank", href="mailto:timothy.wiemken@health.slu.edu")), 
                "Version 1.1, June 10, 2020"
            )),
        dashboardBody(
            tabItems(
                tabItem(tabName = "countrymort",
                        column(6, selectInput('countrydropdown', "Choose your country of interest", choices=sort(countrylist), selected="US")),
                        column(6, sliderInput('breakslider_1', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                        plotOutput("plot_both"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "Data Source (Mortality by Country), Center for Systems Science and Engineering at Johns Hopkins University, GitHub"), style = "font-size:10px"),
                ),
                
                tabItem(tabName = "usmortality",
                        column(4, selectInput('state', 'Choose your state of interest', choices = statelist)),
                        column(4, selectInput('county', 'Choose your county of interest', "")),
                        column(4, sliderInput('breakslider_2', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                        plotOutput("plot_us"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(tags$a(href="https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv", "Data Source (US Mortality by State/County), New York Times, GitHub"), style = "font-size:10px")
                ),
            
                tabItem(tabName = "moutil",
                    fluidRow(tags$head(tags$style(".butt{background-color:#add8e6;}")),
                        box(actionButton("go", "Click to view map.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        br(),
                        br(),
                        title="Healthcare Utilization:",
                        "This map uses data from the Kaiser Health Network to identify counties (in gray)
                        without any hospitals as well as the number of ICU beds per 1,000 residents in counties
                        that do have hospitals. Keep in mind that some hospitals may have expanded ICU bed capacity
                        in anticipation of increased need. For Kansas City, all hospital and ICU bed data have
                        been allocated to Jackson, Clay, Cass, and Platte Counties.",
                        leafletOutput("icu"), collapsible = T, collapsed = F),
                        box(actionButton("go2", "Click to view map.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        br(),
                        br(),
                        title = "Infection Rates by County:",
                        "This map shows infections as a rate per 1,000 residents. It is important not to map the raw
                        counts themselves, but if you want to see those data, click on a county. You can also view the hospital 
                        infrastructure details from the Healthcare Utilization map for each county by clicking on them.",
                        leafletOutput("case_rate"), collapsible = T, collapsed = F)),
                    br(), 
                    p(tags$a(href="https://zenodo.org/record/3746387#.XuEP5mpKhpV", "Data Source - slu-openGIS/covid_daily_viz: v0.1.0 - Initial reporting Structure"), style = "font-size:10px")
                    ),
                tabItem(tabName = "monew",
                    fluidRow(
                        box(actionButton("go3", "Click to view map.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        br(),
                        br(),
                        title = "Average New Cases by County:",
                        "This map shows a seven-day rolling average of new cases. For this map, this covers 2020-05-25 back through
                        2020-05-18. There is not a threshold for what constitutes a high or low average, but the higher the average
                        number of new cases, the more new spread we can infer. For mapping purposes, these are displayed as a rate
                        per 1,000 residents. As with the prior maps, additional details are available by clicking on each county.",
                        leafletOutput("avg_rate"), collapsible = T, collapsed = F),
                        box(actionButton("go4", "Click to view map.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        br(),
                        br(),
                        title = "Mortality Rates by County:",
                        "This map shows mortality as a rate per 1,000 residents. As with the prior maps, raw counts of deaths and
                        hospital infrastructure details are available by clicking on individual counties.",
                        leafletOutput("mort_rate"), collapsible = T, collapsed = F)),
                    br(), 
                    p(tags$a(href="https://zenodo.org/record/3746387#.XuEP5mpKhpV", "Data Source - slu-openGIS/covid_daily_viz: v0.1.0 - Initial reporting Structure"), style = "font-size:10px")
                    ), 
                tabItem(tabName = "mozip",
                    fluidRow(align = "center",
                        box(width = 12,
                        actionButton("go5", "Click to view map.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        br(),
                        br(),
                        title = "Infection Rates by St. Louis (ZIP Code):",
                        "This map shows infections as a rate per 1,000 residents for all ZCTAs (ZIP Code Tabulation Area) with five
                        or more patients. It is important not to map the raw counts themselves, but if you want to see those
                        data, click on a ZCTA. If a ZCTA straddles the city and the county, and only has reported infection numbers
                        in one entity, its estimated partial population will be displayed. Similarly, estimated partial populations
                        for zip codes that straddle outlying counties are used.",
                        leafletOutput("demo"), collapsible = T, collapsed = F)),
                    br(), 
                    p(tags$a(href="https://zenodo.org/record/3746387#.XuEP5mpKhpV", "Data Source - slu-openGIS/covid_daily_viz: v0.1.0 - Initial reporting Structure"), style = "font-size:10px")
                    ), 
                tabItem(tabName = "stltf",
                    fluidRow(
                        column(6, selectInput("newdata", "Select your metric here.", choices = ddlist)),
                        column(6, sliderInput('breakslider_3', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200)),
                        plotOutput("task"))),
                tabItem(tabName = "template",
                    fluidRow(
                        box(title = "Data template", collapsible = T,
                        downloadButton("template", strong("STEP 1: Download the template and paste in your data")),
                        br(),
                        br(),
                        fileInput('file1', strong("STEP 2: Upload your data - please use the template (.csv format)"),
                            accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
                        htmlOutput("compat")
                        ),
                        box(title = "Plot options", collapsible = T,
                            column(4, textInput("amount", "X-axis ticks to divide by:", value = 1)),
                            column(4, selectInput("breaker", "Show X-axis ticks by:", choices = c("day","week", "month", "year"), selected = "month")),
                            column(4, textInput("xaxis", "X-axis label")),
                            column(12, textInput("yaxis", "Y-axis label")),
                            column(12, sliderInput('breakslider_4', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200))
                            ),
                        conditionalPanel(condition = "output.compat != 'Error: Data needs at least 15 rows for analysis.' & output.compat != 'Error: Uploaded data has too many columns. Please use the template with two columns, date and count or rate.'",
                        plotOutput("custom"))
                        ))
        ) #TabItems
    ) #DashboardBody 
) #DashboardPage
    
    
    
    
    
    
    
    
#     
#     
#     
#     
#     
#     br(),
#             br(),
#             #sliderInput('alphaslider', "Width of the normal range:",min=0.01, max=0.5, value=0.05, step=0.01, width=200),
#             #sliderInput('anomslider', "Maximum proportion of anomalies:",min=0.05, max=1, value=.2, step=.05, width=200),
#             #sliderInput('breakslider', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200),
#             br(),
#             #p("Want to learn more about anomaly detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2015/introducing-practical-and-robust-anomaly-detection-in-a-time-series.html"),
#             br(),
#             br(),
#             #p("Want to learn more about breakout detection?"), a("Click here.", target="_blank", href="https://blog.twitter.com/engineering/en_us/a/2014/breakout-detection-in-the-wild.html"),
#             br(), 

#             p(tags$a(href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "Data Source (Mortality by Country), Center for Systems Science and Engineering at Johns Hopkins University, GitHub"), style = "font-size:10px"),
#             p(tags$a(href="https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv", "Data Source (US Mortality by State/County), New York Times, GitHub"), style = "font-size:10px")
#             



library(shinydashboard)

server <- function(input, output, session) {
    set.seed(122)
    
    outVar <- reactive({
        mydata <- input$state
        mydata
    })
    
    observe({
        updateSelectInput(session, "county", choices = c("All", sort(as.character(unique(data$county[data$state == outVar()[1]]))))
        )})
    
    output$plot_both <- renderPlot({
        anombreak.covid.mort(alpha = 0.05, max_anoms = 0.2, country = input$countrydropdown, n.break=input$breakslider_1)
    })
    
    output$plot_us <- renderPlot({
        anombreak.covid.mort_sc(alpha = 0.05, max_anoms = 0.2, n.break=input$breakslider_2, state = input$state, county = input$county)
    })
    #ICU button
    icubutton <- eventReactive(input$go, {
        icu
    })
    output$icu <- renderLeaflet({
        icubutton()
    })
    #Case rate button
    casebutton<-eventReactive(input$go2, {
        case_rate
    })
    output$case_rate <-renderLeaflet({
        casebutton()
    })
    #Avg_rate button
    avg_ratebutton<-eventReactive(input$go3, {
        avg_rate
    })
    output$avg_rate <-renderLeaflet({
        avg_ratebutton()
    })
    #Mort_rate button
    mort_ratebutton<-eventReactive(input$go4, {
        mort_rate
    })
    output$mort_rate <-renderLeaflet({
        mort_ratebutton()
    })
    #Demo button
    demobutton<-eventReactive(input$go5, {
        demo
    })
    output$demo <-renderLeaflet({
        demobutton()
    })
    output$task <- renderPlot({
        anombreak.covid.mort_task(alpha=0.05, subchoice = input$newdata, max_anoms=0.2, n.break=input$breakslider_3)
    })
    output$template <- downloadHandler(
        filename="template.csv",  # desired file name on client 
        content=function(file){
            write.csv(read.csv("template.csv", stringsAsFactors = F), file, row.names=F, na="")
    })
    output$custom <- renderPlot({
        inFile<-input$file1
        if(is.null(inFile)) return(NULL)
        dataup<-read.csv(inFile$datapath, sep = ",", header = T, fill = T)
        anombreak.covid.mort_upload(dataup, alpha=0.05, max_anoms=0.2, n.break=input$breakslider_4, yaxis=input$yaxis, xaxis = input$xaxis, breaks = paste(input$amount, input$breaker))
    })
    output$compat<-renderText({
        inFile<-input$file1
        if(is.null(inFile)) return(NULL)
        dataup<-read.csv(inFile$datapath, sep = ",", header = T, fill = T)
        compat(dataup)
    })

}
shinyApp(ui = ui, server = server)