library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)
library(leaflet)
library(shinyWidgets)
library(lubridate)
library(data.table)
library(htmltools)
library(htmlwidgets)
library(leaflet.minicharts)

bikeshare <- read.csv("austin_bike_share_with_hour_3.csv")
bikeshare <- bikeshare[,-c(1)]

# make a new feature for purpose of visualization
day_hour <- paste(bikeshare[,"Date"],bikeshare[,"hour"])
day_hour <- paste(day_hour,":00:00")
bikeshare$day_hour <- day_hour


bikeshare$year <- factor(bikeshare$year)
number_of_rides = nrow(bikeshare)
number_of_minutes = sum(bikeshare$duration_minutes)
group_by_year = bikeshare %>% group_by(year) %>% tally()
group_by_year$year <- factor(group_by_year$year)
names(group_by_year) <- c("Year","n")
station_name_s <- unique(bikeshare$start_station_name)

date_format <- format(as.Date(bikeshare$Date), "%Y-%m")
bikeshare$date_format <- date_format
group_by_year <- bikeshare %>% group_by(date_format) %>% tally()
TemAvgFMonth <- bikeshare %>% group_by(date_format) %>% summarise(TempAvg = mean(TempAvgF))
group_by_year$TempAvgF <- ceiling(TemAvgFMonth$TempAvg)


# Preprocessing for daily trip anaylysis
# make a new feature for purpose of visualization
day_hour <- paste(bikeshare[,"Date"],bikeshare[,"hour"])
day_hour <- paste(day_hour,":00:00", sep="")
day_hour <- day_hour %>% ymd_hms()
bikeshare$day_hour <- day_hour
agg_start_station <- bikeshare %>% group_by(start_station_name, start_station_latitude, start_station_longitude, day_hour) %>% tally()
names(agg_start_station) <- c("station_name", "latitude", "longitude","day_hour","start_n")
agg_end_station <- bikeshare %>% group_by(end_station_name, day_hour) %>% tally()
names(agg_end_station) <- c("station_name", "day_hour","end_n")

agg_start_station <-  data.table(agg_start_station, id="station_name")
agg_end_station <- as.data.table(agg_end_station, id="station_name")

agg_station <- merge(agg_start_station, agg_end_station)

station_update <- distinct(agg_station[,c("station_name","latitude","longitude")])
station_update$start_n <- 0
station_update$end_n <- 0
station_update$id <- "station_name"
station_update <- station_update %>% arrange(station_name)


variables = reactiveValues(currentDate = as.Date(min(bikeshare$day_hour)-3600,format="%Y-%m-%d"),
                           data_update = NULL
)



# Preprocessing for station analysis
start_station <- bikeshare %>% group_by(start_station_name, start_station_latitude, start_station_longitude, end_station_name,
                                        end_station_latitude, end_station_longitude) %>% tally() %>% arrange(-n)
agg_start_station <- bikeshare %>% group_by(start_station_name, start_station_latitude, start_station_longitude) %>% tally()
agg_end_station <- bikeshare %>% group_by(end_station_name, end_station_latitude, end_station_longitude) %>% tally()
start_station$start_station_name <- as.character(start_station$start_station_name)
start_map_label <- paste("",agg_start_station$start_station_name, "-",agg_start_station$n) %>% lapply(htmltools::HTML)
end_map_label <- paste("", agg_end_station$end_station_name, "-", agg_end_station$n) %>% lapply(htmltools::HTML)


antPlugin <- htmlDependency(name = "leaflet-ant-path", version = "1.2.1",
                            # src = c(href = ""),
                            src=normalizePath("ant_path"),
                            script = "leaflet-ant-path.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

registerPlugin_2 <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}


# UI for the dashboard
dashboard_ui_1 <- fluidRow(
  valueBoxOutput("total_ride"),
  valueBoxOutput("total_number_minutes")
)

dashboard_ui_2 <- fluidRow(
  box(
    title="Number of Rides",
    # status = "primary",
    solidHeader = TRUE,
    collapsible = FALSE,
    width = 9,
    plotlyOutput("plot_ride_over_year", height="450px"),
    footer = "*Click to the bar grpah above to see daily traffic of particular month"
  ),
  box(
    width=3,
    solidHeader = TRUE,
    collapsible = FALSE,
    fluidRow(
      box(
        solidHeader = TRUE,
        collapsible = FALSE,
        title="Select Year",
        width = 12,
        # Define the sidebar with one input
        sidebarPanel(
          width = 12,
          selectInput("year", "Year:", 
                      choices=c("All"="all","2014"=2014,"2015"=2015,"2016"=2016)),
          hr(),
          helpText("")
        )
      ),
      fluidRow(
        box(
          width = 12,
          solidHeader = TRUE,
          collapsed = FALSE,
          title="Temperature",
          sidebarPanel(
            width=12,
            materialSwitch(inputId = "toogleID", label = "Show Temperature", status = "danger"),
            hr(),
            helpText("")
            
          )
          
        )
      )
      
    )
  )
)

dashboard_ui_6 <- fluidRow(
  box(
    title="System usage by Day of Months",
    solidHeader = TRUE,
    collapsible = FALSE,
    # status="primary",
    width=9,
    plotlyOutput("db2_day_of_month", height = "400px"),
    footer = "*Click to the bar graph above to see the traffic hour of particular day"
  ),
  box(
    title="By Weekday",
    width=3,
    solidHeader = TRUE,
    collapsible = FALSE,
    plotlyOutput("db2_by_weekday", height= '400px')
  )
)
dashboard_ui_7 <- fluidRow(
  box(
    title="System usage by Hours",
    solidHeader = TRUE,
    collapsible = FALSE,
    # status="primary",
    width=9,
    plotlyOutput("db2_day_of_hour", height = "400px")
  )
)

dashboard_ui_3 <- fluidRow(
  box(
    title="Number of trip by duration",
    solidHeader = TRUE,
    collapsible = FALSE,
    width=10,
    plotlyOutput("user_type_duration", height = 300)
  )
  
)

dashboard_ui_4 <- fluidRow(
  box(
    # title="Average Number of Rides between Weekdays and Weekends",
    solidHeader = TRUE,
    # collapsible = TRUE,
    width = 6,
    # status="primary",
    plotlyOutput("db4_popular_station", height = 650)
  ),
  box(
    # title="Average Minutes of Trip between Weekdays and Weekends",
    solidHeader = TRUE,
    # collapsible = TRUE,
    width=6,
    # status="primary",
    plotlyOutput("db4_popular_route", height = 650)
  )
)



dashboard <- fluidPage(
  includeCSS(path="AdminLTE.css"),
  includeCSS(path="shinydashboard.css"),
  tags$style(".tab-content {background-color: #FCFCFC}", 
             ".container-fluid {background-color: #FCFCFC; margin-top:20px",
             ".navbar {background-color: #FCFCFC}"),
  dashboard_ui_1, dashboard_ui_2, dashboard_ui_6,
  dashboard_ui_7, dashboard_ui_3, dashboard_ui_4
)

station_analysis <- bootstrapPage(
  div(class="outer",
      tags$head(
        includeCSS("styles.css")
      ),
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      # "shiny-input-container:not(.shiny-input-container-inline) {width: 300px; max-width: 100%;height: 150px; }"),
      leafletOutput("mymap", width = "100%", height = "100%"),
      absolutePanel( id="controls", top=50, right=10, class="panel panel-default", fixed=TRUE,
                     draggable= TRUE, left="auto",bottom="auto", width=400, height="100%",
                     h3(tags$b("Stations Explorer")),
                     box(
                       width = 12,
                       splitLayout(
                         cellWidths = c("50%", "50%"),
                         selectInput("map_station", "Orign/Destination:", 
                                     choices=c("Origin"="origin","Destination"="destination")),
                         selectInput("map_station_top", "Top:", 
                                     # The reason I put 100 instead of all just because of the convenience to have light code in server side
                                     choices=c("All"=100,"Top 5"=5, "Top 10"=10)),
                         tags$head(tags$style(HTML(".selectize-control {height: 120px;}")))
                       )
                     ),
                     # selectInput("map_station", "Orign/Destination:", 
                     #             choices=c("Origin"="origin","Destination"="destination")),
                     htmlOutput("selected_station"),
                     plotlyOutput("map_count_station", height=630)
      ),
      absolutePanel( id="controls", top=50, right="auto", class="panel panel-default", fixed=TRUE,
                     draggable= TRUE, left=10,bottom="auto", width=350, height="100%",
                     h3(tags$b("Stations Analysis")),
                     textOutput("selected_station_2"),
                     br(),
                     h4("Borrow activity by hours"),
                     plotlyOutput("map_borrow_activity_hour", height=300),
                     br(),
                     h4("Activity by weekeday"),
                     plotlyOutput("map_activity_week_day", height=300)
      )
      
  )
  
)


daily_trip <- bootstrapPage(
  div(class="outer",
      tags$head(
        includeCSS("styles.css")
      ),
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("map_daily_analysis", width="100%", height = "100%"),
      
      absolutePanel( id="controls_time", top=60, right="auto", class="panel panel-default", fixed=TRUE,
                     draggable= TRUE, left=20,bottom="auto", width=360, height=530,
                     h3("Timeline"),
                     br(),
                     sliderInput("timerange", "Time range",
                                 min=min(bikeshare$day_hour),
                                 max=max(bikeshare$day_hour),
                                 value=min(bikeshare$day_hour),
                                 step = 3600,
                                 timezone = "+0000",
                                 animate = T),
                     br(),
                     h4(textOutput("mini_trip_day_text")),
                     h5("*Hit the play button, the time slider will automatically move"),
                     hr(),
                     h5("*This map illustrates the number of trip by hour of day"),
                     h5("*Blank map (no circle) mean no operation"),
                     h5("*The blue indicates the number of trip which start from station at specific period of time"),
                     h5("*The organce indicates number of trips which arrive to station at specific period of time"),
                     h5("*Click to the specific circle to see concrete number")
                     
                     
      )
  )
)

ui <- navbarPage("Austin Bikeshare Visualization",
                 tabPanel("System",
                          dashboard
                 ),
                 tabPanel("Daily Trip",
                          daily_trip
                 ),
                 tabPanel("Station Analysis",
                          station_analysis
                 )
)


server <- function(input, output){
  
  # render frontend for Dashboard
  output$total_ride <- renderValueBox({
    valueBox(
      formatC(number_of_rides, format = "d", big.mark = ","),
      paste("Total Rides",""),
      icon = icon("bicycle"),
      color="purple"
    )
  })
  output$total_number_minutes <- renderValueBox({
    valueBox(
      formatC(number_of_minutes, format = "d", big.mark = ","),
      "Minutes",
      icon= icon("clock"),
      color="green"
    )
  })
  output$plot_ride_over_year <- renderPlotly({
    bikeshare_year <- bikeshare %>% group_by(year, month) %>% tally()
    bikeshare_year$month <- factor(bikeshare_year$month) 
    names(bikeshare_year) <- c("Year","Month","n")
    avg <- bikeshare %>% group_by(year, month) %>% summarise(TempAvg = mean(TempAvgF))
    if(input$year == 'all'){
      
      p <- plot_ly(group_by_year, source="bar2") %>%
        add_trace(x=~date_format, y=~n, type = 'bar', name="Month", 
                  marker = list(color = '#4876ba'),
                  hoverinfo = "text",
                  text=~paste(date_format,n))
      if(input$toogleID == TRUE){
        p <- p %>% add_trace(x=~date_format, y=~TempAvgF, type="scatter", mode="line", name="Temperature", yaxis='y2',
                             line=list(color="#bc3725"),
                             hoverinfo = 'text',
                             text=~paste(TempAvgF,"F")) %>% 
          layout(title="Number of Rides over Year from 2014 to 2016", 
                 xaxis= list(title=""),
                 yaxis=list(side='left', title="Number of Rides"),
                 yaxis2=list(side='right', overlaying='y',title='Temperature in degrees F'))
      }else{
        p <- p %>% layout(title="Number of Rides over Year from 2014 to 2016", 
                          xaxis= list(title=""),
                          yaxis=list(side='left', title="Number of Rides"))
      }
      p 
    }else if(input$year == 2014){
      
      year_2014 <- bikeshare_year[bikeshare_year$Year==2014,]
      avg_2014  <- avg[avg$year == 2014,]
      year_2014$TempAvgF <- avg_2014$TempAvg
      if(input$toogleID == TRUE){
        plot_ly(year_2014, source="bar2") %>%
          add_trace(x=~Month, y=~n, type = 'bar', name="Month", 
                    # color=~Month,
                    marker = list(color = '#4876ba'),
                    hoverinfo = "text",
                    text=~paste(Month,n))%>% 
          add_trace(x=~Month, y=~TempAvgF, type="scatter", mode="line", name="Temperature", yaxis='y2',
                    line=list(color="#bc3725"),
                    hoverinfo = 'text',
                    text=~paste(TempAvgF,"F")) %>% 
          layout(title="Number of Rides over Month in 2014", 
                 xaxis= list(title=""),
                 yaxis=list(side='left', title="Number of Rides"),
                 yaxis2=list(side='right', overlaying='y',title='Temperature in degrees F'))
      }else{
        plot_ly(year_2014, x=~Month, y=~n, 
                # color=~Month, 
                type='bar', source="bar2") %>% layout(title="Number of Rides over Month in 2014",
                                                      yaxis=list(title="Number of Rides"))
      }
      
    }else if(input$year == 2015){
      year_2015 <- bikeshare_year[bikeshare_year$Year==2015,]
      avg_2015  <- avg[avg$year == 2015,]
      year_2015$TempAvgF <- avg_2015$TempAvg
      if(input$toogleID == TRUE){
        plot_ly(year_2015, source="bar2") %>%
          add_trace(x=~Month, y=~n, type = 'bar', name="Month", 
                    # color=~Month,
                    marker = list(color = '#4876ba'),
                    hoverinfo = "text",
                    text=~paste(Month,n))%>% 
          add_trace(x=~Month, y=~TempAvgF, type="scatter", mode="line", name="Temperature", yaxis='y2',
                    line=list(color="#bc3725"),
                    hoverinfo = 'text',
                    text=~paste(TempAvgF,"F")) %>% 
          layout(title="Number of Rides over Month in 2014", 
                 xaxis= list(title=""),
                 yaxis=list(side='left', title="Number of Rides"),
                 yaxis2=list(side='right', overlaying='y',title='Temperature in degrees F'))
      }else{
        plot_ly(year_2015, x=~Month, y=~n, 
                # color=~Month,
                type='bar', source="bar2") %>% layout(title="Number of Rides over Month in 2015",
                                                      yaxis=list(title="Number of Rides"))
      }
    }else if(input$year == 2016){
      year_2016 <- bikeshare_year[bikeshare_year$Year==2016,]
      avg_2016  <- avg[avg$year == 2016,]
      year_2016$TempAvgF <- avg_2016$TempAvg
      if(input$toogleID == TRUE){
        plot_ly(year_2016, source="bar2") %>%
          add_trace(x=~Month, y=~n, type = 'bar', name="Month", 
                    # color=~Month,
                    marker = list(color = '#4876ba'),
                    hoverinfo = "text",
                    text=~paste(Month,n))%>% 
          add_trace(x=~Month, y=~TempAvgF, type="scatter", mode="line", name="Temperature", yaxis='y2',
                    line=list(color="#bc3725"),
                    hoverinfo = 'text',
                    text=~paste(TempAvgF,"F")) %>% 
          layout(title="Number of Rides over Month in 2014", 
                 xaxis= list(title=""),
                 yaxis=list(side='left', title="Number of Rides"),
                 yaxis2=list(side='right', overlaying='y',title='Temperature in degrees F'))
      }else{
        plot_ly(year_2016, x=~Month, y=~n, 
                # color=~Month,
                type='bar', source="bar2") %>% layout(title="Number of Rides over Month in 2016",
                                                      yaxis=list(title="Number of Rides"))
      }
    }
  })
  output$db2_day_of_month <- renderPlotly({
    s <- event_data("plotly_click", source = "bar2", priority = "event")
    if(length(s)){
      vars <- c(s[["x"]], s[["y"]])
      year_month <- vars[1]
      if(nchar(year_month)>5){
        year_month <- paste(year_month,"-01", sep="")
        year_month <- as.factor(year_month)
        month <- format(as.Date(year_month),"%m")
        year  <- format(as.Date(year_month),"%Y")
        bikeshare_year <- bikeshare %>% group_by(year,month,Date, Events) %>% tally()
        avg <-            bikeshare %>% group_by(year,month,Date, Events) %>% summarise(TempAvg = mean(TempAvgF))
        bikeshare_year$TempAvg <- avg$TempAvg
        data_year <- bikeshare_year[bikeshare_year$year==year,]
        data_month <- data_year[data_year$month==as.numeric(month),]
        data_month$Day <- format(as.Date(data_month$Date), "%d")
        
        if(input$toogleID == TRUE){
          plot_ly(data_month, source="bar3") %>%
            add_trace(x=~Day, y =~n,type="bar",
                      # name="Day of Month",
                      color=~Events,
                      # marker = list(color = "#4876ba"),
                      hoverinfo="text",
                      text=~paste(Day, n)) %>% 
            add_trace(x=~Day, y=~TempAvg,
                      type='scatter', 
                      mode='line', 
                      name="Temperature", 
                      yaxis='y2',
                      line = list(color="#bc3725"),
                      hoverinfo = 'text',
                      text=~paste(TempAvg,"F")) %>%
            layout(title=paste("System used By Day in",month.name[as.numeric(month)]),
                   xaxis= list(title=""),
                   yaxis=list(side='left', title="Number of Rides", showgrid=FALSE, zeroline=FALSE),
                   yaxis2=list(side='right', overlaying='y',title='Temperature in degrees F')
            )
        }
        else{
          plot_ly(data_month, x=~Day, y=~n, type='bar', color = ~Events, source = "bar3")  %>% 
            layout(title=paste("System used By Day in",month.name[as.numeric(month)]),
                   yaxis=list(title="Number of Rides"))
        }
        
      }else{
        month <- as.numeric(vars[1])
        year <- input$year
        bikeshare_year <- bikeshare %>% group_by(year,month,Date, Events) %>% tally()
        avg <-            bikeshare %>% group_by(year,month,Date, Events) %>% summarise(TempAvg = mean(TempAvgF))
        bikeshare_year$TempAvg <- avg$TempAvg
        bikeshare_year$month <- factor(bikeshare_year$month)
        data_year <- bikeshare_year[bikeshare_year$year==year,]
        data_month <- data_year[data_year$month==month,]
        data_month$Day <- format(as.Date(data_month$Date), "%d")
        if(input$toogleID == TRUE){
          plot_ly(data_month, source="bar3") %>%
            add_trace(x=~Day, y =~n,type="bar",
                      # name="Day of Month",
                      color=~Events,
                      # marker = list(color = "#4876ba"),
                      hoverinfo="text",
                      text=~paste(Day, n)) %>% 
            add_trace(x=~Day, y=~TempAvg,
                      type='scatter', 
                      mode='line', 
                      name="Temperature", 
                      yaxis='y2',
                      line = list(color="#bc3725"),
                      hoverinfo = 'text',
                      text=~paste(TempAvg,"F")) %>%
            layout(title=paste("System used By Day in",month.name[as.numeric(month)]),
                   xaxis= list(title=""),
                   yaxis=list(side='left', title="Number of Rides", showgrid=FALSE, zeroline=FALSE),
                   yaxis2=list(side='right', overlaying='y',title='Temperature in degrees F')
            )
        }
        else{
          plot_ly(data_month, x=~Day, y=~n, type='bar', color = ~Events, source = "bar3")  %>% 
            layout(title=paste("System used By Day in",month.name[as.numeric(month)]),
                   yaxis=list(title="Number of Rides"))
        }
      }
    }
    
  })
  
  output$db2_day_of_hour <- renderPlotly({
    s2 <- event_data("plotly_click", source = "bar3", priority = "event")
    s1 <- event_data("plotly_click", source = "bar2", priority = "event")
    if(length(s2)){
      vars1 <- c(s1[["x"]], s1[["y"]])
      vars2 <- c(s2[["x"]], s2[["y"]])
      if(nchar(vars1[1])>5){
        year_month <- vars1[1]
        day <- as.numeric(vars2[1])
        year_month <- paste(year_month,"-01", sep="")
        year_month <- as.factor(year_month)
        month <- as.numeric(format(as.Date(year_month),"%m"))
        year  <- format(as.Date(year_month),"%Y")
        bikeshare_year <- bikeshare %>% group_by(year, month,Date,hour) %>% tally()
        bikeshare_year$month <- factor(bikeshare_year$month)
        data_year <- bikeshare_year[bikeshare_year$year==year,]
        data_month <- data_year[data_year$month==month,]
        data_month$Date <- as.character(data_month$Date)
        date <- str_split_fixed(data_month$Date, "-", 3)
        data_month$Day <- date[,3]
        data_day <- data_month[as.numeric(data_month$Day)==day,]
        plot_ly(data_day, x=~hour, y =~n, type = 'bar') %>% 
          layout(title=paste("System used By Hour of Day ",""),
                 yaxis=list(title="Number of Ride"))
        
      }else{
        month <- as.numeric(vars1[1])
        day <- as.numeric(vars2[1])
        year <- input$year
        bikeshare_year <- bikeshare %>% group_by(year, month,Date,hour) %>% tally()
        bikeshare_year$month <- factor(bikeshare_year$month)
        data_year <- bikeshare_year[bikeshare_year$year==year,]
        data_month <- data_year[data_year$month==month,]
        data_month$Date <- as.character(data_month$Date)
        date <- str_split_fixed(data_month$Date, "-", 3)
        data_month$Day <- date[,3]
        data_day <- data_month[as.numeric(data_month$Day)==day,]
        plot_ly(data_day, x=~hour, y =~n, type = 'bar') %>% 
          layout(title=paste("System used By Hour of Day ",""),
                 yaxis=list(title="Number of Ride"))
      }
      
      
    }
  })
  
  output$db2_by_weekday <- renderPlotly({
    if(input$year == 'all'){
      s1 <- event_data("plotly_click", source = "bar2", priority = "event")
      
      if(!is.null(s1)){
        if(!is.null(nchar(s1$x)) & nchar(s1$x)>3){
          s11 <- event_data("plotly_click", source="bar2", priority = "event")
          if(length(s11)){
            vars1 <- c(s11[["x"]], s11[["y"]])
            year_month <- vars1[1]
            year_month <- paste(year_month,"-01", sep="")
            year_month <- as.factor(year_month)
            month <- as.numeric(format(as.Date(year_month),"%m"))
            year  <- format(as.Date(year_month),"%Y")
            
            data_year <- bikeshare[bikeshare$year==year,]
            data_month <- data_year[data_year$month==month,]
            austin_bike_share <- data_month %>% group_by(day) %>% tally()
            avg_hours_per_weekdays <- austin_bike_share %>% filter(day != "Sat", day !="Sun")
            avg_hours_per_weekend <- austin_bike_share %>% filter(day == "Sat" | day =="Sun")
            data <- rbind(avg_hours_per_weekdays, avg_hours_per_weekend)
            plot_ly(data, x=~n, y=~reorder(day,n), type='bar', orientation='h')
          }
        }
        else{
          austin_bike_share <- bikeshare %>% group_by(day) %>% tally()
          avg_hours_per_weekdays <- austin_bike_share %>% filter(day != "Sat", day !="Sun")
          avg_hours_per_weekend <- austin_bike_share %>% filter(day == "Sat" | day =="Sun")
          data <- rbind(avg_hours_per_weekdays, avg_hours_per_weekend)
          plot_ly(data, x=~n, y=~reorder(day,n), type='bar', orientation='h')
        }
      }
      else{
        austin_bike_share <- bikeshare %>% group_by(day) %>% tally()
        avg_hours_per_weekdays <- austin_bike_share %>% filter(day != "Sat", day !="Sun")
        avg_hours_per_weekend <- austin_bike_share %>% filter(day == "Sat" | day =="Sun")
        data <- rbind(avg_hours_per_weekdays, avg_hours_per_weekend)
        plot_ly(data, x=~n, y=~reorder(day,n), type='bar', orientation='h')
      }
    }else{
      s2 <- event_data("plotly_click", source = "bar2", priority = "event")
      if(!is.null(s2)){
        if(!is.null(nchar(s2$x)) & nchar(s2$x)<3){
          s22 <- event_data("plotly_click", source = "bar2", priority = "event")
          if(length(s22)){
            year <- input$year
            vars2 <- c(s22[["x"]], s22[["y"]])
            month <- as.numeric(vars2[1])
            data_year <- bikeshare[bikeshare$year==year,]
            data_month <- data_year[data_year$month==month,]
            austin_bike_share <- data_month %>% group_by(day) %>% tally()
            avg_hours_per_weekdays <- austin_bike_share %>% filter(day != "Sat", day !="Sun")
            avg_hours_per_weekend <- austin_bike_share %>% filter(day == "Sat" | day =="Sun")
            data <- rbind(avg_hours_per_weekdays, avg_hours_per_weekend)
            plot_ly(data, x=~n, y=~reorder(day,n), type='bar', orientation='h')
          }
        }else{
          year <- input$year
          austin_bike_share <- bikeshare %>% group_by(year, day) %>% tally()
          austin_bike_share <- austin_bike_share[austin_bike_share$year == year,]
          avg_hours_per_weekdays <- austin_bike_share %>% filter(day != "Sat", day !="Sun")
          avg_hours_per_weekend <- austin_bike_share %>% filter(day == "Sat" | day =="Sun")
          data <- rbind(avg_hours_per_weekdays, avg_hours_per_weekend)
          plot_ly(data, x=~n, y=~reorder(day,n), type='bar', orientation='h')
        } 
      }else{
        year <- input$year
        austin_bike_share <- bikeshare %>% group_by(year, day) %>% tally()
        austin_bike_share <- austin_bike_share[austin_bike_share$year == year,]
        avg_hours_per_weekdays <- austin_bike_share %>% filter(day != "Sat", day !="Sun")
        avg_hours_per_weekend <- austin_bike_share %>% filter(day == "Sat" | day =="Sun")
        data <- rbind(avg_hours_per_weekdays, avg_hours_per_weekend)
        plot_ly(data, x=~n, y=~reorder(day,n), type='bar', orientation='h')
      }
    }
  })
  
  
  output$db4_popular_station <- renderPlotly({
    popular_station <- bikeshare %>% group_by(start_station_name) %>% tally() %>% arrange(-n)
    p <- plot_ly(
      type='table',
      columnorder=c(1,2),
      columnwidth=c(400,80),
      header = list(
        values = c('<b>Top 10 Popular Stations</b>', '<b># of trip</b>'),
        line = list(color='#ffffff'),
        fill = list(color='#ffffff'),
        align=c('left'),
        font=list(color='black', size=20),
        height=50
      ),
      cells = list(
        values = unname(popular_station[1:10,]),
        line=list(color='white'),
        fill=list(color='white'),
        align=c('left'),
        font=list(color=c('black'), size=16),
        height = 50
      )
    )
    p
  })
  
  output$db4_popular_route <- renderPlotly({
    popular_route <- bikeshare %>% group_by(start_station_name, end_station_name) %>% tally() %>% arrange(-n)
    p <- plot_ly(
      type='table',
      columnorder=c(1,2,3),
      columnwidth=c(200,200,80),
      header = list(
        values = c('<b>Top 10 Popular Route-Origin</b>','<b>Destination</b>','<b># of trip</b>'),
        line = list(color='#ffffff'),
        fill = list(color='#ffffff'),
        align=c('left'),
        font=list(color='black', size=20),
        height=50
      ),
      cells = list(
        values = unname(popular_route[1:10,]),
        line=list(color='white'),
        fill=list(color='white'),
        align=c('left'),
        font=list(color=c('black'), size=16),
        height = 50
      )
    )
    p
  })
  
  output$user_type_duration <- renderPlotly({
    duration <- bikeshare$duration_minutes
    duration <- cut(duration, breaks=c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,70,80,90,100,110,120,Inf), 
                    labels=c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60","60-70",
                             "70-80","80-90","90-100","100-110","110-120",">120"))
    bikeshare$duration_discrete <- duration
    if(input$year == "all"){
      new_data <- bikeshare %>% group_by(duration_discrete) %>% tally()
      plot_ly(new_data, x=~duration_discrete, y=~n, type="bar") %>% 
        layout(title="Rides by Duration Over Years from 2014 to 2016",
               xaxis=list(title="Duration"),
               yaxis=list(title="Number of Trip"))
    }else{
      num <- as.numeric(input$year)
      data <- bikeshare %>% group_by(year, duration_discrete) %>% tally()
      data <- data[data$year == num,]
      plot_ly(data, x=~duration_discrete, y=~n, type="bar") %>% 
        layout(title=paste("Rides by Duration Over a Year",num),
               xaxis=list(title="Duration"),
               yaxis=list(title="Number of Trip"))
    }
  })
  
  output$map_daily_analysis <- renderLeaflet({
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    
    basemap <- leaflet(width = "100%", height = "100%") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "dark")
    
    colors <- c("#3093e5", "#fcba50")
    data <- agg_station
    basemap %>% 
      addMinicharts(
        lng = data$longitude, 
        lat = data$latitude,
        chartdata = data[,c("start_n","end_n")],
        layerId = data$station_name,
        width = 60 * sqrt(data$start_n) / sqrt(data$start_n + data$end_n),
        type='pie',
        legend = FALSE,
        colorPalette = colors
      ) %>%
      addLegend(
        "topright",
        colors = colors, 
        opacity = 1,
        labels = c("Start Station", "Destination Station")
      )
    
  })
  
  output$mini_trip_day_text <- renderText({
    paste("Date, hour:",input$timerange)
  })
  
  output$mymap <- renderLeaflet({
    if(input$map_station == "origin"){
      m <- leaflet(agg_start_station) %>%
        addCircleMarkers(lat = ~start_station_latitude, lng = ~start_station_longitude, radius=~n/800,
                         label = start_map_label, stroke = TRUE, color = "#7374F7", fillOpacity = 0.7,
                         layerId = ~start_station_name) %>%
        addTiles() %>% addProviderTiles("Wikimedia")
      # addTiles(
      #   urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      # )
      selected <- start_station[start_station$start_station_name == input$mymap_marker_click$id,]
      selected$end_station_name <- as.character(selected$end_station_name)
      if(input$map_station_top == 100){
        k <- 12
      }else{
        k <- input$map_station_top
      }
      
      selected <- selected[1:k,]
      
      m %>% registerPlugin(antPlugin) %>%
        onRender("function(el, x, data) {
                 var myMap = this;
                 myMap.on('layeradd',
                 function(e){
                 var lyr = e.layer;
                 if('need_decorator' in lyr.options){
                 for (var i = 0; i < data.start_station_name.length; i++) {
                 polylinePoints = [
                 new L.LatLng(data.start_station_latitude[i], data.start_station_longitude[i]),
                 new L.LatLng(data.end_station_latitude[i], data.end_station_longitude[i]),
                 ];
                 
                 polylineOptions = {color: '#138207', 
                 weight:data.n[i]/300,  delay:400, dashArray:[30,30], opacity: 0.7,
                 hardwareAccelerated: true};
                 L.polyline.antPath(polylinePoints, polylineOptions).addTo(this);
                 }
                 }
                 }
                 )
                 
    }", data = selected)
    }else{
      m <- leaflet(agg_end_station) %>%
        addCircleMarkers(lat = ~end_station_latitude, lng = ~end_station_longitude, radius=~n/800,
                         label = end_map_label, stroke = TRUE, color = "#7374F7", fillOpacity = 0.9,
                         layerId = ~end_station_name) %>%
        addProviderTiles("Wikimedia")
      start_station$end_station_name <- as.character(start_station$end_station_name)
      selected <- start_station[start_station$end_station_name == input$mymap_marker_click$id,]
      if(input$map_station_top == 100){
        k <- 12
      }else{
        k <- input$map_station_top
      }
      
      selected <- selected[1:k,]
      
      m %>% registerPlugin_2(antPlugin) %>%
        onRender("function(el, x, data) {
                 var myMap1 = this;
                 myMap1.on('layeradd',
                 function(e){
                 var lyr1 = e.layer;
                 if('need_decorator' in lyr1.options){
                 for (var i = 0; i < data.start_station_name.length; i++) {
                 polylinePoints1 = [
                 new L.LatLng(data.end_station_latitude[i], data.end_station_longitude[i]),
                 new L.LatLng(data.start_station_latitude[i], data.start_station_longitude[i]),
                 ];
                 
                 polylineOptions1 = {color: '#f4426b', 
                 weight:data.n[i]/300,  delay:400, dashArray:[30,30], opacity: 0.7,
                 hardwareAccelerated: true, reverse: true};
                 L.polyline.antPath(polylinePoints1, polylineOptions1).addTo(this);
                 }
                 }
                 }
                 )
                 
  }", data = selected)
    }
})
  
  output$selected_station <- renderText({
    if(!is.null(input$mymap_marker_click)){
      if(input$map_station =="origin"){
        paste("<b>","Origin Station:",input$mymap_marker_click$id,"</b>")
      }else{
        paste("<b>","Destination Station:", input$mymap_marker_click$id,"</b>")
      }
    }
  })
  
  output$selected_station_2 <- renderText({
    if(!is.null(input$mymap_marker_click)){
      paste("Station:",input$mymap_marker_click$id)
    }else{
      "All Station"
    }
  })
  
  output$map_count_station <- renderPlotly({
    
    if(!is.null(input$mymap_marker_click)){
      if(input$map_station == "origin"){
        ax <- list(
          title = "Destination Station",
          zeroline = TRUE,
          showline = TRUE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        selected <- start_station[start_station$start_station_name == input$mymap_marker_click$id,]
        selected <- selected[1:input$map_station_top, c(4,7)]
        plot_ly(selected, x=~n, y=~reorder(end_station_name,n), type='bar', orientation='h') %>% 
          layout(xaxis=ax, yaxis=ax)
      }else{
        ax <- list(
          title = "Origin Station",
          zeroline = TRUE,
          showline = TRUE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        selected <- start_station[start_station$end_station_name == input$mymap_marker_click$id,]
        selected <- selected[1:input$map_station_top, c(1,7)]
        plot_ly(selected, x=~n, y=~reorder(start_station_name,n), type='bar', orientation='h') %>% 
          layout(xaxis=ax, yaxis=ax)
      }
      
    }
  })
  output$map_borrow_activity_hour <- renderPlotly({
    if(!is.null(input$mymap_marker_click)){
      bikeshare$start_station_name <- as.character(bikeshare$start_station_name)
      station_statistic <- bikeshare[bikeshare$start_station_name == input$mymap_marker_click$id,]
      station_statistic_hour <- station_statistic %>% group_by(hour) %>% tally()
      plot_ly(station_statistic_hour, x=~hour, y=~n, type='bar') %>% layout(xaxis=list(title="Hours"), yaxis=list(title="Number of trip"))
    }else{
      data <- bikeshare %>% group_by(hour) %>% tally()
      plot_ly(data, x=~hour, y=~n, type='bar') %>% layout(xaxis=list(title="Hours"), yaxis=list(title="Number of trip"))
    }
  })
  
  output$map_activity_week_day <- renderPlotly({
    if(!is.null(input$mymap_marker_click)){
      bikeshare$start_station_name <- as.character(bikeshare$start_station_name)
      station_statistic <- bikeshare[bikeshare$start_station_name == input$mymap_marker_click$id,]
      station_statistic_week <- station_statistic %>% group_by(day) %>% tally()
      plot_ly(station_statistic_week, x=~day, y=~n, type='bar') %>% layout(xaxis=list(title="Days"), yaxis=list(title="Number of trip"))
    }else{
      data <- bikeshare %>% group_by(day) %>% tally()
      plot_ly(data, x=~day, y=~n, type='bar') %>% layout(xaxis=list(title="Day"), yaxis=list(title="Number of trip"))
    }
    
  })
  
  
  # Update minicharts when the slider value change
  observeEvent(input$timerange ,{
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    if(hour(input$timerange) > 18 | hour(input$timerange) < 6){
      if(variables$currentDate == as.Date(input$timerange, format="%Y-%m-%d")){
        if(input$timerange %>% ymd_hms() %in% agg_station$day_hour){
          data <- agg_station[agg_station$day_hour == input$timerange %>% ymd_hms(),]
          station_update$day_hour <- input$timerange %>% ymd_hms()
          station_data_update <- station_update[!(station_update$station_name %in% data$station_name),]
          data <- rbind(data, station_data_update)
          data <- data %>% arrange(station_name)
          
          if(is.null(variables$data_update) == TRUE){
            variables$data_update <- data
            leafletProxy("map_daily_analysis") %>% clearGroup(group = "light") %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "dark") %>%
              updateMinicharts(
                chartdata = variables$data_update[, c("start_n","end_n")],
                layerId = variables$data_update$station_name,
                legend = FALSE
                
              )
            
          }else{
            variables$data_update$start_n <- variables$data_update$start_n + data$start_n
            variables$data_update$end_n <- variables$data_update$end_n + data$end_n
            leafletProxy("map_daily_analysis") %>% clearGroup(group = "light") %>%
              addProviderTiles(providers$CartoDB.DarkMatter, group = "dark") %>%
              updateMinicharts(
                chartdata = variables$data_update[, c("start_n","end_n")],
                layerId = variables$data_update$station_name,
                legend = FALSE
              )
            
          }
          
        }else{
          d <- station_update
          d$day_hour <- 0
          d$start_n <- 0
          d$end_n <- 0
          leafletProxy("map_daily_analysis") %>% clearGroup(group = "light") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "dark") %>%
            updateMinicharts(
              chartdata = d[,c("start_n","end_n")],
              layerId = d$station_name,
              legend = FALSE
            )
          
        }
      }else{
        variables$data_update = NULL
        variables$currentDate = as.Date(input$timerange, format="%Y-%m-%d")
        if(input$timerange %>% ymd_hms() %in% agg_station$day_hour){
          data <- agg_station[agg_station$day_hour == input$timerange %>% ymd_hms(),]
          station_update$day_hour <- input$timerange %>% ymd_hms()
          station_data_update <- station_update[!(station_update$station_name %in% data$station_name),]
          data <- rbind(data, station_data_update)
          variables$data_update <- data %>% arrange(station_name)
          leafletProxy("map_daily_analysis") %>%  clearGroup(group = "light") %>%
            addProviderTiles(providers$CartoDB.DarkMatter,group = "dark") %>%
            updateMinicharts(
              chartdata = variables$data_update[, c("start_n","end_n")],
              layerId = variables$data_update$station_name,
              legend = FALSE
            )
          
        }else{
          d <- station_update
          d$day_hour <- 0
          d$start_n <- 0
          d$end_n <- 0
          leafletProxy("map_daily_analysis") %>% clearGroup(group = "light") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "dark") %>%
            updateMinicharts(
              chartdata = d[,c("start_n","end_n")],
              layerId = d$station_name,
              legend = FALSE
            )
        }
      } 
      
    }else{
      # clearGroup("map_daily_analysis",map="dark")
      if(variables$currentDate == as.Date(input$timerange, format="%Y-%m-%d")){
        if(input$timerange %>% ymd_hms() %in% agg_station$day_hour){
          data <- agg_station[agg_station$day_hour == input$timerange %>% ymd_hms(),]
          station_update$day_hour <- input$timerange %>% ymd_hms()
          station_data_update <- station_update[!(station_update$station_name %in% data$station_name),]
          data <- rbind(data, station_data_update)
          data <- data %>% arrange(station_name)
          
          if(is.null(variables$data_update) == TRUE){
            variables$data_update <- data
            leafletProxy("map_daily_analysis") %>% clearGroup(group = "dark") %>%
              addProviderTiles(providers$CartoDB.Positron, group = "light") %>%
              updateMinicharts(
                chartdata = variables$data_update[, c("start_n","end_n")],
                layerId = variables$data_update$station_name,
                legend = FALSE
                
              )
            
          }else{
            variables$data_update$start_n <- variables$data_update$start_n + data$start_n
            variables$data_update$end_n <- variables$data_update$end_n + data$end_n
            leafletProxy("map_daily_analysis") %>% clearGroup(group = "dark") %>%
              addProviderTiles(providers$CartoDB.Positron, group = "light") %>%
              updateMinicharts(
                chartdata = variables$data_update[, c("start_n","end_n")],
                layerId = variables$data_update$station_name,
                legend = FALSE
              )
            
          }
          
        }else{
          d <- station_update
          d$day_hour <- 0
          d$start_n <- 0
          d$end_n <- 0
          leafletProxy("map_daily_analysis") %>% clearGroup(group = "dark") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "light") %>%
            updateMinicharts(
              chartdata = d[,c("start_n","end_n")],
              layerId = d$station_name,
              legend = FALSE
            )
          
        }
      }else{
        variables$data_update = NULL
        variables$currentDate = as.Date(input$timerange, format="%Y-%m-%d")
        if(input$timerange %>% ymd_hms() %in% agg_station$day_hour){
          data <- agg_station[agg_station$day_hour == input$timerange %>% ymd_hms(),]
          station_update$day_hour <- input$timerange %>% ymd_hms()
          station_data_update <- station_update[!(station_update$station_name %in% data$station_name),]
          data <- rbind(data, station_data_update)
          variables$data_update <- data %>% arrange(station_name)
          leafletProxy("map_daily_analysis") %>% clearGroup(group = "dark") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "light") %>%
            updateMinicharts(
              chartdata = variables$data_update[, c("start_n","end_n")],
              layerId = variables$data_update$station_name,
              legend = FALSE
              
            )
          
        }else{
          d <- station_update
          d$day_hour <- 0
          d$start_n <- 0
          d$end_n <- 0
          leafletProxy("map_daily_analysis") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "light") %>% clearGroup(group = "dark") %>%
            updateMinicharts(
              chartdata = d[,c("start_n","end_n")],
              layerId = d$station_name,
              legend = FALSE
            )
        }
      }
    }
  })
  
  observe({
    if(!is.null(input$mymap_marker_click)){
      if(input$map_station == "origin"){
        p <- leafletProxy("mymap", data=agg_start_station) %>% clearShapes()
        selected <- start_station[start_station$start_station_name == input$mymap_marker_click$id,]
        selected$end_station_name <- as.character(selected$end_station_name)
        if(input$map_station_top == 100){
          k <- 20
        }else{
          k <- input$map_station_top
        }
        selected <- selected[1:k,]
        for(i in 1:nrow(selected)){
          
          line_label = paste("",selected[i,1], "-",as.character(selected[i,4]),":",selected[i,7]) %>% lapply(htmltools::HTML)
          p <- addPolylines(p, lat=as.numeric(selected[i, c(2,5)]), lng=as.numeric(selected[i, c(3,6)]),
                            weight = as.numeric(selected[i,7]/60), opacity = 0.7,
                            label = line_label,
                            color="#138207",
                            options = list(need_decorator = T))
        }
        
        p
      }else{
        p <- leafletProxy("mymap", data=agg_end_station) %>% clearShapes()
        selected <- start_station[start_station$end_station_name == input$mymap_marker_click$id,]
        selected$start_station_name <- as.character(selected$start_station_name)
        selected$end_station_name <- as.character(selected$end_station_name)
        if(input$map_station_top == 100){
          k <- 20
        }else{
          k <- input$map_station_top
        }
        selected <- selected[1:k,]
        for(i in 1:nrow(selected)){
          line_label = paste("",as.character(selected[i,1]), "-",as.character(selected[i,4]),":",selected[i,7]) %>% lapply(htmltools::HTML)
          p <- addPolylines(p, lat=as.numeric(selected[i, c(2,5)]), lng=as.numeric(selected[i, c(3,6)]),
                            weight = as.numeric(selected[i,7]/60), opacity = 0.7,
                            label = line_label,
                            color="#f4426b",
                            options = list(need_decorator = T))
        }
        p
      }
    }else{
    }
  })
  
  }

shinyApp(ui = ui, server = server)



