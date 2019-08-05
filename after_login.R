# devtools::install_github("paulc91/shinyauthr")

library(shiny)
library(shinyauthr)
library(shinyjs)
# Querying dataset
library(tidyverse)
# Hashing Passwords with 'sodium'
library(sodium)
library(shinydashboard)
library(jsonlite)
# Define a json data format for data storage and querying
library(rjson)
library(RSQLite)
library(RJSONIO)
library(RSQLite)
library(DBI)

individual <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Internship/Polished-Fribble/afte_login/Individual_Information.csv",stringsAsFactors = FALSE)
individual_rs <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Internship/Polished-Fribble/afte_login/individual room status.csv",stringsAsFactors = FALSE)
timetable_data <- data.frame(ID=individual_rs$ID,Date=individual_rs$Date,Available_period=individual_rs$Available_period)
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Login",
                fluidPage(box(
                    # Login
                    ##################################################################################
                    shinyjs::useShinyjs(),
                    tags$head(tags$style(".table{margin: 0 auto;}"),
                              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                          type="text/javascript"),
                              includeScript("returnClick.js")
                    ),
                    shinyauthr::loginUI("login"),
                    
                    tabPanel("My Room Status", DT::dataTableOutput("user_table")),
                    # actionButton("change_schedule", "Click Me to Adjust Schedule")
                    HTML('<div data-iframe-height></div>')
                    ##################################################################################
                ))),
        tabItem(tabName = "my_room",
                # header
                fluidRow(box(width=3,height = 15,
                             h4("Import your room status"), 
                             selectInput("choice",label = "Choice",
                                         choices = c("Room Status Update" = "rsu","Room Booking" = "rb"),
                                         selected = "rsu"),
                             conditionalPanel(
                                 condition = "input.choice == 'rsu'",
                                 selectInput("day1",label = "Day",
                                             choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                         "Thursday" = "Thursday", "Friday" = "Friday"),selected = ""),
                                 checkboxGroupInput("time1","Time",choices = c("AM","PM"),selected = ""),
                                 textInput("notes1", label = "Notes", value = ""),
                                 actionButton("save1","Save",width = "20%")
                             ),
                             conditionalPanel(
                                 condition = "input.choice == 'rb'",
                                 selectInput("day2",label = "Day",
                                             choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                         "Thursday" = "Thursday", "Friday" = "Friday","All"="All"),selected = "All"),
                                 selectInput("room",label = "The room you want to check:",
                                             choices = c("1","2","all"),selected = "all"),
                                 checkboxGroupInput("time2","Time",choices = c("AM","PM"),selected = ""),
                                 actionButton("search","Search",width = "20%")
                                 
                             )),
                         box(width = 6,height = 10,
                             conditionalPanel(condition = "input.choice == 'rsu'",dataTableOutput(outputId = 'personal')),
                             conditionalPanel(condition = "input.choice == 'rb'",dataTableOutput(outputId = 'all'))
                         )))
    )# End of tabItems
)# End of body

#Side bar
sidebar<-dashboardSidebar(
    collapsed = TRUE,
    div(textOutput('Welcome'), style = 'padding: 20px'),
    sidebarMenu(width=70,
                ############## Add a new tab for login page #############################
                menuItem(text='Login', tabName='Login', icon=icon('users')),
                menuItem(text='Stats Department Room Booking System', tabName='my_room',icon=icon('table'))))  

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(tags$li(class = "dropdown", style = "padding: 8px;",
                                     shinyauthr::logoutUI("logout")),
                             tags$li(class = "dropdown", 
                                     tags$a(icon("github"), 
                                            href = "https://github.com/paulc91/shinyauthr",
                                            title = "See the code on github"))),
    sidebar = sidebar,
    body = body
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input,output,session){
    ############################# Database setting ############################
    update <- function(id,date,day_of_the_week,available_period,notes) {
        db <- dbConnect(SQLite(), 'room_store.sqlite')
        query <- paste0("UPDATE room_table SET ",
                        "Available_period = '", available_period , "', ",
                        "Notes = '", notes, "' ",
                        "WHERE ID = ", id," AND ",
                        "Date = '", date,"'")
        print(query)#for debug
        dbSendQuery(db, query)
        dbDisconnect(db)
    }
    
    create <- function(id,date,day_of_the_week,available_period,notes) {
        db <- dbConnect(SQLite(), 'room_store.sqlite')
        table=dbReadTable(db,"room_table")
        com <- paste0("(", id , ", ",
                      "'", date, "','",
                      day_of_the_week,
                      "','", available_period,"','",notes,"')")
        query=paste0("INSERT INTO room_table VALUES ",paste(com, collapse = ", "))
        print(query)#for debug
        dbGetQuery(db, query)
        dbDisconnect(db)
    }
    
    #Load data currently saved in sqlite
    loadData <- function() {
        db <- dbConnect(SQLite(), "room_store.sqlite")
        query <- sprintf("SELECT * FROM %s", 'room_table')
        data <- dbGetQuery(db, query)
        dbDisconnect(db)
        data
    }
    
    #Deletion
    delete <- function(id,date,day_of_the_week) {
        db <- dbConnect(SQLite(), "room_store.sqlite")
        query <- paste0('DELETE FROM room_table WHERE ID = ', id," AND ",
                        "Date = '", date,"'")
        print(query)#for debug
        dbSendQuery(db, query)
        dbDisconnect(db)
    }
    
    ## connect to the database
    db <- dbConnect(SQLite(), 'room_store.sqlite')
    dbWriteTable(db, "room_table", individual_rs, overwrite = TRUE)
    dbReadTable(db,'room_table')
    
#####################################################################################
    # call the logout module with reactive trigger to hide/show
    logout_init <- callModule(shinyauthr::logout, 
                              id = "logout", 
                              active = reactive(credentials()$user_auth))
    
    # call login module supplying data frame, user and password cols
    # and reactive trigger
    login_info <- data.frame(ID=individual$UserName,Password=individual$Password)
    
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = login_info,
                              user_col = ID,
                              pwd_col = Password,
                              sodium_hashed = FALSE,
                              log_out = reactive(logout_init()))
    
####################################################################################
    # pulls out the user information returned from login module
    user_data <- reactive({credentials()$info})
    time <- Sys.time()

    output$user_table <- renderTable({
        # use req to only render results when credentials()$user_auth is TRUE
        req(credentials()$user_auth)
        # print(credentials$user_auth)
        # print(user_data()$ID)
        timetable_data[individual_rs$ID == user_data()$ID,]
    })
    
    ## Save Updated Room information
    observeEvent(input$save1, 
                 {   
                     current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = F)
                     a <- data.frame(number=1:5,day = c("Monday","Tuesday","Wednesday","Thursday", "Friday"))
                     if(current=="Monday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-1)+7}
                     else if(current=="Tuesday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-2)+7}
                     else if(current=="Wednesday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-3)+7}
                     else if(current=="Thursday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-4)+7}
                     else if(current=="Friday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-5)+7}
                     
                     available_time1 <- ""
                     for(j in 1:length(input$time1)){
                         available_time1 <- paste(available_time1,input$time1[j],sep = "\n")
                     }
                     
                     #Update or Create?
                     if(date_update%in%individual_rs[individual_rs$ID==individual$RoomNumber[individual$UserName==user_data()$ID],]$Date){
                         update(individual$RoomNumber[user_data()$ID==individual$UserName],
                                date_update,weekdays(as.POSIXct(date_update), abbreviate = F),
                                available_time1,input$notes1)
                     }else{
                         delete(individual$RoomNumber[user_data()$ID==individual$UserName],
                                date_update)
                         create(individual$RoomNumber[user_data()$ID==individual$UserName],
                                date_update,weekdays(as.POSIXct(date_update), abbreviate = F),
                                available_time1,input$notes1)
                     }
                         
                     output$personal <- renderDataTable({
                         updated_room=loadData()
                         updated_room = updated_room[updated_room$ID==individual$RoomNumber[individual$UserName==user_data()$ID],]
                         updated_room
                     })
                 })
    
    observeEvent(input$search,
                 {   
                     available_time2 <- ""
                     for(j in 1:length(input$time2)){
                         available_time2 <- paste(available_time2,input$time2[j],sep = "\n")
                     }
                     ##day2
                     ##room
                     ##time2
                     
                     output$all <- renderDataTable({
                         all_room <- loadData()
                         if(input$day2=="All"){
                             if(input$room=="All"){ 
                                 all_room1 <- all_room}else{
                                     all_room1 <- all_room[all_room$ID==input$room,]
                                 }
                             all_room2 <- all_room1[all_room1$Available_period==available_time2,]
                             return(all_room2)
                         }
                         else{
                             if(input$room=="All"){ 
                                 all_room1 <- all_room}else{
                                     all_room1 <- all_room[all_room$ID==input$room,]
                                 }
                             all_room2 <- all_room1[all_room1$Available_period==available_time2,]
                             all_room3 <- all_room2[all_room2$Day==input$day2,]
                             return(all_room3)
                         }
                         
                     })
                 })
    
}) 

# Run the application 
shinyApp(ui = ui, server = server)


# 1. updates on current week data
# 2. historical data records
# 3. overview of other people's data
# 4. Realize the above by db

    

        
    
    


