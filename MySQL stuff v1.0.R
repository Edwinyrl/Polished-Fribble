library(RMySQL)


#You can now read from the database I created remotely(based on a free server)

options(edwinyu = list(
  "host" = "db4free.net",
  "port" = 3306,
  "user" = "edwinyu",
  "password" = "Edwinyrl2019"
))


loadData <- function(database,table) {
  db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                  port = options()$edwinyu$port, user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  query <- sprintf("SELECT * FROM %s", table)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}




create <- function(name,date,day_of_the_week,available_period,notes,database,table) {
  db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                  port = options()$edwinyu$port, user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  com <- paste0("('", name , "', ",
                "'", date, "','",
                day_of_the_week,
                "','", available_period,"','",notes,"')")
  query=paste0("INSERT INTO ",table," VALUES ",paste(com, collapse = ", "),"ON DUPLICATE KEY UPDATE Name=VALUES(Name),
Date=VALUES(Date),
Day_of_the_week=VALUES(Day_of_the_week),
Available_period=VALUES(Available_period),
Notes=VALUES(Notes)")
  print(query)#for debug
  dbGetQuery(db, query)
  dbDisconnect(db)
}




delete <- function(name,date_1,date_2=NULL,database,table) {
  db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                  port = options()$edwinyu$port, user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  if (is.null(date_2)){
    query <- paste0('DELETE FROM ',table," WHERE Name = '", name,"' AND ",
                    "Date = '", date_1,"'")
  }else{
    query <- paste0('DELETE FROM ',table," WHERE Name = '", name,"' AND ",
                    "Date BETWEEN '", date_1,"' AND '",date_2,"'")
  }
  
  print(query)#for debug
  dbSendQuery(db, query)
  dbDisconnect(db)
}

#Automatically update the empty table of next month
create_next_month <- function(name,database,table){
  
  date_to_cha<-function(date){format(date, format="%Y-%m-%d")}
  
  weekday_to_cha<-function(date){format(date, format="%a")}
  
  room_data=loadData(database,table)
  
  name_vec=c()
  date_vec=c()
  day_vec=c()
  for(i in 0:30){
    if(    sum(room_data$Name==name & room_data$Date==date_to_cha(Sys.Date()+i) )==0 &  !(weekday_to_cha(Sys.Date()+i) %in% c('Sat','Sun'))     ){
      name_vec=append(name_vec,name)
      date_vec=append(date_vec,date_to_cha(Sys.Date()+i))
      day_vec=append(day_vec, weekday_to_cha(Sys.Date()+i))
      
    }
  }
  
  create(name=name_vec,date=date_vec,day_of_the_week = day_vec,available_period=NA,notes=NA,database=database,table=table)
  
  
}









#Demo

loadData('room_avail','room_table')

#Now all functions support operating multiple rows at a time
#create function now can be used to update exisitng rows and add new rows

#Add new row
create('Richard','2019-07-29','Mon','both','first let available period take value both',database='room_avail',table='room_table')
loadData('room_avail','room_table')

#Change existing row
create('Richard','2019-07-29','Mon','neither','then change it to neither with the same function',database='room_avail',table='room_table')
loadData('room_avail','room_table')
#The logic of deciding whether a row exists or not
#is checking if both Name and Date are the same as ones in the table


delete('Richard','2019-07-29',database='room_avail',table = 'room_table')
loadData('room_avail','room_table')

create_next_month(name='Richard',database = 'room_avail',table = 'room_table') #Take a few seconds
loadData('room_avail','room_table')

delete('Richard','2019-07-29','2019-08-28',database='room_avail',table = 'room_table')#Delete all rows between two dates
loadData('room_avail','room_table')

#Next steps
#1. I'll create tables&user accounts for individual users and admin(each account have limited access to tables)
#2. Find a decent server to save our database. The current one I used is a free-to-use one--- only for development but not production
# (limited storage & no guarantee for data security)
#3. ...



db <- dbConnect(MySQL(), dbname = 'room_availability', host = 'localhost', 
                port = 3306, user = 'root', 
                password = 'YRL2019')
query="CREATE USER 'Sean'@'localhost' IDENTIFIED BY 'sean2019'"
query="GRANT ALL ON user1_table TO 'Sean'@'localhost'"
query="ALTER USER 'Sean'@'localhost' IDENTIFIED WITH mysql_native_password BY 'sean2019'"
query="FLUSH PRIVILEGES"
dbSendQuery(db, query)
dbDisconnect(db)



dbListTables(db)
dbReadTable(db,'user1_table')









