# Uploading the required libraries 
setwd("J:/MA DABS/SEM 2/Advanced R/project/Make my own")
library(DT)
library(lubridate)
library(tidyverse)
library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(readr)
library(tibble)
library(shiny)
library(plotly)
library(shinydashboard)
library(tibble)
library(data.table)
library(timetk)

function(input, output, session){
  
  
  baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
  
  minutesSinceLastUpdate = function(fileName) {
    (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
  }
  
  
  loadData = function(fileName, columnName) {
    if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
      data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
        select(-Lat, -Long) %>% 
        pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
        mutate(
          date=as.Date(date, format="%m/%d/%y"),
          `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
          `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
        )
      save(data, file=fileName)  
    } else {
      load(file=fileName)
    }
    return(data)
  }
  
  allData = 
    loadData(
      "time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
    inner_join(loadData(
      "time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
    inner_join(loadData(
      "time_series_covid19_recovered_global.csv","CumRecovered"))
  
  #Unique Elements 
  Country = as.list(sort(unique(allData$`Country/Region`)))
  Latest_date = tail(unique(allData$date), n = 1 )
  all_the_days <- unique(allData$date)
  
  #For inforBoxes 
  total_covid_cases_0 <- allData %>% filter(`Province/State`== '<all>')   
  total_covid_cases_1 = total_covid_cases_0 %>% filter(`date`== Latest_date)
  total_covid_cases_2 = total_covid_cases_1  %>% distinct(`Country/Region`, .keep_all = TRUE)
  total_Confirmed = sum(total_covid_cases_2$`CumConfirmed`)
  total_Recovered = sum(total_covid_cases_2$CumRecovered)
  total_Deaths = sum(total_covid_cases_2$CumDeaths)
  
  #Creating New Set of Columns: DAILY CONFIRMED CASES, 
  
  data123 = as.data.frame(total_covid_cases_0 %>% group_by(`Country/Region`) %>% mutate(CumConfirmed, 'NewConfirmed' = CumConfirmed - lag(CumConfirmed,default = 0)))
  
  data1234 = as.data.frame(data123 %>% group_by(`Country/Region`) %>% mutate(CumDeaths, 'NewDeaths' = CumDeaths - lag(CumDeaths,default = 0)))
  
  Final_dataset = as.data.frame(data1234 %>% group_by(`Country/Region`) %>% mutate(CumRecovered, 'NewRecovered' = CumRecovered - lag(CumRecovered,default = 0)))
  transform(Final_dataset, NewConfirmed = as.integer(NewConfirmed), NewDeaths = as.integer(NewDeaths),NewRecovered = as.integer(NewRecovered))
  
  New_cases1 = Final_dataset %>% filter(`Province/State`=='<all>') 
  New_cases0 = New_cases1 %>% filter(`date`== Latest_date)
  New_cases = New_cases0 %>%  distinct(`Country/Region`, .keep_all = TRUE)
  
  New_cases_yesterday = sum(New_cases$NewConfirmed)
  New_Deaths_Yesterday = sum(New_cases$NewDeaths)
  New_Recovered_Yesterday = sum(New_cases$NewRecovered)
  
  
  
  #Plotting the cases: 
  Daily_confirmed_cases = aggregate(New_cases1["NewConfirmed"],by=New_cases1["date"],sum)
  Daily_Recovered_cases = aggregate(New_cases1["NewRecovered"],by=New_cases1["date"],sum)
  Daily_Death_cases = aggregate(New_cases1["NewDeaths"],by=New_cases1["date"],sum)
  

  ####################### Data Table  ####################### 
  
  #To print the data_table
  print_data <- total_covid_cases_0[-c(1)]

  ####################### Data Table  ####################### 
  
  
  
  
  ####################### Dashboard  ####################### 
  
  output$mytable <- renderDT(
    print_data %>%
      filter(`date`== Latest_date)
  )
  
  output$total_confirmed <- renderInfoBox({
    infoBox(
      "Total Confirmed Cases:",paste0(format(round(total_Confirmed/1e6),trim = TRUE),"M"),icon = icon("viruses"),color = "purple"
    )
    
  })
  output$total_recovered <- renderInfoBox({
    infoBox(
      "Total Recovered:",paste0(format(round(total_Recovered/1e6),trim = TRUE),"M"),icon = icon("file-medical"),color = "green"
    )
  })
  output$total_deaths <-renderInfoBox({
    infoBox(
      "Total Deaths:",paste0(format(round(total_Deaths/1e6),trim = TRUE),"M"),icon = icon("skull-crossbones"),color = "red"
    )
    
  })
  
  output$New_Confirmed <- renderInfoBox({
    infoBox(
      "Total New Cases:",paste0(New_cases_yesterday),icon = icon("cart-plus "),color = "maroon"
    )

    
  })
  
  output$New_Recovered <- renderInfoBox({
    infoBox(
      "Total New Recovered:",paste0(New_Recovered_Yesterday),icon = icon("smile "),color = "blue"
    )
    
    
  })
  
  output$New_Deaths <- renderInfoBox({
    infoBox(
      "Total New Deaths:",paste0(New_Deaths_Yesterday),icon = icon("sad-tear "),color = "black"
    )
    
    
  })
  
  # Plots 
  output$newConfirmed = renderPlot({
    ggplot(Daily_confirmed_cases, aes(x=date, y=NewConfirmed)) +
      geom_line( color="steelblue") + scale_x_date(date_breaks = "1 month",date_labels = "%b %y") +
      labs(title = "Daily Confirmed Cases", x = "Date", y = "No. of Cases")+
      geom_point() +
      xlab("") 
  })
  output$newRecovered = renderPlot({
    Daily_Recovered_cases %>% 
      ggplot(aes(x = date, y =NewRecovered))+ geom_line(color="steelblue") + scale_x_date(date_breaks = "1 month",date_labels = "%b %y") + coord_cartesian(ylim = c(0,1500000) )+
      labs(title = "Daily Recovered Patients ",x = "Month", y ="No. of Patients")
    
    
  })
  
  output$newDeaths = renderPlot({
    Daily_Death_cases %>% 
      ggplot(aes(x = date, y =NewDeaths))+ geom_line(color="steelblue") + scale_x_date(date_breaks = "1 month",date_labels = "%b %y") + coord_cartesian(ylim = c(0,20000) )+
      labs(title = "Daily Reported Deaths ",x = "Month", y ="No. of People")
    
    
  })
  

  
  ####################### Dashboard  ####################### 
  
  
  ####################### Mapping   ####################### 
  library(maps)
  # Renaming the region column: 
  
  allData1 = allData %>% filter(`date`== Latest_date)
  allData12 = allData1  %>% distinct(`Country/Region`, .keep_all = TRUE)
  names(allData12)[names(allData12) == "Country/Region"] <- "region"
  mapdata <- map_data("world") ##ggplot2
  
  
  #mapping the current dataset in to the world map dataset
  mapdata <- left_join(mapdata, allData12, by="region")
  
  # Get rid of null rows 
  mapdata1 = mapdata %>% filter(!is.na(mapdata$CumConfirmed))
  
  
  output$mapping = renderPlot({ 
    #colm = as.numeric(input$data_input)
    ggplot(mapdata1,aes(x = long,y = lat,group=group))+
      geom_polygon(aes(fill= CumConfirmed), color = "white") +
      scale_fill_viridis_c(option = "C")
    
    
  })
  
  

  
  
  ####################### Mapping  ####################### 
  
  observeEvent(input$country, {
    states = allData %>%
      filter(`Country/Region` == input$country) %>% 
      pull(`Province/State`)
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, 
                      selected=states[1])
  })
  
  countries = sort(unique(allData$`Country/Region`))
  
  



  
  
}