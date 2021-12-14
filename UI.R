# Uploading the required libraries 
setwd("C:\Users\sAVIO\Google Drive\Github Projects\Real-Time Covid Tracker")

library(lubridate)
library(tidyverse)
library(tidyr)
library(shiny)
library(dplyr)
library(rmarkdown)
library(readr)
library(DT)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dashboardthemes)

shinyUI(dashboardPage(skin = "black",
                       
                       dashboardHeader(title = "Real-Time Covid-19 Tracker",titleWidth = 350),
                       dashboardSidebar(
                         sidebarMenu(
                           menuItem(text = "Overview",tabName = "Maps",icon = icon("map-marked")),
                           menuItem(text = "Dashboard",tabName = "Dashboard",icon = icon("chart-line")),
                           menuItem(text = "Data",tabName = "Dataset",icon = icon("table"))
                         )
                       ),
                       dashboardBody(
                         
                         tabItems(
                           tabItem(tabName = "Maps",h2("Global Covid -19 Overview "),
                                   fluidPage(
                                     sidebarPanel(
                                       selectInput(inputId = "data_input",
                                                   label = "Case Type",
                                                   choices = list("Total Confirmed"="CumConfirmed","Total Recovered"="CumRecovered","Total Deaths"="CumDeaths")
                                       )
                                       
                                     ),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                   box(plotOutput(outputId = "mapping",width = "1000px",height = "750px")
                                       
                                       
                                       )
                                   )
                                   ),
                
                           tabItem(tabName = "Dataset",h3("Information based on :" ,Latest_date),
                                   DTOutput(outputId = "mytable")),
                           tabItem(tabName = "Dashboard",
                                   fluidRow(h4("Last Update:",Latest_date),
                                     infoBoxOutput("total_confirmed"),
                                     infoBoxOutput("total_recovered"),
                                     infoBoxOutput("total_deaths"),
                                     br(),
                                     infoBoxOutput("New_Confirmed"),
                                     infoBoxOutput("New_Recovered"),
                                     infoBoxOutput("New_Deaths")
                                   ),
                                   fluidRow(tabName="Plotting",
                                            box(width = 11,height = "400px",status = "warning",solidHeader = TRUE,plotOutput("newConfirmed")),
                                            br(),
                                            br(),
                                            box(width = 11,height = "400px",status = "success", solidHeader = TRUE,plotOutput("newRecovered")),
                                            br(),
                                            br(),
                                            box(width = 11,height = "400px",status = "success", solidHeader = TRUE,plotOutput("newDeaths"))
                                    
                                   )
                                   
                           )
                         )
                         
                         
                       )
                      
                      
)
)
                       

