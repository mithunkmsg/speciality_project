library(shiny)
library(dygraphs)
library(xts)
library(dplyr)
library(tidyr)
library(reshape2)
speciality_wise<-read.csv("speciality_wise.csv")
city_wise<-read.csv("city_wise.csv")
speciality_wise[is.na(speciality_wise)]<-0
city_wise[is.na(city_wise)]<-0
df1<-melt(speciality_wise,id.vars = c("Date","Speciality","Category"))
df2<-melt(city_wise,id.vars = c("Date","City_Name","Category"))
df1$Date<-as.Date(df1$Date, format="%m/%d/%Y")
df2$Date<-as.Date(df2$Date, format="%m/%d/%Y")


shinyUI(
  pageWithSidebar(
    
    titlePanel(div(h4("TREND GRAPH FOR REQUEST, BOOKING, OPD AND IPD IN DIFFERENT MONTH", align = "center"), style = "color:red"),windowTitle = "Request Analysis"),

    sidebarPanel(
      h4(div("SPECIALITY_WISE PLOT",style="color:blue")),
      selectInput("Speciality","1. Please select a Speciality:",choices = levels(df1$Speciality),selected = "Renal Sciences"),
      radioButtons("Type","2. Please select a Type:",choices = levels(df1$Category),selected = "Domestic"),
      br(),
      br(),
      br(),
      br(),
      br(),
      h4(div("CITY_WISE PLOT",style="color:blue")),
      selectInput("City","1. Please select a City:",choices = levels(df2$City_Name),selected = "Mumbai"),
      radioButtons("Category","2. Please select a Type:",choices = levels(df2$Category),selected = "International")
    
      ),
    mainPanel(
       
       h4(dygraphOutput("dygraph",height = 200)),
       br(),
       br(),
       br(),
       br(),
       br(),
       h4(dygraphOutput("dygraph2",height = 200))    
       )
)
)
