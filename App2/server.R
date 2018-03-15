library(reshape2)
library(dplyr)
library(xts)
library(dygraphs)
speciality_wise<-read.csv("speciality_wise.csv")
city_wise<-read.csv("city_wise.csv")
speciality_wise[is.na(speciality_wise)]<-0
city_wise[is.na(city_wise)]<-0
df1<-melt(speciality_wise,id.vars = c("Date","Speciality","Category"))
df2<-melt(city_wise,id.vars = c("Date","City_Name","Category"))
df1$Date<-as.Date(df1$Date, format="%m/%d/%Y")
df2$Date<-as.Date(df2$Date, format="%m/%d/%Y")

request<-df1 %>% filter(variable=="Request")
booking<-df1 %>% filter(variable=="Booking")
opd<-df1 %>% filter(variable=="OPD")
ipd<-df1 %>% filter(variable=="IPD") 

request2<-df2 %>% filter(variable=="Request")
booking2<-df2 %>% filter(variable=="Booking")
opd2<-df2 %>% filter(variable=="OPD")
ipd2<-df2 %>% filter(variable=="IPD") 

shinyServer(
  function(input,output){
    selected1 <- reactive({request %>% 
        filter(Speciality==input$Speciality,Category == input$Type) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected2 <- reactive({booking %>% 
        filter(Speciality==input$Speciality,Category == input$Type) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected3 <- reactive({opd %>% 
        filter(Speciality==input$Speciality,Category == input$Type) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected4 <- reactive({ipd %>% 
        filter(Speciality==input$Speciality,Category == input$Type) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected5 <- reactive({request2 %>% 
        filter(City_Name==input$City,Category == input$Category) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected6 <- reactive({booking2 %>% 
        filter(City_Name==input$City,Category == input$Category) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected7 <- reactive({opd2 %>% 
        filter(City_Name==input$City,Category == input$Category) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    selected8 <- reactive({ipd2 %>% 
        filter(City_Name==input$City,Category == input$Category) %>% 
        group_by(Date) %>%
        summarise(n = value)})
    
    
    output$dygraph<-renderDygraph({
      spe_xts <- xts(cbind(selected1()$n,selected2()$n,selected3()$n,selected4()$n), order.by = as.Date(selected1()$Date))
      dygraph(spe_xts,xlab = "Month (Plot For Speciality_wise)",ylab = "Value")%>%
        dySeries("V1",label="Request",color="red", fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V2",label="Booking",color="green",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V3",label="OPD",color="purple",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V4",label="IPD",color="orange",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dyLegend(labelsDiv = "legendDivID",labelsSeparateLines = T)
        
             })
    
    output$dygraph2<-renderDygraph({
      spe_xts2 <- xts(cbind(selected5()$n,selected6()$n,selected7()$n,selected8()$n), order.by = as.Date(selected5()$Date))
      dygraph(spe_xts2,xlab = "Month (Plot For City_wise)",ylab = "Value")%>%
        dySeries("V1",label="Request",color="red", fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V2",label="Booking",color="green",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V3",label="OPD",color="purple",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dySeries("V4",label="IPD",color="orange",fillGraph = F, strokeWidth = 3, drawPoints = T,pointSize=3)%>%
        dyLegend(labelsDiv = "legendDivID",labelsSeparateLines = T)
        
      
    })
      
    
    
  }
)
    
    
    
  
