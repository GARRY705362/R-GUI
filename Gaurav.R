#Python file
import subprocess
a=input("Enter your name to proceed with analysis: ")
if (a=="gaurav"):
    #subprocess.call("D:\\scripting project\\library.R", shell=True)
    #subprocess.call("D:\\scripting project\\Net.R", shell=True)
    subprocess.call("D:\\scripting project\\Gaurav.R", shell=True)
elif(a=="naveen"):
    subprocess.call("D:\scripting project\\Naveen.R", shell=True)
elif (a=="seema"):
    subprocess.call("D:\scripting project\\Seema.R", shell=True)
elif(a=="goutham"):
    subprocess.call("D:\scripting project\\Goutham.R", shell=True)
else:
    print("Invalid User")

#R GUI file
#loading all the libraries
library(data.table)
library(dplyr)
library(fgui)
library(ggplot2)
library(gWidgets)
library(gWidgetsRGtk2)
library(lubridate)
library(plyr)
library(reshape)
library(reshape2)
library(RGtk2)
library(sqldf)
library(xlsx)
library(xlsxjars)
library(shiny)

#the user inter face of the code
ui<-fluidPage(
  #the title of the app
  titlePanel("Analysis"),
  fluidRow(
    
    column(3, wellPanel(
      #drop down with the analysis type
      selectInput("input_type", "Enter the type of analysis",
                  c("Year", "Month")
      )
    )),
    
    #action button which updates the value dynamically
    actionButton(inputId="OK",label="Click Me to update"),
    
    #creates space for stats related display
    verbatimTextOutput("stats"),
    plotOutput("hist"),
    
    
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    ))
  ))

#server function drives the logic of all the temp mentioned in UI function
server<-function(input, output) {
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "Month" = sliderInput("Common1", "Select a month",
                                 min = 1, max = 12, value = 1),
           "Year" = radioButtons("Common1", "Select the year:",
          c("2015","2014","2013","9999"))
    )
  })
  
  #waits for an event from the user to proceed further
  observeEvent(input$OK,{
    d<-as.numeric(input$Common1)
    cat(d,"\n")
    e<-d/2
    output$stats<-renderPrint(
      {
        train <- read.csv("D:/R project/Gaurav/train.csv")
        if(e>6&&e<4000)
        {
          #code for year wise analysis by breaking a year into 2 halves i.e. Jan-Jun and Jul-Dec
          cat("This is data for year no: ",d,"\n")
          trainf<-filter(train,year(train$Date)==d)
          traing<-filter(train,month(train$Date)<=6)
          print(head(traing))
          trainh<-filter(train,month(train$Date)>=7)
          print(head(trainh))
          b<-round(sqldf("Select avg(sales) from traing"))
          b<-as.numeric(b)
          cat("Average sales from Jan-Jun is:-",b,"\n")
          c<-round(sqldf("Select avg(Customers) from traing"))
          c<-as.numeric(c)
          cat("Average customers from Jan-Jun is:-",c,"\n")
          z<-round(sqldf("Select avg(sales) from trainh"))
          z<-as.numeric(z)
          cat("Average sales from Jan-Jun is:-",z,"\n")
          x<-round(sqldf("Select avg(Customers) from trainh"))
          x<-as.numeric(x)
          cat("Average Customers from Jan-Jun is:-",x,"\n")
          t<-as.data.frame(cbind(b,c,z,x))
          
          t <-rbind(c(t$b,t$c),c(t$z,t$x))
          row.names(t) <- c("Jan-June","July-December")
          colnames(t) <- c("sales","customers")
          output$hist=renderPlot({barplot(t,beside = T,col = c("blue","green"),main = paste("Plot showing half yearly sales of year-",d))})
        }
        else if(e<=6)
        {
          #month wise analysis for whole data set
          cat("This is data for month no: ",d,"\n")
          #adding two columns to our data set helps sort data year wise 
          train["year"] <- year(train$Date)
          train["month"] <- month(train$Date)
          trainf<-filter(train,month==d)
          print(head(trainf))
          #data grouped by year for sales
          data_year_sales <-tapply(trainf$Sales,trainf$year,mean)
          #data grouped by year for customers
          data_year_customers <- tapply(trainf$Customers,trainf$year,mean)
          #plot_data
          plot_data <- cbind(data_year_sales,data_year_customers)
          colnames(plot_data) <- c("Sales","Customers")
          output$hist=renderPlot({barplot(plot_data,main=paste("year wise Customers Sales for the month-",d),col = c("green","blue","pink"),beside = T,names.arg = c(row.names(plot_data),row.names(plot_data)))})
        }
        else{
          #this is analysis of whole data set
          b<-round(sqldf("Select avg(sales) from train"))
          b<-as.numeric(b)
          cat("Average sales for whole data set:-",b,"\n")
          c<-round(sqldf("Select avg(Customers) from train"))
          c<-as.numeric(c)
          cat("Average sales for whole data set:-",c,"\n")
          q <- rbind(b,c)
          colnames(q) <- "Sales and Customers"
          output$hist=renderPlot({barplot(q,beside=T,col=c("blue","green"),main="Average sales and customers of the years 2013,2014,2015")})
        }
        
      })
  })
}
#calls both the server and UI page
shinyApp(ui=ui,server=server)
