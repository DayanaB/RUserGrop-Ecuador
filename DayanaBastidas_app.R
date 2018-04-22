#setwd("C:/Users/toshiba/Documents/DocumentosR")
#data <-read.csv("train.csv",na.strings = c(""," ",NA))
#str(data)
#View(data)
#save(data,file = "DataTrain.RData")
load(url("https://github.com/DayanaB/RUserGrop-Ecuador/raw/master/DataTrain.RData"))

library(shiny)
library(dplyr)
library(highcharter)
library(ggplot2)
library(DT)
ui <- fluidPage(
  titlePanel("PRESTAMOS"),
  tabsetPanel(type = "tabs",
              tabPanel("Principal", 
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("n",
                                       "Registros",min = 1,max = 30,value = 10),
                           selectInput("variable","Seleccione la variable:",
                                       choices = names(data)[-c(1)],selected = "Loan_Status"), 
                           tableOutput("tabla_resumen")
                         ),
                         mainPanel(
                           
                           tabsetPanel(type ="pills",
                                       tabPanel("Tabla",dataTableOutput("tabla"),
                                                downloadButton("descarga","Descargar")
                                       ),
                                       tabPanel("Grafico",
                                                plotOutput("grafico"), 
                                                
                                                plotOutput("grafico1"),
                                                
                                                plotOutput("grafico2"),
                                                
                                                plotOutput("grafico3")
                                            
                                                )
                                       )
                           )
                         )
                       )
              ),
              
              tabPanel("Hora",
                       h2(textOutput("currentTime"))
              )
  )

server <- function(input, output, session) {
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Hora", Sys.time())
  })
  
  output$tabla <- renderDataTable({
    datatable(head(data[,c("Gender", "Married", "Education","LoanAmount","Loan_Status",input$variable)],input$n),
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel','pdf','print')
              ))
  })
  
  ####################################
  
  output$tabla_resumen <- renderTable({
    table(data[, c(input$variable)])
    #group_by(data,input$variable) %>% summarize(Numero=n())
  })
  output$grafico <- renderPlot({
    ggplot(data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("ESTADO PRESTAMO POR ESTADO CIVIL")
  })

  output$grafico1 <- renderPlot({
  ggplot(data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("ESTADO PRESTAMO POR GENERO")
  })
  output$grafico2 <- renderPlot({
    ggplot(data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("ESTADO PRESTAMO POR EDUCACION")
  })

  output$grafico3 <- renderPlot({
  ggplot(data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("CONDICIONES PRESTAMO POR MESES")
  })

}
shinyApp(ui = ui, server = server)