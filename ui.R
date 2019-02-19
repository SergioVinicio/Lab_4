library(shiny)
library(dplyr)


ui <- fluidPage(
  fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        
        fileInput('file1', 'Type A',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
     
        fileInput('file2', 'Type B',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      
        
        fileInput('file3', 'Type C',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        
        fileInput('file4', 'Business Rules',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
       
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        tableOutput('contents')
      )
    )
  )
)
