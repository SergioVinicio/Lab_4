library(shiny)
library(dplyr)

server <- function(input, output) {
  
  getData <- reactive({
    
    #Cargar archivos
    inFile <- input$file1
    inFile2 <- input$file2
    inFile3 <- input$file3
    inFile4 <- input$file4
    
    
    if (is.null(input$file1)){
      return(NULL)}
    
    if (is.null(input$file2)){
      return(NULL)}
    
    if (is.null(input$file3)){
      return(NULL)}
    
    if (is.null(input$file4)){
      return(NULL)}
    
   #Leer archivoo y omitir levels de los factores

    dataA <- na.omit(read.csv(inFile$datapath, header=input$header, sep=input$sep,
                              stringsAsFactors = FALSE))
    
    dataB <- read.csv(inFile2$datapath, header=input$header, sep=input$sep,
                              stringsAsFactors = FALSE)
    
    dataC <- read.csv(inFile3$datapath, header=input$header, sep=input$sep,
                      stringsAsFactors = FALSE)
    
    data <- read.csv(inFile4$datapath, header=input$header, sep=input$sep,
                      stringsAsFactors = FALSE)

    
    #                         Codigo para archivos Type A
    
   #Quedarse con las columnas importantes
   dataA <- dataA %>% select(Applicant.Email, Opportunity.Create.Date.Time, Sum.Product.Presented)
   
   #Cambiar nombre de columnas
   colnames(dataA) <- c("email","date","disposition")
   
   #Sale if disposition > 0 else no sale
   dataA$disposition <- ifelse(dataA$disposition > 0,"Sale", "no sale")
   
   #Cambiar formato de la fecha
   dataA$date <- substr(dataA$date , 0, 10)
   
   #Nueva columna
   dataA$is_sale <- ifelse(dataA$disposition == "Sale",1,0)
   
   #Agregando columnas vacias
   dataA$lead_id <- NA
   dataA$phone <- NA
   
   #Reordenar columnas
   dataA <- dataA[,c(2,5,6,1,3,4)]
   
   
   
   
   #                         Codigo para archivos Type B
   
   #Seleccionar columnas importantes
   dataB <- dataB %>% select(CONTACT.CREATE.TIMESTAMP, DISPOSITION, Email.Address)
   
   #Cambiar nombre de columnas
   colnames(dataB) <- c("date","disposition","email")
   
   #Nueva columna
   dataB$is_sale <- ifelse(dataB$disposition == "Sale",1,0)
   
   #Cambiar formato fecha mm/dd/YYYY
   
   dataB$date <- substr(dataB$date,5,16)
   
   dataB$date <- gsub("[[:space:]]","",dataB$date)
   
   dataB$date <- as.Date(dataB$date , "%d%B%Y")
   
   dataB$date <- format(dataB$date, "%m/%d/%Y")
   
   #Agregando columnas vacias
   dataB$lead_id <- NA
   dataB$phone <- NA
   
   #Reordenar las columnas
   dataB <- dataB[,c(1,5,6,3,2,4)]
   
   
   #                         Codigo para archivos Type C
   #Seleccionar columnas importantes
   dataC <- dataC %>% select(TIMESTAMP, Email.Address, DISPOSITION)
   
   #Cambiar nombre de columnas
   colnames(dataC) <- c("date","email","disposition")
   
   #Cambiar formato fecha mm/dd/YYYY
   
   dataC$date <- substr(dataC$date,5,16)
   
   dataC$date <- gsub("[[:space:]]","",dataC$date)
   
   dataC$date <- as.Date(dataC$date , "%d%B%Y")
   
   dataC$date <- format(dataC$date, "%m/%d/%Y")
   
   #Nueva columna
   dataC$is_sale <- ifelse(dataC$disposition == "Sale",1,0)
   
   #Agregando columnas vacias
   dataC$lead_id <- NA
   dataC$phone <- NA
   
   
   
   
   
   #               Codigo para archivo de "Data" o Business rules
   
   #Eliminar filas innecesarias
   data <- data[-(1:40),]
   
   #Poner primera fila como header
   colnames(data) <-  as.character(unlist(data[1,]))
   data <- data[-1,]
   
   #Reiniciar indice
   row.names(data) <- NULL
   
   #Quedarse con las columnas importantes
   data <- data %>% select(`Date Created`, `Lead Primary Email`, Phone, `Lead Status`)
   
   #Renombrar columnas
   colnames(data) <- c("date","email","phone","disposition")
   
   #Nueva columna para ventas
   data$is_sale <- NA
   
   for(i in 1: length(data$date)){
     
     if(data$disposition[i] =="SALE"){ data$is_sale[i] <- 1}else{data$is_sale[i] <- 0}
     
   }
   
   #Formatear la forma de los numeros de telefono,  ###-###-####
   for(i in 1: length(data$date)){
     
     data[i,3] <-  gsub("[[:punct:]]", "", data[i,3]) 
     data[i,3] <-  gsub("[[:space:]]", "",data[i,3])
     data[i,3] <-  gsub("(^\\d{3})(\\d{3})(\\d{4}$)", "\\1-\\2-\\3",data[i,3])
     
   }
   
   #Formatear la fecha de las columnas,   mm/dd/yyyy
   data$date <- substr(data$date , 0, 10)
   
   #Agregar columnas vacias
   data$lead_id <- NA
   
   #Cambiar orden de las columnas
   data <- data[,c(1,6,3,2,4,5)]
   
   # Union de todos 
   
   dataA <- do.call("rbind", list(dataA, dataB, dataC, data))

  })
  
  
  output$contents <- renderTable(
    
    getData()
    
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      #Nombre del arhivo a bajar
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(getData(), file)
      
    })
  
}
