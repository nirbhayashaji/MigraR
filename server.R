# Define server logic for MigraR app

library(shinydashboard)
library(dplyr)
library(stats)
library(openxlsx)
library(ggplot2)
library(shinyjs)
library(tools)
library(shinyWidgets)


data_4 <<- NULL
up_data <<- reactive({NULL})
temp <<- NULL
velocityDataforDownload <- NULL
straigtnessDataforDownload <<- NULL
warningTextIndicator <<- 0

server <- function(input, output, session) {
  
  my_data <<- reactive({
    data.frame()
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    else {
      fileExtension <- file_ext(inFile)
      if(fileExtension == "xlsx"){
        dataRaw <- read.xlsx(inFile$datapath, cols = c(1:5))
      }
      else if(fileExtension == "csv"){
        checkDecimalSeperator <- read.csv(inFile$datapath, header = FALSE)
        if(ncol(checkDecimalSeperator) !=5){
          print("well here atleast")
          dataRaw <- 222
        } else {
          dataRaw <- read.csv(inFile$datapath, header=TRUE, sep=",", blank.lines.skip = FALSE)
        }
      }
      else if(fileExtension == "txt"){
        dataRaw <- read.table(inFile$datapath, header=T)
        dataRaw[] <- lapply(dataRaw, function(x) as.numeric(gsub(",", ".", x)))
      }

      }

    if(dataRaw == 222 ){
      data = 222
    }
    else if(any(is.na(dataRaw))){
      print(is.na(dataRaw))
      data <- 333
    } else if(!identical(colnames(dataRaw) , c("Position.X","Position.Y","Position.Z","Time","TrackID"))){
      print(colnames(dataRaw))
      data <- 555
    } else if(dataRaw$TrackID == 0){
      data <- 444
    } else {
      data <- dataProcess(dataRaw)
    }
      
    

  })

  observeEvent(input$file,{

    if(my_data()==555){
      output$warningText <- renderText({ return("Input data not Valid") })
      shinyalert("Oops!", "The colomn names has to be 'Position.X','Position.Y', 'Position.Z', 'Time' , 'TrackID'. Please correct ( or insert correct column names) and try again.", type = "error")
    } else if(my_data()==444){
      output$warningText <- renderText({ return("Input data not Valid") })
      shinyalert("Oops!", "Track id has to be a non zero numeric value. Please change all your track ids to non zero and re upload your data!", type = "error")
    } else if(my_data()==333){
      output$warningText <- renderText({ return("Input data not Valid") })
      shinyalert("Oops!", "Tab seperator has to be ','. Please change and try again", type = "error")
    } else if(my_data()==222){
      output$warningText <- renderText({ return("Input data not Valid") })
      shinyalert("Oops!", "The decimal separater has to be '.' not ','. Please change the input data and try again.", type = "error")
    }
    else {
      output$warningText <- renderText({
        return("")
      })
    }
  })
  


  # Filter data based on selections
  output$table <- DT::renderDataTable(
    if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){
    DT::datatable(
    up_data(), options = list(searching = FALSE,scrollX=TRUE )
  )
    }
  )


  observeEvent(input$tabset1,{
    if(input$tabset1 == "Velocity" || input$tabset1 == "Straightness" || input$tabset1 == "Direction" || input$tabset1 == "DataTable"){
      hide("eixox")
      hide("eixoy")
      hide("radio")
    }
    else {
      show("eixox")
      show("eixoy")
      show("radio")
    }

  })

  observeEvent(input$tabset1,{
    if(input$tabset1 == "Direction"){
      show("radioCor")
    }
    else {
      hide("radioCor")
    }

  })


  observeEvent(input$radioCor,{ if(input$radioCor == 4){
    hide("Box1")
    hide("Box2")
    show("bsSelecter")
    }
    else {
      show("Box1")
      show("Box2")
      hide("bsSelecter")
    }
  })
  
  observeEvent(input$slider,{

    if(!is.null(my_data()))
    {
      if ( input$trans != "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2], my_data()$TrackID==input$trans)
        up_data <<- reactive({temp})
        
      }
      if ( input$trans == "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2])
        up_data <<- reactive({temp})
        
      }

    }
    })
  
  observeEvent(input$sliderSpeed,{

    if(!is.null(my_data()))
    { 
      if ( input$trans != "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2], my_data()$TrackID==input$trans)
        up_data <<- reactive({temp})
        
      }
      if ( input$trans == "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2])
        up_data <<- reactive({temp})
        
      }

    }
  })
  
  
  observeEvent(input$sliderStraight,{
   
    if(!is.null(my_data()))
    { 
      if ( input$trans != "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2], my_data()$TrackID==input$trans)
        up_data <<- reactive({temp})
        
      }
      if ( input$trans == "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2])
        up_data <<- reactive({temp})
        
      }

    }
  })
  
  observeEvent(input$trans,{

    if(!is.null(my_data()))
    {
      if ( input$trans != "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2], my_data()$TrackID==input$trans)
        up_data <<- reactive({temp})

      }
      if ( input$trans == "All") {
        temp <- filter(my_data(), my_data()$Time>= input$slider[1], my_data()$Time<= input$slider[2], my_data()$Speed>= input$sliderSpeed[1], my_data()$Speed<= input$sliderSpeed[2], my_data()$Straightness>= input$sliderStraight[1], my_data()$Straightness<= input$sliderStraight[2])
        up_data <<- reactive({temp})

      }
    }
  })
  
  
  observeEvent(input$bsSelecter,{
    
  })

  
  observeEvent(input$reset,{
    updateSliderInput(session,'slider',value = c(min,max))
    updateSliderInput(session,'sliderSpeed',value = c(min,max))
    updateSliderInput(session,'sliderStraight',value = c(min,max))
    updateSliderInput(session,'eixox',value = c(-500, 100))
    updateSliderInput(session,'eixoy',value = c(-300, 100))
    updateSelectInput(session,'trans',selected = "All")
  })
  
 output$trans <- renderUI({
   options(scipen=999) 
     selectInput("trans",
               "TrackID:",
               c("All",
                 unique(as.character(my_data()[,8]))
                 ))
     
 })
 
 
 
 output$slider <- renderUI({
   if(is.null(my_data()))
     sliderInput("slider",
                 "",
                 min   = 0,
                 max   = 0,
                 value = c(0, 0))
   else {
     maxkaw <- max(my_data()$Time)
     minkaw <-  min(my_data()$Time)
     sliderInput(
       "slider",
       "Time Interval Min-Max",
       min   = as.numeric(minkaw),
       max   = as.numeric(maxkaw),
       value = c(as.numeric(minkaw), as.numeric(maxkaw))
     )
   }
 })
 
 output$sliderSpeed <- renderUI({
   if(is.null(my_data()))
     sliderInput("sliderSpeed",
                 "",
                 min   = 0,
                 max   = 0,
                 value = c(0, 0))
   else {
     maxkaw <- max(round(as.numeric(as.character(my_data()$Speed)),0))
     minkaw <-  min(round(as.numeric(as.character(my_data()$Speed)),0))
     sliderInput(
       "sliderSpeed",
       "Speed Interval Min-Max",
       min   = as.numeric(minkaw),
       max   = as.numeric(maxkaw),
       value = c(as.numeric(minkaw), as.numeric(maxkaw))
     )
   }
 })
 
 output$sliderStraight <- renderUI({
   if(is.null(my_data()))
     sliderInput("sliderStraight",
                 "",
                 min   = 0,
                 max   = 0,
                 value = c(0, 0))
   else{
     maxkaw <- max(round(as.numeric(as.character(my_data()$Straightness)),1))
     minkaw <-  min(round(as.numeric(as.character(my_data()$Straightness)),1))
     sliderInput(
       "sliderStraight",
       "Straigtness Interval Min-Max",
       min   = as.numeric(minkaw),
       max   = as.numeric(maxkaw),
       value = c(as.numeric(minkaw), as.numeric(maxkaw))
     )
   }
 })
 

 plot1 <- function(){
   
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){
     if (input$radio == 1) {
       if (is.null(my_data()))
         return(NULL)
       if (input$trans != "All") 
         print(ggplot(up_data(), aes(filter(up_data(), up_data()$TrackID==input$trans)$Position.X, filter(up_data(), up_data()$TrackID==input$trans)$Position.Y)) + 
                 geom_line() + 
                 geom_point() + 
                 xlab("Distance (X)") +
                 ylab("Distance (Y)") + 
                 theme(legend.position = "none"))
         # plot(filter(up_data(), up_data()$TrackID==input$trans)$Position.X, filter(up_data(), up_data()$TrackID==input$trans)$Position.Y, xlab="Distance (X)", ylab="Distance (Y)", type = "l")
       else{
         # View(up_data())
         # plot(up_data()$Position.X, up_data()$Position.Y, xlab="Distance (X)", ylab="Distance (Y)", type = "o")
         print(ggplot(up_data(), aes(Position.X, Position.Y, group = TrackID)) + 
                 geom_line() + 
                 geom_point() + 
                 xlab("Distance (X)") +
                 ylab("Distance (Y)") + 
                 theme(legend.position = "none"))
       }

     }
     if (input$radio == 2) {
       if (is.null(my_data()))
         return(NULL)
       if (input$trans != "All") 
         print(ggplot(up_data(), aes(filter(up_data(), up_data()$TrackID==input$trans)$Position.X, filter(up_data(), up_data()$TrackID==input$trans)$Position.Y,colour=factor(TrackID))) + 
                 geom_line() + 
                 geom_point() + 
                 xlab("Distance (X)") +
                 ylab("Distance (Y)") + 
                 theme(legend.position = "none"))
       
         # plot(filter(up_data(), up_data()$TrackID==input$trans)$Position.X, filter(up_data(), up_data()$TrackID==input$trans)$Position.Y, col=filter(up_data(), up_data()$TrackID==input$trans)$TrackID, xlab="Distance (X)", ylab="Distance (Y)",  type = "l")
       else
         # plot(up_data()$Position.X, up_data()$Position.Y, col=up_data()$TrackID, xlab="Distance (X)", ylab="Distance (Y)", type = "o")
       print(ggplot(up_data(), aes(Position.X, Position.Y,colour=factor(TrackID), group = TrackID)) + 
               geom_line() + 
               geom_point() + 
               xlab("Distance (X)") +
               ylab("Distance (Y)") + 
               theme(legend.position = "none"))
       
     }
     if (input$radio == 3) {
       if (is.null(my_data()))
         return(NULL)
       else {
         x <- character(0)
         y <- character(0)
         
         count = 1
         Track_ID_f = 100
         for (i in 1:length(up_data()[, 1]))
         {
           Track_ID = up_data()[i, 8]
           
           if (Track_ID_f != Track_ID)
           {
             j = i
           }
           
           
           x[count] = as.numeric(up_data()[i, 1]) - as.numeric(up_data()[j, 1])
           count = count + 1
           
           Track_ID_f = Track_ID
         }
         
         count = 1
         Track_ID_f = 0
         for (i in 1:length(up_data()[, 1]))
         {
           Track_ID = up_data()[i, 8]
           
           if (Track_ID_f != Track_ID)
           {
             j = i
             
           }
           
           y[count] = as.numeric(up_data()[i, 2]) - as.numeric(up_data()[j, 2])
           count = count + 1
           Track_ID_f = Track_ID
           
         }
         x = as.numeric(x)
         y = as.numeric(y)
         roseDF = data.frame(x,y,up_data()$TrackID)
         print(str(roseDF))
         print(colnames(roseDF))

         print(ggplot(roseDF, aes(x, y,colour=factor(up_data...TrackID), group = up_data...TrackID)) +
                 geom_line() +
                 geom_point() +
                 scale_x_discrete(limits=c(min(x),max(x),range(x)/2)) +
                 scale_y_discrete(limits=c(min(y),max(y),range(y)/2)) + 
                 xlab("Distance (X)") +
                 ylab("Distance (Y)") +
                 theme(legend.position = "none")) + 
           geom_vline(xintercept = 0, colour = "red") +
           geom_hline(yintercept = 0, colour = "red")
         
         
         # plot(
         #   x,
         #   y,
         #   col = up_data()$TrackID,
         #   main = input$title,
         #   xlab = "Distance (um)",
         #   ylab = "Distance (um)",
         #   xlim = c(min(as.numeric(x)),max(as.numeric(x))),
         #   ylim = c(min(as.numeric(y)),max(as.numeric(y))),
         #   type = "o"
         # )
         # abline(v = 0)
         # abline(h = 0)

         
       }
       
     }
   }
   
 }
 
 output$downloadPlot1 <- downloadHandler(
   filename =  function() {
     paste("positionPlot", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     plot1()
     dev.off()  # turn the device off
     
   })
 
 output$plot1 <- renderPlot({
   plot1()
 })
 

 
 

 
 plot2 <- function(){
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){
    
   velocidade_media<- character(0)
   count=1
   Track_ID_f=0
   velocidade=0
   n=1
   f=1
   for (i in 1:length(up_data()[,10]))
   {
     Track_ID=up_data()[i,8]
     
     if(Track_ID_f != Track_ID && f != 1 )
     {
       n=1
       velocidade_media[count]=mean(as.numeric(velocidade))
       count=count+1
       velocidade=0
     }
     if(Track_ID_f != Track_ID)
     {
       velocidade[n]=up_data()[i,10]
       n=n+1
     }
     if(i== length(up_data()$Speed) )
     {       
       velocidade[n]=up_data()[i,10]
       velocidade_media[count]=mean(as.numeric(velocidade))
       count=count+1
       velocidade=0
     }
     
     if(Track_ID_f == Track_ID)
     {
       velocidade[n]=up_data()[i,10]
       n=n+1
     }
     
     f=f+1
     
     Track_ID_f=Track_ID
   }
   
   boxplot(as.numeric(velocidade_media), ylab="Speed (um/min)")
   legend("right", 
          legend = round(var(as.numeric(velocidade_media)),2), 
          title  = c("Variance"), horiz=TRUE)
   legend("left", 
          legend = round(mean(as.numeric(velocidade_media)),2), 
          title  = c("Mean"), horiz=TRUE)
   velocityDataforDownload <<- velocidade_media

   }
 }
 


 output$plot2 <- renderPlot({
   if(!is.null(my_data()))
   plot2()
 })
 
 output$downloadPlot2 <- downloadHandler(
   filename =  function() {
     paste("velocityPlot", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     plot2()
     dev.off()  # turn the device off
     
   })
 
 output$velocityDataDownload <- downloadHandler(
   filename = function() {
     paste("velocityData", ".csv", sep = "")
   },
   content = function(file) {
     write.csv(velocityDataforDownload, file, row.names = FALSE)
   }
 )
 
 
 plot3 <- function(){
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){
     
     boxplot(unique(up_data()$Straightness), ylab="Straightness")
     mtext("*Cells that move with a straightness = 1 were removed.",col = "red", font = 3, side=1,line=0, adj = 1)
     legend("right", 
            legend = round(var(as.numeric(unique(up_data()$Straightness))),2), 
            title  = c("Variance"), horiz=TRUE)
     legend("left", 
            legend = round(mean(as.numeric(unique(up_data()$Straightness))),2), 
            title  = c("Mean"), horiz=TRUE)
     straigtnessDataforDownload <<- unique(up_data()$Straightness)
   }
 }
 


 output$plot3 <- renderPlot({
   if(!is.null(my_data()))
 plot3()
 })
 
 output$downloadPlot3 <- downloadHandler(
   filename =  function() {
     paste("straigtnessPlot", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     plot3()
     dev.off()  # turn the device off
     
   })
 
 output$straigtnessDataDownload <- downloadHandler(
   filename = function() {
     paste("straigtnessData", ".csv", sep = "")
   },
   content = function(file) {
     write.csv(straigtnessDataforDownload, file, row.names = FALSE)
   }
 )
 
 

 
 cPlot1 <- function(){
   
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){ # | input$spaceTraveled| input$numPoints){

     if (input$radioCor == 2) {

         dados <- data.frame(Posicao_x=as.numeric(as.character(up_data()[,12])), Posicao_y=as.numeric(as.character(up_data()[,13])), Track_ID= as.numeric(as.character(up_data()[,8])), cos_angulo=round(as.numeric(as.character(up_data()[,15])),1))

         d<- ggplot(dados, aes(x=Posicao_x, y=Posicao_y, group=Track_ID , color=cos_angulo)) + 
           geom_path()  + labs(x = "Distance (um)", y = "Distance (um)", colour = "Direction (cos)")  + 
           ggtitle('Rose Plot')   + 
           theme(legend.position="bottom", legend.direction="horizontal") + 
           scale_x_continuous(limits=c(input$eixox1[1], input$eixox1[2])) + 
           scale_y_continuous(limits=c(input$eixoy1[1], input$eixoy1[2])) +
           scale_colour_gradient2(low = "red",mid="yellow", high = "blue")
         
         directionalityDataforDownload <<- up_data()[,14]
         print(d)
         
      
     }
     if (input$radioCor == 4) {
       
       dados <- data.frame(Posicao_x=up_data()[,12], Posicao_y=up_data()[,13], cos_angulo=up_data()[,15], angulo=as.numeric(as.character(up_data()[,14])), pers=up_data()[,11])
       # View(dados)
       bw = input$bsSelecter
       # make the bw ueser input from a slider
       # make x labels at the middle of the bars
       
       d <- ggplot (dados , aes (angulo, fill = pers)) + geom_histogram (binwidth = bw, fill="black", col="grey") + labs(x="Angle",
                                                                                 y="Number of cells",
                                                                                 title="Angle Frequency", caption = "*Please be aware that angle histograms may lead to biased conclusions, especially for datasets with few cell tracks.") + theme(plot.caption = element_text(color = "red", face = "italic")) +
         scale_x_continuous(
           breaks= seq(floor(min(dados$angulo)) - bw,ceiling(max(dados$angulo)) + bw,bw), 
           labels = seq(floor(min(dados$angulo)) - bw,ceiling(max(dados$angulo)) + bw,bw) 
         )
       directionalityDataforDownload <<- up_data()[,14]
       print(d)
     }
   }
 }
 
 output$plotCor1 <- renderPlot({
   cPlot1()
 })
 
 
 
 output$downloadCorPlot1 <- downloadHandler(
   filename =  function() {
     paste("correlationPlot1", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     cPlot1()
     dev.off()  # turn the device off
     
   })
 

 
 cPlot2 <- function(){
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){ 

     if (input$radioCor == 2) {
       
       
       dados <- data.frame(Posicao_x=as.numeric(as.character(up_data()[,1])), Posicao_y=as.numeric(as.character(up_data()[,2])), Track_ID= as.numeric(as.character(up_data()$TrackID)), cos_angulo=round(as.numeric(as.character(up_data()[,15])),1))
       d <- ggplot(dados, aes(x=Posicao_x, y=Posicao_y, group=Track_ID , color=cos_angulo)) + labs(x = "Distance (um)", y = "Distance (um)", colour = "Direction (cos)") +
         scale_colour_gradient2(low = "red",mid="yellow", high = "blue") + ggtitle('Normal View') +  theme(legend.position="bottom", legend.direction="horizontal") + geom_line()
         directionalityDataforDownload <<- up_data()[,14]
       print(d)
       
     }

     
     if (input$radioCor == 4) {
       dados <- data.frame(Posicao_x=up_data()[,12], Posicao_y=up_data()[,13], cos_angulo=up_data()[,15], angulo=as.numeric(as.character(up_data()[,14])), pers=up_data()[,11])
       d <- ggplot (dados , aes (angulo)) + geom_histogram (binwidth = 4) + labs(x="Angle", 
                                                                                 y="Number of cells", 
                                                                                 title="Rose Plot")
       d <- d + coord_polar(theta="x" , direction = -1)         
       directionalityDataforDownload <<- up_data()[,14]
       print(d)
     }
   }
 }
 
 output$plotCor2 <- renderPlot({
   cPlot2()
 })
 
 
 output$downloadCorPlot2 <- downloadHandler(
   filename =  function() {
     paste("correlationPlot2", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     cPlot2()
     dev.off()  # turn the device off
     
   })
 
 #NEW
 cPlot3 <- function(){
   
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){ # | input$spaceTraveled| input$numPoints){
     if (input$radioCor == 2) {
       
       dados <- data.frame(Posicao_x=as.numeric(as.character(up_data()[,12])), Posicao_y=as.numeric(as.character(up_data()[,13])), Track_ID= as.numeric(as.character(up_data()[,8])), sin_angulo=sin(round(as.numeric(as.character(up_data()[,14])),3)))
       d<- ggplot(dados, aes(x=Posicao_x, y=Posicao_y, group=Track_ID , color=sin_angulo)) + 
         geom_path()  + labs(x = "Distance (um)", y = "Distance (um)", colour = "Direction (sin)")  + 
         ggtitle('Rose Plot')   + 
         theme(legend.position="bottom", legend.direction="horizontal") + 
         scale_x_continuous(limits=c(input$eixox1[1], input$eixox1[2])) + 
         scale_y_continuous(limits=c(input$eixoy1[1], input$eixoy1[2])) +
         scale_colour_gradient2(low = "pink",mid="green", high = "black")
       
       directionalityDataforDownload <<- up_data()[,14]
       print(d)
       
       
     }
   }
 }
 
 output$plotCor3 <- renderPlot({
   cPlot3()
 })
 
 
 
 output$downloadCorPlot3 <- downloadHandler(
   filename =  function() {
     paste("correlationPlot3", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     cPlot3()
     dev.off()  # turn the device off
     
   })
 
 
 
 cPlot4 <- function(){
   req(input$slider[1] , input$sliderSpeed[1] , input$sliderStraight[1] , input$trans)
   if(input$slider[1] | input$sliderSpeed[1] | input$sliderStraight[1] | !is.null(input$trans)){ # | input$spaceTraveled) | input$numPoints){
     if (input$radioCor == 1) {
       z <- unique(as.character(as.numeric(up_data()[,11])))
       s <- character(0)
       med <- character(0)
       count=1
       count2=1
       x=1
       for (j in 1:length(z))
       { 
         for (i in 1:length(up_data()[,1]))
         {
           if (x !=z[j] && count != 1 && length(s) != 0 )
           {
             med[count2]=mean(as.numeric(as.character(s)), na.rm = TRUE)
             s= ""
             count2=count2+1
           }
           if (j==length(z) && i==length(up_data()[,1]))
           {
             med[count2]=mean(as.numeric(as.character(s)), na.rm = TRUE)
             s<- character(0)
             count2=count2+1
           }
           if (z[j] == up_data()[i,11])
           {
             s[count]=up_data()[i,10]
             count=count+1
             
           }
           
           x=z[j]
         }
       }

     }
     if (input$radioCor == 2) {
       
       
       dados <- data.frame(Posicao_x=as.numeric(as.character(up_data()[,1])), Posicao_y=as.numeric(as.character(up_data()[,2])), Track_ID= as.numeric(as.character(up_data()$TrackID)), sin_angulo=sin(round(as.numeric(as.character(up_data()[,14])),3)))
       d <- ggplot(dados, aes(x=Posicao_x, y=Posicao_y, group=Track_ID , color=sin_angulo)) + labs(x = "Distance (um)", y = "Distance (um)", colour = "Direction (sin)") +
         scale_colour_gradient2(low = "pink",mid="green", high = "black") + ggtitle('Normal View') +  theme(legend.position="bottom", legend.direction="horizontal") + geom_line()
       directionalityDataforDownload <<- up_data()[,14]
       print(d)
       
     }

   }
 }
 
 output$plotCor4 <- renderPlot({
   cPlot4()
 })
 
 
 output$downloadCorPlot4 <- downloadHandler(
   filename =  function() {
     paste("correlationPlot4", "png", sep=".")
   },
   # content is a function with argument file. content writes the plot to the device
   content = function(file) {
     
     png(file) # open the png device
     cPlot4()
     dev.off()  # turn the device off
     
   })
 

 output$directionalityDataDownload <- downloadHandler(
   filename = function() {
     paste("directionalityData", ".csv", sep = "")
   },
   content = function(file) {
     write.csv(directionalityDataforDownload, file, row.names = FALSE)
   }
 )
 

dataProcess <- function(data){

  Position <- matrix(nrow=(length(data[,1])), ncol=12)
  
  print("Starting Data Processing...")
  #intializes V1,2,3,7,8 (Position.X Position.Y Position.Z Time TrackID) and rest to NA
  for (i in 1:length((data[,1]))) {
    
    for (j in 1:length(data[1,])) {
      
      if (j==4) {
        
        Position[i,7]=data[i,j]
        
      }
      if (j==5) {
        
        Position[i,8]=data[i,j]
        
      }
      
      if (j!=5 && j!=4) {
        Position[i,j]=data[i,j]
      }
      
    }
    
  }

  #make V4,5,6,9,10,11,12 all 0
  for (i in 1:length((Position[,1]))) {
    
    for (j in 1:length(Position[1,])) {
      
      if (is.na(Position[i,j])==TRUE) {
        
        Position[i,j]=0
        
      }
      
    }
    
  }
 
  
  # ________________________________________________________________________________________________
  # CALCULO DA PERSISTENCIA  set values to V12 as persistence
  # ________________________________________________________________________________________________
  
  #same size matrix as Position
  Position_Pers<- matrix(nrow=(length(Position[,1])), ncol=12)
  L=0
  celula=0
  for (i in 1:length(Position[,1])) {
    
    Track_ID=Position[i,8]
    
    if (i==1) {
      Dx=Position[i,1]
      Dy=Position[i,2]
      f=i
      D=0
      L=0
    }
    if (celula != Track_ID && i!=1) {
      D=sqrt((Dx-Position[i-1,1])^2+(Dy-Position[i-1,2])^2)
      S=D/L
      for (j in f:i) {
        Position_Pers[j,1]=Position[j,1]
        Position_Pers[j,2]=Position[j,2]
        Position_Pers[j,3]=Position[j,3]
        Position_Pers[j,4]=Position[j,4]
        Position_Pers[j,5]=Position[j,5]
        Position_Pers[j,6]=Position[j,6]
        Position_Pers[j,7]=Position[j,7]
        Position_Pers[j,8]=Position[j,8]
        Position_Pers[j,9]=Position[j,9]
        Position_Pers[j,10]=Position[j,10]
        Position_Pers[j,11]=Position[j,11]
        Position_Pers[j,12]=S
      }
      Dx=Position[i,1]
      Dy=Position[i,2]
      f=i
      D=0
      L=0
    }
    if (i==length(Position[,1])) {
      D=sqrt((Dx-Position[i-1,1])^2+(Dy-Position[i-1,2])^2)
      S=D/L
      for (j in f:i) {
        Position_Pers[j,1]=Position[j,1]
        Position_Pers[j,2]=Position[j,2]
        Position_Pers[j,3]=Position[j,3]
        Position_Pers[j,4]=Position[j,4]
        Position_Pers[j,5]=Position[j,5]
        Position_Pers[j,6]=Position[j,6]
        Position_Pers[j,7]=Position[j,7]
        Position_Pers[j,8]=Position[j,8]
        Position_Pers[j,9]=Position[j,9]
        Position_Pers[j,10]=Position[j,10]
        Position_Pers[j,11]=Position[j,11]
        Position_Pers[j,12]=S
      }
      Dx=Position[i,1]
      Dy=Position[i,2]
      f=i
      D=0
      L=0
    }
    
    if (Track_ID==celula) {
      Lx=abs(Position[i,1]-Position[i-1,1])
      Ly=abs(Position[i,2]-Position[i-1,2])
      L=L+sqrt(Lx^2+Ly^2)
    }
    
    
    celula=Track_ID
    
  }

  
  # ________________________________________________________________________________________________
  # CALCULO DA VELOCIDADE set values to v10 as velocity
  # ________________________________________________________________________________________________
  
  v1=0
  celula=0
  count=0
  for (i in 1:length(Position[,1])) {
    count=count+1
    Track_ID=Position[i,8]
    
    if (i==1) {
      v1=sqrt((Position[2,1]-Position[1,1])^2+(Position[2,2]-Position[1,2])^2)
      Position_Pers[i,10]=round(v1,digits=2)
    }
    if (celula != Track_ID && i!=1) {
      v1=sqrt((Position[i-1,1]-Position[i-2,1])^2+(Position[i-1,2]-Position[i-2,2])^2)
      Position_Pers[i-1,10]=round(v1,digits=2)
      
      count=1
      v1=sqrt((Position[i,1]-Position[i+1,1])^2+(Position[i,2]-Position[i+1,2])^2)
      Position_Pers[i,10]=round(v1,digits=2)
      
    }
    if (i==length(Position[,1])) {
      v1=sqrt((Position[i,1]-Position[i-1,1])^2+(Position[i,2]-Position[i-1,2])^2)
      Position_Pers[i,10]=round(v1,digits=2)
      
    }
    
    if (Track_ID==celula && i!=length(Position[,1])) {
      
      #EFG
      
      #v1=sqrt(((Position[i+1,1]-Position[i-1,1])^2+(Position[i+1,2]-Position[i-1,2])^2))/abs(Position[i+1,7]-Position[i-1,7])
      
      v1=(sqrt(((Position[i+1,1]-Position[i,1])^2+(Position[i+1,2]-Position[i,2])^2))+sqrt(((Position[i,1]-Position[i-1,1])^2+(Position[i,2]-Position[i-1,2])^2)))/abs(Position[i+1,7]-Position[i-1,7])
      
      Position_Pers[i,10]=round(v1,digits=2)
      
    }
    
    
    
    celula=Track_ID
    
  }
  
  

  
  
  #_________________________________________________________________________________________________________________________________
  #_________________________________________________________________________________________________________________________________
  #_________________________________________________________________________________________________________________________________
  #_________________________________________________________________________________________________________________________________
  #_________________________________________________________________________________________________________________________________
  
  
  # _______________________________________________________________________________________
  # Extracao dos elementos que tem um Straightness superior a 1
  
  # Extraction of elements that have a Straightness equal to 1
  # _______________________________________________________________________________________
  
  
  
  count=0
  for (i in 1:length(Position_Pers[,1]))
  {
    if (Position_Pers[i,12] >= 1) 
    {
      count=count+1
      
    }
  }

 
  
  #Straightness_correct same length as position or Position_Pers
  Straightness_correct <- matrix(nrow=(length(Position_Pers[,1]) - count), ncol=12)
  fcount=0
  for (i in 1:length(Position_Pers[,1]))
  {
    if (Position_Pers[i,12] < 1) 
    {
      fcount=fcount+1
      Straightness_correct[fcount,1]= Position_Pers[i,1]
      Straightness_correct[fcount,2]= Position_Pers[i,2]
      Straightness_correct[fcount,3]= Position_Pers[i,3]
      Straightness_correct[fcount,4]= Position_Pers[i,4]
      Straightness_correct[fcount,5]= Position_Pers[i,5]
      Straightness_correct[fcount,6]= Position_Pers[i,6]
      Straightness_correct[fcount,7]= Position_Pers[i,7]
      Straightness_correct[fcount,8]= Position_Pers[i,8]
      Straightness_correct[fcount,9]= Position_Pers[i,9]
      Straightness_correct[fcount,10]= Position_Pers[i,10]
      Straightness_correct[fcount,11]= Position_Pers[i,11]
      Straightness_correct[fcount,12]= Position_Pers[i,12]
    }
  }

  

  # __________________________________________________________________________________________________________________
  # Coordenadas normalizadas
  # _______Normalized coordinates___________________________________________________________________________________________________________
  

  x<- character(0) 
  y<- character(0)
  
  count=1
  Track_ID_f=0
  # now the loop is in Straightness_correct length
  for (i in 1:length(Straightness_correct[,1]))
  {
    Track_ID=Straightness_correct[i,8]
    
    if(Track_ID_f != Track_ID)
    {
      j=i;
    }
    if(i== length(Straightness_correct[,1])) #for the last element 
    {
      j=i;
      x[count]= as.numeric(Straightness_correct[i,2]) - as.numeric(Straightness_correct[j,2])
    }
    
    x[count]= as.numeric(Straightness_correct[i,1]) - as.numeric(Straightness_correct[j,1])
    count=count+1
    
    Track_ID_f=Track_ID
  }

  
  
  count=1
  Track_ID_f=0
  for (i in 1:length(Straightness_correct[,1]))
  {
    Track_ID=Straightness_correct[i,8]
    
    if(Track_ID_f != Track_ID)
    {
      j=i;
    }
    if(i== length(Straightness_correct[,1]))
    {
      j=i;
      y[count]= as.numeric(Straightness_correct[i,2]) - as.numeric(Straightness_correct[j,2])
    }
    y[count]= as.numeric(Straightness_correct[i,2]) - as.numeric(Straightness_correct[j,2])
    count=count+1
    Track_ID_f=Track_ID
  }
  

  
  
  coordenadas <- data.frame(Straightness_correct[,1], Straightness_correct[,2], Straightness_correct[,3], Straightness_correct[,4], Straightness_correct[,5], Straightness_correct[,6], Straightness_correct[,7], Straightness_correct[,8], Straightness_correct[,9], Straightness_correct[,10], Straightness_correct[,11], Straightness_correct[,12],x,y)

  
  # _______________________________________________________________________________________________________
  # Determinacao do angulo de direcao das celulas
  # Determination of the direction angle of the cells
  # _______________________________________________________________________________________________________
  print("Processing...")
  
  
  count=1
  Track_ID_f=0
  angulos <- character(0)
  f=1
  for (i in 1:length(coordenadas[,1])) 
  {
    
    Track_ID=coordenadas[i,8]
    
    
    if (Track_ID_f != Track_ID && f != 1) 
    {
      if (as.numeric(as.character(coordenadas[i-1,14])) == 0 && as.numeric(as.character(coordenadas[i-1,13])) == 0) 
      {
        angulos[count]=0
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i-1,14])) > 0 && as.numeric(as.character(coordenadas[i-1,13])) > 0) || (as.numeric(as.character(coordenadas[i-1,14])) == 0 && as.numeric(as.character(coordenadas[i-1,13])) > 0) ) 
      {
        angulos[count]=180*(abs(atan((as.numeric(as.character(coordenadas[i-1,14])))/as.numeric(as.character((coordenadas[i-1,13]))))))/pi
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i-1,14])) > 0 && as.numeric(as.character(coordenadas[i-1,13])) < 0) || (as.numeric(as.character(coordenadas[i-1,14])) > 0 && as.numeric(as.character(coordenadas[i-1,13])) == 0)) 
      {
        angulos[count]=180 - 180*(abs(atan((as.numeric(as.character(coordenadas[i-1,14])))/(as.numeric(as.character(coordenadas[i-1,13]))))))/pi 
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i-1,14])) < 0 && as.numeric(as.character(coordenadas[i-1,13])) < 0)  || (as.numeric(as.character(coordenadas[i-1,14])) == 0 && as.numeric(as.character(coordenadas[i-1,13])) < 0)) 
      {
        angulos[count]=180*(abs(atan((as.numeric(as.character(coordenadas[i-1,14])))/(as.numeric(as.character(coordenadas[i-1,13]))))))/pi + 180
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i-1,14])) < 0 && as.numeric(as.character(coordenadas[i-1,13])) > 0) || (as.numeric(as.character(coordenadas[i-1,14])) < 0 && as.numeric(as.character(coordenadas[i-1,13])) == 0)) 
      {
        angulos[count]=360 - 180*(abs(atan((as.numeric(as.character(coordenadas[i-1,14])))/(as.numeric(as.character(coordenadas[i-1,13]))))))/ pi
        count=count+1
      }
    }
    
    
    
    if (i== length(coordenadas[,1]) && f != 1) 
    {
      
      if (as.numeric(as.character(coordenadas[i,14])) == 0 && as.numeric(as.character(coordenadas[i,13])) == 0 ) 
      {
        angulos[count]=0
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i,14])) > 0 && as.numeric(as.character(coordenadas[i,13])) > 0) || (as.numeric(as.character(coordenadas[i-1,14])) == 0 && as.numeric(as.character(coordenadas[i-1,13])) > 0) ) 
      {
        angulos[count]=180*(abs(atan((as.numeric(as.character(coordenadas[i,14])))/as.numeric(as.character((coordenadas[i,13]))))))/pi
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i,14])) > 0 && as.numeric(as.character(coordenadas[i,13])) < 0) || (as.numeric(as.character(coordenadas[i-1,14])) > 0 && as.numeric(as.character(coordenadas[i-1,13])) == 0) ) 
      {
        angulos[count]=180 - 180*(abs(atan((as.numeric(as.character(coordenadas[i,14])))/(as.numeric(as.character(coordenadas[i,13]))))))/pi 
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i,14])) < 0 && as.numeric(as.character(coordenadas[i,13])) < 0)  || (as.numeric(as.character(coordenadas[i-1,14])) == 0 && as.numeric(as.character(coordenadas[i-1,13])) < 0)) 
      {
        angulos[count]=180*(abs(atan((as.numeric(as.character(coordenadas[i,14])))/(as.numeric(as.character(coordenadas[i,13]))))))/pi + 180
        count=count+1
      }
      
      if ((as.numeric(as.character(coordenadas[i,14])) < 0 && as.numeric(as.character(coordenadas[i,13])) > 0) || (as.numeric(as.character(coordenadas[i-1,14])) < 0 && as.numeric(as.character(coordenadas[i-1,13])) == 0) ) 
      {
        angulos[count]=360- 180*(abs(atan((as.numeric(as.character(coordenadas[i,14])))/(as.numeric(as.character(coordenadas[i,13]))))))/ pi
        count=count+1
      }
    }
    
    Track_ID_f=Track_ID
    f=2
  }
  

  
  count=1
  Track_ID_f=0
  x=1
  f <- character(0)
  for (i in 1:length(coordenadas[,1]))
  {Track_ID=coordenadas[i,8]
  
  if (Track_ID_f!=Track_ID) 
  {
    x=angulos[count]
    count=count+1
  }
  f[i]=x
  
  Track_ID_f=Track_ID
  }

  
  
  
  coordenadas_angulos <- data.frame(coordenadas[,1], coordenadas[,2], coordenadas[,3], coordenadas[,4], coordenadas[,5], coordenadas[,6], coordenadas[,7], coordenadas[,8], coordenadas[,9], coordenadas[,10], coordenadas[,11], coordenadas[,12],coordenadas[,13],coordenadas[,14],f,cos((as.numeric(as.character(f))*pi)/180))
  

  
  # ____________________________
  #  Filtragem por tempo
  #  Time filtering
  # ____________________________
  
  t1=0
  t2=100000000000000000000
  count=0
  for (i in 1:length(coordenadas_angulos[,1]))
  {
    if ((as.numeric(as.character(coordenadas_angulos[i,7])) >= t1) &&  (as.numeric(as.character(coordenadas_angulos[i,7])) <= t2)) 
    {
      count=count+1
    }
  }
  

  

  coordenadas_angulos_time<- matrix(nrow=count, ncol=16)

  count=1
  for (i in 1:length(coordenadas_angulos[,1]))
  {
    if ((as.numeric(as.character(coordenadas_angulos[i,7])) >= t1) &&  (as.numeric(as.character(coordenadas_angulos[i,7])) <= t2)) 
    {
      coordenadas_angulos_time[count,1]=as.numeric(as.character(coordenadas_angulos[i,1]))
      coordenadas_angulos_time[count,2]=as.numeric(as.character(coordenadas_angulos[i,2]))
      coordenadas_angulos_time[count,3]=as.character(coordenadas_angulos[i,3])
      coordenadas_angulos_time[count,4]=as.character(coordenadas_angulos[i,4])
      coordenadas_angulos_time[count,5]=as.character(coordenadas_angulos[i,5])
      coordenadas_angulos_time[count,6]=as.character(coordenadas_angulos[i,6])
      coordenadas_angulos_time[count,7]=as.numeric(as.character(coordenadas_angulos[i,7]))
      coordenadas_angulos_time[count,8]=as.numeric(as.character(coordenadas_angulos[i,8]))
      coordenadas_angulos_time[count,9]=as.numeric(as.character(coordenadas_angulos[i,9]))
      coordenadas_angulos_time[count,10]=as.numeric(as.character(coordenadas_angulos[i,10]))
      coordenadas_angulos_time[count,11]=as.numeric(as.character(coordenadas_angulos[i,11]))
      coordenadas_angulos_time[count,12]=as.numeric(as.character(coordenadas_angulos[i,12]))
      coordenadas_angulos_time[count,13]=as.numeric(as.character(coordenadas_angulos[i,13]))
      coordenadas_angulos_time[count,14]=as.numeric(as.character(coordenadas_angulos[i,14]))
      coordenadas_angulos_time[count,15]=as.numeric(as.character(coordenadas_angulos[i,15]))
      coordenadas_angulos_time[count,16]=as.numeric(as.character(coordenadas_angulos[i,16]))
      
      
      count=count+1
    }
  }
  
  print("pinem Processing...")

  
  # _______________________________________________________________________________________________________________
  # Filtragem por espaco percorrido. Colocacao de uma coluna com o espaco percorrido total de cada celula
  # _Space-driven filtration. Placement of a column with the total traveled space of each cell_____________________
  # run_space
  espaco_percorrido<- matrix(nrow=length(coordenadas_angulos_time[,1]), ncol=4)
  count=1
  Track_ID_f=0
  for (i in 1:length(coordenadas_angulos_time[,1]))
  {
    Track_ID=coordenadas_angulos_time[i,8]
    if (count==1 || Track_ID_f!=Track_ID) 
    {
      espaco_percorrido[i,1]=coordenadas_angulos_time[i,1]
      espaco_percorrido[i,2]=coordenadas_angulos_time[i,2]
      espaco_percorrido[i,3]=coordenadas_angulos_time[i,8]
      espaco_percorrido[i,4]=0
      count=count+1
    }
    
    if (Track_ID_f==Track_ID)
    {
      espaco_percorrido[i,1]=coordenadas_angulos_time[i,1]
      espaco_percorrido[i,2]=coordenadas_angulos_time[i,2]
      espaco_percorrido[i,3]=coordenadas_angulos_time[i,8]
      espaco_percorrido[i,4]=sqrt((abs(as.numeric(as.character(coordenadas_angulos_time[i,1]))-as.numeric(as.character(coordenadas_angulos_time[i-1,1]))))^2 + (abs(as.numeric(as.character(coordenadas_angulos_time[i,2]))-as.numeric(as.character(coordenadas_angulos_time[i-1,2]))))^2)
      count=count+1
    }
    Track_ID_f=Track_ID
  }

  num=0
  count=0
  for (i in 1:length(espaco_percorrido[,1]))
  {
    Track_ID=coordenadas_angulos_time[i,8]
    if (Track_ID_f!=Track_ID) 
    {
      if (num > 0) 
      {
        for (j in f:(i-1)) 
        {
          num=0
          count=count+1
        }
        
      }
      
    }
    
    if (Track_ID_f==Track_ID)
    {
      num=num+as.numeric(as.character(espaco_percorrido[i,4]))
    }
    
    if (Track_ID_f!=Track_ID)
    {
      f=i
      
    }
    Track_ID_f=Track_ID
  }
  
  
  

  
  
  
  dados_filtrados<- matrix(nrow=count, ncol=17)
  num=0
  count=1
  for (i in 1:length(espaco_percorrido[,1]))
  {
    Track_ID=coordenadas_angulos_time[i,8]
    if (Track_ID_f!=Track_ID) 
    {
      if (num > 0) 
      {
        for (j in f:(i-1)) 
        {
          dados_filtrados[count,1]=as.numeric(as.character(coordenadas_angulos_time[j,1]))
          dados_filtrados[count,2]=as.numeric(as.character(coordenadas_angulos_time[j,2]))
          dados_filtrados[count,3]=as.numeric(as.character(coordenadas_angulos_time[j,3]))
          dados_filtrados[count,4]=as.character(coordenadas_angulos_time[j,4])
          dados_filtrados[count,5]=as.character(coordenadas_angulos_time[j,5])
          dados_filtrados[count,6]=as.character(coordenadas_angulos_time[j,6])
          dados_filtrados[count,7]=as.numeric(as.character(coordenadas_angulos_time[j,7]))
          dados_filtrados[count,8]=as.numeric(as.character(coordenadas_angulos_time[j,8]))
          dados_filtrados[count,9]=as.numeric(as.character(coordenadas_angulos_time[j,9]))
          dados_filtrados[count,10]=as.numeric(as.character(coordenadas_angulos_time[j,10]))
          dados_filtrados[count,11]=as.numeric(as.character(coordenadas_angulos_time[j,11]))
          dados_filtrados[count,12]=as.numeric(as.character(coordenadas_angulos_time[j,12]))
          dados_filtrados[count,13]=as.numeric(as.character(coordenadas_angulos_time[j,13]))
          dados_filtrados[count,14]=as.numeric(as.character(coordenadas_angulos_time[j,14]))
          dados_filtrados[count,15]=as.numeric(as.character(coordenadas_angulos_time[j,15]))
          dados_filtrados[count,16]=as.numeric(as.character(coordenadas_angulos_time[j,16]))
          dados_filtrados[count,17]=as.numeric(as.character(num))
          
          
          count=count+1
        }
        num=0
      }
      
    }
    
    if (Track_ID_f==Track_ID)
    {
      num=num+as.numeric(as.character(espaco_percorrido[i,4]))  # here is it adding the traveled spaces together!!!
    }
    
    if (Track_ID_f!=Track_ID)
    {
      f=i
      
    }
    Track_ID_f=Track_ID
  }
  
  
  # __________________________________________________________________________________________
  # Colocacao de uma coluna com os valores de cos(angulo) para cada time.
  # ____Placing a column with the cos (angle) values for each team.___________________________
  
  angulos <- character(0)
  count=1
  Track_ID_f=0
  for (i in 1:length(dados_filtrados[,1]))
  {
    
    
    if (as.numeric(as.character(dados_filtrados[i,14])) == 0 && as.numeric(as.character(dados_filtrados[i,13])) == 0) 
    {
      angulos[count]=0
      count=count+1
    }
    
    if ((as.numeric(as.character(dados_filtrados[i,14])) > 0 && as.numeric(as.character(dados_filtrados[i,13])) > 0) || (as.numeric(as.character(dados_filtrados[i,14])) == 0 && as.numeric(as.character(dados_filtrados[i,13])) > 0) ) 
    {
      angulos[count]=180*(abs(atan((as.numeric(as.character(dados_filtrados[i,14])))/as.numeric(as.character((dados_filtrados[i,13]))))))/pi
      count=count+1
    }
    
    if ((as.numeric(as.character(dados_filtrados[i,14])) > 0 && as.numeric(as.character(dados_filtrados[i,13])) < 0) || (as.numeric(as.character(dados_filtrados[i,14])) > 0 && as.numeric(as.character(dados_filtrados[i,13])) == 0)) 
    {
      angulos[count]=180- 180*(abs(atan((as.numeric(as.character(dados_filtrados[i,14])))/(as.numeric(as.character(dados_filtrados[i,13]))))))/pi
      count=count+1
    }
    
    if ((as.numeric(as.character(dados_filtrados[i,14])) < 0 && as.numeric(as.character(dados_filtrados[i,13])) < 0)  || (as.numeric(as.character(dados_filtrados[i,14])) == 0 && as.numeric(as.character(dados_filtrados[i,13])) < 0)) 
    {
      angulos[count]=180*(abs(atan((as.numeric(as.character(dados_filtrados[i,14])))/(as.numeric(as.character(dados_filtrados[i,13]))))))/pi + 180
      count=count+1
    }
    
    if ((as.numeric(as.character(dados_filtrados[i,14])) < 0 && as.numeric(as.character(dados_filtrados[i,13])) > 0) || (as.numeric(as.character(dados_filtrados[i,14])) < 0 && as.numeric(as.character(dados_filtrados[i,13])) == 0)) 
    {
      angulos[count]=360- 180*(abs(atan((as.numeric(as.character(dados_filtrados[i,14])))/(as.numeric(as.character(dados_filtrados[i,13]))))))/ pi
      count=count+1
    }
    ?atan
    
  }

  
  
  
  dados_filtrados <- data.frame(dados_filtrados[,1], dados_filtrados[,2], dados_filtrados[,3], dados_filtrados[,4], dados_filtrados[,5], dados_filtrados[,6], dados_filtrados[,7], dados_filtrados[,8], dados_filtrados[,9], dados_filtrados[,10], dados_filtrados[,11], dados_filtrados[,12],dados_filtrados[,13],dados_filtrados[,14],dados_filtrados[,15], dados_filtrados[,16], dados_filtrados[,17],cos((as.numeric(as.character(angulos))*pi)/180))
  

  #____________________________________________________________________________________________________
  # Extracao dos elementos cujo numero de ocorrencias e menor ou igual a 5.
  #_____Extraction of elements whose number of occurrences is less than or equal to 5._________________
  
  
  
  Track_ID_f=0
  n=0
  count=0
  for (i in 1:length(dados_filtrados[,1])) 
  {
    Track_ID=dados_filtrados[i,8]
    if (Track_ID != Track_ID_f && Track_ID_f != 0 || i==length(dados_filtrados[,1])) 
    {
      if ((count+1) > 0 && i!=length(dados_filtrados[,1])) 
      {
        n=n+count+1
      }
      if ((count+2) > 0 && i==length(dados_filtrados[,1])) 
      {
        n=n+count+2
      }
      count=0
    }
    
    if (Track_ID == Track_ID_f && i!=length(dados_filtrados[,1]) ) 
    {
      count=count+1
    }
    Track_ID_f=Track_ID
  }
  
  
  
  print("sheda ithe koreyundalo...")
  
  Track_ID_f=0
  count=0
  numero_de_ocorrencias=0
  dados_filtrados_1 <- matrix(nrow=n, ncol=19)
  for (i in 1:length(dados_filtrados[,1])) 
  {
    Track_ID=dados_filtrados[i,8]
    
    if (Track_ID != Track_ID_f && Track_ID_f != 0 || i==length(dados_filtrados[,1]))
    {
      if ((numero_de_ocorrencias + 1) > 0 && i!=length(dados_filtrados[,1])) 
      {
        for (j in (numero_de_ocorrencias +1):1)
        {
          dados_filtrados_1[count,1]=as.numeric(as.character(dados_filtrados[i-j,1]))
          dados_filtrados_1[count,2]=as.numeric(as.character(dados_filtrados[i-j,2]))
          dados_filtrados_1[count,3]=as.numeric(as.character(dados_filtrados[i-j,3]))
          dados_filtrados_1[count,4]=dados_filtrados[i-j,4]
          dados_filtrados_1[count,5]=dados_filtrados[i-j,5]
          dados_filtrados_1[count,6]=dados_filtrados[i-j,6] 
          dados_filtrados_1[count,7]=as.numeric(as.character(dados_filtrados[i-j,7]))
          dados_filtrados_1[count,8]=dados_filtrados[i-j,8]
          dados_filtrados_1[count,9]=as.numeric(as.character(dados_filtrados[i-j,9]))
          dados_filtrados_1[count,10]=as.numeric(as.character(dados_filtrados[i-j,10]))
          dados_filtrados_1[count,11]=as.numeric(as.character(dados_filtrados[i-j,11]))
          dados_filtrados_1[count,12]=as.numeric(as.character(dados_filtrados[i-j,12]))
          dados_filtrados_1[count,13]=as.numeric(as.character(dados_filtrados[i-j,13]))
          dados_filtrados_1[count,14]=as.numeric(as.character(dados_filtrados[i-j,14]))
          dados_filtrados_1[count,15]=as.numeric(as.character(dados_filtrados[i-j,15]))
          dados_filtrados_1[count,16]=as.numeric(as.character(dados_filtrados[i-j,16]))
          dados_filtrados_1[count,17]=as.numeric(as.character(dados_filtrados[i-j,17]))
          dados_filtrados_1[count,18]=as.numeric(as.character(dados_filtrados[i-j,18]))
          dados_filtrados_1[count,19]=numero_de_ocorrencias
          count=count+1
        }
        
      }
      
      if ((numero_de_ocorrencias + 1) > 0 && i==length(dados_filtrados[,1])) 
      {
        for (j in (numero_de_ocorrencias +1):0)
        {
          dados_filtrados_1[count,1]=as.numeric(as.character(dados_filtrados[i-j,1]))
          dados_filtrados_1[count,2]=as.numeric(as.character(dados_filtrados[i-j,2]))
          dados_filtrados_1[count,3]=as.numeric(as.character(dados_filtrados[i-j,3]))
          dados_filtrados_1[count,4]=dados_filtrados[i-j,4]
          dados_filtrados_1[count,5]=dados_filtrados[i-j,5]
          dados_filtrados_1[count,6]=dados_filtrados[i-j,6] 
          dados_filtrados_1[count,7]=as.numeric(as.character(dados_filtrados[i-j,7]))
          dados_filtrados_1[count,8]=dados_filtrados[i-j,8] 
          dados_filtrados_1[count,9]=as.numeric(as.character(dados_filtrados[i-j,9]))
          dados_filtrados_1[count,10]=as.numeric(as.character(dados_filtrados[i-j,10]))
          dados_filtrados_1[count,11]=as.numeric(as.character(dados_filtrados[i-j,11]))
          dados_filtrados_1[count,12]=as.numeric(as.character(dados_filtrados[i-j,12]))
          dados_filtrados_1[count,13]=as.numeric(as.character(dados_filtrados[i-j,13]))
          dados_filtrados_1[count,14]=as.numeric(as.character(dados_filtrados[i-j,14]))
          dados_filtrados_1[count,15]=as.numeric(as.character(dados_filtrados[i-j,15]))
          dados_filtrados_1[count,16]=as.numeric(as.character(dados_filtrados[i-j,16]))
          dados_filtrados_1[count,17]=as.numeric(as.character(dados_filtrados[i-j,17]))
          dados_filtrados_1[count,18]=as.numeric(as.character(dados_filtrados[i-j,18]))
          dados_filtrados_1[count,19]=numero_de_ocorrencias
          count=count+1
        }
        
      }
      
      
      numero_de_ocorrencias=0
    }
    
    if (Track_ID == Track_ID_f || Track_ID_f== 0 && i!=length(dados_filtrados[,1]) ) 
    {
      numero_de_ocorrencias=numero_de_ocorrencias+1
    }
    
    Track_ID_f=Track_ID
    
  }
  
  print("Ah dhey theernu...")

  
  
  
  
  
  
  
  
  data_4 <<- data.frame(Position.X=dados_filtrados_1[,1], Position.Y=dados_filtrados_1[,2], Position.Z=dados_filtrados_1[,3], Unit=dados_filtrados_1[,4], Category=dados_filtrados_1[,5], Collection=dados_filtrados_1[,6] ,Time=dados_filtrados_1[,7], TrackID=dados_filtrados_1[,8], ID=dados_filtrados_1[,9], Speed=dados_filtrados_1[,10], Straightness=dados_filtrados_1[,12], x_norm=dados_filtrados_1[,13], y_norm=dados_filtrados_1[,14], Angles=dados_filtrados_1[,15], Cos_angles=dados_filtrados_1[,16], Space_traveled=dados_filtrados_1[,17], 
                       Number_of_points=dados_filtrados_1[,19])
  
  print("Ending!")

  data_4 <- as.data.frame(sapply( data_4, as.numeric))

  return(data_4)
}
}

