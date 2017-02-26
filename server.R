library(imager)
library(shiny)
library(shinyjs)
library(spatstat)
library(sp)
library(plotKML)
library(maptools)
library(raster)
library(DT)
function(input, output, session) {
  
  ### Function to read in images
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
    im
  }
  
  ### Function to select points
  ### Returns a mask that's the same dimension as the image
  ### With a 1 if that point is to be included
  select.points <- function(im, x, y){
    if(is.null(x) | is.null(y)){
      mask <- matrix(1L, nrow=nrow(im), ncol=ncol(im))
    }else{
      xy<-cbind(x,y)
      xy<-as.data.frame(xy)
      coordinates(xy)=c("x","y")
      pnts<-vect2rast(xy)
      poly <- owin(poly=list(x=x, y=y), check=F)
      SpP<- as(poly,  "SpatialPolygons")
      attr  =  data.frame(a=1,  b=1)
      SrDf  =  SpatialPolygonsDataFrame(SpP,  attr)
      rast <- vect2rast(SrDf,cell.size=1)
      r <- raster(rast)
      crop <- coordinates(r)[!is.na(values(r)),]
      crop <- as.data.frame(crop)
      mask <- matrix(0L, nrow=nrow(im), ncol=ncol(im))
      for(x.coord in unique(crop$x)){
        t <- crop[crop$x==x.coord,]
        mask[t$x, t$y] <- 1
      }
    }
    mask
  }
  
  
  ### Makes all points in image that have a 0 in the mask white
  removePoints <- function(im, mask){
    im[mask==0] <- 1
    im
  }
  
  ### Generic function for plotting the image
  app.plot <- function(im, clicks.x = NULL, clicks.y = NULL, lineslist = NULL){
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){
      #plot(paw, xaxt='n', yaxt='n', ann=FALSE)
      plot(im, xaxt='n', yaxt='n', ann=FALSE)
    }else{
      plot(im, xaxt='n', yaxt='n', ann=FALSE, xlim=ranges$x,  ylim=c(ranges$y[2], ranges$y[1]))
    }
    if(length(clicks.x) > 1){
      lines(c(clicks.x, clicks.x[1]), c(clicks.y, clicks.y[1]), col='red')
    }
    if(!is.null(lineslist)){
      for(i in 1:length(lineslist)){
        x <- lineslist[[i]][[1]]
        y <- lineslist[[i]][[2]]
        lines(c(x, x[1]), c(y, y[1]), col='red')
      }
    }
  }
  
  ### Set ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  ### Code to zoom in on brushed area when double clicking for plot 1
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  v <- reactiveValues(
    originalImage = NULL,
    croppedImage = NULL,
    imgMask = NULL,
    imgclick.x = NULL,
    imgclick.y = NULL,
    crop.img = FALSE,
    imageName = NULL
  )
  
  ### Read in image
  ### Automatically set image name to file name
  observeEvent(input$file1, {
    v$originalImage <- read.image(input$file1$datapath)
    v$croppedImage = NULL
    v$imgMask = NULL
    v$imgclick.x = NULL
    v$imgclick.y = NULL
    v$crop.img = FALSE
    v$imageName <- gsub("(.jpg|.png)","", input$file1$name)
    updateTextInput(session, inputId = "imgName", label = NULL, value = v$imageName)
    output$plot1 <- renderPlot({
      app.plot(v$originalImage,v$imgclick.x, v$imgclick.y)
    })
  })
  
  observeEvent(input$imgName, {
    v$imageName <- input$imgName
  })
  
  # Handle clicks on the plot for tracing foreground
  observeEvent(input$selectForeground, {
    v$crop.img <- TRUE
    disable("selectForeground")
    enable("pauseCropping")
    enable("resetCropping")
    enable("cropBackground")
  })
  
  ### Pause cropping
  observeEvent(input$pauseCropping, {
    v$crop.img <- FALSE
    disable("pauseCropping")
    enable("selectForeground")
    enable("resetTracePaw")
    enable("cropBackground")
  })
  
  
  ## Reset cropping
  observeEvent(input$resetCropping, {
    v$croppedImage <- NULL
    v$imgMask <- NULL
    v$crop.img <- FALSE
    v$imgclick.x  <- NULL
    v$imgclick.y <- NULL
    enable("pauseCropping")
    enable("selectForeground")
    disable("resetTracePaw")
    enable("cropBackground")
    output$plot1 <- renderPlot({
      app.plot(v$originalImage,v$imgclick.x, v$imgclick.y)
    })
  })
  
  
  observeEvent(input$cropBackground,{
    if(is.null(v$imgclick.x) | is.null(v$imgclick.y)){
      v$croppedImage <- v$originalImage
      v$imgMask <- select.points(v$originalImage, v$imgclick.x, v$imgclick.y)
    }else{
      v$imgMask <- select.points(v$originalImage, v$imgclick.x, v$imgclick.y)
      v$croppedImage <- removePoints(v$originalImage, v$imgMask)
      v$crop.img <- FALSE
      v$imgclick.x  <- NULL
      v$imgclick.y <- NULL
      enable("pauseCropping")
      enable("selectForeground")
      enable("resetTracePaw")
      disable("cropBackground")
    }
    output$plot1 <- renderPlot({
      app.plot(v$croppedImage)
    })
  })
  
  observeEvent(input$downloadImage, {
    imager::save.image(v$croppedImage, "croppedImage.jpeg")
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = "croppedImage.jpeg",
  #   content = function(file){
  #     jpeg(file)
  #     app.plot(file)
  #     dev.off()
  #     #imager::save.image(v$croppedImage, filename)
  #   }
  # )
  
  ### Keep track of click locations if tracing paw or tumor 
  observeEvent(input$plot1_click, {
    # Keep track of number of clicks for line drawing
    if(v$crop.img){
      v$imgclick.x <- c(v$imgclick.x, round(input$plot1_click$x))
      v$imgclick.y <- c(v$imgclick.y, round(input$plot1_click$y))
    }
  })
  
  
  ### Original Image
  output$plot1 <- renderPlot({
    app.plot(v$originalImage, v$imgclick.x, v$imgclick.y)
  })
  
}