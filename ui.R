library(shinyjs)
library(DT)
fluidPage(
  titlePanel("Crop Image"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Image",
                accept=c(".png",
                         ".jpg")),
      textInput("imgName", "Image Name", ""),
      actionButton("selectForeground", "Begin Crop"),
      p("Click the button to begin cropping the image"),
      actionButton("pauseCropping", "Pause Cropping"),
      p("Click the button to pause background cropping"),
      actionButton("resetCropping", "Reset Cropping"),
      p("Click the button to reset background cropping"),
      actionButton("cropBackground", "Crop Image"),
      p("Click after you have cropped out the background"),
      actionButton("downloadImage", "Download Image"),
      p("Click to download the cropped image")
    ),
    mainPanel(
      plotOutput("plot1", click="plot1_click",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 ))
    )
  )
)
