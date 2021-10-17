# Define UI for the MigraR app


library(shinydashboard)
library(dplyr)
library(openxlsx)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)

linebreaks <- function(n){HTML(strrep(br(), n))}
ui <- dashboardPage(
  # tags$style(mycss),
  dashboardHeader(title = "MigraR"),
  dashboardSidebar(
    sidebarMenu(
      fileInput("file", "Choose input File"),
      uiOutput("trans"),
      div(style="text-align:center;color:#DEB887;font-style: italic;font-size:95%","Bars underneath enable a three-step",br(), "filtering approach towards improved ",br(),"data quality. Use these functions ",br()," cautiously to avoid erroneous removal ",br()," of meaningful data points."),
      uiOutput("slider"),
      uiOutput("sliderSpeed"),
      uiOutput("sliderStraight"),
      useShinyjs(), 
      sliderInput(
        "eixox",
        "X axis",
        min = -800,
        max = 800,
        value = c(-500, 100)
      ),
      sliderInput(
        "eixoy",
        "Y axis",
        min = -300,
        max = 300,
        value = c(-150, 200)
      ),
      radioButtons(
        "radio",
        label = "View Mode",
        #3 radio buttons down to the above one
        choices = list(
          "Normal" = 1,
          "Track ID" = 2,
          "Rose Plot" = 3
        ),
        selected = 1
      ), 
      actionButton("reset", "Reset"),
      radioButtons(
        "radioCor",
        label = "View Mode",
        #3 radio buttons down to the above one
        choices = list(
          "Direction of Trajectories" = 2,
          # "Ratio Straightness/ Direction" = 3,
          "Angle of Trajectories" = 4
        ),
        selected = 2
      )
      
    )
   # tags$footer(tags$p(style="text-align:center","Bars above enable a three-step filtering approach towards improved data quality.  Use these functions cautiously to avoid erroneous removal of meaningful data points."))
  ),
  dashboardBody(fluidRow(
    useShinyalert(),
    tabBox(
      title = textOutput("warningText"),
      width = 12,
      id = "tabset1",
      height = "250px",
      tabPanel("Trajectories",
               box(
                 title = "Cell Track",
                 width = "100%",
                 plotOutput("plot1", height = 450) %>% withSpinner(color="#0dc5c1"),
                 downloadButton(outputId = "downloadPlot1", label = "Download plot")
               )),
      tabPanel("Velocity", id = "v",
               box(
                 title = "Motion Speed",
                 width = "100%",
                 plotOutput("plot2", height = 450) %>% withSpinner(color="#0dc5c1"),
                 downloadButton(outputId = "downloadPlot2", label = "Download plot"),
                 downloadButton("velocityDataDownload", "Download the Velocity data")
                 )),
      tabPanel("Straightness", id = "s",
               box(
                 title = "Cellular Straigtness*",
                 footer = "*This analysis is only meaningful for tracks of equal length.",
                 width = "100%",
                 
                 plotOutput("plot3", height = 450) %>% withSpinner(color="#0dc5c1"),
                 
                 downloadButton(outputId = "downloadPlot3", label = "Download plot"),
                 downloadButton("straigtnessDataDownload", "Download the Straigtness data")
                 )),
      tabPanel("Direction",
                   box(
                     plotOutput("plotCor1")  %>% withSpinner(color="#0dc5c1"),
                     sliderInput("bsSelecter", "Select Band Width:",
                                 min = 0, max = 100,
                                 value = 50),
                     downloadButton(outputId = "downloadCorPlot1", label = "Download plot")
                     ),
                   box(
                     plotOutput("plotCor2")  %>% withSpinner(color="#0dc5c1"),
                     downloadButton(outputId = "downloadCorPlot2", label = "Download plot")
                     
                     ),
               box(id = "Box1",
                 plotOutput("plotCor3")  %>% withSpinner(color="#0dc5c1"),
                 downloadButton(outputId = "downloadCorPlot3", label = "Download plot")
               ),
               box(id = "Box2",
                 plotOutput("plotCor4")  %>% withSpinner(color="#0dc5c1"),
                 downloadButton(outputId = "downloadCorPlot4", label = "Download plot")
               ),
               
               downloadButton("directionalityDataDownload", "Download the Directionality data")
               
               ),
      tabPanel("DataTable",
               DT::dataTableOutput("table")
               ),
      tabPanel("Disclaimer",
               div(style="text-align:center;font-size:150%",linebreaks(2), "MigraR is an academic tool made by researchers to serve the research community. It is not guaranteed to be free of errors and we assume that its users are knowledgeable and responsible scientists. Please communicate any error or imprecision found to the authors so that we can continue improving this tool. Since this is an open-source tool, users can also improve the code. We kindly ask you to support MigraR by acknowledging the resource in your presentations, publications and grants.")
      )
      
    )
  )
  ), 
  tags$head(tags$style(
    # color palette used
    # https://www.color-hex.com/color-palette/16518
    # https://www.color-hex.com/color-palette/36399
    HTML(
      '
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0c0b06;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #e6dbd7;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #5f7992;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0c0b06;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #0c0b06;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #aebdc3;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #c9d1ff;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #0c0b06;
         }

        .irs--shiny .irs-grid-text {
                             bottom: 5px;
                             color: #FFFFFF;
        }
        
        .nav-tabs-custom>.nav-tabs>li.header {
                             line-height: 35px;
                             padding: 0 10px;
                             font-size: 20px;
                             color: #dd4b39;
        }

    '
    )
  ))
)


