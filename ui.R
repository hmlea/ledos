library(shiny)
library(plotly)

plainDLButton = function(buttonID, buttonLabel="Download") {
  tags$a(id=buttonID, class="btn btn-default shiny-download-link", href="",
         target="_blank", download=NA, NULL, buttonLabel,
         style="width:100%; padding-right:0.75rem; padding-left:0.75rem;")
}

ui = fluidPage(
  titlePanel(span("Leatherwood's Electron Density Orbital Simulator",
                  div(actionButton("about", "About"), style="float:right"))),
  
  sidebarPanel(
    h4("Quantum Numbers"),
    
    fluidRow(
      column(4, selectInput("n_num", "n", seq(1, 4))),
      column(4, selectInput("l_num", "l", c(0))),
      column(4, selectInput("m_num", "m", c(0))),
    ),
    
    h4("Other"),
    sliderInput("num_points", "Number of points", 10, 10000, 2048,
                step=1, ticks=FALSE),
    
    tags$div(style="margin-bottom:16px;",
             htmlOutput("details")),
    
    fluidRow(
      column(6, plainDLButton("export", "Export Data")),
      column(6, plainDLButton("save", "Save Image"))
    ),
    
    # ADD DETAILS HERE
      # show axis
      # background color
      # colorscale color
      # show nodes
  ),
  
  mainPanel(
    align = "center",
    
    plotlyOutput("output_plotly", height="auto", width="auto")
  )
)

