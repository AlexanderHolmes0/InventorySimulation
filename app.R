library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    HTML('<!-- Quick & Dirty HTML Meta Tags -->
<title>Inventory Simulation</title>
<meta name="description" content="SImulate Inventory Demands">

<!-- Google / Search Engine Tags -->
<meta itemprop="name" content="Inventory Simulation">
<meta itemprop="description" content="SImulate Inventory Demands">
<meta itemprop="image" content="https://media2.giphy.com/media/3ohs7UjgdqCnkEYcsE/giphy.gif?cid=ecf05e47mqaav8fsgfigc5ajmhzfs3797vkekvp2gutdnmgt&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Google / Search Engine Tags -->
<meta name="title" content="Inventory Simulation">
<meta name="description" content="SImulate Inventory Demands">
<meta name="image" content="https://media2.giphy.com/media/3ohs7UjgdqCnkEYcsE/giphy.gif?cid=ecf05e47mqaav8fsgfigc5ajmhzfs3797vkekvp2gutdnmgt&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Facebook Meta Tags -->
<meta property="og:url" content="https://aholmes25.shinyapps.io/Inventory_Simulation/">
<meta property="og:type" content="website">
<meta property="og:title" content="Inventory Simulation">
<meta property="og:description" content="SImulate Inventory Demands">
<meta property="og:image" content="https://media2.giphy.com/media/3ohs7UjgdqCnkEYcsE/giphy.gif?cid=ecf05e47mqaav8fsgfigc5ajmhzfs3797vkekvp2gutdnmgt&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Twitter Meta Tags -->
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:url" content="https://aholmes25.shinyapps.io/Inventory_Simulation/">
<meta name="twitter:title" content="Inventory Simulation">
<meta name="twitter:description" content="SImulate Inventory Demands">
<meta name="twitter:image" content="https://media2.giphy.com/media/3ohs7UjgdqCnkEYcsE/giphy.gif?cid=ecf05e47mqaav8fsgfigc5ajmhzfs3797vkekvp2gutdnmgt&ep=v1_gifs_search&rid=giphy.gif&ct=g">')
  ,tags$link(rel="shortcut icon",href="favicon.ico"),
tags$style(HTML('
      .pretty input:checked~.state.p-danger label:after, .pretty.p-toggle .state.p-danger label:after {
      background-color: #8859b7!important;
      }'))),
theme = shinytheme('united'),
  # Application title
  titlePanel("Inventory Control Simulation - Alexander Holmes"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      chooseSliderSkin(skin = "Flat",color = "purple"),
      sliderInput("days",
        "Number of Days:",
        min = 30,
        max = 365,
        value = 100
      ),
      sliderInput("inventory",
        "Initial Inventory:",
        min = 0,
        max = 100,
        value = 20
      ),
      sliderInput("probstock",
        "Probability of overnight restock:",
        min = 0.01,
        max = 0.99,
        value = .3
      ),
      sliderInput("amtrestock",
        "Amount of restock:",
        min = 1,
        max = 20,
        value = 6
      ),
      prettyRadioButtons(
        inputId = "probdistro",
        label = "Probability Distribution for Demand:", 
        choices = c("Poisson (average is 4)" = "Poisson", "Uniform (0-8)" = "Uniform08", "Uniform (3-5)" = "Uniform35"),
        icon = icon("check"), 
        bigger = TRUE,
        status = "danger",
        fill=TRUE,
        animation = "jelly"
      ),
      numericInput("seed",
        value = 533,
        label = "Random Number Seed",
        min = 1, max = 99999
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      span("Summary of Number of Missed Demands"),
      verbatimTextOutput("NumSum"),
      plotlyOutput("Histogram",height = "370px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  INVENTORY <- reactive({
    set.seed(input$seed)
    days <- input$days
    initialinventory <- input$inventory

    beginningofday <- rep(0, days)
    endofday <- rep(0, days)

    if (input$probdistro == "Poisson") {
      demands <- rpois(n = days, lambda = 4)
    } else if (input$probdistro == "Uniform08") {
      demands <- sample(x = 0:8,size = days,replace = TRUE)
    } else if (input$probdistro == "Uniform35") {
      demands <- sample(x = 3:5,size = days,replace = TRUE)
    }



    restock <- sample(c(input$amtrestock, 0), size = days, replace = TRUE, prob = c(input$probstock, 1 - input$probstock))
    missed <- rep(0, days)

    beginningofday[1] <- initialinventory

    for (i in 1:days) {
      if (demands[i] > beginningofday[i]) {
        missed[i] <- demands[i] - beginningofday[i]
        endofday[i] <- 0
      } else {
        endofday[i] <- beginningofday[i] - demands[i]
      }

      beginningofday[i + 1] <-
        endofday[i] + restock[i]
    }

    return(list(beginningofday = beginningofday, endofday = endofday, missed = missed))
  })


  output$distPlot <- renderPlotly({
    ggplot(data.frame(), aes(x = 1:input$days, y = INVENTORY()$endofday)) +
      geom_line(color="purple") +
      labs(x = "Days", y = "Inventory at End of Day") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$NumSum <- renderPrint({
    summary(INVENTORY()$missed)
  })

  output$Title <- renderText({
    "Summary of Number of Missed Demands"
  })


  output$Histogram <- renderPlotly({
    ggplot(data.frame(), aes(x = INVENTORY()$beginningofday)) +
      labs(x = "Demand at Beginning of Day") +
      ggtitle("Histogram of Inventory Amount at Beginning of Day") +
      geom_histogram(fill='purple',breaks = 0:max(INVENTORY()$beginningofday)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
