# Project 2: Conditions, Loops & Functions - Rocket Launch Control
# Setup once: install.packages("shiny")   |   Run: click "Run App" in RStudio
library(shiny)

ui <- fluidPage(
  titlePanel("Rocket Launch Control"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("fuel", "Fuel (%)", 0, 100, 80),
      selectInput("weather", "Weather", c("clear", "cloudy", "storm")),
      actionButton("go", "Launch!")
    ),
    mainPanel(h3(textOutput("status")), plotOutput("rplot"),
              verbatimTextOutput("rlog"))
  )
)

server <- function(input, output, session) {
  # Function with if / else if / else
  launch_status <- function(fuel, weather) {
    if (fuel < 50) {
      "ABORT: Low fuel"
    } else if (weather == "storm") {
      "HOLD: Bad weather"
    } else {
      "GO for launch!"
    }
  }

  flight <- eventReactive(input$go, {
    s <- launch_status(input$fuel, input$weather)
    alt <- 0; t <- 0; alts <- 0; log <- c()
    if (s == "GO for launch!") {
      log <- c(paste(5:1, collapse = " ... "), "LIFTOFF!")   # countdown
      while (alt < 100) {                                      # while loop
        t <- t + 1
        alt <- alt + t * 5
        alts <- c(alts, alt)
        log <- c(log, paste0("t = ", t, " s | altitude = ", alt, " km"))
      }
      log <- c(log, paste("Reached space in", t, "seconds!"))
    }
    list(status = s, alts = alts, log = log)
  })

  output$status <- renderText(if (input$go == 0) "Press Launch!" else flight()$status)
  output$rplot <- renderPlot({
    f <- flight()
    if (length(f$alts) > 1)
      plot(0:(length(f$alts) - 1), f$alts, type = "b", col = "darkgreen",
           xlab = "Time (s)", ylab = "Altitude (km)", main = "Rocket climb")
  })
  output$rlog <- renderText(paste(flight()$log, collapse = "\n"))
}

shinyApp(ui, server)
