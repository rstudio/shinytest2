library(deSolve)
library(shiny)

van_der_pol <- function(t, y, mu) {
  d_x <- y[2]
  d_y <- mu * (1 - y[1] ^ 2) * y[2] - y[1]
  list(c(X = d_x, Y = d_y))
}

server <- function(input, output) {
  output$brussels <- renderPlot({
    y0 <- c(X = input$X, Y = input$Y)
    # Not necessary to set .001 but to induce longer calculation
    times <- seq(0, 1000, .001)
    out <- ode(y0, times, van_der_pol, input$mu)
    par(mfrow = c(1, 1))
    plot(out[, 2:3], type = "l", xlab = "X", ylab = "Y", main = "state diagram")
  })
}

ui <- fluidPage(
  headerPanel("Van der Pol oscillator"),
  sidebarLayout(
    sidebarPanel(
      h3("Init values"),
      numericInput("X", label = "X", min = 0.0, max = 5,  value = 1, step = 0.2),
      numericInput("Y", label = "Y", min = 0.0, max = 5,  value = 1, step = 0.2),

      h3("Parameters"),
      numericInput("mu", label = "mu", min = 0.0, max = 5,  value = 1, step = 0.1)
    ),
    mainPanel(
      h3("Simulation results"),
      plotOutput("brussels")
    )
  )
)

shinyApp(ui = ui, server = server)
