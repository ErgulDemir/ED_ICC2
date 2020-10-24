library(shiny)

ui <- fluidPage(

  titlePanel("Item Characteristics Curves for Dichotomous Variables in IRT"),
  hr(),
  
  sidebarPanel(width = 3,
    h4(strong("Enter the item parameters:")),
    numericInput("a", "a", value = NULL),
    numericInput("b", "b", value = NULL),
    numericInput("c", "c", value = NULL),
    numericInput("d", "d", value = NULL),
    hr(),
    actionButton("go1", "Draw graphs", class = "btn-primary"),
    actionButton("go2", "Add graphs", class = "btn-success"),
    actionButton("go3", "Clear", class = "btn-light")
  ),
  
  mainPanel(
    plotOutput("plot", width = 800, height = 500),
    hr(),
    tableOutput("table")
  )
)

server <- function(input, output, session){
  
  vals <- reactiveValues(m = matrix(nrow = 4))

  observeEvent(input$go1, { 
    vals$m <- matrix(c(input$a, input$b, input$c, input$d), ncol = 1)
  })

  observeEvent(input$go2, {
    vals$m <- cbind(vals$m, c(input$a, input$b, input$c, input$d))
  })

  observeEvent(input$go3,{
      session$reload()
  })  
  
  output$plot <- renderPlot({
    
    xv <- seq(-3.5, 3.5, .01)
    yv <- seq(0, 1, length.out = length(xv))
    plot(xv, yv, type = "n", las = 1, yaxt = "n", xaxt = "n", 
      xlab = "Theta", ylab = "Probability",
      main = "Item Characteristics Curves\n with different parameters")

    n <- dim(vals$m)[2]
    for(i in 1:n){
      a <- vals$m[1, i]
      b <- vals$m[2, i]
      c <- vals$m[3, i]
      d <- vals$m[4, i]
    
      pt <- function(th) c + (d - c) * 1 / (1 + exp(-1.7 * a * (th - b)))
    
      curve(pt, -3.5, 3.5, lwd = 2.5, xlim = c(-3.5, 3.5),
        ylim = c(0, 1), add = TRUE)
      lines(c(b, b), c(0, pt(b)), lty = 2)
      lines(c(-3.5, b), c(pt(b), pt(b)), lty = 2)
      text(b + .2 * i, pt(b + .2 * i), i, font = 2, pos = 1)

      axis(1, c(-3.01, 0, round(b, 2), 3.01), font = 2)
      axis(2, c(seq(0, 1, .25), round(pt(b), 2)), font = 2, las = 1)
      }
  })

  output$table <- renderTable({
    rownames(vals$m) <- letters[1:4]
    n <- dim(vals$m)[2]
    colnames(vals$m) <- paste("item", 1:n)
    vals$m
  }, rownames = TRUE)

}


shinyApp(ui, server)
