#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=4,
                 div(style="height: 70px;",numericInput("corr", "Guess the correlation:", min = -1, max = 1, value = "", step = 0.1)),
                 actionButton("submit", "Submit Guess"),
                 div(style="margin-bottom:10px"),
                 actionButton("reset", "New Plot"),
                 div(style="margin-bottom:10px"),
                 checkboxInput("trend",
                               "Show trend line",
                               value=FALSE
                 ),
                 htmlOutput("answer")
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot", width = "550px", height = "300px")
    )
  )
)

gendata <- function(N){
  r <- runif(n = 1, min = -0.99, max = 0.99) 
  d <- as.data.frame(MASS::mvrnorm(n=50, mu = c(0,0), Sigma = matrix(c(1,r,r,1), nrow = 2), empirical = T))
  d$f <- 1 
  colnames(d) <- c("x", "y", "f")
  return(d)
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  origdata <- gendata()
  
  my <- reactiveValues(
    olddata = origdata,
    newdata = origdata
  )
  
  output$distPlot <- renderPlotly({
    fit <- lm(y ~ x, data = origdata)
    origdata$fitted <- fitted(fit)
    ord_origdata <- origdata[order(origdata$x),]
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      range = c(-4, 4)
    )
    if (input$trend){
      ord_origdata %>% 
        plot_ly(x = ~x) %>% 
        add_markers(y = ~y, size=2) %>%
        add_lines(x=~x[c(1,50)], y=~fitted[c(1,50)], line = list(color = "rgba(252, 194, 3, 0.75)", width=5)) %>%
        layout(xaxis = ax, yaxis = ax, showlegend = F) %>%
        config(displayModeBar = F)
    } else {
      origdata %>% 
        plot_ly(x = ~x) %>% 
        add_markers(y = ~y, size=2) %>% 
        layout(xaxis = ax, yaxis = ax, showlegend = F) %>%
        config(displayModeBar = F)
    }
    
  })
  
  observeEvent(input$reset,{
    updateNumericInput(session, "corr", value="")
    req(my$newdata)
    cat("observeEvent input$reset\n")
    my$olddata <- my$newdata # save old data
    my$newdata <- gendata() %>% # generate new data
      mutate(f=my$olddata$f+1)
    origdata <<- my$newdata
    fit <- lm(y ~ x, data = my$newdata)
    my$newdata$fitted <- fitted(fit)
    my$orddata <- my$newdata[order(my$newdata$x),]
    if (input$trend){
      plotlyProxy("distPlot", session=shiny::getDefaultReactiveDomain(), deferUntilFlush=FALSE) %>%
        plotlyProxyInvoke("animate",
                          list(
                            data = list(list(
                              x = my$orddata$x,
                              y = my$orddata$y,
                              frame = my$orddata$f
                            ), list(
                              x = my$orddata$x[c(1,50)],
                              y = my$orddata$fitted[c(1,50)],
                              frame = my$orddata$f
                            )
                            ),
                            traces = list(as.integer(0), as.integer(1))
                          ) 
                          
        )
    } else {
      plotlyProxy("distPlot", session=shiny::getDefaultReactiveDomain(), deferUntilFlush=FALSE) %>%
        plotlyProxyInvoke("deleteTraces", list(as.integer(1)))%>%
        plotlyProxyInvoke("animate",
                          # frameOrGroupNameOrFrameList
                          list(
                            data = list(list(
                              x = my$orddata$x,
                              y = my$orddata$y,
                              frame = my$orddata$f, 
                              line = list(x=my$orddata$x,y=my$orddata$fitted)
                            )
                            )
                          ))
    }
    
  })
  
  v <- reactiveValues(text = NULL)
  
  observeEvent(input$submit, {
    req(input$corr)
    rr <- cor(my$newdata)[2]
    rr1 <- rr-0.05
    rr2 <- rr+0.05
    st1 <- c("Too high, try again", "Try again, too high")
    st2 <- c("Too low, try again", "Try again, too low")
    v$text <- ifelse((input$corr>rr1 & input$corr<rr2), paste('<span style=\"color:#00c5cd\"> Good guess! The exact correlation is: ', round(rr, 3)),
                     ifelse(input$corr>rr2, paste('<span style=\"color:#e13163\">', if ((input$submit %% 2) == 0){st1[1]} else {st1[2]}), paste('<span style=\"color:red\">',  if ((input$submit %% 2) == 0){st2[1]} else {st2[2]})))
  })
  
  observeEvent(input$reset,{
    v$text <- NULL
  })
  
  output$answer <- renderText({
    if (is.null(v$text)) return()
    v$text
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

