library(shiny)
library(ggplot2)

source("utils.R")

ui <- fluidPage(
    titlePanel("Case2: 在不同的CV、alpha和CI半宽度限制下的chance：\n"),
    
    numericInput("a", label = "alpha", value = 0.1), 
    numericInput("width_limit", label = "半宽度（log scale下）", 
                 value = 0.35), 
    
    sliderInput("n_vec", label = "Range of sample size", 
                min = 5, max = 12, step = 1, 
                value = c(5, 7)),
    textOutput("n_vec"), 
    
    sliderInput("cv_vec", label = "Range of CV", 
                min = 0.1, max = 0.5, step = 0.05, 
                value = c(0.15, 0.25)),
    textOutput("cv_vec"), 
    
    plotOutput("case2_plot"), 
    tableOutput("case2_table")
)

server <- function(input, output, session) {
    
    n_vec <- reactive(seq(from = input$n_vec[1], 
                          to = input$n_vec[2], 
                          by = 1))
    cv_vec <- reactive(seq(from = input$cv_vec[1], 
                           to = input$cv_vec[2], 
                           by = 0.05))
    
    res <- reactive(Case2_Compute(cv_vec = cv_vec(), 
                                  n_vec = n_vec(), 
                                  width_limit = input$width_limit, 
                                  a = input$a))
    
    output$cv_vec <- renderText(paste0("Choice of CV: ", 
                                       paste(cv_vec(), collapse = ", "), 
                                       "\n"))
    
    
    output$n_vec <- renderText(paste0("Choice of sample size: ", 
                                      paste(n_vec(), collapse = ", "), 
                                      "\n"))
    
    output$case2_plot <- renderPlot({
        ggplot(res(), aes(x = cv, y = chance)) + 
            geom_line(aes(color = factor(n))) + 
            geom_point(aes(color = factor(n))) + 
            labs(color = "sample size(n)", 
                 title = paste0(
                     "alpha =", input$a, 
                     ", 对数变换后CI半宽度限制：", input$width_limit))
    })
    
    output$case2_table <- renderTable({
        res <- res()
        res$n <- as.integer(res$n)
        res
    }, digits = 4)
    
}

shinyApp(ui, server)