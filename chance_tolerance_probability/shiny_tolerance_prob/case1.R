library(shiny)

source("utils.R")

ui <- fluidPage(
    titlePanel("Case1: 在不同的CV、chance和样本量组合下的CI边界情况：\n"),
    
    numericInput("a", label = "alpha", value = 0.1), 
    
    sliderInput("n_vec", label = "Range of sample size", 
                min = 5, max = 12, step = 1, 
                value = c(5, 7)),
    textOutput("n_vec"), 
    
    sliderInput("cv_vec", label = "Range of CV", 
                min = 0.1, max = 0.5, step = 0.05, 
                value = c(0.15, 0.25)),
    textOutput("cv_vec"), 
    
    sliderInput("chance_vec", label = "Chance", 
                min = 0.6, max = 0.9, step = 0.1, 
                value = c(0.7, 0.8)), 
    textOutput("chance_vec"),  
    plotOutput("case1_plot"), 
    tableOutput("case1_table")
)

server <- function(input, output, session) {
    
    n_vec <- reactive(seq(from = input$n_vec[1], 
                          to = input$n_vec[2], 
                          by = 1))
    cv_vec <- reactive(seq(from = input$cv_vec[1], 
                           to = input$cv_vec[2], 
                           by = 0.05))
    chance_vec <- reactive(seq(from = input$chance_vec[1], 
                               to = input$chance_vec[2], 
                               by = 0.1))
    res <- reactive(Case1_Compute(cv_vec = cv_vec(), 
                                  chance_vec = chance_vec(), 
                                  n_vec = n_vec(), 
                                  a = input$a))
    
    output$cv_vec <- renderText(paste0("Choice of CV: ", 
                                       paste(seq(from = input$cv_vec[1], 
                                                 to = input$cv_vec[2], 
                                                 by = 0.05), 
                                             collapse = ", "), 
                                       "\n"))
    
    output$chance_vec <- renderText(paste0("Choice of chance: ", 
                                           paste(seq(from = input$chance_vec[1], 
                                                     to = input$chance_vec[2], 
                                                     by = 0.1), collapse = ", "), 
                                           "\n"))
    
    output$n_vec <- renderText(paste0("Choice of sample size: ", 
                                      paste(seq(from = input$n_vec[1], 
                                                to = input$n_vec[2], 
                                                by = 1), 
                                            collapse = ", "), 
                                      "\n"))
    
    output$case1_plot <- renderPlot({
        
        
        ggplot2::ggplot(res(), ggplot2::aes(y = cv)) + 
            ggplot2::geom_errorbarh(
                ggplot2::aes(xmin = lower, xmax = upper), 
                height = 0.01) + 
            ggplot2::facet_grid(rows = ggplot2::vars(chance), 
                                cols = ggplot2::vars(n), 
                                labeller = "label_both") + 
            ggplot2::labs(x = "")
    })
    
    output$case1_table <- renderTable({
        res <- res()
        res$n <- as.integer(res$n)
        tmp <- colnames(res)
        tmp[which(tmp == "width_log")] <- "Half width (log scale)"
        colnames(res) <- tmp
        res
    }, digits = 4)
    
}

shinyApp(ui, server)