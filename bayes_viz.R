library(shiny)

library(ggplot2)
library(grid)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Bayes' Rule Visualization"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("p.a", "Probability of A:", 0.5, min = 0, max = 1),
      numericInput("p.b.given.a", "Probability of B given A:", 0.5, min = 0, max = 1),
      numericInput("p.b.given.nota", "Probability of B given not-A:", 0.5, min = 0, max = 1),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("bayes_viz")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bayes_viz <- renderPlot({
    
    x <- c(0,1)
    y <- c(0,1)
    df <- expand.grid(x, y)
    names(df) <- c("x", "y")
    
    p.a <- input$p.a
    p.b.given.a <- input$p.b.given.a
    p.b.given.nota <- input$p.b.given.nota
    

p <- ggplot(df) +
  geom_rect(xmin = 0, xmax = p.a,   ymin = 0, ymax = 1-p.b.given.a,   fill = "#a6cee3") +
  geom_rect(xmin = p.a,    xmax = 1, ymin = 0, ymax = 1-p.b.given.nota,   fill = "#1f78b4") +
  geom_rect(xmin = p.a,    xmax = 1, ymin = 1-p.b.given.nota,    ymax = 1, fill = "#b2df8a") +
  geom_rect(xmin = 0, xmax = p.a,   ymin = 1-p.b.given.a,    ymax = 1, fill = "#33a02c") +
  theme(plot.margin= unit(c(5,5,5,5), "lines"),
        panel.background = element_rect(fill = "white",
                                        colour = "white")) +
  coord_cartesian(clip = "off")


p1 = p + annotation_custom(grob = textGrob("P(B | A)"),  
                           xmin = -0.4, xmax = -0.4, ymin = 1-p.b.given.a, ymax = 1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="#33a02c", lwd=1.5)), 
                    xmin = -0.1, xmax = -0.1, ymin = 1-p.b.given.a, ymax = 1)

p1 = p1 + annotation_custom(grob = textGrob("P(~B | A)"),  
                            xmin = -0.4, xmax = -0.4, ymin = 0, ymax = 1-p.b.given.a) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="#a6cee3", lwd=1.5)), 
                    xmin = -0.1, xmax = -0.1, ymin = 0, ymax = 1-p.b.given.a)

p1 = p1 + annotation_custom(grob = textGrob("P(A)"),  
                            xmin = 0, xmax = p.a, ymin = -0.2, ymax = -0.2) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = 0, xmax = p.a, ymin = -0.1, ymax=-0.1)

p1 = p1 + annotation_custom(grob = textGrob("P(~A)"),  
                            xmin = p.a, xmax = 1, ymin = -0.2, ymax = -0.2) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = p.a, xmax = 1, ymin = -0.1, ymax=-0.1)

p1 = p1 + annotation_custom(grob = textGrob("P(~B | ~A)"),  
                            xmin = 1.4, xmax = 1.4, ymin = 0, ymax = 1-p.b.given.nota) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="#1f78b4", lwd=1.5)), 
                    xmin = 1.1, xmax = 1.1, ymin = 0, ymax = 1-p.b.given.nota)

p1 = p1 + annotation_custom(grob = textGrob("P(B | ~A)"),  
                            xmin = 1.4, xmax = 1.4, ymin = 1-p.b.given.nota, ymax=1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="#b2df8a", lwd=1.5)), 
                    xmin = 1.1, xmax = 1.1, ymin = 1-p.b.given.nota, ymax=1)

p1 = p1 + annotation_custom(grob = textGrob("P(A)"),  
                            xmin = 0, xmax = p.a, ymin = 1.2, ymax = 1.2) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = 0, xmax = p.a, ymin = 1.1, ymax=1.1)

p1 = p1 + annotation_custom(grob = textGrob("P(~A)"),  
                            xmin = p.a, xmax = 1, ymin = 1.2, ymax = 1.2) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = p.a, xmax = 1, ymin = 1.1, ymax=1.1)


p1 = p1 + annotation_custom(grob = textGrob("P(A&B)"),
                            xmin = 0, xmax = p.a,   ymin = 1-p.b.given.a, ymax = 1) +
  annotation_custom(grob = textGrob("P(A&~B)"),
                    xmin = 0, xmax = p.a,   ymin = 0, ymax = 1-p.b.given.a) +
  annotation_custom(grob = textGrob("P(~A&B)"),
                    xmin = p.a,    xmax = 1, ymin = 1-p.b.given.nota,    ymax = ) +
  annotation_custom(grob = textGrob("P(~A&~B)"),
                    xmin = p.a,    xmax = 1, ymin = 0, ymax = 1-p.b.given.nota)
p1

bayes_viz = p1

  })
}

# Run the application 
shinyApp(ui = ui, server = server)
