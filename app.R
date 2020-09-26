library(shiny)

library(ggplot2)
library(grid)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Bayes' Rule Visualization"),
  
  
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
  
  verticalLayout(
  withMathJax(),
  p(""),
    p("Suppose that for two events $\\color{red}{A}$ and $\\color{blue}{B}$, we know $P(\\color{red}{A})$, $P(\\color{blue}{B})$, and $P(\\color{blue}{B}|\\color{red}{A})$. Can we find $P(\\color{red}{A}|\\color{blue}{B})$?"),
    p("Yes! To see why, remember that we can write $P(\\color{red}{A} \\& \\color{blue}{B})$ in two ways: $ P(\\color{blue}{B}|\\color{red}{A}) \\times P(\\color{red}{A}) = P(\\color{red}{A} \\& \\color{blue}{B}) = P(\\color{red}{A}|\\color{blue}{B}) \\times P(\\color{blue}{B}) $"),
    p("This means that the yellow area of both squares equals $P(\\color{red}{A} \\& \\color{blue}{B})$. So if we know $P(\\color{red}{A})$, $P(\\color{blue}{B})$, and $P(\\color{blue}{B}|\\color{red}{A})$, then we know that: $ P(\\color{red}{A}|\\color{blue}{B}) = \\frac{ P(\\color{blue}{B}|\\color{red}{A}) \\times P(\\color{red}{A}) }{P(\\color{blue}{B})} $"),
  p(""),
  p(""),
  p(""),

  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("p.a", "Probability of A:", value=0.5, step=0.01, min = 0, max = 1),
      sliderInput("p.b", "Probability of B:", value=0.8, step=0.01, min = 0, max = 1),
      sliderInput("p.b.given.a", "Probability of B given A:", value=0.6, step=0.01, min = 0, max = 1),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("bayes_viz")
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {

  
  output$bayes_viz <- renderPlot({
    
    p.a <- input$p.a
    p.b <- input$p.b
    p.b.given.a <- input$p.b.given.a
    # use Bayes' rule to compute P(A|B)
    p.a.given.b <- (p.b.given.a * p.a)/p.b
    p.a.and.b <- p.b.given.a * p.a
    
    # create dataset for blank square plot
    x <- c(0,1)
    y <- c(0,1)
    df <- expand.grid(x, y)
    names(df) <- c("x", "y")
    

p <- ggplot(df) +
 # bottom left
  geom_rect(xmin = 0, xmax = p.a,   ymin = 0, ymax = 1-p.b.given.a,   fill = "#a6cee3") +
  # top left
  geom_rect(xmin = 0, xmax = p.a,   ymin = 1-p.b.given.a,    ymax = 1, fill = "#f3f470") +
  # RHS (not broken up into 2 blocks, because we don't actually need to know P(B|~A))
  geom_rect(xmin = p.a, xmax = 1, ymin = 0, ymax = 1,   fill = "#1f78b4") +
  labs(title="Factoring by P(A)") + 
  coord_cartesian(clip = "off") + coord_fixed(ratio=1, ylim=c(0, 1.3), xlim=c(-0.3, 1)) +
  theme(plot.margin= unit(c(-3,0,0,0), "lines"),
        plot.title = element_text(margin=margin(b = -31, unit = "pt"), face="bold", size=18),
        panel.background = element_rect(fill = "white",
                                        colour = "white"))


p1 = p + annotation_custom(grob = textGrob("P(B | A)"),  
                           xmin = -0.2, xmax = -0.2, ymin = 1-p.b.given.a, ymax = 1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = -0.05, xmax = -0.05, ymin = 1-p.b.given.a, ymax = 1)

p1 = p1 + annotation_custom(grob = textGrob("P(~B | A)"),  
                            xmin = -0.2, xmax = -0.2, ymin = 0, ymax = 1-p.b.given.a) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = -0.05, xmax = -0.05, ymin = 0, ymax = 1-p.b.given.a)


p1 = p1 + annotation_custom(grob = textGrob("P(A)"),  
                            xmin = 0, xmax = p.a, ymin = 1.1, ymax = 1.1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = 0, xmax = p.a, ymin = 1.05, ymax=1.05)

p1 = p1 + annotation_custom(grob = textGrob("P(~A)"),  
                            xmin = p.a, xmax = 1, ymin = 1.1, ymax = 1.1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = p.a, xmax = 1, ymin = 1.05, ymax=1.05)


p1 = p1 + annotation_custom(grob = textGrob("P(A&B)"),
                            xmin = 0, xmax = p.a,   ymin = 1-p.b.given.a, ymax = 1)

p2 <- ggplot(df) +
  # bottom left
  geom_rect(xmin = 0, xmax = p.b,   ymin = 0, ymax = 1-p.a.given.b,   fill = "#b7aef0") +
  # top left
  geom_rect(xmin = 0, xmax = p.b,   ymin = 1-p.a.given.b,    ymax = 1, fill = "#f3f470") +
  # RHS (not broken up into 2 blocks, because we don't actually need to know P(B|~A))
  geom_rect(xmin = p.b, xmax = 1, ymin = 0, ymax = 1,   fill = "#7b72b8") +
  labs(title="Factoring by P(B)") + 
  coord_cartesian(clip = "off") + coord_fixed(ratio=1, ylim=c(0, 1.3), xlim=c(-0.3, 1)) +
  theme(plot.margin= unit(c(-3,0,0,0), "lines"),
        plot.title = element_text(margin=margin(b = -31, unit = "pt"), face="bold", size=18),
        panel.background = element_rect(fill = "white",
                                        colour = "white"))


p2 = p2 + annotation_custom(grob = rectGrob(gp=gpar(fill="red", alpha=0.5)),  
                            xmin = -0.33, xmax = -0.08, ymin = 1-p.a.given.b/2 -0.04, ymax = 1-p.a.given.b/2 +0.04) +
          annotation_custom(grob=textGrob("P(A | B)"),
                            xmin = -0.3, xmax = -0.1, ymin = 1-p.a.given.b, ymax = 1) +
          annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = -0.05, xmax = -0.05, ymin = 1-p.a.given.b, ymax = 1)
  # add box to highlight the unknown quantity P(A|B)

p2 = p2 + annotation_custom(grob = textGrob("P(~A | B)"),  
                            xmin = -0.2, xmax = -0.2, ymin = 0, ymax = 1-p.a.given.b) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = -0.05, xmax = -0.05, ymin = 0, ymax = 1-p.a.given.b)

p2 = p2 + annotation_custom(grob = textGrob("P(B)"),  
                            xmin = 0, xmax = p.b, ymin = 1.1, ymax = 1.1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = 0, xmax = p.b, ymin = 1.05, ymax=1.05)

p2 = p2 + annotation_custom(grob = textGrob("P(~B)"),  
                            xmin = p.b, xmax = 1, ymin = 1.1, ymax = 1.1) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="both", length=unit(2,"mm")), 
                                     gp=gpar(col="black", lwd=1.5)), 
                    xmin = p.b, xmax = 1, ymin = 1.05, ymax=1.05)


p2 = p2 + annotation_custom(grob = textGrob("P(A&B)"),
                            xmin = 0, xmax = p.b,   ymin = 1-p.a.given.b, ymax = 1)


bayes_viz = grid.arrange(p1, p2, ncol=2)


bayes_viz

  })
}

# Run the application 
shinyApp(ui = ui, server = server)