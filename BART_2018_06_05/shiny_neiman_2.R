library(shiny)
library(ggplot2)
library(magrittr)

Ft_innovation <- function(F0, Ne, time, mu) {
  if(time == 0) {return(F0)}
  (1/Ne + (1 - 1/Ne) * Ft_innovation(F0, Ne, time - 1, mu)) * (1 - mu)^2
}

server <- function(input, output) {
  output$simplot <-   renderPlot({
    # read input
    F0 <- input$F0_homogeneity_drift_with_innovation
    Ne <- input$Ne_homogeneity_drift_with_innovation
    mu <- input$mu_homogeneity_drift_with_innovation
    time <- input$t_homogeneity_drift_with_innovation
    
    timesteps <- 0:time
    
    # apply function for every timestep
    Ft_innovation_time <- sapply(timesteps, function(x) {Ft_innovation(F0, Ne, x, mu)})
    
    data.frame(
      t = timesteps,
      Ft = Ft_innovation_time
    ) %>%
      ggplot() +    
      geom_hline(aes(yintercept = 1), color = "dodgerblue4") +
      geom_hline(aes(yintercept = 0), color = "dodgerblue4") +
      geom_line(aes(t, Ft), size = 1) +
      theme_bw() +
      xlab(latex2exp::TeX("t")) +
      ylab(latex2exp::TeX("F_{t}"))
  })
}

ui <- fluidPage(
  tags$head(tags$style(
    HTML('
      body, label, input, button, select { 
      font-family: "Calibri";
      background-color: #f1f1f1;
      }')
    )
  ),
  inputPanel(
    sliderInput("F0_homogeneity_drift_with_innovation", label = "F0 (Ft for t == 0)",
                min = 0, max = 1, value = 0.5, step = 0.05),
    sliderInput("Ne_homogeneity_drift_with_innovation", label = "Ne",
                min = 10, max = 100, value = 20, step = 10),
    sliderInput("t_homogeneity_drift_with_innovation", label = "t",
                min = 50, max = 200, value = 100, step = 50),
    sliderInput("mu_homogeneity_drift_with_innovation", label = "μ",
                min = 0, max = 1, value = 0.1, step = 0.1)
  ),
  plotOutput("simplot")
)

shinyApp(ui = ui, server = server)
