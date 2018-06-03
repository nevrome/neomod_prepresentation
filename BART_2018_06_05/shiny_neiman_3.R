library(shiny)
library(ggplot2)
library(magrittr)
source(file = "group_drift_simulation.R")
source(file = "squared_euclidian_distance.R")

server <- function(input, output) {
  group_drift_simulation_data <- eventReactive(
    input$run_button_group_drift_simulation, {
      
      # run simulation (code in source file)
      group_drift_simulation(
        input$k_two_groups, 
        input$N_two_groups, 
        input$t_two_groups, 
        input$mi_two_groups
      )
      
    },
    ignoreNULL = FALSE
  )
  
  output$simplot <- renderPlot({
    A <- group_drift_simulation_data() %>%
      ggplot() +
      geom_area(
        aes(x = time, y = individuals_with_variant, fill = variant, group = variant)
      ) +
      geom_line(
        aes(x = time, y = individuals_with_variant, group = variant), 
        position = "stack"
      ) +
      theme_bw() +
      xlab("t") +
      ylab("variants and their occurence in the population [%]") +
      facet_grid(group ~ .)
    
    B <- calculate_sed_for_group_drift_simulation_result(
      group_drift_simulation_data()
    ) %>%
      ggplot() +
      geom_hline(aes(yintercept = 0), color = "dodgerblue4") +
      geom_hline(aes(yintercept = 2), color = "dodgerblue4") +
      geom_line(aes(x = t, y = sed), color = "black", size = 1, alpha = 0.3) +
      geom_point(aes(x = t, y = sed), color = "black", size = 1) +
      theme_bw()  +
      xlab(latex2exp::TeX("t")) +
      ylab(latex2exp::TeX("d_{ij}^2"))
    
    cowplot::plot_grid(A, B, nrow = 2, align = "v", axis = "lr")
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
    sliderInput("k_two_groups", label = "k",
                min = 5, max = 15, value = 10, step = 1),
    sliderInput("N_two_groups", label = "N",
                min = 10, max = 100, value = 20, step = 10),
    sliderInput("t_two_groups", label = "t",
                min = 50, max = 200, value = 100, step = 50),
    sliderInput("mi_two_groups", label = "mi",
                min = 0, max = 1, value = 0.1, step = 0.01),
    actionButton("run_button_group_drift_simulation", "Run simulation")
  ),
  plotOutput("simplot")
)

shinyApp(ui = ui, server = server)
