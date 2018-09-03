library(shiny)
library(ggplot2)
library(magrittr)

server <- function(input, output) {
  drift_simulation_with_innovation_data <- eventReactive(
    input$run_button_drift_simulation_with_innovation, {
      
      # read input
      k <- input$k_drift_simulation_with_innovation
      N <- input$Ne_drift_simulation_with_innovation
      time <- input$t_drift_simulation_with_innovation
      mu <- input$mu_drift_simulation_with_innovation
      
      # prepare population parameters
      population <- 1:N
      variants <- 1:k
      timesteps <- 2:time
      
      # create starting population
      pop0 <- tibble::tibble(
        time = as.integer(0),
        individual = 1:N,
        variant = rep_len(1:k, N)
      )
      
      # list to store population stages over time
      pop_devel <- list()
      pop_devel[[1]] <- pop0
      
      # simulation loop
      last_variant <- max(pop_devel[[1]]$variant)
      for (p1 in timesteps) {
        pop_new <- pop_devel[[p1 - 1]]
        pop_new$time <- p1 - 1
        pop_new$variant <- sample(pop_new$variant, length(pop_new$variant), replace = T)
        
        # innovation
        innovate_here <- sample(
          c(TRUE, FALSE), 
          length(pop_new$variant), 
          prob = c(mu, 1 - mu), 
          replace = T
        )
        new_variants <- seq(last_variant + 1, last_variant + sum(innovate_here))
        last_variant <- last_variant + sum(innovate_here)
        pop_new$variant[innovate_here] <- new_variants
        
        pop_devel[[p1]] <- pop_new
      }
      
      # bind individual population stages into data.frame
      pop_devel_df <- do.call(rbind, pop_devel)
      
      # calculate number of individuals per timestep and variant
      pop_devel_sum <- pop_devel_df %>%
        dplyr::group_by(
          time, variant
        ) %>%
        dplyr::summarise(
          individuals_with_variant = n()
        ) %>%
        dplyr::ungroup() %>%
        tidyr::complete(
          time, 
          variant, 
          fill = list(individuals_with_variant = as.integer(0))
        )
      
      pop_devel_sum
      
    },
    ignoreNULL = FALSE
  )
  
  output$simplot <-   renderPlot({
    drift_simulation_with_innovation_data() %>%
      ggplot() +
      geom_area(aes(x = time, y = individuals_with_variant, fill = variant, group = variant)) +
      geom_line(aes(x = time, y = individuals_with_variant, group = variant), position = "stack") +
      theme_bw() +
      xlab("t") +
      ylab("variants and their occurence in the population [%]")
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
    sliderInput("k_drift_simulation_with_innovation", label = "k for t == 0",
                min = 5, max = 15, value = 10, step = 1),
    sliderInput("Ne_drift_simulation_with_innovation", label = "Ne",
                min = 10, max = 100, value = 20, step = 10),
    sliderInput("t_drift_simulation_with_innovation", label = "t",
                min = 50, max = 200, value = 100, step = 50),
    sliderInput("mu_drift_simulation_with_innovation", label = "μ",
                min = 0, max = 0.1, value = 0, step = 0.01),
    actionButton("run_button_drift_simulation_with_innovation", "Run simulation")
  ),
  plotOutput("simplot")
)

shinyApp(ui = ui, server = server)
