library(shiny)

shinyUI(
    fluidPage(
        
        # Application title
        titlePanel("Success rate for categorical confidence intervals"),
        
        # Sidebar with a slider for sample size and action button to get a new sample.
        fluidRow( position = 'left',
                  # Put slider and button in the sidebar
                  column(5,
                         sliderInput("p_val","Set the population proportion",
                                     min=.1, max=.9, step=.05,value=.3),
                         plotOutput("sampling",
                                    height="350px",
                                    width="400px"),
                         h4(textOutput("p_hat"))
                  ),
                  column(7,
                         plotOutput("conf_int",
                                    height="450px"))
        ),
        
        fluidRow(
            column(4,
                   actionButton("get_samp", "Get one sample"),
                   actionButton("get_50", "Get 50 samples"),
                   sliderInput("sample_size","Set the fixed sample size",
                               min=10, max = 100, step=5, value=15)
            ),
            column(4,
                   sliderInput("ME","Choose a margin of error for the confidence interval",
                               min=0.01, max = .99, step=.01, value=.1),
                   actionButton("reset","Reset plot"),
                   checkboxInput("show_interval","Show confidence interval",
                                 value=F)
                   
            ),
            column(4,
                   h3(textOutput("success_rate"))
            )
        )
        
    )
)