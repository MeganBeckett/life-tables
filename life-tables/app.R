library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("lumen"),

    fluidRow(titlePanel("Population Growth Simulation")
    ),

    fluidPage(
        column(width = 3,
               wellPanel(

               )
               ),
        column(width = 9,
               h3("Life Table"),
               dataTableOutput("life_table"),
               br(),
               h3("Plots"),
               tabsetPanel(id = "plots",
                           tabPanel(
                               "Survival",
                               br(),
                               plotlyOutput("plot_survival")
                           ),
                           tabPanel(
                               "Mortality",
                               br(),
                               plotlyOutput("plot_mortality")
                           ),
                           tabPanel(
                               "Reproduction",
                               br(),
                               plotlyOutput("plot_reproductive")
                           )
               )

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


# DATA ----------------------------------------------------------------------------------------
    data_life_table <- reactive({

        age_stage = c(0,1,2,3,4,5)
        num_ind = c(530, 159, 80, 48, 21, 5)
        num_ind_birth = c(0, 2, 3, 3, 3, 0)

        df <- data.frame(age_stage, num_ind, num_ind_birth)

        # Get the starting populating number
        starting_num = df[1, 2]

        # Calculate all the rates
        df <- df %>%
            mutate(prob_survival = round(num_ind/starting_num, 2),
                   age_mortality_num = num_ind - lead(num_ind, default = 0),
                   age_mortality_rate = round(age_mortality_num/num_ind, 2),
                   survivorship = 1 - age_mortality_rate,
                   age_reproductive_rate = prob_survival*num_ind_birth
        )
    })

    output$life_table <- renderDataTable({
        datatable(data_life_table(),
                  selection = "none",
                  rownames = FALSE,
                  colnames = c("Age/stage </br>x",
                               "Individuals at time x </br> N<sub>x</sub>",
                               "Individuals born at time x </br>b<sub>x</sub>",
                               "Probability at birth of surviving to time x </br>I<sub>x</sub>",
                               "Age specific mortality </br>(N<sub>x</sub>-N<sub>x+1</sub>) </br>d<sub>x</sub>",
                               "Age specific mortality rate </br>(d<sub>x</sub>/N<sub>x</sub>) </br>q<sub>x</sub>",
                               "Survivorship </br>(1-q<sub>x</sub>) </br>s<sub>x</sub>",
                               "Age/stage specific reproductive rate </br>I<sub>x</sub>b<sub>x</sub>"),
                  escape = FALSE,
                  # editable = list(target = "cell", disable = list(columns = c(0, 3, 4, 5, 6, 7))),
                  options = list("searching" = FALSE,
                                 "paging" = FALSE)
        )
    })


# PLOTS ---------------------------------------------------------------------------------------
    output$plot_survival <- renderPlotly({
        (ggplot(data_life_table(), aes(x = age_stage, y = prob_survival, group = 1,
                                    text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                  "Probability: ", prob_survival))) +
            geom_line(color = "darkblue") +
            geom_point(color = "darkblue") +
            labs(title = "Probability at birth of surviving to time x",
                 x = "Age/stage", y = "Probability") +
            theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_mortality <- renderPlotly({
        (ggplot(data_life_table(), aes(x = age_stage, y = age_mortality_rate, group = 1,
                                       text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                     "Mortality rate: ", age_reproductive_rate))) +
             geom_line(color = "darkred") +
             geom_point(color = "darkred") +
             labs(title = "Age/stage specific mortality rate",
                  x = "Age/stage", y = "Rate") +
             theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_reproductive <- renderPlotly({
        (ggplot(data_life_table(), aes(x = age_stage, y = age_reproductive_rate, group = 1,
                                       text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                     "Reproductive rate: ", age_reproductive_rate))) +
             geom_line(color = "darkgreen") +
             geom_point(color = "darkgreen") +
             labs(title = "Age/stage specific reproductive rate",
                  x = "Age/stage", y = "Rate") +
             theme_classic()) %>%
            ggplotly(tooltip = "text")
    })


}

# Run the application
shinyApp(ui = ui, server = server)
