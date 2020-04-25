library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(tibble)
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
                   strong("Species characteristics"),
                   textInput("name", label = "Species name:", value = "Jabberwocky"),
                   br(),
                   strong("Population and ecosystem dynamics"),
                   numericInput("lambda", label = "Lambda:", value = 1.205),
                   numericInput("carrying_cap", label = "Carrying capacity:", value = 10000)

               )
               ),
        column(width = 9,
               tabsetPanel(id = "tabs",
                           tabPanel(
                               "Life table",
                               column(width = 12,
                                      br(),
                                      dataTableOutput("life_table"),
                                      br(),
                                      br(),
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
                                      ),
                                      br()
                               )
                               ),
                           tabPanel(
                               "Generations",
                               column(width = 12,
                                      br(),
                                      h3("Generational numbers"),
                                      dataTableOutput("generations"),
                                      br()
                               ),
                               column(width = 4,
                                      h3("Lamda calculation"),
                                      dataTableOutput("pop_summary"),
                                      br()
                               )
                           ),
                           tabPanel(
                               "Population growth",
                               column(width = 12,
                                      br()
                               )
                           )

                           ),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


# DATA ----------------------------------------------------------------------------------------
    data_life_table <- reactive({

        age_stage = c(0,1,2,3,4,5)
        num_ind = c(530, 159, 80, 48, 21, 5)
        num_ind_birth = c(0, 2, 3, 3, 2, 0)

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

    data_generations <- reactive({
        # Very manual calculation of set number of generations for now
        data_life_table() %>%
            select(age_stage, num_ind_birth, survivorship) %>%
            mutate(starting_pop = ifelse(age_stage == 0, 20,
                                         ifelse(age_stage == 1, 10, 0))) %>%
            # Generation 1
            mutate(gen_1 = ifelse(age_stage > 0, lag(starting_pop, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_1 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_1), gen_1)) %>%
            # Generation 2
            mutate(gen_2 = ifelse(age_stage > 0, lag(gen_1, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_2 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_2), gen_2))  %>%
            # Generation 3
            mutate(gen_3 = ifelse(age_stage > 0, lag(gen_2, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_3 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_3), gen_3)) %>%
            # Generation 4
            mutate(gen_4 = ifelse(age_stage > 0, lag(gen_3, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_4 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_4), gen_4)) %>%
            # Generation 5
            mutate(gen_5 = ifelse(age_stage > 0, lag(gen_4, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_5 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_5), gen_5)) %>%
            # Generation 6
            mutate(gen_6 = ifelse(age_stage > 0, lag(gen_5, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_6 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_6), gen_6)) %>%
            # Generation 7
            mutate(gen_7 = ifelse(age_stage > 0, lag(gen_6, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_7 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_7), gen_7)) %>%
            # Generation 8
            mutate(gen_8 = ifelse(age_stage > 0, lag(gen_7, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_8 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_8), gen_8))

    })

    data_pop_summary <- reactive({
        data_generations() %>%
            select(-num_ind_birth, -survivorship, -starting_pop) %>%
            pivot_longer(cols = starts_with("gen"), names_to = "gen") %>%
            group_by(gen) %>%
            summarise(total_pop = sum(value)) %>%
            mutate(lambda = lead(total_pop, default = 0)/total_pop) %>%
            column_to_rownames(var = "gen")
    })

# TABLES --------------------------------------------------------------------------------------
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
                                 "paging" = FALSE,
                                 "ordering" = FALSE
                                 )
        ) %>%
        formatStyle(
            'prob_survival',
             backgroundColor = "#e5ecff"
        ) %>%
        formatStyle(
            'age_mortality_rate',
            backgroundColor = "#fff1ed"
        ) %>%
        formatStyle(
            'age_reproductive_rate',
            backgroundColor = "#e9fce0"
        )
    })

    output$generations <- renderDataTable({
        datatable(data_generations(),
                  selection = "none",
                  rownames = FALSE,
                  colnames = c("Age/stage </br>x",
                               "Reproductive rate </br>I<sub>x</sub>b<sub>x</sub>",
                               "Survivorship </br>(1-q<sub>x</sub>) </br>s<sub>x</sub>",
                               "Starting population",
                               "Gen 1",
                               "Gen 2",
                               "Gen 3",
                               "Gen 4",
                               "Gen 5",
                               "Gen 6",
                               "Gen 7",
                               "Gen 8"
                               ),
                  escape = FALSE,
                  options = list("searching" = FALSE,
                                 "paging" = FALSE,
                                 "ordering" = FALSE)
        ) %>%
            formatRound(columns = 0:-1)
    })

    output$pop_summary <- renderDataTable({
        datatable(data_pop_summary(),
                  selection = "none",
                  colnames = c("Total population",
                               "lamda"
                  ),
                  escape = FALSE,
                  options = list("searching" = FALSE,
                                 "paging" = FALSE,
                                 "ordering" = FALSE)
        ) %>%
            formatRound(columns = 0:2)
    })

# PLOTS ---------------------------------------------------------------------------------------
    output$plot_survival <- renderPlotly({

        (ggplot(data_life_table(), aes(x = age_stage, y = prob_survival, group = 1,
                                    text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                  "Probability: ", prob_survival))) +
            geom_line(color = "mediumblue") +
            geom_point(color = "mediumblue") +
            scale_y_continuous(trans = 'log10') +
            labs(title = "Probability at birth of surviving to time x",
                 x = "Age/stage", y = "Probability (log scale)") +
            theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_mortality <- renderPlotly({
        (ggplot(data_life_table(), aes(x = age_stage, y = age_mortality_rate, group = 1,
                                       text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                     "Mortality rate: ", age_reproductive_rate))) +
             geom_line(color = "orangered") +
             geom_point(color = "orangered") +
             labs(title = "Age/stage specific mortality rate",
                  x = "Age/stage", y = "Rate") +
             theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_reproductive <- renderPlotly({
        (ggplot(data_life_table(), aes(x = age_stage, y = age_reproductive_rate, group = 1,
                                       text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                     "Reproductive rate: ", age_reproductive_rate))) +
             geom_line(color = "forestgreen") +
             geom_point(color = "forestgreen") +
             labs(title = "Age/stage specific reproductive rate",
                  x = "Age/stage", y = "Rate") +
             theme_classic()) %>%
            ggplotly(tooltip = "text")
    })


}

# Run the application
shinyApp(ui = ui, server = server)
