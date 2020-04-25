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
                   radioButtons("species_input", label = "", choices = c("Select species", "Create my own"), inline = TRUE),
                   conditionalPanel("input.species_input == 'Select species'",
                                    selectInput("name", label = "Species:", choices = c("Spotted froggit", "Leaping ostoodle", "Lesser humanoid"),
                                                selected = "Leaping ostoodle")
                                    ),
                   conditionalPanel("input.species_input == 'Create my own'",
                                    p("Still coming!")
                   ),
                   br(),
                   strong("Population and ecosystem dynamics"),
                   numericInput("lambda", label = "Lambda:", value = 1.205, step = 0.1, width = "50%", min = 0),
                   uiOutput("pop_params"),
                   sliderInput("carrying_cap", label = "Carrying capacity:",
                                value = 10000, min = 1000, max = 100000, step = 1000),
                   sliderInput("time_frame", label = "Time frame:",
                                value = 30, min = 5, max = 100, step = 5)

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
                                      br(),
                                      plotlyOutput("plot_growth")
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

        if (input$name == "Leaping ostoodle") {
            age_stage = c(0,1,2,3,4,5,6, 7, 8)
            num_ind = c(950, 530, 269, 160, 80, 48, 21, 11, 5)
            num_ind_birth = c(0, 2, 2, 3, 3, 3, 2, 0, 0)
        } else if (input$name == "Spotted froggit") {
            age_stage = c(0,1,2,3,4, 5)
            num_ind = c(700, 55, 25, 15, 10, 3)
            num_ind_birth = c(0, 10, 10, 5, 0, 0)
        } else {
            age_stage = c(0,1,2,3,4,5, 6, 7, 8, 9, 10)
            num_ind = c(120, 115, 111, 109, 105, 95, 90, 74, 52, 21, 2)
            num_ind_birth = c(0, 0, 0, 0, 1, 2, 2, 1, 0, 0, 0)
        }


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

    starting_pop <- reactive({
        sum(data_generations()$starting_pop)
    })

    data_pop_summary <- reactive({
        data_generations() %>%
            select(-num_ind_birth, -survivorship) %>%
            pivot_longer(cols = starts_with("gen"), names_to = "gen") %>%
            group_by(gen) %>%
            summarise(total_pop = sum(value)) %>%
            mutate(lambda = lead(total_pop, default = 0)/total_pop) %>%
            column_to_rownames(var = "gen")
    })

    lambda <- reactive({
        l = data_pop_summary() %>%
            filter(row_number() == (n() - 1)) %>%
            select(lambda) %>%
            pull()

        round(l, digits = 3)
    })

    growth_rate <- reactive({
        input$lambda - 1
    })

    data_pop_growth <- reactive({

        # Get time frame
        time = seq(from = 0, to = input$time_frame)

        # Create df
        pop_growth = data.frame(time)

        # Get starting pop
        starting_pop = starting_pop()

        print(starting_pop)
        pop_growth = pop_growth %>%
            # Add starting population and exponential
            mutate(pop_size_exp = ifelse(time == 0, starting_pop, starting_pop*exp(time*growth_rate())),
                   pop_size_log = ifelse(time == 0, starting_pop, NA))

        for (i in 1:(nrow(pop_growth) - 1)) {
            pop_growth <- pop_growth %>%
                # Add logarithmic row wise through loop
                mutate(pop_size_log = ifelse(time > 0, input$lambda*lag(pop_size_log)*((input$carrying_cap - lag(pop_size_log))/input$carrying_cap), pop_size_log))
        }

        print(pop_growth)
#
#         pop_growth <- pop_growth %>%
#             pivot_longer(cols = c("pop_size_exp", "pop_size_log"))
        pop_growth
    })

# TABLES --------------------------------------------------------------------------------------
    output$life_table <- renderDataTable({
        datatable(data_life_table(),
                  selection = "none",
                  rownames = FALSE,
                  colnames = c("Age/stage </br>x",
                               "Individuals at time x </br> N<sub>x</sub>",
                               "Offspring from one individual at time x </br>b<sub>x</sub>",
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
                               "Offspring from individual </br>b<sub>x</sub>",
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
                               "lambda"
                  ),
                  escape = FALSE,
                  options = list("searching" = FALSE,
                                 "paging" = FALSE,
                                 "ordering" = FALSE)
        ) %>%
            formatRound(columns = 1) %>%
            formatRound(columns = 2, digits = 3)
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

    output$plot_growth <- renderPlotly({
        plot_ly(data_pop_growth(), x = ~time) %>%
            add_lines(y = ~pop_size_exp,
                      name = "Exponential growth",
                                line = list(color = "purple")) %>%
            add_lines(y = ~pop_size_log,
                      name = "Logarithmic growth",
                      line = list(color = "orange")) %>%
            layout(title = "Population growth over time",
                   xaxis = list(title = "Time"),
                   yaxis = list(title = "Population size"),
                   legend = list(orientation = 'h'))
    })


# RENDER UI -----------------------------------------------------------------------------------
    output$pop_params <- renderUI({
        tagList(
            p("Growth rate:"),
            p(growth_rate())
        )
    })



}

# Run the application
shinyApp(ui = ui, server = server)
