library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(tibble)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyMatrix)

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("lumen"),

    fluidRow(titlePanel("Population Growth Simulation")
    ),

    fluidPage(
        column(width = 3,
               wellPanel(
                   strong("Species"),
                   radioButtons("species_input", label = "", choices = c("Select species", "Create my own"), inline = TRUE),
                   conditionalPanel("input.species_input == 'Select species'",
                                    selectInput("name", label = "Species:", choices = c("Spotted froggit", "Leaping ostoodle", "Lesser humanoid"),
                                                selected = "Leaping ostoodle")
                                    ),
                   conditionalPanel("input.species_input == 'Create my own'",
                                    textInput("name_create", label = "Species name:"),
                                    selectInput("age_stage_no", label = "Number of ages/stages:", choices = c(5, 8, 10),
                                                selected = 5, width = "50%"),
                                    uiOutput("data_create")
                   ),
                   br(),
                   strong("Population and ecosystem dynamics"),
                   br(),
                   uiOutput("pop_params"),
                   br(),
                   sliderInput("carrying_cap", label = "Carrying capacity:",
                                value = 10000, min = 100, max = 100000, step = 100),
                   sliderInput("time_frame", label = "Time frame:",
                                value = 20, min = 5, max = 100, step = 5)

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
                                      uiOutput("starting_pop"),
                                      br(),
                                      dataTableOutput("pop_summary"),
                                      br()
                               )
                           ),
                           tabPanel(
                               "Population growth",
                               column(width = 12,
                                      br(),
                                      plotlyOutput("plot_growth_exp"),
                                      br(),
                                      plotlyOutput("plot_growth_log")
                               ),
                               column(width = 4,
                                      h3("Data"),
                                      br(),
                                      dataTableOutput("pop_growth"),
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
        # Different starting populations for different species
        if (input$name == "Leaping ostoodle") {
            pop_0 = 20
            pop_1 = 10
        } else if (input$name == "Spotted froggit") {
            pop_0 = 80
            pop_1 = 20
        } else {
            pop_0 = 15
            pop_1 = 14
        }

        # Create 3 generations by default
        df_gen <- data_life_table() %>%
            select(age_stage, num_ind_birth, survivorship) %>%
            mutate(starting_pop = ifelse(age_stage == 0, pop_0,
                                         ifelse(age_stage == 1, pop_1, 0))) %>%
            # Generation 1
            mutate(gen_01 = ifelse(age_stage > 0, lag(starting_pop, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_01 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_01), gen_01)) %>%
            # Generation 2
            mutate(gen_02 = ifelse(age_stage > 0, lag(gen_01, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_02 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_02), gen_02))  %>%
            # Generation 3
            mutate(gen_03 = ifelse(age_stage > 0, lag(gen_02, default = 0)*lag(survivorship, default = 0), 0)) %>%
            mutate(gen_03 = ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = gen_03), gen_03))

        # Get summary of total pops and calculate lambda
        pop_summary <- df_gen %>%
            select(-num_ind_birth, -survivorship) %>%
            pivot_longer(cols = starts_with("gen"), names_to = "gen") %>%
            group_by(gen) %>%
            summarise(total_pop = sum(value)) %>%
            mutate(lambda = round(lead(total_pop, default = 0)/total_pop, 4)) %>%
            column_to_rownames(var = "gen")

        # Total generations calculated so far
        n_gens <- nrow(pop_summary)

        # Initial difference in lambda
        lambda_diff_imm <- pop_summary[n_gens - 1, "lambda"] - pop_summary[n_gens - 2, "lambda"]

        # Add generations until lambda stablizes to within 0.0001
        while (abs(lambda_diff_imm) > 0.0001) {
            gen_curr <- paste0("gen_", formatC(n_gens, width = 2, format = "d", flag = 0))
            gen_add <- paste0("gen_", formatC(n_gens + 1, width = 2, format = "d", flag = 0))

            df_gen <- df_gen %>%
                mutate(!!gen_add := ifelse(age_stage > 0, lag(!! sym(gen_curr), default = 0)*lag(survivorship, default = 0), 0)) %>%
                mutate(!!gen_add := ifelse(age_stage == 0, crossprod(x = num_ind_birth, y = !! sym(gen_add)), !! sym(gen_add)))

            pop_summary <- df_gen %>%
                select(-num_ind_birth, -survivorship) %>%
                pivot_longer(cols = starts_with("gen"), names_to = "gen") %>%
                group_by(gen) %>%
                summarise(total_pop = sum(value)) %>%
                mutate(lambda = round(lead(total_pop, default = 0)/total_pop, 4)) %>%
                column_to_rownames(var = "gen")

            n_gens <- nrow(pop_summary)

            lambda_diff_imm <- pop_summary[n_gens - 1, "lambda"] - pop_summary[n_gens - 2, "lambda"]
            print(lambda_diff_imm)
        }

        # print(df_gen)
        df_gen

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

    data_pop_growth <- reactive({

        # Get time frame
        time = seq(from = 0, to = input$time_frame)

        # Create df
        pop_growth = data.frame(time)

        # Get starting pop
        starting_pop = starting_pop()

        pop_growth = pop_growth %>%
            # Add starting population and exponential
            mutate(pop_size_exp = ifelse(time == 0, starting_pop,
                                         starting_pop*exp(time*growth_rate())),
                   pop_size_log = ifelse(time == 0, starting_pop,
                                         input$carrying_cap/(1 + ((input$carrying_cap - starting_pop)/starting_pop)*exp(-time*growth_rate()))))

        # for (i in 1:(nrow(pop_growth) - 1)) {
        #     pop_growth <- pop_growth %>%
        #         # Add logarithmic row wise through loop
        #         mutate(pop_size_log = ifelse(time > 0, input$lambda*lag(pop_size_log)*((input$carrying_cap - lag(pop_size_log))/input$carrying_cap), pop_size_log))
        # }

        pop_growth
    })


# OTHER REACTIVES -----------------------------------------------------------------------------
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

    gen_to_stabilize <- reactive({
        nrow(data_pop_summary())
    })

    starting_pop <- reactive({
        sum(data_generations()$starting_pop)
    })

    species_name <- reactive({
        # if (input$species_input == 'Select species') {
            input$name
        # } else {
        #     input$name_create
        # }
    })

# TABLES --------------------------------------------------------------------------------------
    output$life_table <- renderDataTable({
        datatable(data_life_table(),
                  extensions = 'Buttons',
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
                                 "ordering" = FALSE,
                                 dom = 'Bfrtip',
                                 buttons =
                                     list(list(
                                         extend = 'collection',
                                         buttons = c('csv', 'excel'),
                                         text = 'Download'))
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
        data_generations() %>%
            rename("Age/stage </br>x" = age_stage,
                   "Offspring from individual </br>b<sub>x</sub>" = num_ind_birth,
                   "Survivorship </br>(1-q<sub>x</sub>) </br>s<sub>x</sub>" = survivorship,
                   "Starting population" = starting_pop) %>%
        datatable(selection = "none",
                  extensions = 'Buttons',
                  rownames = FALSE,
                  escape = FALSE,
                  options = list("searching" = FALSE,
                                 "paging" = FALSE,
                                 "ordering" = FALSE,
                                 "scrollX" = TRUE,
                                 dom = 'Bfrtip',
                                 buttons =
                                     list(list(
                                         extend = 'collection',
                                         buttons = c('csv', 'excel'),
                                         text = 'Download')))
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
                                 "paging" = TRUE,
                                 "ordering" = FALSE)
        ) %>%
            formatRound(columns = 1) %>%
            formatRound(columns = 2, digits = 3)
    })

    output$pop_growth <- renderDataTable({
        datatable(data_pop_growth(),
                  extensions = 'Buttons',
                  selection = "none",
                  rownames = FALSE,
                  colnames = c("Time",
                                "Exponential growth",
                               "Logarithmic growth"
                  ),
                  escape = FALSE,
                  options = list("searching" = FALSE,
                                 "paging" = TRUE,
                                 dom = 'Bfrtip',
                                 buttons =
                                     list(list(
                                         extend = 'collection',
                                         buttons = c('csv', 'excel'),
                                         text = 'Download')))
        ) %>%
            formatRound(columns = 2:3, digits = 1) %>%
            formatStyle(
                'pop_size_exp',
                backgroundColor = "#f6edff"
            ) %>%
            formatStyle(
                'pop_size_log',
                backgroundColor = "#ffedd6"
            )
    })

# PLOTS ---------------------------------------------------------------------------------------
    output$plot_survival <- renderPlotly({
        title_species = paste0("Probability at birth of surviving to time x for ", species_name())

        (ggplot(data_life_table(), aes(x = age_stage, y = prob_survival, group = 1,
                                    text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                  "Probability: ", prob_survival))) +
            geom_line(color = "mediumblue") +
            geom_point(color = "mediumblue") +
            scale_y_continuous(trans = 'log10') +
            labs(title = title_species,
                 x = "Age/stage", y = "Probability (log scale)") +
            theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_mortality <- renderPlotly({
        title_species = paste0("Age/stage specific mortality rate for ", species_name())

        (ggplot(data_life_table(), aes(x = age_stage, y = age_mortality_rate, group = 1,
                                       text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                     "Mortality rate: ", age_reproductive_rate))) +
             geom_line(color = "orangered") +
             geom_point(color = "orangered") +
             labs(title = title_species,
                  x = "Age/stage", y = "Rate") +
             theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_reproductive <- renderPlotly({
        title_species = paste0("Age/stage specific reproductive rate for ", species_name())

        (ggplot(data_life_table(), aes(x = age_stage, y = age_reproductive_rate, group = 1,
                                       text = paste0("Age/stage: ", age_stage, "</br></br>",
                                                     "Reproductive rate: ", age_reproductive_rate))) +
             geom_line(color = "forestgreen") +
             geom_point(color = "forestgreen") +
             labs(title = title_species,
                  x = "Age/stage", y = "Rate") +
             theme_classic()) %>%
            ggplotly(tooltip = "text")
    })

    output$plot_growth_exp <- renderPlotly({
        title_species = paste0("Exponential population growth over time for ", species_name())

        plot_ly(data_pop_growth(), x = ~time) %>%
            add_lines(y = ~pop_size_exp,
                      name = "Exponential growth",
                      line = list(color = "purple")) %>%
            layout(title = title_species,
                   xaxis = list(title = "Time"),
                   yaxis = list(title = "Population size"),
                   showlegend = TRUE)
    })

    output$plot_growth_log <- renderPlotly({
        title_species = paste0("Logarithmic population growth over time for ", species_name())

        plot_ly(data_pop_growth(), x = ~time) %>%
            add_lines(y = ~pop_size_log,
                      name = "Logarithmic growth",
                      line = list(color = "orange")) %>%
            add_lines(y = ~input$carrying_cap,
                      name = "Carrying capacity",
                      line = list(color = "grey",
                                  dash = "dash")) %>%
            layout(title = title_species,
                   xaxis = list(title = "Time"),
                   yaxis = list(title = "Population size"))
    })


# RENDER UI -----------------------------------------------------------------------------------
    output$pop_params <- renderUI({
        if (input$name == "Leaping ostoodle") {
            lambda_species = 1.702
        } else if (input$name == "Spotted froggit") {
            lambda_species = 1.182
        } else if (input$name == "Lesser humanoid") {
            lambda_species = 1.333
        }

        tagList(
            numericInput("lambda", label = "Lambda:", value = lambda_species, step = 0.1, width = "50%", min = 0),
            p(paste0("Growth rate: ", growth_rate())),
            p(paste0("Population at start: ", starting_pop())),
            p(paste0("Generations to stabilize: ", gen_to_stabilize())),
        )
    })

    output$starting_pop <- renderUI({
        tagList(
            p(strong(paste0("Total population at start: ", starting_pop())))
        )
    })

    output$data_create <- renderUI({
        req(input$species_input == 'Create my own')

        col_names <- seq(from = 0, to = input$age_stage_no, by = 1)
        rows = as.numeric(input$age_stage_no) + 1

        m1 = matrix(nrow = rows, ncol = 2,
               dimnames = list(col_names,
                               c("Individuals at time x", "Offspring at time x")))

        m2 = matrix(nrow = 2, ncol = 1,
                    dimnames = list(c(0, 1),
                                    c("Starting population")))
        tagList(
            p("Enter data:"),
            column(width = 12,
                   matrixInput(
                       inputId = "matrix_data_create",
                       value = m1,
                       class = "numeric",
                       cols = list(
                           names = TRUE
                       ),
                       rows = list(
                           names = TRUE
                       )
                   )
                   ),
            column(width = 8,
                   matrixInput(
                       inputId = "matrix_data_start",
                       value = m2,
                       class = "numeric",
                       cols = list(
                           names = TRUE
                       ),
                       rows = list(
                           names = TRUE
                       )
                   )
            )
        )
    })

}

# Run the application
shinyApp(ui = ui, server = server)
