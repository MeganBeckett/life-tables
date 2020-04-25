library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Life Tables"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data_life_table <- reactive({

        age_stage = c(0,1,2,3,4,5)
        num_ind = c(530, 159, 80, 48, 21, 5)
        num_ind_birth = c(0, 2, 3, 3, 3, 0)

        df <- data.frame(age_stage, num_ind, num_ind_birth)

        starting_num = df[1, 2]

        df <- df %>%
            mutate(prob_survival = num_ind/starting_num,
                   age_mortality_num = num_ind - lead(num_ind, default = 0),
                   age_mortality_rate = age_mortality_num/num_ind,
                   survivorship = 1 - age_mortality_rate,
                   age_reproductive_rate = prob_survival*num_ind_birth
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
