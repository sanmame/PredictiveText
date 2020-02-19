library(shiny)
library(DT)

# load n-gram data tables
unigrams <- readRDS(file = "unigrams.rds")
bigrams <- readRDS(file = "bigrams.rds")
trigrams <- readRDS(file = "trigrams.rds")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Text predictor"),

    # Sidebar for query and number of choices input
    sidebarLayout(
        sidebarPanel(
            # js script to allow action on Enter key
            tags$head(tags$script(src = "enter_button.js")),
            # collect user input
            textInput("query", label = h4("Text input"), value = ""),
            numericInput("num_choices", label = h4("Number of choices"), 3, min = 1, max = 8),
            checkboxInput("prob", "Show probability", value = TRUE, width = NULL),
            # add action button
            actionButton("button", "Go!")
        ),

        # Show a table with the next word choices
        mainPanel(
           dataTableOutput("next_words")
        )
    )
)

# Define server logic required to predict next words
server <- function(input, output) {
    
    # source text prediction functions
    source('get_trigram_prob.R', local=TRUE)
    source('predict_next_word.R', local=TRUE)
    
    # control evaluation on click
    q <- eventReactive(input$button, {
        input$query
    })
    n <- eventReactive(input$button, {
        input$num_choices
    })
    
    # return data table with next words
    output$next_words <- renderDataTable({
        if(!input$prob){
            next_words_df <- predict_next_word(gamma = 0.75, 
                                               query = q(), 
                                               num_choices = n(), 
                                               unigrams = unigrams, 
                                               bigrams = bigrams, 
                                               trigrams = trigrams)
            next_words_dt <- datatable(next_words_df[,1, drop = FALSE],
                                       colnames=c("next word"),
                                       rownames = FALSE,
                                       options = list(dom = 't'))
            } else {
                next_words_df <- predict_next_word(gamma = 0.75, 
                                                   query = q(), 
                                                   num_choices = n(), 
                                                   unigrams = unigrams, 
                                                   bigrams = bigrams, 
                                                   trigrams = trigrams)
                next_words_df$prob <- round(next_words_df$prob, 2)
                next_words_dt <- datatable(next_words_df, 
                                           colnames=c("next word", "probability"),
                                           rownames = FALSE,
                                           options = list(dom = 't'))
    }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
