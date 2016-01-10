# Adopted from shiny.rstudio.com/articles/persistent-data-storage.html

require(shiny)
require(DT)
require(shinyBS)
# source('bsModal2.R')

# Define the fields we want to save from the form
fields <- c('phrase','NextWord','used_shiny','r_num_years')

saveData <- function(data)
{
    data <- as.data.frame(t(data))
    if (exists('responses'))
    {
        responses <<- rbind(responses, data)
    }
    else
    {
        responses <<- data
    }
}

loadData <- function()
{
    if (exists('responses'))
    {
        responses
    }
}

# Shiny app with 3 fields that the user can predict data for
shinyApp(
    ui = fluidPage(
        textInput('phrase', 'Your phrase', '')
        ,actionButton('predict', 'Predict Next Word')
        ,tags$hr()
        ,DT::dataTableOutput('responses')#, width = 300)
        ,checkboxInput('used_shiny', 'I\'ve built a Shiny app in R before', F)
        ,sliderInput('r_num_years', 'Number of years using R', 0, 25, 2, ticks = F)
        ,bsModal2('ModalPrediction'
                 ,'The Prediction'
                 ,'predict', size='large'#,close.button=F
                 ,tags$p('The predicted next word is "the". Hit "Close" to accept that or
                         enter the correct word then hit "Close".')
                 ,textInput('NextWord','The next word will be','the')
            )
    ),
    server = function(input, output, session)
    {
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
        })

        # When the predict button is clicked, save the form data
        observeEvent(
            input$predict
            ,{
                saveData(formData())
            }
        )

        # Show the previous responses
        # (update with current response when predict is clicked)
        output$responses <- DT::renderDataTable(
            {
                input$predict
                loadData()
            }
            ,server=F
            ,caption='Predition History'
            ,callback = JS('table.page("last").draw(false);')
        )
    }
)
