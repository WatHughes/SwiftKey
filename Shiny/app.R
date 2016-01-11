# Adopted from shiny.rstudio.com/articles/persistent-data-storage.html

require(shiny)
require(DT)
require(shinyBS)

# Define the fields we want to display in the DataTable
fields <- c('Phrase','NextWord','Prediction','Prediction Match','Correct Matches')

CumulativeCorrect <<- 0

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

FieldToDatum = function(input, FieldName, Prediction, Actual)
{
    if (FieldName == 'Prediction')
    {
        Prediction
    }
    else if (FieldName == 'Prediction Match')
    {
        datum = tolower(Prediction) == tolower(Actual)
        if (datum)
        {
            CumulativeCorrect <<- CumulativeCorrect + 1
        }
        datum
    }
    else if (FieldName == 'Correct Matches')
    {
        CumulativeCorrect
    }
    else
    {
        input[[FieldName]]
    }
}

# Shiny app with 3 fields that the user can predict data for
shinyApp(
    ui = fluidPage(
        textInput('Phrase', 'Your phrase', '')
        ,actionButton('predict', 'Predict Next Word')
        ,tags$hr()
        ,DT::dataTableOutput('responses')#, width = 300)
        ,checkboxInput('used_shiny', 'I\'ve built a Shiny app in R before', F)
        ,sliderInput('r_num_years', 'Number of years using R', 0, 25, 2, ticks = F)
        ,bsModal('ModalPrediction'
                 ,'The Prediction'
                 ,'predict', size='large'#,close.button=F
                 ,tags$p('Accept or correct the predicted word then hit "Close".')
                 ,verbatimTextOutput('CurrentPhrase')
                 ,textInput('NextWord','The next word will be','the')
                 ,actionButton('accept','Accept Displayed Word')
            )
    ),
    server = function(input, output, session)
    {
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) FieldToDatum(input,x,PredictedWord,ActualWord))
            data
        })

        # When the word is accepted clicked, save the form data and update the phrase.
        observeEvent(
            input$accept
            ,{
                ActualWord <<- input[['NextWord']]
                saveData(formData())
                NewPhrase = paste0(input[['Phrase']],' ',ActualWord)
                updateTextInput(session,'Phrase',value=NewPhrase)
                PredictedWord <<- 'xyzzy2'
                updateTextInput(session,'NextWord',value=PredictedWord)
            }
        )

        observeEvent(
            input$predict
            ,{
                PredictedWord <<- 'xyzzy1'
                updateTextInput(session,'NextWord',value=PredictedWord)
            }
        )

                # Show the previous responses
        # (update with current response when predict is clicked)
        output$responses <- DT::renderDataTable(
            {
                input$accept
                loadData()
            }
            ,server=F # Otherwise our JS call, below, to show the most recent data is useless.
            ,caption='Predition History'
            ,callback = JS('table.page("last").draw(false);')
        )
    output$CurrentPhrase = renderText(input[['Phrase']])
    }
)
