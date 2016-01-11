# Heavily adapted from shiny.rstudio.com/articles/persistent-data-storage.html

require(shiny)
require(DT)
require(shinyBS)

# Define the fields we want to display in the DataTable
fields <- c('Phrase','NextWord','Prediction','Prediction Match','Correct Matches')

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

# This returns the correct value for each field in the next row of the DataTable.
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
            if (exists('CumulativeCorrect'))
            {
                CumulativeCorrect <<- CumulativeCorrect + 1
            }
            else
            {
                CumulativeCorrect <<- 1
            }
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
} # FieldToDatum

# Shiny app with 3 fields that the user can predict data for
shinyApp(ui = fluidPage(
    tabsetPanel
    (
        type='tabs',id='tabs'
        ,tabPanel
        (
            'Documentation',value='Doc'
            ,titlePanel('Application Documentation and Background')
            ,br()
            ,'This application helps authors who are stuck for an idea for the next word.'
            ,br(),br()
            ,'There are two modes. Each provides help for a distinct writing style.'
            ,'The first mode helps users of'
            ,'Twitter compose their Tweets.'
            ,'The second mode helps with longer and more formal pieces of writing.'
            ,'Each mode is available on its on tab. The user can go back and forth as needed.'
            ,br(),br()
            ,'The application also keeps track of everything the author writes, each'
            ,'predicted next word, and each actual word chosen by the user. That'
            ,'information is on the last tab.'
            ,br(),br()
            ,'To get started, pick a mode from the tab bar, above.'
            ,'Starting writing a phrase in the indicated box. When stuck for a word,'
            ,'use the Predict button to get a suggestion. You can then either accept that'
            ,'or enter a better word. When happy with the word, press the Accept button.'
            ,'When you no longer need a suggestion for your next word, press the Close button.'
            ,br(),br()
            ,'This application was developed as part of the Johns Hopkins University'
            ,'Data Science Specialiation. For more info: '
            ,tags$a(href='https://www.coursera.org/specializations/jhu-data-science','JHU Data Science'),'.'
            ,br(),br()
            ,'Thanks to Hans Christensen of HC Corpora for the data (via Coursera:'
            ,tags$a(href='https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip','Coursera-SwiftKey.zip'),')'
            ,'used to train the models that drive the predictions. For more information about this data:'
            ,tags$a(href='http://www.corpora.heliohost.org/','Corpora'),'.'
            ,br(),br()
        ), # tabPanel - Documentation
        tabPanel
        (
            'Tweet Helper',value='Tweet'
            ,textInput('Phrase', 'Your phrase', '')
            ,actionButton('predict', 'Predict Next Word')
            ,tags$hr()
            ,bsModal('ModalPrediction'
                     ,'The Prediction'
                     ,'predict', size='large'#,close.button=F
                     ,tags$p('Accept or correct the predicted word then hit "Close".')
                     ,verbatimTextOutput('CurrentPhrase')
                     ,textInput('NextWord','The next word will be','the')
                     ,actionButton('accept','Accept Displayed Word')
                )
        ), # tabPanel - Tweet
        tabPanel
        (
            'General Writing Helper',value='General'
            ,br(),br()
            ,'TBD'
        ), # tabPanel - General
        tabPanel
        (
            'Writing and Prediction History',value='History'
            ,tags$hr()
            ,DT::dataTableOutput('responses')#, width = 300)
        ) # tabPanel - DT
    ) ), # tabsetPanel fluidpage
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

        observeEvent(
            input$tabs
            ,{
                # Insert code to lode selected model
                print(input$tabs)
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
