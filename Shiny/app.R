F# Heavily adapted from shiny.rstudio.com/articles/persistent-data-storage.html

require(shiny)
require(DT)
require(shinyBS)
require(quanteda)
require(rdrop2) # Dropbox wrapper

source('CondLoadDataTable.R')

# Globals are first computed or just plain set to parameterize behavior.

hostname = system('hostname', intern=T)

if (hostname == 'AJ')
{
    DataDir = paste0('../../data/Rdata')
} else if (hostname == 'VM-EP-3')
{
    DataDir = paste0('../../data/Rdata')
} else
{
    DataDir = '.'
}
CompressedRDataDir <<- DataDir

# If we don't have the data locally, check DropBox using these globals.

DropBoxDataDir = '/Data'
DropBoxCompressedRDataDir <<- DropBoxDataDir
# This works around an apparent limitation of publishing to shinyapps.io:
if (!file.exists('.httr-oauth') & file.exists('httr-oauth')) {file.rename('httr-oauth','.httr-oauth')}

# ngram datafile names without the '.rda' so these become variable names, too.

Twitter2 = 'TwitterU2Top5p' # 1,638,148 bytes
Twitter3 = 'TwitterU3Top1p' #   904,496
Twitter4 = 'TwitterU4Top1p' # 1,424,192
BlogsNews2 = 'BNCU2Top5p' # 3,661,672
BlogsNews3 = 'BNCU3Top1p' # 2,466,662
BlogsNews4 = 'BNCU4Top1p' # 4,385,745

# Define the fields we want to display in the DataTable:
DisplayFields <- c('Phrase','Next Word','Prediction','Match?','Match Count','Mode')
# Define the fields we want to save from input:
InputFieldsT <- c('PhraseT','NextWordT','Prediction','Prediction Match','Correct Matches','tabs')
InputFieldsG <- c('PhraseG','NextWordG','Prediction','Prediction Match','Correct Matches','tabs')

saveData = function(data)
{
    data = as.data.frame(t(data))
    colnames(data) = DisplayFields
    if (exists('responses'))
    {
        responses <<- rbind(responses, data)
    }
    else
    {
        responses <<- data
    }
} # saveData

loadData = function()
{
    if (exists('responses'))
    {
        responses
    }
} # loadData

# This is a wrapper for quanteda::tokenize with settings to match the defaults for dfm
# plus standardized debugging output.

dfmStyleTokenize = function(NewPhrase)
{
    ret = tokenize(toLower(NewPhrase),removeNumbers=T,removePunct=T,removeSeparators=T)[[1]]
    print(paste(ret,collapse='_'))
    ret
} # dfmStyleTokenize

GetPredictedWord = function(ngrams,pattern)
{
    print(pattern)
    predictions = head(ngrams[grep(pattern,attr(ngrams,'names'))],4) # ngrams are presorted
    print(predictions)
    if (length(predictions) <= 0)
    {
        return(NULL)
    }
    ngram = attr(predictions[1],'names')
    ngramTokens = strsplit(ngram,'_',fixed=T)[[1]]
    ngramTokens[length(ngramTokens)]
} # GetPredictedWord

GetTweetPrediction = function(NewPhrase)
{
    tokens = dfmStyleTokenize(NewPhrase)
    tokenCount = length(tokens)
    if (tokenCount >= 3)
    {
        ngrams = CondLoadDataTable(Twitter4)
        pattern = paste0('^',paste0(tokens[(tokenCount-2):tokenCount],collapse='_'),'_')
        ret = GetPredictedWord(ngrams,pattern)
        if (length(ret) > 0)
        {
            return(ret) # Otherwise fall through
        }
    }
    if (tokenCount >= 2)
    {
        ngrams = CondLoadDataTable(Twitter3)
        pattern = paste0('^',paste0(tokens[(tokenCount-1):tokenCount],collapse='_'),'_')
        ret = GetPredictedWord(ngrams,pattern)
        if (length(ret) > 0)
        {
            return(ret) # Otherwise fall through
        }
    }
    if (tokenCount >= 1)
    {
        ngrams = CondLoadDataTable(Twitter2)
        pattern = paste0('^',tokens[tokenCount],'_')
        ret = GetPredictedWord(ngrams,pattern)
        if (length(ret) > 0)
        {
            return(ret)
        }
        return('that') # Most frequent unigram of fairly general applicability
    }
    return('I') # Empty phrase prediction, from the Twitter unigrams.
} # GetTweetPrediction

GetGeneralPrediction = function(NewPhrase)
{
    tokens = dfmStyleTokenize(NewPhrase)
    tokenCount = length(tokens)
    if (tokenCount >= 3)
    {
        ngrams = CondLoadDataTable(BlogsNews4)
        pattern = paste0('^',paste0(tokens[(tokenCount-2):tokenCount],collapse='_'),'_')
        ret = GetPredictedWord(ngrams,pattern)
        if (length(ret) > 0)
        {
            return(ret) # Otherwise fall through
        }
    }
    if (tokenCount >= 2)
    {
        ngrams = CondLoadDataTable(BlogsNews3)
        pattern = paste0('^',paste0(tokens[(tokenCount-1):tokenCount],collapse='_'),'_')
        ret = GetPredictedWord(ngrams,pattern)
        if (length(ret) > 0)
        {
            return(ret) # Otherwise fall through
        }
    }
    if (tokenCount >= 1)
    {
        ngrams = CondLoadDataTable(BlogsNews2)
        pattern = paste0('^',tokens[tokenCount],'_')
        ret = GetPredictedWord(ngrams,pattern)
        if (length(ret) > 0)
        {
            return(ret) # Otherwise fall through
        }
        return('that') # Most frequent unigram of fairly general applicability
    }
    return('The') # Empty phrase prediction, from the Blog and News unigrams.
} # GetGeneralPrediction

# This returns the correct value for each field in the next row of the DataTable.
FieldToDatum = function(input, FieldName)
{
    if (FieldName == 'Prediction')
    {
        PredictedWord # Global
    }
    else if (FieldName == 'Actual')
    {
        ActualWord # Global
    }
    else if (FieldName == 'Prediction Match')
    {
        datum = tolower(PredictedWord) == tolower(ActualWord)
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
            if (exists('CumulativeCorrect'))
            {
                CumulativeCorrect
            }
            else
            {
                0
            }
    }
    else
    {
        input[[FieldName]]
    }
} # FieldToDatum

shinyApp(ui = fluidPage(
    tabsetPanel
    (
        type='tabs',id='tabs'
        ,tabPanel
        (
            'Documentation',value='Doc'
            ,titlePanel('Application Documentation and Background')
            ,br()
            ,'This application helps authors who are stuck for an idea for the next word in a phrase.'
            ,br(),br()
            ,'There are two modes. Each provides help for a distinct writing style.'
            ,'The first mode (Tweet Helper) helps users of'
            ,'Twitter compose their Tweets.'
            ,'The second mode (General Writing Helper) helps with longer and more formal pieces of writing.'
            ,'Each mode is available on its own tab. The user can go back and forth as needed.'
            ,br(),br()
            ,'The application also keeps track of everything the author writes, each'
            ,'predicted next word, and each actual word chosen by the user. That'
            ,'information is on the last tab (Phrase and Prediction History).'
            ,br(),br()
            ,'To get started, pick a mode from the tab bar, above.'
            ,'Starting writing a phrase in the indicated box. When stuck for a word,'
            ,'use the Predict button to get a suggestion. You can then either accept that'
            ,'or enter a better word. When happy with the word, press the Accept button.'
            ,'When you no longer need a suggestion for your next word, press the Close button.'
            ,br(),br()
            ,'This application was developed as part of the Johns Hopkins University'
            ,'Data Science Specialization For more info: '
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
            ,br()
            ,textInput('PhraseT', 'Please enter your phrase in this box:', width='90%')
            ,br()
            ,p(strong('Once your phrase has been entered, please press this button to retrieve a suggested next word:'))
            ,actionButton('predictT', 'Predict Next Word')
            ,tags$hr()
            ,bsModal('ModalPredictionT'
                ,'The Prediction'
                ,'predictT', size='large'#,close.button=F
                ,tags$p(strong('Your phase so far:'))
                ,verbatimTextOutput('CurrentPhraseT')
                ,tags$p(strong('If needed correct the predicted word. Press Accept to add
                               the displayed word to the phrase. When finished adding to
                               the phrase, press Close.'))
                ,textInput('NextWordT','The next word will be:','the')
                ,actionButton('acceptT','Accept Displayed Word')
                )
        ), # tabPanel - Tweet
        tabPanel
        (
            'General Writing Helper',value='General'
            ,br()
            ,textInput('PhraseG', 'Please enter your phrase in this box:', width='90%')
            ,br()
            ,p(strong('Once your phrase has been entered, please press this button to retrieve a suggested next word:'))
            ,actionButton('predictG', 'Predict Next Word')
            ,tags$hr()
            ,bsModal('ModalPredictionG'
                ,'The Prediction'
                ,'predictG', size='large'#,close.button=F
                ,tags$p(strong('Your phase so far:'))
                ,verbatimTextOutput('CurrentPhraseG')
                ,tags$p(strong('If needed correct the predicted word. Press Accept to add
                               the displayed word to the phrase. When finished adding to
                               the phrase, press Close.'))
                ,textInput('NextWordG','The next word will be:','the')
                ,actionButton('acceptG','Accept Displayed Word')
                )
        ), # tabPanel - General
        tabPanel
        (
            'Phrase and Prediction History',value='History'
            ,br()
            ,'This table shows the most recent phrases that you typed, each correct next word'
            ,'you chose, and the value that the helper model predicted. You can choose how'
            ,'many recent phrases to show. The default is 10. You can find older phrases'
            ,'by searching or by paging.'
            ,br(),br()
            ,'This also marks correct and incorrect predictions with TRUE or FALSE and'
            ,'keeps a running count of the correct predictions. Finally, it records the'
            ,'writing style mode used for each prediction.'
            ,br(),br()
            ,DT::dataTableOutput('responses')#, width = 300)
        ) # tabPanel - DT
    ) ), # tabsetPanel fluidpage
    server = function(input, output, session)
    {
        # Whenever a field is filled, aggregate all form data
        formDataT <- reactive({
            data <- sapply(InputFieldsT, function(x) FieldToDatum(input,x))
            data
        })
        formDataG <- reactive({
            data <- sapply(InputFieldsG, function(x) FieldToDatum(input,x))
            data
        })

        # When the word is accepted clicked, save the form data and update the phrase.
        observeEvent(
            input$acceptT
            ,{
                ActualWord <<- input[['NextWordT']]
                saveData(formDataT())
                NewPhrase = paste0(input[['PhraseT']],' ',ActualWord)
                updateTextInput(session,'PhraseT',value=NewPhrase)
                PredictedWord <<- GetTweetPrediction(NewPhrase)
                updateTextInput(session,'NextWordT',value=PredictedWord)
            }
        )
        observeEvent(
            input$acceptG
            ,{
                ActualWord <<- input[['NextWordG']]
                saveData(formDataG())
                NewPhrase = paste0(input[['PhraseG']],' ',ActualWord)
                updateTextInput(session,'PhraseG',value=NewPhrase)
                PredictedWord <<- GetGeneralPrediction(NewPhrase)
                updateTextInput(session,'NextWordG',value=PredictedWord)
            }
        )

        observeEvent(
            input$predictT
            ,{
                PredictedWord <<- GetTweetPrediction(input[['PhraseT']])
                updateTextInput(session,'NextWordT',value=PredictedWord)
            }
        )
        observeEvent(
            input$predictG
            ,{
                PredictedWord <<- GetGeneralPrediction(input[['PhraseG']])
                updateTextInput(session,'NextWordG',value=PredictedWord)
            }
        )

        observeEvent(
            input$tabs
            ,{
                # Insert code to lode selected model
                CurrentTab <<- input$tabs
                # print(CurrentTab)
            }
        )

        # Show the previous responses
        # (update with current response when accept is clicked)
        output$responses <- DT::renderDataTable(
            {
                input$acceptT | input$acceptG
                loadData()
            }
            ,server=F # Otherwise our JS call, below, to show the most recent data is useless.
            ,caption='Predition History'
            ,callback = JS('table.page("last").draw(false);')
        )
#         output$responses <- DT::renderDataTable(
#             {
#                 input$acceptT
#                 loadData()
#             }
#             ,server=F # Otherwise our JS call, below, to show the most recent data is useless.
#             ,caption='Predition History'
#             ,callback = JS('table.page("last").draw(false);')
#         )

        output$CurrentPhraseT = renderText(input[['PhraseT']])
        output$CurrentPhraseG = renderText(input[['PhraseG']])
    } # server
) # shinyApp
