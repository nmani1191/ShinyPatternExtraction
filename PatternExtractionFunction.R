# This is an R Shiny Application helping to do Pattern Extraction using Text Mining

pattern_extraction <- function()
{
  #Package Loading
  library(shinydashboard)
  library(shiny)
  library(dplyr)
  library(DT)
  library(RColorBrewer)
  library(stringr)
  library(tm)
  library(wordcloud)
  library(RWeka)
  
  #Function Loading
  
  #Function Loading
  #######################################################
  Unique_NA_counts <- function(input_data)
  {
    df1 <- data.frame(character(),character(),integer(),integer(),integer(),numeric())
    
    for (name in colnames(input_data)) {
      
      df1 <- rbind(df1,data.frame(ColName=name,Datatype=class(input_data[,name]),Total_Records=nrow(input_data),
                                  Unique_Counts=length(unique(input_data[,name])),
                                  NA_Counts=sum(is.na(input_data[,name])),
                                  NA_Percent=round(sum(is.na(input_data[,name]))/nrow(input_data),2)))
      
    }
    
    df1 <- as.data.frame(df1 %>% arrange(-NA_Counts))
    
    return(df1)
  }
  
  #######################################################
  Text_Processing<- function (FreeText, wordstoRemove)
  {
    FreeText <- iconv(FreeText, "latin1", "ASCII", sub="")
    wordstoRemove <- iconv(wordstoRemove,"latin1", "ASCII", sub = "")
    
    FreeText <- as.character(FreeText)
    wordstoRemove <- tolower(wordstoRemove)
    FreeText <- gsub("[^ -z]", " ", FreeText)
    FreeText <- gsub("[0-9]", " ", FreeText)
    FreeText <- gsub("[34-47]", " ", FreeText)#Spl character
    FreeText <- stringi::stri_trim(FreeText, side = c("both"))
    FreeText <- tolower(FreeText)
    FreeText <- gsub("[[:punct:]]", " ", FreeText)
    FreeText <- gsub("\\s+", " ", str_trim(FreeText))
    FreeText <- paste0("wordstart ", FreeText, " wordend")
    for (i in 1:length(wordstoRemove)) {
      FreeText <- gsub(paste("*\\b", wordstoRemove[i], "\\b*"), 
                       " ", FreeText)
    }
    FreeText <- gsub("\\s+", " ", str_trim(FreeText))
    FreeText <- substr(FreeText, 10, stringi::stri_length(FreeText) - 
                         8)
    FreeText <- str_trim(FreeText)
    return(FreeText)
  }
  #######################################################
  ngram_generation <- function(analysisdata,ngram)
  {
    #Remove Invalid Characters
    analysisdata <- iconv(analysisdata, "latin1", "ASCII", sub="")
    
    #Replace the NA documents with blank value ""
    Modifiedanalysisdata <- data.frame(analysisdata)
    Modifiedanalysisdata$analysisdata <- as.character(Modifiedanalysisdata$analysisdata)
    Modifiedanalysisdata[is.na(Modifiedanalysisdata)] <- ""
    text_data <- as.character(Modifiedanalysisdata$analysisdata)
    
    #Convert the character into corpus
    corpus.ng<- VCorpus(VectorSource(text_data))
    
    #Use RWeka function to generate Ngrams
    set.seed(3000)
    ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram, max = ngram))
    
    #Create Document Term Matrix
    dtm.ngram = DocumentTermMatrix(corpus.ng,control = list(tokenize = ngramTokenizer))
    #Remove sparse terms
    dtm.ngram <- removeSparseTerms(dtm.ngram, 0.99)
    ngramdf <- as.data.frame(as.matrix(dtm.ngram))
    rownames(ngramdf) <- NULL
    
    #Create Term Presence (TP) -> Replace 0 values with NA and > 0 values with 1 in the above DTM
    df <- ngramdf
    df[df==0] <- NA
    df[df>0] <- 1
    output <- df
    
    #Compute IDF -> log(Total no of Documents/No of Documents contains the term)
    id=function(col){sum(!col==0)}
    idf <- log(nrow(ngramdf)/apply(ngramdf, 2, id))
    
    #Repeatable Pattern in Document -> TP*IDF
    for(word in names(idf)){output[,word] <- df[,word] * idf[word]}
    
    #Extract pattern which has min IDF score (This will be the pattern which occured more frequently)
    row_result <- function(row) {names(sort(row))[1]}
    output$Ngram_pattern <- apply(output,1,function(x) row_result(x))
    
    #Replace NA with 0 where we dont have pattern
    output$Ngram_pattern[is.na(output$Ngram_pattern)] <- 0
    
    ngram_pattern <- output$Ngram_pattern
    
    return(ngram_pattern)
  }
  #######################################################
  GetUnigram_SummData<- function(ins_data_set) {
    #Summarization of data based on the category/pattern:
    Unigram_Summ_data<- as.data.frame(ins_data_set %>%  dplyr::filter(Unigram != 0) %>%
                                        dplyr::group_by(Unigram) %>%
                                        dplyr::summarise(Ticketvol =n()) %>%
                                        dplyr::mutate(TicketProp = paste(round((Ticketvol/nrow(ins_data_set))*100,2),"%"))  %>%  dplyr::arrange(-Ticketvol))
    
    return(Unigram_Summ_data)
    
  }
  #######################################################
  GetBigram_SummData<- function(ins_data_set) {
    #Summarization of data based on the category/pattern:
    Bigram_Summ_data<- as.data.frame(ins_data_set %>%  dplyr::filter(Bigram != 0) %>%
                                       dplyr::group_by(Bigram) %>%
                                       dplyr::summarise(Ticketvol =n()) %>%
                                       dplyr::mutate(TicketProp = paste(round((Ticketvol/nrow(ins_data_set))*100,2),"%"))  %>%  dplyr::arrange(-Ticketvol))
    
    return(Bigram_Summ_data)
    
  }
  #######################################################
  GetTrigram_SummData<- function(ins_data_set) {
    #Summarization of data based on the category/pattern:
    Trigram_Summ_data<- as.data.frame(ins_data_set %>%  dplyr::filter(Trigram != 0) %>%
                                        dplyr::group_by(Trigram) %>%
                                        dplyr::summarise(Ticketvol =n()) %>%
                                        dplyr::mutate(TicketProp = paste(round((Ticketvol/nrow(ins_data_set))*100,2),"%"))  %>%  dplyr::arrange(-Ticketvol))
    
    return(Trigram_Summ_data)
    
  }
  #######################################################
  options(shiny.maxRequestSize = 1024*1024^2)
  #Shiny App
  shinyApp (
    
    ui= dashboardPage(
      dashboardHeader(title = "Pattern Extraction",titleWidth=350),
      dashboardSidebar(width = 300,
                       conditionalPanel(condition="input.conditionedPanels==1",
                                        fileInput('dataset', 'Choose CSV File',accept=c('.csv')),
                                        actionButton("Validate","validate"),
                                        tags$hr(),
                                        uiOutput("TextVar"),
                                        uiOutput("Topvar"),
                                        uiOutput("Key"),
                                        uiOutput("runbutton"))
      ),
      dashboardBody(
        
        tabsetPanel(id="conditionedPanels",
                    tabPanel("Outputs",value = 1,
                             fluidRow(column(width=12,h4("Data Summary",style="color:darkgreen"),DT::dataTableOutput("dataSummary"))),
                             uiOutput("dataSummary_download"),
                             tags$hr(),
                             fluidRow(column(width=6,h4("Unigram Pattern",style="color:darkgreen"),DT::dataTableOutput("Unigram")),
                                      column(width=6,h4("Unigram WordCloud",style="color:darkgreen"),plotOutput("Unigram_wordcloud"))),
                             tags$hr(),
                             fluidRow(column(width=12,uiOutput("UnigramPattern")),
                                      column(width=12,DT::dataTableOutput("Unigram_OriginalText"))),
                             tags$hr(),
                             fluidRow(column(width=6,h4("Bigram Pattern",style="color:darkgreen"),DT::dataTableOutput("Bigram")),
                                      column(width=6,h4("Bigram WordCloud",style="color:darkgreen"),plotOutput("Bigram_wordcloud"))),
                             tags$hr(),
                             fluidRow(column(width=12,uiOutput("BigramPattern")),
                                      column(width=12,DT::dataTableOutput("Bigram_OriginalText"))),
                             tags$hr(),
                             fluidRow(column(width=6,h4("Trigram Pattern",style="color:darkgreen"),DT::dataTableOutput("Trigram")),
                                      column(width=6,h4("Trigram WordCloud",style="color:darkgreen"),plotOutput("Trigram_wordcloud"))),
                             tags$hr(),
                             fluidRow(column(width=12,uiOutput("TrigramPattern")),
                                      column(width=12,DT::dataTableOutput("Trigram_OriginalText"))),
                             tags$hr(),
                             uiOutput("Pattern_download")
                    )
        )
      )
    ),
    
    server= function(input, output, session) {
      
      observeEvent(input$Validate,{
        
        input$Validate # Re-run when button is clicked
        
        withProgress(message = 'Validating...', value = 0, {
          
          incProgress(0.25, detail = " 25%")
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"),fileEncoding = "latin1")
          Column_names <- colnames(ins_data_set)
          
          output$TextVar <- renderUI({
            selectInput("textField", label = "Select Text Field for mining:",  c("--select--", Column_names))
          })
          
          output$Key <- renderUI({
            fileInput('keyword', 'Choose Keyword CSV File',accept=c('.csv'))
          })
          
          output$Topvar <- renderUI({
            numericInput("top", "Display Top Pattern:", 15, min = 1, max = 200)
          })
          
          output$runbutton <- renderUI({
            actionButton("run","Run")
          })
          
          incProgress(0.5, detail = " 50%")
          
          dataSummary <- reactive({
            
            validate(
              need(input$Validate != 0, "Please Upload Date & Validate")
            )
            
            isolate({
              summary<- Unique_NA_counts(ins_data_set)
              return(summary)
            })
          })
          
          incProgress(0.75, detail = " 75%")
          
          output$dataSummary<- DT::renderDataTable((datatable(dataSummary())),filter='top',options=list(autoWidth=TRUE))
          
          output$dataSummary_download <- renderUI({
            fluidRow(
              column(width=6,h4("Download Data Summary Table",style="color:darkgreen")),
              column(width=3,downloadButton('dataSummary_downloader',"Download Table"))
            )
          })
          
          output$dataSummary_downloader <- downloadHandler(
            filename = "DataSummary.csv",
            content = function(file) {
              write.csv(dataSummary(), file,row.names = F) })
          
          incProgress(1, detail = " 100%")
        })
      })
      
      observeEvent(input$run,{
        
        input$run
        
        withProgress(message = 'Processing...', value = 0, {
          
          incProgress(0.05, detail = " 5%")
          
          inFile <- input$dataset
          ins_data_set <- read.csv(inFile$datapath,header = T,strip.white = T,
                                   na.strings = c(""," ","NA","NULL","na","null"),fileEncoding = "latin1")
          Column_names <- colnames(ins_data_set)
          
          incProgress(0.1, detail = " 10%")
          
          
          output$Pattern_download <- renderUI({
            fluidRow(
              column(width=6,h4("Download Data with Pattern File",style="color:darkgreen")),
              column(width=3,downloadButton('dataPattern_download',"Download Data"))
            )
          })
          
          incProgress(1, detail = " 100%")
          
          ngram_output <- reactive({
            
            validate(
              need(input$run != 0,"Please Upload Data & Run it")
            )
            isolate({
              withProgress(message = 'Processing...', value = 0, {
                
                inFileKey <- input$keyword
                if(!is.null(inFileKey))
                {
                  keywords <- read.csv(inFileKey$datapath,header = T,strip.white = T,fileEncoding = "latin1")
                  keywords <- as.character( keywords[,1])
                  keywords <- c(stopwords("english"), keywords)
                  Keywordstoremove <- as.data.frame(keywords)
                  colnames(Keywordstoremove) <- "Keyword"
                }
                else
                {
                  Keywordstoremove <- as.data.frame(stopwords("english"))
                  colnames(Keywordstoremove ) <- c("Keyword")
                }
                
                incProgress(0.1, detail = " 10%")
                total_rows <- nrow(ins_data_set)
                
                Textcolumn <- input$textField
                ins_data_set$TextField_Original <- ins_data_set[,which(colnames(ins_data_set)==Textcolumn)]
                
                preproccessdata <- Text_Processing(ins_data_set$TextField_Original,Keywordstoremove$Keyword)
                
                incProgress(0.25, detail = " 25%")
                
                ins_data_set$Unigram <- ngram_generation(preproccessdata,1)
                Unigram_Summ_data <- GetUnigram_SummData(ins_data_set) # get Unigram with correct Freq
                topUnigram<-as.data.frame(head(Unigram_Summ_data,input$top))
                names(topUnigram)[names(topUnigram) == "Unigram"] <- "Pattern"
                
                incProgress(0.5, detail = " 50%")
                
                ins_data_set$Bigram <- ngram_generation(preproccessdata,2)
                Bigram_Summ_data <- GetBigram_SummData(ins_data_set)
                topBigram<-as.data.frame(head(Bigram_Summ_data,input$top))
                names(topBigram)[names(topBigram) == "Bigram"] <- "Pattern"
                
                incProgress(0.75, detail = " 75%")
                
                ins_data_set$Trigram <- ngram_generation(preproccessdata,3)
                Trigram_Summ_data <- GetTrigram_SummData(ins_data_set)
                topTrigram<-as.data.frame(head(Trigram_Summ_data,input$top))
                names(topTrigram)[names(topTrigram) == "Trigram"] <- "Pattern"
                
                return(list(topUnigram=topUnigram,unigramdf=Unigram_Summ_data,
                            topBigram=topBigram,bigramdf=Bigram_Summ_data,
                            topTrigram=topTrigram,trigramdf=Trigram_Summ_data,
                            dataset_pattern=ins_data_set))
                
                incProgress(1, detail = " 100%")
                
              })
            })
          })
          
          output$Unigram <- DT::renderDataTable((datatable(ngram_output()$topUnigram)),filter='top',options=list(autoWidth=TRUE))
          
          output$Unigram_wordcloud<- renderPlot({
            plotdata <- ngram_output()$unigramdf
            colorPalette<-"Dark2"
            set.seed(1234)
            wordcloud(plotdata$Unigram,plotdata$Ticketvol, random.order=FALSE, rot.per=0.35, scale = c(3,1),
                      use.r.layout=FALSE, colors= brewer.pal(11, colorPalette),min.freq = 1 )
          })
          
          output$UnigramPattern <- renderUI({
            patterndf<- ngram_output()$topUnigram
            pattern <- patterndf$Pattern
            selectInput("unigramPattern", label = h5("Choose Unigram Pattern to get sample Description",style="color:darkgreen"),
                        c("--select--", patterndf$Pattern))
          })
          
          UnigramOriginalText <-  reactive({
            validate(
              need(input$unigramPattern!="" & input$unigramPattern!="--select--","")
            )
            isolate({
              pattern<- input$unigramPattern
              data <- ngram_output()$dataset_pattern
              unigramDes <- as.data.frame(data[data$Unigram==pattern,"TextField_Original"])
              names(unigramDes) <- "TextField_Original"
              return(head(unigramDes,50))
            })
          })
          
          output$Unigram_OriginalText <- DT::renderDataTable((datatable(UnigramOriginalText())),options=list(autoWidth=TRUE))
          
          output$Bigram <- DT::renderDataTable((datatable(ngram_output()$topBigram)),filter='top',options=list(autoWidth=TRUE))
          
          output$Bigram_wordcloud<- renderPlot({
            plotdata <- ngram_output()$bigramdf
            colorPalette<-"Dark2"
            set.seed(1234)
            wordcloud(plotdata$Bigram,plotdata$Ticketvol, random.order=FALSE, rot.per=0.35,scale = c(3,0.5),
                      use.r.layout=FALSE, colors= brewer.pal(11, colorPalette),min.freq = 1 )
          })
          
          output$BigramPattern <- renderUI({
            patterndf<- ngram_output()$topBigram
            pattern <- patterndf$Pattern
            selectInput("bigramPattern", label = h5("Choose Bigram Pattern to get sample Description",style="color:darkgreen"),
                        c("--select--", patterndf$Pattern))
          })
          
          BigramOriginalText <-  reactive({
            validate(
              need(input$bigramPattern!="" & input$bigramPattern!="--select--","")
            )
            isolate({
              pattern<- input$bigramPattern
              data <- ngram_output()$dataset_pattern
              bigramDes <- as.data.frame(data[data$Bigram==pattern,"TextField_Original"])
              names(bigramDes) <- "TextField_Original"
              return(head(bigramDes,50))
            })
          })
          
          output$Bigram_OriginalText <- DT::renderDataTable((datatable(BigramOriginalText())),options=list(autoWidth=TRUE))
          
          output$Trigram <- DT::renderDataTable((datatable(ngram_output()$topTrigram)),filter='top',options=list(autoWidth=TRUE))
          
          output$Trigram_wordcloud<- renderPlot({
            plotdata <- ngram_output()$trigramdf
            colorPalette<-"Dark2"
            set.seed(1234)
            wordcloud(plotdata$Trigram,plotdata$Ticketvol, random.order=FALSE, rot.per=0.35,scale = c(3,0.5),
                      use.r.layout=FALSE, colors= brewer.pal(11, colorPalette),min.freq = 1 )
          })
          
          output$TrigramPattern <- renderUI({
            patterndf<- ngram_output()$topTrigram
            pattern <- patterndf$Pattern
            selectInput("trigramPattern", label = h5("Choose Trigram Pattern to get sample Description",style="color:darkgreen"),
                        c("--select--", patterndf$Pattern))
          })
          
          TrigramOriginalText <-  reactive({
            validate(
              need(input$trigramPattern!="" & input$trigramPattern!="--select--","")
            )
            isolate({
              pattern<- input$trigramPattern
              data <- ngram_output()$dataset_pattern
              trigramDes <- as.data.frame(data[data$Trigram==pattern,"TextField_Original"])
              names(trigramDes) <- "TextField_Original"
              return(head(trigramDes,50))
            })
          })
          
          output$Trigram_OriginalText <- DT::renderDataTable((datatable(TrigramOriginalText())),options=list(autoWidth=TRUE))
          
          output$dataPattern_download <- downloadHandler(
            filename = "DataWithPattern.csv",
            content = function(file) {
              write.csv(ngram_output()$dataset_pattern, file,row.names = F) })
          
        })
      })
      
    }
  )
  
}
