#### Load Packages ####
library(shiny)
library(shinyBS)
library(udpipe)
library(wordcloud)
library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud2)
library(webshot)
library(htmlwidgets)
webshot::install_phantomjs() 
webshot:::find_phantom()

# Allow file sizes up to 100 MB
options(shiny.maxRequestSize=100*1024^2)

# Pathmatics Colors
pm_colors <- c("#00A3AD","#1B365D","#D62598","#DA291C","#FF5F00","#00AB84","#6A2A5B","#CE0056","#8DB9CA","#F93822","#FEDD00")  


#### Udpipe model is used to annotate text data (NLP) #### 
ud_model <- udpipe_download_model(language = 'english')
ud_model <- udpipe_load_model(ud_model$file_model)


#### Get Top Creatives from Report Builder function ####
# This function takes a dataframe loaded from the Report Builder CSV 
# and returns the top creatives by spend
get_top_creatives_from_RB <- function(df, top_n_creatives=500, 
                                     advertiser_filter="All", 
                                     brand_filter="All"){
  
  # If the user has advertiser or brand filters applied, only select creatives from that advertiser/brand
  if (advertiser_filter != "All") {
    df <- df %>% filter(Advertiser == advertiser_filter)
  }
  
  if (brand_filter != "All") {
    df <- df %>% filter(Brand == brand_filter)
  }

  # Need Creative ID or Link to Creative to group by Creative  
  if ("Creative ID" %in% names(df)) {
    df <- df %>% 
      filter(`Creative Text` != "") %>% 
      group_by(`Creative ID`, `Creative Text`) %>%
      summarise(Spend = sum(Spend)) %>%
      ungroup() %>%
      arrange(desc(Spend)) %>% 
      select(`Creative Text`) %>% 
      head(top_n_creatives)
  } else if ("Link to Creative" %in% names(df)){
    df <- df %>% 
      filter(`Creative Text` != "") %>% 
      group_by(`Link to Creative`, `Creative Text`) %>%
      summarise(Spend = sum(Spend)) %>%
      ungroup() %>%
      arrange(desc(Spend)) %>% 
      select(`Creative Text`) %>% 
      head(top_n_creatives)
  } else { # Otherwise just take top 500 rows by spend
    df <- df %>% 
      filter(`Creative Text` != "") %>% 
      arrange(desc(Spend)) %>% 
      select(`Creative Text`) %>% 
      head(top_n_creatives)
  }
  
  return(df)
  
}


#### Get Annotated Data function ####
# This function takes a dataframe with the top creatives' text 
# and returns a tokenized, annotated data set with part of speech tagging and lemmatization
get_annotated_data <- function(df){
  
  # fix for ' - right single quotation mark
  df <- df %>% 
    mutate(`Creative Text` = str_replace_all(`Creative Text`, "[\u2018\u2019\u201A\u201B\u2032\u2035]", "'"))
  
  texts <- unlist(df[1])
  
  df_annotated <- udpipe_annotate(ud_model, x = texts)
  df_annotated <- as.data.frame(df_annotated)
  
}


#### Get Top Single Words function ####
# This function returns the top single words appearing in the creative text
# if udpipe is marked FALSE, simple tokenization is used since it is significantly faster
get_top_single_words <- function(df, nwords, use_udpipe=T){ 

  if(use_udpipe == F) {
    # Simple tokenization
    top_single_words <- df %>%
      unnest_tokens(output = term, input = `Creative Text`, token = "words", to_lower = F) %>% 
      mutate(term_lower = tolower(term)) %>%
      filter(!(term_lower %in% stop_words$word)) %>%
      filter(nchar(term) > 1) %>%
      group_by(term, term_lower) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq)) %>%
      group_by(term_lower) %>%
      summarise(freq = max(freq),
                term = first(term)) %>%
      arrange(desc(freq)) %>%
      transmute(term, freq, ngram = 1) %>% 
      head(nwords)
  } else {
    # With annotated data
    top_single_words <- df %>%
      filter(!(upos %in% c("PUNCT", "PART"))) %>%
      group_by(lemma, upos) %>%
      mutate(freq = n(),
             word = first(token)) %>%
      summarise(freq = max(freq),
                word = first(word)) %>%
      arrange(desc(freq)) %>%
      filter(!(lemma %in% stop_words$word)) %>%
      ungroup() %>%
      filter(nchar(word) > 1) %>%
      head(nwords) %>%
      group_by(word) %>%
      summarise(freq = sum(freq)) %>%
      ungroup %>%
      arrange(desc(freq)) %>%
      transmute(term = word, freq = freq, ngram = 1)
  }
  
  return(top_single_words)
  
}

#### Get Top Phrases function ####
# This uses the annotated text data to parse relevant multi-word phrases
get_top_phrases <- function(df, nwords, ngrams = 3) {
  phrase_tag <- as_phrasemachine(df$upos, type = "upos")
  phrases <- keywords_phrases(x = phrase_tag,
                              term = df$token,
                              pattern = "(A|N)+N(P+D*(A|N)*N)*", # To do: understand this pattern and others that might be beneficial
                              is_regex = T, ngram_max = ngrams, detailed = F)
  
  top_phrases <- phrases %>% head(nwords) %>% transmute(term = keyword, freq = freq, ngram = ngram) 
  
  return(top_phrases)
  
}


#### Keyword extraction function ####
# This function combines the results of getting the top single words and the top phrases
keyword_extraction <- function(df, nwords=250, ngrams=3){
  
  top_single_words <- get_top_single_words(df, nwords)
  top_phrases <- get_top_phrases(df, nwords, ngrams)
  words_and_phrases <- bind_rows(top_single_words, top_phrases)
  
  sentences <- unique(df['sentence'])
  
  # Use to check that the key phrase actually appears within a post
  contained_in_sentences <- function(x) { any(grepl(gsub("([+.])","\\\\\\1", x), sentences$sentence, fixed=TRUE)) }
  
  all_terms <- words_and_phrases %>%
    mutate(keep = sapply(term, contained_in_sentences)) %>%
    filter(keep == T) %>%
    arrange(desc(freq, ngram))
  
  nrows <- all_terms %>% count() %>% sum()
  
  # This checks for any overlapping phrases and keywords
  for (i in 1:nrows) {
    t <- gsub("([+.])","\\\\\\1", all_terms$term[i])
    n_gram <- all_terms$ngram[i]
    
    t_freq <- all_terms %>%
      mutate(term = gsub("([+.])","\\\\\\1", term)) %>%
      filter(grepl(tolower(t), tolower(term)) & term != t) %>%
      filter(ngram == n_gram+1) %>%
      select(freq) %>% head(1)
    
    if (length(t_freq$freq) > 0) {
      t_sum <- t_freq %>% sum()
    } else t_sum <- 0
    
    all_terms$freq[i] <- all_terms$freq[i] - t_sum
    
  }
  
  return(all_terms %>% arrange(desc(freq*sqrt(ngram))))
  
}


#### Get Top Keywords from Report Builder function ####
# This function pulls the top creatives from report builder,
# and gets the top keywords.  
get_top_keywords_from_RB <- function(df, top_n_creatives=500, 
                                      advertiser_filter="All", 
                                      brand_filter="All", 
                                      n_grams=3){
  
  rb <- get_top_creatives_from_RB(df, top_n_creatives, advertiser_filter, brand_filter)
  
  if (n_grams == 1) {
    keywords <- get_top_single_words(rb, nwords = 300, use_udpipe = F)
  } else {
    
    df_an <- get_annotated_data(rb)
    
    keywords <- keyword_extraction(df_an, nwords = 200, ngrams = n_grams)
  }
  
  return(keywords)
  
}

generate_word_cloud <- function(keywords, max_words=200, min_word_size = 0.5, max_word_size=4){
  wordcloud(words = keywords$word,
            scale = c(max_word_size, min_word_size),
            freq = keywords$freq, 
            max.words = max_words,
            random.order = F, 
            colors = pm_colors)
}

# height settings for images
h <- 700
png_res <- 144
h2 <- h*png_res/72


#### UI ####
ui <- fluidPage(
  titlePanel("Word Cloud Generator", windowTitle = "Pathmatics Word Cloud Generator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      p("Instructions: Upload your Report Builder file with Creative Text below.
        After you have uploaded your report, click the Generate Word Cloud button."),
      
      p("For more detailed instructions, reference the ", 
        a("Tutorial.",
          href = "https://docs.google.com/document/d/1QE2amOn3rjUeaZELAolSAUn-lQJYVNgRjMXukxcSQBI/edit?usp=sharing", 
          target = "_blank")),
      
      fileInput(inputId = "file", 
                label = h3("Upload Report Builder File"),
                width = 300,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
    
      actionButton(inputId = "button", "Generate Word Cloud"),
      
      hr(),
      
      h3("Optional Data and Layout Adjustments"),
      
      # Collapsible panel group
      bsCollapse(id = "collapse", 
                 
                 # Data Filters panel
                 bsCollapsePanel(title = "Data Filters", style = "primary",
                                 
                                 p("Below are some filters you can apply to the data used to generate the word cloud."),
                                 
                                 p("After you have made your selections, click the Generate Word Button to update your word cloud."),
                                 
                                 tags$div(title="Filter for an advertiser (must have Advertiser in Report Builder columns)",
                                          selectInput("AdvertiserFilter", "Filter by Advertiser",
                                                      choices = c("All"),
                                                      selected = "All")
                                          ),
                                 
                                 tags$div(title="Filter for a brand (must have Brand in Report Builder columns)",
                                          selectInput("BrandFilter", "Filter by Brand",
                                                      choices = c("All"),
                                                      selected = "All")
                                          ),
                                 
                                 tags$div(title="By default, the word cloud will only consider the top 500 creatives by spend in your report.\nYou can choose to include up to 1000 creatives as inputs to the Word Cloud.",
                                          sliderInput(inputId = "num_creatives", 
                                                       label = "Number of Creatives:",
                                                       min = 100,
                                                       max = 1000,
                                                       value = 500, 
                                                       step = 50)
                                          ),
                                 
                                 tags$div(title="Phrases will load more slowly than single words only",
                                          radioButtons(inputId = "ngram_max",
                                                       label = "Should any keyword phrases in the Creative Text be included?",
                                                       choices = list(
                                                         "Single words only" = 1,
                                                         "2-word phrases" = 2,
                                                         "3-word or 2-word phrases" = 3
                                                         ), 
                                                       selected = 1)
                                          )
                                 ),
                 
                 # Layout Adjustments panel
                 bsCollapsePanel("Layout Adjustments", style = "primary",
                                 
                                 p("Use the controls below to make adjustments to the layout of the word cloud."),
                                 
                                 p("Any word cloud already generated will dynamically update as you make changes."),
                                 
                                 tags$div(
                                   title="Which type to use",
                                   radioButtons(inputId = "type", 
                                                label = "Word Cloud Type",
                                                choices = list(
                                                  "Type 1" = 1,
                                                  "Type 2" = 2
                                                  ),
                                                selected = 1)
                                 ),
                                 
                                 # Dynamically generated ui - only shown for Word Cloud Type 1
                                 uiOutput(outputId = "num_words_ui"),
                                 
                                 fluidRow(
                                   
                                   column(width = 5,
                                          uiOutput(outputId = "word_size_min_ui")
                                          ),
                                   
                                   column(width = 7,
                                          uiOutput(outputId = "word_size_max_ui")
                                          )
                                 ),
                                 
                                 # Dynamically generated ui - only shown for Word Cloud Type 2
                                 uiOutput(outputId = "wc2_shape_ui"),
                                 
                                 uiOutput(outputId = "wc2_size_ui"),
                                 
                                 uiOutput(outputId = "wc2_ellipticity_ui")
                                 
                                 )
                 )
      ),
  
    mainPanel(
      h1(img(src = "logo.png", height = 40, width = 30, style="vertical-align: bottom"), 
         "Word Cloud will appear in the space below"),
      hr(),
      
      div(h4(textOutput(outputId = 'report_error')), style = "color:red"),
      
      h4(textOutput(outputId = "topterms")), br(),
      
      tags$div(title="Click this button to download as a png",
               uiOutput("downloadpng")
               ),
      
      uiOutput("wordcloud_ui"),
      
      uiOutput("wordcloud2_ui")
      
      )
    )
  )

#### server ####
server <- function(input, output, session) {
  
  # Dynamically generate ui - only show for Word Cloud Type 1
  output$num_words_ui <- renderUI({
    if(input$type == 1){
      tags$div(
        title="Adjust the number of words that will be displayed in the Word Cloud.",
        sliderInput(inputId = "num_words",
                    label = "Number of Words:",
                    min = 100,
                    max = 300,
                    value = 200,
                    step = 10)
      )    
    }
  })
  
  output$word_size_min_ui <- renderUI({
    if (input$type == 1) {
      tags$div(
        title="Adjust the size of the smallest word in the Word Cloud.",
        sliderInput(inputId = "word_size_min",
                    label = "Size of Smallest Word",
                    min = 0.5, 
                    max = 2,
                    value = 0.5,
                    step = 0.25
        )
      )
    }    
  })
  
  output$word_size_max_ui <- renderUI({
    if (input$type == 1) {
      tags$div(
        title="Adjust the size of the largest word in the Word Cloud.",
        sliderInput(inputId = "word_size_max",
                    label = "Size of Largest Word",
                    min = 4, 
                    max = 10,
                    value = 5,
                    step = 0.25
        )
      )
    }
  })
  
  # Dynamically generate ui - only show for Word Cloud Type 2
  output$wc2_shape_ui <- renderUI({
    if (input$type == 2) {
      tags$div(
        title="Select the outline shape for the word cloud",
        radioButtons(inputId = "shape", 
                     label = "Change Shape",
                     choices = list(
                       "circle",
                       "cardioid",
                       "diamond", 
                       "triangle", 
                       "pentagon", 
                       "star"),
                     selected = "circle")
      )
    }
  })
  
  output$wc2_size_ui <- renderUI({
    if (input$type == 2) {
      tags$div(
        title="Adjust the size of the word cloud",
        sliderInput(inputId = "wc2_size",
                    label = "Word Cloud Size",
                    min = 0.2, 
                    max = 1.5,
                    value = 1,
                    step = 0.1
        )
      )
    }
  })

  output$wc2_ellipticity_ui <- renderUI({
    if (input$type == 2) {
      tags$div(
        title="Adjust the width/height ratio",
        sliderInput(inputId = "wc2_ellipticity",
                    label = div(style='width:400px;',
                                div(style='float:left;', 'Wider'), 
                                div(style='float:right;', 'Taller')),
                    width = '400px',
                    min = 0.1, 
                    max = 2,
                    value = 0.65,
                    step = 0.05
        )
      )
    }
  })
  
  
  # Read the csv file upon user upload
  report <- reactive({
    
    req(input$file)
    
    inFile <- input$file
  
    rb <- readr::read_csv(inFile$datapath, skip = 1)
      
    if (!('Creative Text' %in% names(rb))) {
      rb <- readr::read_csv(inFile$datapath)
    }
    
    if (!('Creative Text' %in% names(rb))) {
      rb <- NULL
    } 
    
    rb
    
  })
  
  output$report_error <- renderText({
    if(is.null(report())){
     "Column 'Creative Text' not found in report. \n 
      Please check that 'Creative Text' is included in your report column headers." 
    }
  })
  
  # Functions that are used to populate AdvertiserFilter and BrandFilter
  get_advertiser_names <- function(df){
    if(!('Advertiser' %in% names(df))){
      return("All")
    } else { return(unique(c("All", df$Advertiser)))}
  }
  
  get_brand_names <- function(df){
    if(!('Brand' %in% names(df))){
      return("All")
    } else { return(unique(c("All", df$Brand))) }
  }

  # This updates the filters for advertiser and brand
  observe({
    advertiser_names <- get_advertiser_names(report())
  
    updateSelectInput(session, "AdvertiserFilter",
                    label = "Filter by Advertiser",
                    choices = advertiser_names, 
                    selected = "All" # To do, multiple = T
                    )
    
    brand_names <- get_brand_names(report())
    
    updateSelectInput(session, "BrandFilter",
                      label = "Filter by Brand",
                      choices = brand_names, 
                      selected = "All" # To do, multiple = T
                      )
  })
  
  # When the Generate Word Cloud button is clicked, get the keywords and cache those values
  keyword_data <- eventReactive(input$button, {

    keywords <- get_top_keywords_from_RB(report(), 
                                         top_n_creatives = input$num_creatives,
                                         advertiser_filter = input$AdvertiserFilter,
                                         brand_filter = input$BrandFilter,
                                         n_grams = input$ngram_max)
    
    kw <- keywords %>% transmute(word = term, freq = freq*sqrt(ngram)) %>% filter(freq > 0)
    
    return(kw)

  })

  # Render the top 10 keywords
  output$topterms <- renderText({
    top_terms <- head(keyword_data(), 10)
      
    paste0("Top 10 Keywords: ", paste(top_terms$word, collapse = ", "))
  })
  
  nwords <- reactive({
    if(is.null(input$num_words)){
      return(200)
    } else { return(input$num_words) }
    
  })
  
  wsize_min <- reactive({
    if(is.null(input$word_size_min)){
      return(0.5)
    } else { return(input$word_size_min) }
  })
  
  wsize_max <- reactive({
    if(is.null(input$word_size_max)){
      return(5)
    } else { return(input$word_size_max) }
    
  })

  # Generate the word cloud plot - should be reactive on # words and text size
  output$wordcloud <- renderPlot({
    if(input$type == 1){ 
        generate_word_cloud(keyword_data(), 
                        max_words = nwords(), 
                        min_word_size = wsize_min(), 
                        max_word_size = wsize_max())
    }
  })
  
  # Render wordcloud ui if type is 1
  output$wordcloud_ui <- renderUI({
    if(input$type == 1){
      plotOutput(outputId = "wordcloud", height = h)
    } 
  })
  
  # Word Cloud Type 2
  wc2 <- reactive({
    wordcloud2(data = keyword_data(), 
               color = rep_len(pm_colors, nrow(keyword_data())),
               size = input$wc2_size,
               # To do: why is the font different in download vs rendered plot?
               fontWeight = 'normal',
               ellipticity = input$wc2_ellipticity,
               shuffle = F,
               shape = input$shape
    )
  })
  
  # Generate the word cloud plot for type 2
  output$wordcloud2 <- renderWordcloud2({
    if(input$type == 2) {
      wc2()
    }
  })
  
  # Render wordcloud2 if type is 2
  output$wordcloud2_ui <- renderUI({
    if(input$type == 2){
      wordcloud2Output(outputId = "wordcloud2", height = h)
    } 
  })

  # Render the download button
  output$downloadpng <- renderUI({
    # download button shouldn't appear until the generate word cloud button is clicked
    if(input$button == 0){ return(NULL)}
    
    downloadButton(outputId = "download", "Download Word Cloud")

  })
  
  # Save off the Word Cloud as a png
  output$download <- downloadHandler(
    filename = "WordCloud.png",
    contentType = "image/png",
    content = function(file) {
      if(input$type == 1){
        png(filename = file, height = h2, width = h2, units = "px", res = png_res)
        generate_word_cloud(keyword_data(), 
                            max_words = nwords(), 
                            min_word_size = wsize_min(), 
                            max_word_size = wsize_max())
        dev.off()
      } else {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        saveWidget(wc2(), "wc2.html", selfcontained = F)
        webshot("wc2.html", file = file, delay = 5)
        }
      }
    )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
