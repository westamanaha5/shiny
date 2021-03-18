library(odbc)
library(shiny)
library(shinyFeedback)
library(dplyr)

conn_args <- config::get("dataconnection")

if (config::is_active("default")) {
  ## Connect to RDS SOW
  SOW.con <- dbConnect(odbc(),
                       Driver = conn_args$driver,
                       Server = conn_args$server,
                       Database = conn_args$database,
                       UID = conn_args$uid,
                       PWD = conn_args$pwd,
                       Port = conn_args$port)
  camps <- dbGetQuery(SOW.con, "SELECT Id, Name FROM Campaigns WHERE Name NOT LIKE '^%'")
  brands <- dbGetQuery(SOW.con, "SELECT ct.CampaignId, c.Name Advertiser, ct.Id, ct.Name Brand
                       FROM CreativeTags ct, Campaigns c
                       WHERE ct.CampaignId = c.Id
                       AND ct.Name NOT LIKE '^%'")
  
  write.csv(camps, "camps.csv", row.names = F)
  write.csv(brands, "brands.csv", row.names = F)
} else {   
  camps <- readr::read_csv("camps.csv")
  brands <- readr::read_csv("brands.csv")
}

ui <- fluidPage(useShinyFeedback(),
  titlePanel("Bulk Upload", windowTitle = "Pathmatics Bulk Upload"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      p("Instructions: Upload your Advertisers and Brands below.
        Advertiser names should be in column 1 and Brand Names in column 2."),
      
      p("Note: Names must match exactly what's in Explorer. 
        Also, brands need to include both the advertiser and brand name in order to find a match."),
      
      p("For more detailed instructions, reference the ", 
        a("Tutorial.",
          href = "https://docs.google.com/document/d/1uhgtSO_W9Cu9qR4R3LNSX-hbyRwKAIVDn8Fk52IY9QQ/edit?usp=sharing", 
          target = "_blank")),
        
      fileInput(inputId = "file", 
                label = p(h3("Upload File"),
                  checkboxInput(inputId = "header", 
                                label = "My Report Includes headers",
                                value = T)
                  ),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
    ),
      
      
    mainPanel(
      h1(img(src = "logo.png", height = 40, width = 30, style="vertical-align: bottom"), 
         "New Report Builder Link will appear below"),
      
      hr(),
      
      textOutput(outputId = "instructions"),
      
      tableOutput(outputId = "example_table"),
      
      htmlOutput(outputId = "link"),
      
      uiOutput(outputId = "mismatched_title"),
      
      uiOutput(outputId = "download_button"),
      
      tableOutput(outputId = "mismatched")
      
    )
  )
)

server <- function(input, output) {

  # To do: How do we give a better error message if format is bad?
  
  # Give some instructions in the main panel if no file has been uploaded
  output$instructions <- renderText({
    if(is.null(input$file)){
      "Upload your csv file in the space to the left. Be sure to use a format like the table below."
    }
  })
  
  output$example_table <- renderTable({
    if(is.null(input$file)){
      return(tibble(Advertiser=c("Nike", "Procter & Gamble"), Brand=c("", "Gillette"))) 
    }
  })
  
  # Read in the csv and lookup the Ids
  data <- reactive({
    
    inFile <- input$file
    
    req(inFile)
    
    df <- readr::read_csv(inFile$datapath, col_names = input$header)

    # If only single column given, assume advertisers are in column 1 and add empty brand column    
    if (length(names(df)) == 1) {
      df$Brand <- ""
    } 

    if(!("Advertiser" %in% names(df))){
      names(df)[1] <- "Advertiser" 
    }
    
    if(!("Brand" %in% names(df))){
      names(df)[2] <- "Brand" 
    }
    
    
    df$Advertiser <- as.character(df$Advertiser)
    df$Brand <- as.character(df$Brand)
    
    df2 <- left_join(df, camps, by = c("Advertiser" = "Name"))
    
    df2 <- df2 %>%
      rename(AdvertiserId = Id)
    
    df2 <- left_join(df2, brands, by = c("Advertiser" = "Advertiser", 
                                         "Brand" = "Brand",
                                         "AdvertiserId" = "CampaignId"))
    
    df_download <- df2 %>% transmute(Advertiser, AdvertiserId, Brand,
                                     BrandId = Id,
                                     IdString = if_else(is.na(Id), as.character(AdvertiserId), 
                                                        paste(AdvertiserId, Id, sep = "%7C")
                                                        )
                                     )
    
    df_download <- unique(df_download)
    
    df_download
    
  })
  
  r_matched <- reactive({
    
    df3 <- data() %>% filter(!is.na(IdString))
    
  })
  
  # Download the data with Ids to csv
  output$download <- 
    downloadHandler(
      filename = "AdvertiserIds.csv",
      content = function(file) {
        write.csv(data(), file, row.names = F)
      })
  
  # Render the Download button
  output$download_button <- renderUI({
    req(count(r_matched()) > 0)
    downloadButton(outputId = "download", "Download File with Ids")
  })
  
  output$link <- renderUI({
    
    id_string <- r_matched()$IdString %>% paste(collapse = "%2C")
    
    RB_link <- paste0("https://explorer.pathmatics.com/ReportBuilder/?AdvertiserBrandIdsString=", id_string)
    
    match_count <- count(r_matched())
    
    if(match_count == 0){
      validate("No Advertisers matched. Please check that Advertiser names are in column 1 and Brand names are in column 2 of your file.")
    }
    
    adv_string <- if (match_count == 1) {
      "Advertiser"
    } else { "Advertisers" }
    
    div(a(h3("New Report Builder Link "), href=RB_link, target="_blank"),
        h4(paste0(match_count, " ", adv_string, " Matched"), style = "color:green"), 
        hr())
    
  })
  
  r_mismatched <- reactive({ data() %>% filter(is.na(IdString)) })
  mismatch_count <- reactive({ count(r_mismatched()) })
  
  # Render the count of mismatched
  output$mismatched_title <- renderUI({ 
    req(mismatch_count() > 0)
    
    adv_string <- if (mismatch_count() == 1) {
      "Advertiser"
    } else { "Advertisers" }
    
    div(
      p(paste0(mismatch_count(), " Mismatched ", adv_string, ":"), style = "color:red"),
      p("You can download the full list of names that did and did not match by clicking the button below.")
    )
    
  })
  
  output$mismatched <- renderTable({ 
    req(mismatch_count() > 0)
    r_mismatched() %>% 
      transmute(Advertiser = iconv(Advertiser, "UTF-8", "UTF-8", ""), 
                AdvertiserId,
                Brand = iconv(Brand, "UTF-8", "UTF-8", ""), 
                BrandId, IdString) 
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)