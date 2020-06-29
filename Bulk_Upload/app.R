library(odbc)
library(shiny)
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

ui <- fluidPage(
  titlePanel("Bulk Upload", windowTitle = "Pathmatics Bulk Upload"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      p("Instructions: Upload your Advertisers and Brands below.
        Advertiser names should be in column 1 and Brand Names in column 2."),
      
      p("Note: Names must match exactly what's in Explorer. 
        Also, brands need to include both the advertiser and brand name in order to find a match."),
        
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
      
      p("After you have uploaded your file, you will be able to download a file that includes Ids. 
        Use this to check if any names didn't match. Download button will appear below."),
      
      uiOutput(outputId = "download_button")
      
      # downloadButton(outputId = "download", "Download File with Ids")
      
    ),
      
      
    mainPanel(
      h1(img(src = "logo.png", height = 40, width = 30, style="vertical-align: bottom"), 
         "New Report Builder Link will appear below"),
      
      htmlOutput(outputId = "link"),
      
      tableOutput(outputId = "table"),
      
      uiOutput(outputId = "mismatched_title"),
      
      tableOutput(outputId = "mismatched")
      
    )
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    inFile <- input$file
    
    if (is.null(inFile))
      return(tibble(Advertiser="", Brand=""))
    
    df <- readr::read_csv(inFile$datapath, col_names = input$header)
    
    if (length(names(df)) == 1) {
      names(df) <- c("Advertiser")
      df2 <- left_join(df, camps, by = c("Advertiser" = "Name"))
      
      ids <- df2 %>%
        filter(Id != "") %>%
        select(Id) 
      
      df_download <- df2
      
      mismatched <- df2 %>% filter(is.na(Id)) %>% select(Advertiser)
      
      id_string <- ids$Id %>% paste(collapse = "%2C")
  
    } else { 
      names(df) <- c("Advertiser", "Brand") 
      
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
      
      mismatched <- df_download %>% filter(is.na(IdString))
      
      df3 <- df_download %>% filter(!is.na(IdString))
      
      id_string <- df3$IdString %>% paste(collapse = "%2C")
      
    }
    
    RB_link <- paste0("https://explorer.pathmatics.com/ReportBuilder/?AdvertiserBrandIdsString=", id_string)
    
    output$link <- renderUI({
      h3(a("New Report Builder Link", href=RB_link, target="_blank"))
    })
    
    mismatch_count <- count(mismatched)
    
    if (mismatch_count > 0 ) {
      output$mismatched_title <- renderUI({ p("Mismatched Advertisers:") })
      output$mismatched <- renderTable({ mismatched })
      
    }
    
    output$download <- 
      downloadHandler(
      filename = "AdvertiserIds.csv",
      content = function(file) {
        write.csv(df_download, file, row.names = F)
      })
    
    output$download_button <- renderUI({
      downloadButton(outputId = "download", "Download File with Ids")
    })
    
    return(head(df_download))
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)