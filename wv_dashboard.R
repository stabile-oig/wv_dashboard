#### wv dashboard demo 1.0 ####

library(shiny)
library(tidyverse)
library(beepr)
library(later)
library(stringdist)
library(DT)
library(progress)
library(janitor)
library(readxl)
library(plotly)
library(zipcodeR)

## Disable scientific notation
options(scipen=999)


#### data prep ####
# load and clean data for maps 
Data.Dir <- "o:\\Data_Analytics\\Support - Investigations\\Projects\\2025\\AUSA case dashboard\\WV data"
WV.File  <- "RawWVdata.xlsx"

WV.Data  <- file.path(Data.Dir, WV.File)
WV.data  <- readxl::read_xlsx(WV.Data)

case.File  <- "West Virgina Cases - approved after 05.01.2015.xlsx"
case.Sheet <- "Sheet1"

case.Data  <- file.path(Data.Dir, case.File)
caseFed.Range <- "A2:E32"
WV.casesFed <- read_excel(case.Data, sheet = case.Sheet, range = caseFed.Range)

caseAlleg.Range <- "A35:E41"
WV.casesAlleg <- read_excel(case.Data, sheet = case.Sheet, range = caseAlleg.Range)

county.Sheet <- "CityCountyDis"
county.file <- "West Virigina City, County, District.xlsx"
county.Data  <- file.path(Data.Dir, county.file)
districts <- read_excel(county.Data, sheet = county.Sheet)

## Shortening Zip column to combine Zipcodes for analysis
WV.data <- WV.data %>% mutate(fulZip = str_trim(Zip)) ## Trimming white space around zipcode so can pull out first five digits
## Shortening Zipcode to only first five digits
WV.data$Zipcode <- substr(WV.data$fulZip, 1, 5)

total_table <- function(col) {
  WV.data %>%
    group_by({{col}}) %>%
    summarise(
      "Total Dollars" = sum(Amount, na.rm = TRUE),
      "Total POs" = n_distinct(`PO#`)) %>%
    arrange(desc(`Total Dollars`))
}

## Getting County FIPS codes
json.counties <- rjson::fromJSON(url("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"))
county_fips_df <- data.frame(
  FIPS = sapply(json.counties$features, function(x) x$id),
  County = sapply(json.counties$features, function(x) x$properties$NAME),
  State = sapply(json.counties$features, function(x) x$properties$STATE),
  stringsAsFactors = FALSE
  ) %>%
  filter(State == "54")

# map counties onto AUSA districts
county_districts <- left_join(districts, county_fips_df, by = "County") %>%
  clean_names()

## Changing Alderson to 54075
county_districts$fips[which(county_districts$city_town == "Alderson")] <- "54075"


# load and clean data for search table 
wv_data_raw <- readxl::read_xlsx("O:/Data_Analytics/Support - Investigations/Projects/2025/AUSA case dashboard/WV data/RawWVdata.xlsx") %>%
  clean_names(); beep(2)

wv_data_raw <- wv_data_raw %>%
  mutate(across(everything(), as.character)); beep(2)

# define cleaning steps with safety checks
clean_data <- function(df) {
  df <- clean_names(df)
  df %>%
    mutate(across(where(is.character), str_trim)) %>%
    filter(vendor_type != "EMPLOYEE") # remove employee info
}; beep(2)

# safely apply cleaning to all raw data with progress bar
wv_data_clean <- {
  raw <- list(wv_data_raw = wv_data_raw)
  pb <- progress_bar$new(
    format = "Cleaning  [:bar] :current/:total (:percent)",
    total = length(raw), clear = FALSE, width = 60
  )
  map2(raw, names(raw), function(df, name) {
    pb$tick()
    result <- safely(clean_data)(df)
    if (!is.null(result$error)) {
      message("\n❌ Error cleaning ", name, ": ", result$error$message)
    } else {
      message("\n✅ Cleaned ", name)
    }
    result
  })
}; beep(2)

state_map <- tibble(
  name = tolower(state.name),
  abbr = tolower(state.abb)
)

#### plot dataviz ####
districtMap <- plot_ly() %>%
  add_trace(
    type="choroplethmapbox",
    geojson=json.counties,
    locations=county_districts$fips,
    z=as.numeric(as.factor(county_districts$ausa_district)),
    colorscale="Set1",
    featureidkey="id",
    text=~paste("County:", county_districts$county, "<br>AUSA District:", county_districts$ausa_district),
    hovertemplate="%{text}<extra></extra>",
    showscale = FALSE
  ) %>% 
  layout(title = "West Virginia Counties by AUSA District", showlegend = FALSE,
         mapbox=list(
           style="carto-positron",
           zoom = 6,
           center = list(lon = -80.5, lat = 39)) # Centering on West Virginia
  )

districtMap

By.Zip <- total_table(Zipcode)
zip_info <- zipcodeR::reverse_zipcode(By.Zip$Zipcode)
zipCounty <- zip_info %>% select(zipcode,county,state)
zipCountyDols <- left_join(By.Zip, zipCounty, by = c("Zipcode" = "zipcode"))

## table of NA counties
zipCountyDols %>% filter(is.na(county) | state != "WV")

## Adding in County based on city listed in WV.data to incorrect Zipcodes in above table
zipCountyDols$county[which(zipCountyDols$Zipcode == "23305")] <- "Kanawha"
zipCountyDols$county[which(zipCountyDols$Zipcode == "32256")] <- "Monongalia"
zipCountyDols$county[which(zipCountyDols$Zipcode == "83110")] <- "Upshur"

## Removing work "County" from county names
zipCountyDols$County <- gsub(" County", "", zipCountyDols$county, ignore.case = TRUE)

## consolidating county_district data
countyBydistrict <- county_districts %>%
  select(county, ausa_district, fips) %>%
  distinct()

## Combining datasets by counties
County_dolls <- full_join(zipCountyDols, countyBydistrict, by = c("County" = "county" ))

## Grouping Dollars and POs by County
By.County <- County_dolls %>% 
  group_by(County, fips, ausa_district) %>%
  summarise(
    "Total Dollars" = sum(`Total Dollars`, na.rm = TRUE),
    "Total POs" = sum(`Total POs`, na.rm = TRUE)) %>%
  arrange(desc(`Total Dollars`))

## Removing "Pocahontas/Greenbrier" from By.County
By.County <- By.County %>%
  filter(County != "Pocahontas;Greenbrier")

## Plotting Dollars by County
countyDolMap <- plot_ly() %>% 
  add_trace(
    type="choroplethmapbox",
    geojson=json.counties,
    locations=By.County$fips,
    z=as.numeric(as.factor(By.County$`Total Dollars`)),
    colorscale="Viridis",
    featureidkey="id",
    text=~paste("County:", By.County$County, "<br>AUSA District:", By.County$ausa_district, 
                "<br>Total Dollars: $", formatC(By.County$`Total Dollars`, format = "f", big.mark = ",", digits = 2),"<br>Total POs: ", By.County$`Total POs`),
    hovertemplate="%{text}<extra></extra>"
  ) %>% 
  layout(title = "West Virginia Counties by DOT Spending",
         mapbox=list(
           style="carto-positron",
           zoom = 6,
           center = list(lon = -80.5, lat = 39))) # Centering on West Virginia

countyDolMap

#### ui layout ####
ui <- navbarPage(
  id = "wv_portal",
  title = "wv Portal",
  tabPanel("Home",
           fluidPage(
             titlePanel("West Virginia Dashboard"),
             fluidRow(
               column(6,
                      plotly::plotlyOutput("countyDolMap", height = "300px"),
                      actionButton("go_nowhere", "Coming Soon")
               ),
               column(6,
                      tags$img(src = "image2.png", height = "300px", width = "100%"),
                      actionButton("go_search", "search data")
               )
             )
           )
  ),
  tabPanel("Search",
           value = "Search",
           fluidPage(
             tags$head(
               tags$style(HTML(
                 ".centered-content { min-height:100vh; display:flex; flex-direction:column; align-items:center; justify-content:center; padding:20px; }
           .loading-text { margin-top:15px; font-style:italic; color:gray; text-align:center; }
           .datatable-wrapper { width:90%; margin-top:20px; }"
               ))
             ),
             h2("West Virginia Search Portal Prototype"),
             div(class = "centered-content",
                 div(style = "display:flex; width:60%; gap:10px;",
                     selectInput("search_filter", NULL,
                                 choices = c("Mode" = "oa", "Vendor" = "vendor", "PO number" = "po_number", "State" = "st"),
                                 width = "30%"),
                     textInput("search", NULL,
                               placeholder = "search wv_data...",
                               width = "70%")
                 ),
                 uiOutput("loading_msg"),
                 uiOutput("results_box"),
                 div(style = "margin-top: 15px;",
                     actionButton("clear_search", "Clear Search", class = "clear-btn"),
                     actionButton("go_home", "Back to Home", class = "back-btn")
                 )
             )
           )
  )
)

#### server logic ####
server <- function(input, output, session) {
  # navigation
  observeEvent(input$go_search, {
    updateNavbarPage(session, "wv_portal", selected = "Search")
  })
  observeEvent(input$go_home, {
    updateNavbarPage(session, "wv_portal", selected = "Home")
  })
  # search reset
  observeEvent(input$clear_search, {
    updateTextInput(session, "search", value = "")
    search_results(NULL)
  })
  
  # render county dollars map
  output$countyDolMap <- renderPlotly({
    countyDolMap
  })
  
  searching <- reactiveVal(FALSE)
  search_results <- reactiveVal(NULL)
  
  observeEvent(input$search, {
    req(input$search)
    local_query <- str_squish(input$search)
    local_filter <- input$search_filter
    q_lower <- tolower(local_query)
    searching(TRUE)
    
    later::later(function() {
      max_dist <- 0.2
      matches <- integer()
      vec_clean <- tolower(str_squish(wv_data_raw[[local_filter]]))
      dist_vals <- stringdist::stringdist(q_lower, vec_clean, method = "jw")
      contains <- str_detect(vec_clean, fixed(q_lower))
      
      if (local_filter == "oa") {
        acronyms <- vec_clean |> 
          str_replace_all("[^a-z0-9\\s]", "") |> 
          str_split("\\s+") |> 
          map_chr(~ paste0(substr(.x[!tolower(.x) %in% c("of","the","and","for","to")],1,1), collapse = ""))
        exact_acr <- which(acronyms == q_lower)
        dist_acr <- stringdist::stringdist(q_lower, acronyms, method = "jw")
        acr_fuzzy <- which(dist_acr <= max_dist)
        matches <- unique(c(which(dist_vals <= max_dist | contains), exact_acr, acr_fuzzy))
      } else if (local_filter == "st") {
        name_idx <- match(q_lower, state_map$name)
        if (!is.na(name_idx)) {
          abbr <- state_map$abbr[name_idx]
          matches <- which(tolower(wv_data_raw$st) == abbr)
        } else {
          st_vec <- tolower(str_squish(wv_data_raw$st))
          dist_vals <- stringdist::stringdist(q_lower, st_vec, method = "jw")
          contains <- str_detect(st_vec, fixed(q_lower))
          matches <- which(dist_vals <= max_dist | contains)
        }
      } else {
        matches <- which(dist_vals <= max_dist | contains)
      }
      
      res <- wv_data_raw[matches, , drop = FALSE]
      search_results(res)
      searching(FALSE)
    }, delay = 0.1)
  })
  
  output$results_table <- renderDT({
    df <- search_results()
    req(!searching())
    if (is.null(df) || nrow(df) == 0) {
      datatable(data.frame(Message = "no matches found"), options = list(dom = 't'), rownames = FALSE)
    } else {
      disp <- df %>% rename_with(~str_to_title(gsub("_", " ", .)))
      datatable(disp, selection = 'single', options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE,
                                                           columnDefs = list(list(className = 'dt-right', targets = which(names(disp) == 'Amount')-1))), rownames = FALSE) %>%
        formatCurrency('Amount', '$', digits = 2, interval = 3, mark = ",", dec.mark = ".")
    }
  })
  
  observeEvent(input$results_table_rows_selected, {
    sel <- input$results_table_rows_selected
    df <- search_results()
    if (length(sel)) {
      entry <- df[sel, , drop = FALSE]
      showModal(modalDialog(
        title = "Contract Detail",
        div(style = "overflow-x: auto; max-height: 400px; width: 100%;", renderTable(entry, rownames = TRUE)),
        footer = tagList(modalButton("Close"), tags$button(type = "button", onclick = "window.print();", class = "btn btn-primary", "Print")),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  
  output$loading_msg <- renderUI({ if (searching()) div(class = 'loading-text', 'searching...') })
  output$results_box <- renderUI({
    df <- search_results()
    if (!is.null(df) && nrow(df) > 0 && !searching()) {
      div(class = 'datatable-wrapper', DTOutput('results_table'))
    }
  })
}

#### run it ####
shinyApp(ui, server)
