#### wv dashboard demo 1.0 ####

library(shiny)
library(tidyverse)
library(beepr)
library(later)
library(stringdist)
library(DT)
library(progress)
library(janitor)

#### demo data ####
wv_data_raw <- readxl::read_xlsx("O:/Data_Analytics/Support - Investigations/Projects/2025/AUSA case dashboard/WV data/RawWVdata.xlsx") %>%
  clean_names(); beep(2)

wv_data_raw <- wv_data_raw %>%
  mutate(across(everything(), as.character)); beep(2)

# define cleaning steps with safety checks
clean_data <- function(df) {
  df <- clean_names(df)
  df %>%
    mutate(across(where(is.character), str_trim)) %>%
    filter(vendor_type != "EMPLOYEE") %>%
    filter(pay_date >= as.Date("2024-01-01"))
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

#### ui layout ####
ui <- navbarPage(
  id = "wv_portal",
  title = "wv Portal",
  tabPanel("Home",
           fluidPage(
             titlePanel("West Virginia Dashboard"),
             fluidRow(
               column(6,
                      tags$img(src = "image1.png", height = "300px", width = "100%"),
                      actionButton("go_nowhere", "Coming Soon")
               ),
               column(6,
                      tags$img(src = "image2.png", height = "300px", width = "100%"),
                      actionButton("go_search", "Go to Search")
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
