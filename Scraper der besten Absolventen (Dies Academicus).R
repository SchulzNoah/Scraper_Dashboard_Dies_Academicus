if (!require("genderdata")) {
  # Wir nutzen remotes, da es auf dem Server oft stabiler ist
  if (!require("remotes")) install.packages("remotes")
  remotes::install_github("lmullen/genderdata", upgrade = "never")
}


library(rvest)
library(shiny)
library(plotly)
library(gender)
library(bslib)
library(tidyverse)

# -------------------- 1. DATEN-SCRAPING & BEREINIGUNG --------------------

scrape_year <- function(year) {
  url <- paste0("https://www.uni-due.de/de/absolventenehrung/fak", year, ".php")
  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)
  
  raw <- page %>% html_node("#content__standard__main") %>% html_text2()
  lines <- str_split(raw, "\n")[[1]] %>% str_trim() %>% discard(~ .x == "")
  
  is_faculty <- function(x) !str_detect(x, ":") && !str_detect(x, "¹|²|³|⁴")
  
  faculty_vec <- character(length(lines))
  current_faculty <- NA
  for (i in seq_along(lines)) {
    if (is_faculty(lines[i])) current_faculty <- lines[i]
    faculty_vec[i] <- current_faculty
  }
  
  df <- tibble(faculty = faculty_vec, line = lines) %>%
    filter(str_detect(line, ":")) %>%
    mutate(
      degree = str_extract(line, "^[^:]+"),
      name_raw = str_trim(str_remove(line, "^[^:]+:")),
      name_clean = str_remove_all(name_raw, "\\s*\\(.*?\\)"),
      name_clean = str_remove(name_clean, "^(Dipl\\.-[A-Za-zäöüÄÖÜ]+\\.?|B\\.A\\.|B\\.Sc\\.|M\\.A\\.|M\\.Sc\\.|M\\.Ed\\.|M\\.Eng\\.|M\\.Phil\\.|Staatsexamen)\\s*"),
      Name = ifelse(str_detect(name_clean, "\\."), str_remove(name_clean, "^.*\\.\\s+"), name_clean),
      Fakultät = case_when(
        str_detect(faculty, "Betriebswirtschaft") ~ "Betriebswirtschaftslehre",
        TRUE ~ str_replace(faculty, "MSM / |Lehramt,\\s*Master of Education", "Lehramt")
      ),
      Fakultät = str_remove(Fakultät, "\\s*\\(Foto\\)"),
      Abschluss = case_when(
        str_detect(degree, "Promotion") ~ "Promotion",
        str_detect(degree, "Master|Magister|Diplom|Staatsexamen|Staatsprüfung|LA ") ~ "Master",
        TRUE ~ "Bachelor"
      ),
      Jahr = year
    ) %>%
    select(Name, Fakultät, Abschluss, Jahr)
  
  return(df)
}

champs <- map_dfr(2013:2025, scrape_year)

champs_final <- champs %>%
  mutate(firstname = str_extract(Name, "^\\S+")) %>%
  left_join(gender(unique(.$firstname), method = "ssa") %>% select(name, gender), 
            by = c("firstname" = "name")) %>%
  mutate(Geschlecht = case_when(
    gender == "female" ~ "Weiblich",
    gender == "male" ~ "Männlich",
    TRUE ~ "Unbekannt"
  )) %>%
  select(-gender)

# -------------------- 2. USER INTERFACE (UI) --------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f7f6; font-family: 'Roboto', sans-serif; }
      
      /* Flexbox-Container für Titel und Logo */
      .header-container { 
        display: flex; 
        justify-content: space-between; 
        align-items: center; 
        padding: 15px; 
      }
      
      .main-title { font-size: 1.8rem; font-weight: bold; color: #2c3e50; margin-bottom: 5px; }
      .sub-title { font-size: 1.3rem; color: #2c3e50; }
      
      
      .sidebar { 
        background-color: white; 
        padding: 20px; 
        box-shadow: 0 4px 12px rgba(0,0,0,0.05); 
        margin-top: 12px; 
        border-radius: 10px; 
        border: 1px solid black; 
      }
      
      .sidebar h5 { 
        color: #2c3e50; 
        font-size: 1.3rem; 
        font-weight: 500; 
        margin-top: 5px; 
        margin-bottom: 15px;
      }
      
      .sidebar h6 { 
        font-size: 1.1rem; 
        color: #34495e; 
        margin-top: 10px;
      }
      
      .sidebar .control-label { 
        font-size: 1rem; 
        color: #444; 
        font-weight: normal; 
      }
      
      .main-card { 
        background: white; 
        padding: 20px; 
        box-shadow: 0 4px 12px rgba(0,0,0,0.05); 
        margin-bottom: 20px; 
        margin-top: 12px; 
        border-radius: 10px; 
        border: 1px solid black; 
      }
      
      .section-title { font-size: 1.4rem; font-weight: 500; margin-bottom: 15px; color: #2c3e50; }
      
      .author-box { position: fixed; bottom: 20px; left: 20px; background: rgba(255,255,255,0.9); padding: 15px; border-radius: 10px; border: 1px solid black; z-index: 1000; }
    "))
  ),
  
  # Titelbereich mit Logo
  div(class = "header-container",
      div(
        div(class = "main-title", "Die besten Absolventen der Universität Duisburg-Essen"),
        div(class = "sub-title", "Eine Analyse der Gewinner des Dies Academicus von 2013-2025")
      ),
      div(class = "uni-logo-box",
          tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/8/86/Uni-duisburg-essen-logo-2022.jpg", 
                   height = "70px")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3, class = "sidebar",
      h5("Analyse-Parameter"),
      hr(),
      selectInput("modus", "Analyse-Fokus:", 
                  choices = c("Nach Abschluss" = "Abschluss", 
                              "Nach Geschlecht" = "Geschlecht",
                              "Geschlecht nach Abschluss" = "gender_by_degree")),
      
      selectInput("zeitraum", "Zeitraum:", choices = c("Gesamter Zeitraum" = "all", "Einzeljahr" = "single")),
      
      conditionalPanel(
        condition = "input.zeitraum == 'single'",
        selectInput("jahr_input", "Jahr wählen:", choices = 2013:2025, selected = 2025)
      ),
      
      conditionalPanel(
        condition = "input.zeitraum == 'all'",
        hr(),
        h6("Vergleich der Fakultäten"),
        selectizeInput("fakultaeten", "Fakultäten wählen:", choices = NULL, multiple = TRUE, options = list(placeholder = 'Wählen...'))
      )
    ),
    
    mainPanel(
      width = 9,
      div(class = "main-card", plotlyOutput("main_plot", height = "480px")),
      div(class = "main-card", 
          div(class = "section-title", "Kennzahlen im Überblick"), 
          uiOutput("summary_table_html"))
    )
  ),
  
  div(class = "author-box",
      HTML("<strong>Autor: Noah Schulz</strong><br>"),
      tags$a(href = "https://www.linkedin.com/in/noah-schulz-971031301/", target = "_blank", icon("linkedin", style = "color:#0077b5; font-size:20px; margin-right:10px;")),
      tags$a(href = "https://github.com/SchulzNoah", target = "_blank", icon("github", style = "color:black; font-size:20px;"))
  )
)

# -------------------- 3. SERVER LOGIC --------------------

server <- function(input, output, session) {
  updateSelectizeInput(session, "fakultaeten", choices = sort(unique(champs_final$Fakultät)), server = TRUE)
  
  filtered_data <- reactive({
    df <- champs_final
    if (input$zeitraum == "all") {
      if (length(input$fakultaeten) > 0) df <- df %>% filter(Fakultät %in% input$fakultaeten)
    } else {
      df <- df %>% filter(Jahr == input$jahr_input)
    }
    df
  })
  
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    label_modus <- case_when(
      input$modus == "Abschluss" ~ "nach Abschluss",
      input$modus == "Geschlecht" ~ "nach Geschlecht",
      input$modus == "gender_by_degree" ~ "Geschlecht nach Abschluss"
    )
    label_zeit <- if(input$zeitraum == "single") paste("im Jahr", input$jahr_input) else "(2013-2025)"
    full_title <- paste("Verteilung der Gewinner", label_modus, label_zeit)
    
    x_var_name <- if(input$modus == "gender_by_degree") "Abschluss" else {
      if(input$zeitraum == "all" && length(input$fakultaeten) > 0) "Fakultät" else "Jahr"
    }
    fill_var <- if(input$modus == "gender_by_degree") "Geschlecht" else input$modus
    
    p <- ggplot(data, aes(x = factor(.data[[x_var_name]]), fill = .data[[fill_var]])) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c("Bachelor"="#004B93", "Master"="#009EE3", "Promotion"="#FFCC00", 
                                   "Weiblich"="#E69F00", "Männlich"="#56B4E9", "Unbekannt"="#aaaaaa")) +
      theme_minimal() + 
      labs(y = "Häufigkeit", x = if(x_var_name == "Jahr") "Jahr" else if(x_var_name == "Abschluss") "Abschluss" else "Fakultät", fill = fill_var) +
      theme(plot.title = element_blank())
    
    gp <- ggplotly(p, tooltip = c("fill", "count")) %>%
      layout(
        title = list(
          text = full_title,
          font = list(family = "Roboto, sans-serif", size = 22, color = "#2c3e50", weight = 500),
          x = 0,
          xanchor = "left",
          y = 0.98
        ),
        margin = list(t = 70, l = 0),
        hoverlabel = list(bgcolor = "white")
      )
    
    for (i in 1:length(gp$x$data)) {
      if (!is.null(gp$x$data[[i]]$text)) {
        gp$x$data[[i]]$text <- gsub("count:", "Häufigkeit:", gp$x$data[[i]]$text)
        gp$x$data[[i]]$text <- gsub("factor\\(.*?\\):.*?<br />", "", gp$x$data[[i]]$text)
        gp$x$data[[i]]$text <- gsub("Jahr:.*?<br />", "", gp$x$data[[i]]$text)
        gp$x$data[[i]]$text <- gsub("Fakultät:.*?<br />", "", gp$x$data[[i]]$text)
        gp$x$data[[i]]$text <- gsub("Abschluss:.*?<br />", "", gp$x$data[[i]]$text)
      }
    }
    
    if(input$zeitraum == "single" && input$modus != "gender_by_degree") {
      gp <- gp %>% layout(xaxis = list(showticklabels = FALSE, title = ""))
    }
    
    gp
  })
  
  output$summary_table_html <- renderUI({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    group_var <- if(input$modus == "gender_by_degree") "Abschluss" else "Fakultät"
    col_var <- if(input$modus == "gender_by_degree") "Geschlecht" else input$modus
    
    tab_raw <- data %>% group_by(!!sym(group_var), !!sym(col_var)) %>% 
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = !!sym(col_var), values_from = n, values_fill = 0)
    
    summary_row <- data %>% group_by(!!sym(col_var)) %>% summarise(n = n(), .groups = "drop")
    total_n <- sum(summary_row$n)
    
    format_cell_html <- function(n, total) {
      pct <- if(total > 0) round((n / total) * 100) else 0
      HTML(paste0("<strong>", n, "</strong><br><small style='color: #888;'>", pct, "%</small>"))
    }
    
    tags$table(class = "table table-hover", style = "background-color: white;",
               tags$thead(tags$tr(lapply(colnames(tab_raw), tags$th))),
               tags$tbody(
                 lapply(1:nrow(tab_raw), function(i) {
                   row_total <- sum(as.numeric(tab_raw[i, -1]))
                   tags$tr(
                     tags$td(as.character(tab_raw[i, 1])),
                     lapply(as.numeric(tab_raw[i, -1]), function(val) tags$td(format_cell_html(val, row_total)))
                   )
                 }),
                 tags$tr(style = "border-top: 3px solid #333; font-weight: bold; background-color: #f8f9fa;",
                         tags$td("Gesamt"),
                         lapply(summary_row$n, function(val) tags$td(format_cell_html(val, total_n)))
                 )
               )
    )
  })
}

shinyApp(ui, server)