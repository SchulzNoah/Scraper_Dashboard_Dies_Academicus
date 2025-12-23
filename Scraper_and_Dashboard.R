# -------------------- Laden relevanter Packages --------------------

# Klassifikation der Namen
if (!require("genderdata")) {
  if (!require("remotes")) install.packages("remotes")
  remotes::install_github("lmullen/genderdata", upgrade = "never")
}

library(rvest)     # Scraping
library(shiny)     # Dashboard
library(plotly)    # Interaktiver Plot
library(gender)    # Klassifikation der Namen
library(bslib)     # Verzierungen Dashboard
library(tidyverse) # Pipes, Data Cleaning


# -------------------- Scraper --------------------


scrape_year <- function(year) {

  # Basis URL
  url <- paste0("https://www.uni-due.de/de/absolventenehrung/fak", year, ".php")
  # Laden der Webseite
  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)

  # Extraktion der Daten
  raw <- page %>% html_node("#content__standard__main") %>% html_text2()
  lines <- str_split(raw, "\n")[[1]] %>% str_trim() %>% discard(~ .x == "")
  
  # Extraktion der Fakultät
  is_faculty <- function(x) !str_detect(x, ":") && !str_detect(x, "¹|²|³|⁴")
  
  faculty_vec <- character(length(lines))
  current_faculty <- NA
  for (i in seq_along(lines)) {
    if (is_faculty(lines[i])) current_faculty <- lines[i]
    faculty_vec[i] <- current_faculty
  }

  # Aufbau Dataframe                 
  df <- tibble(faculty = faculty_vec, line = lines) %>%
    filter(str_detect(line, ":")) %>%
    mutate(
      degree = str_extract(line, "^[^:]+"),

      # Cleanen der Namen (Titel raus)
      name_raw = str_trim(str_remove(line, "^[^:]+:")),
      name_clean = str_remove_all(name_raw, "\\s*\\(.*?\\)"),
      name_clean = str_remove(name_clean, "^(Dipl\\.-[A-Za-zäöüÄÖÜ]+\\.?|B\\.A\\.|B\\.Sc\\.|M\\.A\\.|M\\.Sc\\.|M\\.Ed\\.|M\\.Eng\\.|M\\.Phil\\.|Staatsexamen)\\s*"),
      Name = ifelse(str_detect(name_clean, "\\."), str_remove(name_clean, "^.*\\.\\s+"), name_clean),
      # Betriebswirtschaft (neue Bezeichnung) und BWL zu einer Fakultät
      Fakultät = case_when(
        str_detect(faculty, "Betriebswirtschaft") ~ "Betriebswirtschaftslehre",
        TRUE ~ str_replace(faculty, "MSM / |Lehramt,\\s*Master of Education", "Lehramt")
      ),
      Fakultät = str_remove(Fakultät, "\\s*\\(Foto\\)"),
      # alte Studiengangsbezeichnunhen in Master umändern
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

# Aufstellen des Meta-Datensatzes (Iteration über alle Jahre)
champs <- map_dfr(2013:2025, scrape_year) %>% 
  mutate(Name = ifelse(Name == "Sc.", "Kathrin Benkel", Name))

# Extraktion des Geschlechts aus Namen
unique_names <- tibble(firstname = unique(str_extract(champs$Name, "^\\S+")))
gender_local <- gender(unique_names$firstname, method = "ssa") %>%
  select(name, gender_ssa = gender)

# Fehlende Geschlechtszuordnungen
missing_names <- unique_names %>%
  left_join(gender_local, by = c("firstname" = "name")) %>%
  filter(is.na(gender_ssa)) %>%
  pull(firstname)

# Funktion für API-Abfrage über genderize.io
get_gender_api <- function(names_vec) {
  if (length(names_vec) == 0) return(tibble())
  
  # API-Abfrage in 10er-Bündeln
  map_dfr(split(names_vec, ceiling(seq_along(names_vec)/10)), function(batch) {
    url <- paste0("https://api.genderize.io?name[]=", paste(batch, collapse = "&name[]="))
    res <- tryCatch(jsonlite::fromJSON(url), error = function(e) return(NULL))
    if (is.null(res)) return(tibble())
    return(as_tibble(res) %>% select(name, gender_api = gender))
  })
}

# API-Abfrage
gender_api_results <- get_gender_api(missing_names)

# Manuelle Kodierung von zwei Namen, die nicht zugeordnet werden konnten
manual_corrections <- tibble(
  name = c("Kaja-Nina", "Sven-Arvid"), 
  gender_manual = c("female", "male"))

# Zusammenführen in finalen Datensatz
champs_final <- champs %>%
  mutate(firstname = str_extract(Name, "^\\S+")) %>%
  left_join(gender_local, by = c("firstname" = "name")) %>%
  left_join(gender_api_results, by = c("firstname" = "name")) %>%
  left_join(manual_corrections, by = c("firstname" = "name")) %>% 
  mutate(Geschlecht = case_when(
    gender_ssa == "female" | gender_api == "female" | gender_manual == "female" ~ "Weiblich",
    gender_ssa == "male"   | gender_api == "male"   | gender_manual == "male"   ~ "Männlich",
    TRUE ~ "Unbekannt")) %>%
  select(Name, Fakultät, Abschluss, Jahr, Geschlecht)


# -------------------- UI --------------------

ui <- fluidPage(
  # Customizing der Page
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

# Server

server <- function(input, output, session) {
  # Auswahl der Fakultäten
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

  # Erstellung interaktiver Barplot
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Umbenennung der 3 Panel-Modi
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

    # Erstellung des Plots
    p <- ggplot(data, aes(x = factor(.data[[x_var_name]]), fill = .data[[fill_var]])) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c("Bachelor"="#004B93", "Master"="#009EE3", "Promotion"="#FFCC00", 
                                   "Weiblich"="#E69F00", "Männlich"="#56B4E9")) +
      theme_minimal() + 
      labs(y = "Häufigkeit", x = if(x_var_name == "Jahr") "Jahr" else if(x_var_name == "Abschluss") "Abschluss" else "Fakultät", fill = fill_var) +
      theme(plot.title = element_blank())

    # Plot wird mit plotly interaktiv gemacht
    gp <- ggplotly(p, tooltip = c("fill", "count")) %>%
      layout(
        title = list(
          text = full_title,
          # Einheitlicher Titel
          font = list(family = "Roboto, sans-serif", size = 22, color = "#2c3e50", weight = 500),
          x = 0,
          xanchor = "left",
          y = 0.98
        ),
        margin = list(t = 70, l = 0),
        hoverlabel = list(bgcolor = "white")
      )

    # Umbenennung des Hover-Texts
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
  
  
    # Erstellung der interaktiven Tabelle
    output$summary_table_html <- renderUI({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    group_var <- if(input$modus == "gender_by_degree") "Abschluss" else "Fakultät"
    col_var <- if(input$modus == "gender_by_degree") "Geschlecht" else input$modus
    
    # Haupttabelle (ohne Gesamt) erstellen
    tab_raw <- data %>% group_by(!!sym(group_var), !!sym(col_var)) %>% 
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = !!sym(col_var), values_from = n, values_fill = 0)
    
    # Spaltennamen extrahieren
    col_names <- colnames(tab_raw)[-1]
    
    # 3. Summen berechnen (basierend auf Spaltennamen der Tabelle)
    summary_values <- map_dbl(col_names, function(cn) {
      sum(tab_raw[[cn]], na.rm = TRUE)
    })
    total_n <- sum(summary_values)
    
    format_cell_html <- function(n, total) {
      pct <- if(total > 0) round((n / total) * 100) else 0
      HTML(paste0("<strong>", n, "</strong><br><small style='color: #888;'>", pct, "%</small>"))
    }
    
    # Formatierung
    tags$table(class = "table table-hover", style = "background-color: white;",
               tags$thead(tags$tr(lapply(colnames(tab_raw), tags$th))),
               tags$tbody(
                 # Daten-Zeilen
                 lapply(1:nrow(tab_raw), function(i) {
                   row_total <- sum(as.numeric(tab_raw[i, -1]))
                   tags$tr(
                     tags$td(as.character(tab_raw[i, 1])),
                     lapply(as.numeric(tab_raw[i, -1]), function(val) tags$td(format_cell_html(val, row_total)))
                   )
                 }),
                 # Gesamt-Zeile
                 tags$tr(style = "border-top: 3px solid #333; font-weight: bold; background-color: #f8f9fa;",
                         tags$td("Gesamt"),
                         lapply(summary_values, function(val) tags$td(format_cell_html(val, total_n)))
                 )
               )
    )
  })
}

# -------------------- Starten der App --------------------
shinyApp(ui, server)


