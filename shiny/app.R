library(shiny)
library(flextable)
library(plotly)
library(tidyverse)
library(bslib)
library(shinythemes)

# Load and clean data
master_df <- read_csv("document_scores_clean.csv") 

# Load stats data
stat_df <- read_csv("3_Analysis/3_Comparative_Analysis/stat_df.csv")

# Load frequency data
freq_df <- read_csv("3_Analysis/1_Frequency/freq_df.csv") %>%
  select(-matches("Unnamed")) %>%
  mutate(across(ends_with("_percent"), ~ as.numeric(stringr::str_remove(as.character(.), "%"))))

freq_long <- freq_df %>%
  pivot_longer(
    cols = -year,
    names_to = c("source", "metric"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  filter(!is.na(rel))

  totals_df <- read_csv("source_counts_per_year.csv")

# Unique lists for filters
sources_list <- master_df %>% pull(Source) %>% unique() %>% sort()
orientations_list <- master_df %>% pull(Political_Orientation) %>% unique() %>% sort()

# Theme setup (Dark with Neon Green accents)
app_theme <- bs_theme(
  version = 5,
  preset = "flatly",
  primary = "#008000",
  secondary = "#008000",
  bg = "#000000",
  fg = "#008000",
  base_font = font_google("Inter"),
  heading_font = font_google("Space Mono")
) %>%
  bs_add_rules(
    "
    body { background-color: #000000; color: #008000; }
    .navbar { background-color: #000000 !important; border-bottom: 2px solid #008000; }
    .navbar-brand { color: #008000 !important; font-weight: bold; letter-spacing: 2px; }
    .nav-link { color: #008000 !important; border: 1px solid rgba(0, 128, 0, 0.2); border-radius: 4px; margin: 0 5px; }
    .nav-link.active { background-color: #008000 !important; border-color: #008000 !important; color: #000000 !important; }
    .card { background-color: #000000; border: 1px solid #008000; border-radius: 8px; }
    .card-header { background-color: #000000; color: #008000; border-bottom: 1px solid #008000; }
    .sidebar { background-color: #000000 !important; border-right: 1px solid #008000; }
    .irs--flat .irs-bar { background-color: #008000; }
    .irs--flat .irs-handle { background-color: #008000; }
    .irs--flat .irs-from, .irs--flat .irs-to, .irs--flat .irs-single { background-color: #008000; }
    
    /* SCM Quadrant Table Styling (B&W Academic Style) */
    .scm-container {
      display: flex;
      justify-content: center;
      padding: 40px 20px;
      background-color: #ffffff;
    }
    .scm-table {
      border-collapse: collapse;
      width: 100%;
      max-width: 1000px;
      border: 1.5px solid #000000;
    }
    .scm-table th, .scm-table td {
      border: 1px solid #000000;
      padding: 25px;
      text-align: center;
      vertical-align: middle;
      font-family: 'Times New Roman', Times, serif;
    }
    .scm-table th {
      background-color: #ffffff;
      color: #000000;
      font-weight: 700;
    }
    .scm-main-header {
      font-size: 1.6rem;
      letter-spacing: 2px;
    }
    .scm-sub-header {
      font-size: 1.2rem;
      background-color: #ffffff !important;
    }
    .scm-vertical-sub-header {
      writing-mode: vertical-rl;
      transform: rotate(180deg);
      white-space: nowrap;
      font-size: 1.1rem;
      background-color: #ffffff !important;
      font-weight: 700;
      color: #000000;
    }
    .scm-side-label {
      font-size: 1.4rem;
      font-weight: 800;
      color: #000000;
      background-color: #ffffff !important;
      writing-mode: vertical-rl;
      transform: rotate(180deg);
      white-space: nowrap;
    }
    .quadrant-title {
      font-weight: 800;
      font-size: 1.3rem;
      margin-bottom: 8px;
      color: #000000;
      text-transform: uppercase;
    }
    .quadrant-subtitle {
      font-weight: 700;
      font-size: 1.15rem;
      margin-bottom: 12px;
      color: #000000;
    }
    .quadrant-content {
      font-size: 0.95rem;
      line-height: 1.6;
      color: #000000;
    }
    .quadrant-examples {
      margin-top: 10px;
      font-style: italic;
      color: #333333;
    }
    .quadrant-cell {
      width: 400px;
    }
    .scm-footer {
      text-align: left;
      margin-top: 15px;
      font-size: 0.9rem;
      font-family: 'Times New Roman', Times, serif;
      color: #000000;
    }
    .scm-title {
      text-align: left;
      margin-bottom: 15px;
      font-family: 'Times New Roman', Times, serif;
      color: #000000;
      font-weight: bold;
      font-size: 1.5em;
    }
    
    /* Horizontal Timeline Styling (Academic B&W) */
    .timeline-container {
      padding: 60px 40px;
      background-color: #ffffff;
      font-family: 'Times New Roman', Times, serif;
      overflow-x: auto;
    }
    .timeline-wrapper {
      max-width: 900px;
      margin: 0 0 40px 0; /* Left-aligned */
    }
    .timeline-horizontal {
      display: flex;
      position: relative;
      width: 100%;
      padding: 120px 0;
      list-style: none;
    }
    .timeline-horizontal::before {
      content: '';
      position: absolute;
      top: 50%;
      left: 0;
      right: 0;
      height: 2px;
      background: #000000;
      transform: translateY(-50%);
    }
    .timeline-item-h {
      position: relative;
      flex: 1;
      display: flex;
      flex-direction: column;
      align-items: center;
      min-width: 0;
    }
    .timeline-item-h::after {
      content: '';
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      width: 14px;
      height: 14px;
      background: #ffffff;
      border: 2px solid #000000;
      border-radius: 50%;
      z-index: 2;
    }
    .timeline-content-h {
      position: absolute;
      width: 150px;
      text-align: center;
    }
    /* Alternating layout: Odd above, Even below */
    .timeline-item-h:nth-child(odd) .timeline-content-h {
      bottom: 55%; 
      margin-bottom: 15px;
    }
    .timeline-item-h:nth-child(even) .timeline-content-h {
      top: 55%; 
      margin-top: 15px;
    }
    .timeline-date-h {
      font-weight: bold;
      font-size: 1.2rem;
      display: block;
      margin-bottom: 4px;
      text-decoration: underline;
    }
    .timeline-event-h {
      font-weight: bold;
      font-size: 1.0rem;
      margin-bottom: 4px;
      line-height: 1.1;
    }
    .timeline-desc-h {
      font-size: 0.9rem;
      line-height: 1.3;
      color: #000000;
    }
    .timeline-title {
      text-align: left;
      margin-bottom: 20px;
      font-family: 'Times New Roman', Times, serif;
      color: #000000;
      font-weight: bold;
      font-size: 1.5em;
      text-decoration: underline;
    }
    .timeline-footer {
      text-align: left;
      margin-top: 40px;
      font-size: 0.9rem;
      font-family: 'Times New Roman', Times, serif;
      color: #000000;
      text-decoration: overline;
    }
        
    "
  )

# UI
ui <- page_navbar(
  title = "AI Sentiment",
  theme = app_theme,
  
  nav_panel(
    title = "Frequency",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Publication Volume", style = "color: #008000; font-family: 'Space Mono';"),
        p("Explore the diachronic salience of AI mentions across news organizations."),
        selectInput(
          inputId = "freq_source_select",
          label = "Select Source(s):",
          choices = sources_list,
          selected = sources_list,
          multiple = TRUE
        ),
        hr(style = "border-top: 1px solid #008000;")
      ),
      card(
        plotlyOutput("freq_plot", height = "800px")
      )
    )
  ),
  nav_panel(title = "Diachronic"),
  nav_menu(
    title = "SCM",
    nav_panel(
      title = "By Source",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          h4("AI Framing by Source", style = "color: #008000; font-family: 'Space Mono';"),
          p("Mean position with 68% confidence ellipses."),
          hr(style = "border-top: 1px solid #008000;")
        ),
        card(
          plotlyOutput("scm_graph_plot", height = "700px")
        )
      )
    ),
    nav_panel(
      title = "Yearly Trajectories",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          h4("Source Yearly Trajectories", style = "color: #008000; font-family: 'Space Mono';"),
          p("Explore the aggregated yearly sentiment by source."),
          selectInput(
            inputId = "medium_2_source_select",
            label = "Select Source(s):",
            choices = sources_list,
            selected = sources_list,
            multiple = TRUE
          ),
          hr(style = "border-top: 1px solid #008000;")
        ),
        card(
          plotlyOutput("medium_2_plot", height = "800px")
        )
      )
    )
  ),
  nav_panel(
    title = "Statistic",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Variable Definitions", style = "color: #008000; font-family: 'Space Mono';"),
        p(strong("Medium:"), " Compares Print (e.g., NYT, WSJ) versus Broadcast (e.g., CNN, FOX, MSNBC, NBC) formats."),
        p(strong("Political Orientation:"), " Groups networks by their typical audience leaning (Left, Center, Right)."),
        p(strong("Source:"), " The specific news organization producing the content."),
        p(strong("Year:"), " A diachronic analysis of how sentiment changes linearly over time."),
        hr(style = "border-top: 1px solid #008000;")
      ),
      card(
        h4("Comparative Analysis Statistics", style = "color: #008000; font-family: 'Space Mono';"),
        hr(style = "border-top: 1px solid #008000;"),
        uiOutput("stats_table_ui")
      )
    )
  ),
  nav_panel(
    title = "About",
    card(
      h3("Introduction", style = "color: #008000;"),
      p(""),
      h3("Background Literature", style = "color: #008000;"),
      p(""),
      h3("Methods", style = "color: #008000;"),
      p(""),
      h3("Results", style = "color: #008000;"),
      p(""),
      h4("Frequency", style = "color: #008000;"),
      p(""),
      h4("Diachronic", style = "color: #008000;"),
      p(""),
      h4("SCM", style = "color: #008000;"),
      p(""),
      h4("Statistics", style = "color: #008000;"),
      p(""),
      h3("Conclusion", style = "color: #008000;"),
      p(""),
      h3("Design Choice", style = "color: #008000;"),
      p("")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Plot: Statistics Table
  output$stats_table_ui <- renderUI({
    final_table <- stat_df %>% rename(Dimension = dimension)
    
    final_table <- final_table %>%
      mutate(
        p_numeric = as.numeric(gsub("< ", "", as.character(p_value))),
        sig = case_when(
          p_numeric < 0.001 ~ "***",
          p_numeric < 0.01  ~ "**",
          p_numeric < 0.05  ~ "*",
          TRUE              ~ ""
        ),
        p_formatted = ifelse(p_numeric < 0.001, "< 0.001", sprintf("%.4f", p_numeric))
      ) %>%
      select(Dimension, Comparison, Test, Statistic, p_formatted, sig)
    
    ft <- flextable(final_table) %>%
      merge_v(j = "Dimension") %>%
      set_header_labels(
        Dimension = "Dimension",
        Comparison = "Comparison Group",
        Test = "Statistical Test",
        Statistic = "Value",
        p_formatted = "p-value",
        sig = ""
      ) %>%
      bold(j = "Dimension", bold = TRUE) %>%
      bold(part = "header", bold = TRUE) %>%
      hline(i = 4, border = officer::fp_border(width = 1.5, color = "gray40")) %>%
      align(align = "left", j = 1:3) %>%
      align(align = "center", j = 4:6) %>%
      font(fontname = "Times New Roman", part = "all") %>%
      fontsize(size = 11, part = "all") %>%
      color(color = "black", part = "all") %>%
      bg(bg = "white", part = "all") %>%
      autofit()
      
    flextable::htmltools_value(ft)
  })


  # Plot: SCM by Source
  output$scm_graph_plot <- renderPlotly({
    source_means <- master_df %>%
      group_by(Source) %>%
      summarise(
        mean_competence = mean(mean_competence, na.rm = TRUE),
        mean_warmth = mean(mean_warmth, na.rm = TRUE),
        n_docs = n(),
        .groups = "drop"
      )
    
    p <- ggplot() +
      geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
      stat_ellipse(data = master_df,
                   aes(x = mean_competence, y = mean_warmth, 
                       color = Source, fill = Source),
                   geom = "polygon", alpha = 0.12, level = 0.68, linewidth = 0.6) +
      geom_point(data = source_means,
                 aes(x = mean_competence, y = mean_warmth, color = Source,
                     text = paste("Source:", Source,
                                  "<br>Mean Warmth:", round(mean_warmth, 4),
                                  "<br>Mean Competence:", round(mean_competence, 4),
                                  "<br>Documents:", n_docs)),
                 size = 5) +
      geom_text(data = source_means,
                aes(x = mean_competence, y = mean_warmth, label = Source, color = Source),
                vjust = -1.8, fontface = "bold", size = 4) +
      scale_color_manual(values = c("CNN" = "#CC0000", "FOX" = "#00008B", "MSNBC" = "#1E90FF", "NBC" = "#FFB000", "NYT" = "#008000", "WSJ" = "#D3D3D3")) +
      scale_fill_manual(values = c("CNN" = "#CC0000", "FOX" = "#00008B", "MSNBC" = "#1E90FF", "NBC" = "#FFB000", "NYT" = "#008000", "WSJ" = "#D3D3D3")) +
      annotate("text", x = 0.13, y = 0.13, label = "Admiration",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.08, y = 0.13, label = "Pity",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
      annotate("text", x = 0.13, y = -0.10, label = "Envy / Threat",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.08, y = -0.10, label = "Contempt",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
      scale_x_continuous(limits = c(-0.10, 0.15)) +
      scale_y_continuous(limits = c(-0.12, 0.15)) +
      labs(
        x = "Competence",
        y = "Warmth"
      ) +
      theme_minimal(base_family = "serif", base_size = 12) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        text = element_text(color = "black", family = "serif"),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90")
      )
    
    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor = "white", 
        paper_bgcolor = "white", 
        font = list(family = "Times New Roman, serif", color = "black"),
        showlegend = FALSE,
        title = list(
          text = paste0("<b>AI Framing in U.S. News Coverage by Source</b>",
                        "<br>",
                        "<sup>Mean position with 68% confidence ellipses</sup>"),
          x = 0
        ),
        margin = list(t = 80)
      )
    })
  })

  
  # Neon color palette
  app_colors <- c("#003366", "#CC0000", "#0080FF", "#FFB000", "#333333", "#666666")
  
  # Plot 1: SCM Scatter Plot (All Documents)
  output$scatter_plot <- renderPlotly({
    doc_data <- master_df %>%
      filter(Source %in% input$source_select, Year == input$year_select)
    
    if(nrow(doc_data) == 0) return(plotly_empty() %>% layout(plot_bgcolor="white", paper_bgcolor="white"))
    
    doc_data <- doc_data %>%
      mutate(
        Legend_Group = case_when(
          input$group_select == "Medium" ~ paste0(Medium, ": ", Source),
          input$group_select == "Political Orientation" ~ paste0(Political_Orientation, ": ", Source),
          TRUE ~ Source
        )
      )
      
    unique_groups <- sort(unique(doc_data$Legend_Group))
    num_groups <- length(unique_groups)
    vibrant_base <- c("#E6194B", "#3CB44B", "#4363D8", "#F58231", "#911EB4", 
                      "#42D4F4", "#F032E6", "#BFEF45", "#FABED4", "#469990", 
                      "#DCBEFF", "#9A6324", "#FFFAC8", "#800000", "#AAFFC3")
    dynamic_palette <- colorRampPalette(vibrant_base)(num_groups)
    
    p <- ggplot(doc_data, aes(x = mean_competence, y = mean_warmth, color = Legend_Group, 
                              text = paste("Title:", Title, "<br>Source:", Source, 
                                           "<br>Group:", Legend_Group,
                                           "<br>Competence:", round(mean_competence, 4), 
                                           "<br>Warmth:", round(mean_warmth, 4)))) +
      geom_hline(yintercept = 0, color = "#cccccc", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "#cccccc", linewidth = 0.4) +
      geom_jitter(alpha = 0.7, size = 1.2, width = 0.015, height = 0.015) +
      scale_color_manual(values = setNames(dynamic_palette, unique_groups), name = "Legend") +
      annotate("text", x = 0.20, y = 0.20, label = "Admiration", color = "#888888", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.10, y = 0.20, label = "Pity", color = "#888888", fontface = "italic", size = 3.5, hjust = 0) +
      annotate("text", x = 0.20, y = -0.10, label = "Envy / Threat", color = "#888888", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.10, y = -0.10, label = "Contempt", color = "#888888", fontface = "italic", size = 3.5, hjust = 0) +
      scale_x_continuous(limits = c(-0.12, 0.22)) +
      scale_y_continuous(limits = c(-0.12, 0.22)) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        text = element_text(color = "black"),
        axis.text = element_text(color = "#cccccc")
      ) +
      labs(x = "Competence", y = "Warmth")
    
    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(plot_bgcolor = "white", paper_bgcolor = "white", font = list(color = "black"))
    })
  })
  
  # Plot: Medium 2 Plot
  output$medium_2_plot <- renderPlotly({
    doc_data <- master_df %>%
      filter(Source %in% input$medium_2_source_select) %>%
      group_by(Source, Year) %>%
      summarise(
        n_docs = n(),
        mean_warmth = mean(mean_warmth, na.rm = TRUE),
        mean_competence = mean(mean_competence, na.rm = TRUE),
        .groups = "drop"
      ) 
    
    if(nrow(doc_data) == 0) return(plotly_empty() %>% layout(plot_bgcolor="white", paper_bgcolor="white"))
    
    p <- ggplot(doc_data, aes(x = mean_competence, y = mean_warmth, 
                              color = Source, #size = n_docs,
                              text = paste("Source:", Source, 
                                           "<br>Year:", Year,
                                           "<br>Documents:", n_docs,
                                           "<br>Mean Competence:", round(mean_competence, 3), 
                                           "<br>Mean Warmth:", round(mean_warmth, 3)))) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
      geom_point(size = 4, alpha = .38) +
      scale_color_manual(values = c("CNN" = "#CC0000", "FOX" = "#00008B", "MSNBC" = "#1E90FF", 
                                    "NBC" = "#FFB000", "NYT" = "#008000", "WSJ" = "#D3D3D3"), 
                         name = "News Networks") +

      
      annotate("text", x = 0.14, y = 0.11, label = "High Warmth/High Competence\n (Admiration)", size = 3.5, fontface = "bold", color = "black", family = "serif") +
      annotate("text", x = 0.14, y = -0.05, label = "Low Warmth/High Competence\n (Envy / Threat)", size = 3.5, fontface = "bold", color = "black", family = "serif") +
      annotate("text", x = -0.03, y = 0.11, label = "High Warmth/Low Competence\n (Pity)", size = 3.5, fontface = "bold", color = "black", family = "serif") +
      annotate("text", x = -0.03, y = -0.05, label = "Low Warmth/Low Competence\n (Contempt)", size = 3.5, fontface = "bold", color = "black", family = "serif") +
      
      expand_limits(x = c(-0.04, 0), y = c(-0.04, 0.12)) +
      scale_x_continuous(breaks = seq(-1, 1, by = 0.02)) +
      scale_y_continuous(breaks = seq(-1, 1, by = 0.02)) +
      theme_minimal(base_family = "serif") +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        text = element_text(color = "black", family = "serif"),
        axis.text = element_text(color = "black")
      ) +
      labs(x = "Competence Dimension", y = "Warmth Dimension")
    
    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor = "white", 
        paper_bgcolor = "white", 
        font = list(family = "Times New Roman, serif", color = "black"),
        legend = list(y = 0.5, yanchor = "middle", bgcolor = "white", bordercolor = "black", borderwidth = 1),
        title = list(
          text = paste0("<b>AI Framing in U.S. News Coverage Across the Stereotype Content Model</b>",
                        "<br>",
                        "<sup>Yearly mean positions by News Organization (2000–2024)</sup>"),
          x = 0
        ),
        margin = list(t = 80)
      )
    })
  })

  # Plot: Frequency Plot
  output$freq_plot <- renderPlotly({
    plot_data <- freq_long %>%
      filter(source %in% input$freq_source_select)
      
    if(nrow(plot_data) == 0) return(plotly_empty() %>% layout(plot_bgcolor="white", paper_bgcolor="white"))
    
    p <- ggplot(plot_data, aes(x = year, y = rel, color = source, group = source,
                               text = paste("Source:", source, 
                                            "<br>Year:", year,
                                            "<br>AI Mentions:", scales::comma(rel)))) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2) +
      geom_vline(xintercept = c(2002, 2011, 2017, 2022), linetype = "dashed", color = "grey50") +
      annotate("text", x = 2020.9, y = 1750, label = "ChatGPT\nRelease", hjust = 0, size = 3, fontface = "italic", color = "grey40") +
      annotate("text", x = 2015.9, y = 1450, label = "Transformers\nAlphaGo", hjust = 1, size = 3, fontface = "italic", color = "grey40") +
      annotate("text", x = 2010, y = 1050, label = "Watson", hjust = 1, size = 3, fontface = "italic", color = "grey40") +
      annotate("text", x = 2001, y = 550, label = "Roomba", hjust = 1, size = 3, fontface = "italic", color = "grey40") +
      scale_x_continuous(limits = c(2000, 2025), breaks = seq(2000, 2024, by = 4)) +
      scale_color_manual(values = c(
        "CNN" = "#CC0000",
        "FOX" = "#00008B",
        "MSNBC" = "#1E90FF",
        "NBC" = "#FFB000",
        "NYT" = "#008000",
        "WSJ" = "#D3D3D3" 
      ), name = "News Network") +
      scale_y_continuous(breaks = seq(0, 2000, by = 250), labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
      theme_minimal(base_family = "serif") +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        text = element_text(color = "black", family = "serif"),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.minor.y = element_line(color = "grey95", linetype = "dotted")
      ) +
      labs(x = "Year", y = "Number of AI Referencing Publications")
      
    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor = "white", 
        paper_bgcolor = "white", 
        font = list(family = "Times New Roman, serif", color = "black"),
        legend = list(y = 0.5, yanchor = "middle", bgcolor = "white", bordercolor = "black", borderwidth = 1),
        title = list(
          text = paste0("<b>Salience of AI in Media Discourse</b>",
                        "<br>",
                        "<sup>Diachronic analysis of total mentions of AI by news organization (2000–2024)</sup>"),
          x = 0
        ),
        margin = list(t = 80)
      )
    })
  })
  # Plot: Totals Plot
  output$totals_plot <- renderPlotly({
    plot_data <- totals_df %>%
      filter(source %in% input$totals_source_select)
      
    if(nrow(plot_data) == 0) return(plotly_empty() %>% layout(plot_bgcolor="white", paper_bgcolor="white"))
    
    p <- ggplot(plot_data, aes(x = year, y = total_articles, fill = source,
                               text = paste("Source:", source, 
                                            "<br>Year:", year,
                                            "<br>Total Articles:", scales::comma(total_articles)))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_x_continuous(limits = c(1999, 2025), breaks = seq(2000, 2024, by = 1)) +
      scale_fill_manual(values = c(
        "CNN" = "#CC0000",
        "FOX" = "#00008B",
        "MSNBC" = "#1E90FF",
        "NBC" = "#FFB000",
        "NYT" = "black",
        "WSJ" = "#D3D3D3" 
      ), name = "News Network") +
      scale_y_continuous(breaks = seq(0, 7000, by = 500), labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
      theme_minimal(base_family = "serif") +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        text = element_text(color = "black", family = "serif"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.minor.y = element_line(color = "grey95", linetype = "dotted")
      ) +
      labs(x = "Year", y = "Total Articles")
      
    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor = "white", 
        paper_bgcolor = "white", 
        font = list(family = "Times New Roman, serif", color = "black"),
        legend = list(y = 0.5, yanchor = "middle", bgcolor = "white", bordercolor = "black", borderwidth = 1),
        title = list(
          text = paste0("<b>Total Articles per Source over Time</b>",
                        "<br>",
                        "<sup>Yearly volume of AI coverage across media outlets (2000–2024)</sup>"),
          x = 0
        ),
        margin = list(t = 80)
      )
    })
  })

  # Plot: Temporal Plot
  output$medium_time_plot <- renderPlotly({
    doc_data <- master_df %>%
      filter(Source %in% input$medium_source_select) %>%
      group_by(Year) %>%
      summarise(
        Warmth     = mean(mean_warmth,     na.rm = TRUE),
        Competence = mean(mean_competence, na.rm = TRUE),
        n_docs     = n(),
        .groups    = "drop"
      ) %>%
      pivot_longer(cols = c(Warmth, Competence), names_to = "Dimension", values_to = "Score") %>%
      mutate(Dimension = factor(Dimension, levels = c("Competence", "Warmth"))) %>%
      filter(!is.na(Score), Year >= 2000, Year <= 2024)

    if (nrow(doc_data) == 0) return(plotly_empty() %>% layout(plot_bgcolor = "white", paper_bgcolor = "white"))

    mean_overall_competence <- mean(master_df$mean_competence, na.rm = TRUE)
    mean_overall_warmth     <- mean(master_df$mean_warmth, na.rm = TRUE)

    p <- ggplot(doc_data, aes(x = Year, y = Score, color = Dimension, group = Dimension,
                              text = paste("Dimension:", Dimension,
                                           "<br>Year:", Year,
                                           "<br>Mean Score:", round(Score, 4),
                                           "<br>Docs in average:", n_docs))) +
      geom_line(linewidth = 0.7, alpha = 0.3) +
      geom_point(size = 2, alpha = 0.3) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 0.9, span = 0.4) +
      scale_color_manual(values = c("Warmth" = "#FA8027", "Competence" = "#0E4C92"),
                         name = "Dimension") +
      scale_x_continuous(limits = c(2000, 2026), breaks = seq(2000, 2024, by = 4)) +
      scale_y_continuous(
        limits = c(0.02, 0.11),
        breaks = seq(-0.15, 0.15, by = 0.01),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      geom_vline(xintercept = c(2002, 2011, 2016, 2022),
                 linetype = "dashed", color = "gray50") +
      geom_label(data = doc_data %>% filter(Year == 2024), 
                 aes(label = Dimension, color = Dimension),
                 hjust = -0.1, fontface = "bold", show.legend = FALSE) +
      annotate("text", x = 2021, y = 0.02, label = "ChatGPT\nRelease", 
               hjust = 0, size = 3, fontface = "italic", color = "grey40") +
      annotate("text", x = 2015, y = 0.02, label = "Transformers", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+
      annotate("text", x = 2010, y = 0.02, label = "Watson", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+
      annotate("text", x = 2001, y = 0.02, label = "Roomba", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+
      annotate("text", x = 2024, y = 0.015, label = "High Warmth/High Competence\n (Admiration)", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+

      theme_minimal(base_family = "serif") +
      theme(
        legend.position = "right",
        plot.background  = element_rect(fill = "white", color = NA),
        plot.title = element_text(family = "serif", face = "bold", size = 22, color = "#1b1b1b"),
        plot.subtitle = element_text(family = "serif", size = 14, color = "grey30"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.minor.y = element_line(color = "grey90", linetype = "dotted"),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.minor.x = element_line(color = "grey90", linetype = "dotted"),
        text             = element_text(color = "black", family = "serif"),
        axis.text        = element_text(color = "#555555"),
        axis.title = element_text(family = "serif", face = "bold"),
        axis.text.y = element_text(vjust = -0.5, margin = margin(l = 0, r = -20)),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black"),
      ) +
      labs(x = "Year", y = "Mean Score")

    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        font          = list(family = "Times New Roman, serif", color = "black"),
        legend        = list(y = 0.5, yanchor = "middle", bgcolor = "white",
                             bordercolor = "black", borderwidth = 1),
          title = list(
          text = paste0("<b>Shift in AI Framing in U.S. News Coverage Shifted Over 25 Years</b>",
                        "<br>",
                        "<sup>Diachronic analysis of annual mean warmth and competence scores aggregated across six U.S. news organizations (2000–2024)</sup>"),
        x = 0
      ),
      margin = list(t = 80)
      )
    })
  })

  # Plot: Political Orientation SCM
  output$temporal2_plot <- renderPlotly({
    plot_data <- master_df %>%
      filter(!is.na(Political_Orientation), !is.na(mean_warmth), !is.na(mean_competence)) %>%
      mutate(Political_Orientation = tools::toTitleCase(Political_Orientation))

    if (nrow(plot_data) == 0) return(plotly_empty() %>% layout(plot_bgcolor = "white", paper_bgcolor = "white"))

    orientation_means <- plot_data %>%
      group_by(Political_Orientation) %>%
      summarise(
        mean_warmth     = mean(mean_warmth,     na.rm = TRUE),
        mean_competence = mean(mean_competence, na.rm = TRUE),
        n_docs          = n(),
        .groups         = "drop"
      )

    pal <- c("Left" = "#1F77B4", "Center" = "#7F7F7F", "Right" = "#D62728")

    p <- ggplot() +
      geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +

      # Confidence ellipses (68%)
      stat_ellipse(data = plot_data,
                   aes(x = mean_competence, y = mean_warmth,
                       color = Political_Orientation, fill = Political_Orientation),
                   geom = "polygon", alpha = 0.25, level = 0.68, linewidth = 0.8) +

      # Mean points
      geom_point(data = orientation_means,
                 aes(x = mean_competence, y = mean_warmth,
                     color = Political_Orientation,
                     text = paste("Orientation:", Political_Orientation,
                                  "<br>Mean Warmth:", round(mean_warmth, 4),
                                  "<br>Mean Competence:", round(mean_competence, 4),
                                  "<br>Documents:", n_docs)),
                 size = 6) +

      # Labels — manually fanned so they don't stack on the tight cluster
      geom_text(data = orientation_means %>%
                  mutate(
                    lx = case_when(
                      Political_Orientation == "Center" ~ mean_competence + 0.002,
                      Political_Orientation == "Left"   ~ mean_competence - 0.028,
                      Political_Orientation == "Right"  ~ mean_competence + 0.028,
                      TRUE ~ mean_competence
                    ),
                    ly = case_when(
                      Political_Orientation == "Center" ~ mean_warmth + 0.022,
                      Political_Orientation == "Left"   ~ mean_warmth - 0.020,
                      Political_Orientation == "Right"  ~ mean_warmth - 0.020,
                      TRUE ~ mean_warmth
                    )
                  ),
                aes(x = lx, y = ly,
                    label = Political_Orientation, color = Political_Orientation),
                fontface = "bold", size = 4.5, family = "serif") +

      scale_color_manual(values = pal, name = "Orientation") +
      scale_fill_manual(values  = pal, name = "Orientation") +

      # Quadrant labels
      annotate("text", x =  0.14, y =  0.138, label = "High Warmth/High Competence\n (Admiration)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.015, y =  0.138, label = "High Warmth/Low Competence\n (Pity)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
      annotate("text", x =  0.14, y = -0.035, label = "Low Warmth/High Competence\n (Envy / Threat)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.015, y = -0.035, label = "Low Warmth/Low Competence\n (Contempt)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
      
      scale_x_continuous(limits = c(0.04, 0.13), breaks = seq(-0.10, 0.15, by = 0.025)) +
      scale_y_continuous(limits = c(0.00, 0.08), breaks = seq(-0.10, 0.15, by = 0.025)) +

      labs(x = "Competence", y = "Warmth",
           subtitle = "Mean positions in the Stereotype Content Model with 68% confidence ellipses (2000–2024)") +

      theme_minimal(base_family = "serif", base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position  = "none",
        panel.grid.minor = element_blank(),
        text             = element_text(color = "black", family = "serif"),
        axis.text        = element_text(color = "#555555")
      )

    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        font          = list(family = "Times New Roman, serif", color = "black"),
        showlegend    = FALSE,
        title = list(
          text = paste0("<b>AI Framing in U.S. News Coverage by Medium and Political Orientation</b>",
                        "<br>",
                        "<sup>Mean positions in the Stereotype Content Model with 68% confidence ellipses (2000–2024)</sup>"),
          x = 0
        ),
        margin = list(t = 80)
      )
    })
  })

  # Plot: Comparison — Political Orientation vs Medium SCM (facet_wrap)
  output$comparison_plot <- renderPlotly({

    # ── raw data, unified with comparison + Group columns ──────────────────
    pol_raw <- master_df %>%
      filter(!is.na(Political_Orientation), !is.na(mean_warmth), !is.na(mean_competence)) %>%
      mutate(Group = tools::toTitleCase(Political_Orientation),
             comparison = "By Political Orientation")

    med_raw <- master_df %>%
      filter(!is.na(Medium), !is.na(mean_warmth), !is.na(mean_competence)) %>%
      mutate(Group = tools::toTitleCase(Medium),
             comparison = "By Medium")

    raw_combined <- bind_rows(pol_raw, med_raw) %>%
      mutate(comparison = factor(comparison,
               levels = c("By Political Orientation", "By Medium")))

    # ── means with per-group label offsets ─────────────────────────────────
    pol_means <- pol_raw %>%
      group_by(Group, comparison) %>%
      summarise(mean_warmth = mean(mean_warmth, na.rm=TRUE),
                mean_competence = mean(mean_competence, na.rm=TRUE),
                n_docs = n(), .groups="drop") %>%
      mutate(
        lx = case_when(
          Group == "Center" ~ mean_competence + 0.002,
          Group == "Left"   ~ mean_competence - 0.028,
          Group == "Right"  ~ mean_competence + 0.028,
          TRUE ~ mean_competence),
        ly = case_when(
          Group == "Center" ~ mean_warmth + 0.022,
          Group == "Left"   ~ mean_warmth - 0.020,
          Group == "Right"  ~ mean_warmth - 0.020,
          TRUE ~ mean_warmth))

    med_means <- med_raw %>%
      group_by(Group, comparison) %>%
      summarise(mean_warmth = mean(mean_warmth, na.rm=TRUE),
                mean_competence = mean(mean_competence, na.rm=TRUE),
                n_docs = n(), .groups="drop") %>%
      mutate(
        lx = case_when(
          Group == "Broadcast" ~ mean_competence + 0.022,
          Group == "Print"     ~ mean_competence - 0.022,
          TRUE ~ mean_competence),
        ly = case_when(
          Group == "Broadcast" ~ mean_warmth + 0.015,
          Group == "Print"     ~ mean_warmth - 0.015,
          TRUE ~ mean_warmth))

    means_combined <- bind_rows(pol_means, med_means) %>%
      mutate(comparison = factor(comparison,
               levels = c("By Political Orientation", "By Medium")))

    # ── unified color palette ───────────────────────────────────────────────
    pal_all <- c("Left"      = "#1F77B4",
                 "Center"    = "#7F7F7F",
                 "Right"     = "#D62728",
                 "Broadcast" = "#E67E22",
                 "Print"     = "#16A085")

    # ── build single ggplot ─────────────────────────────────────────────────
    p <- ggplot() +
      geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +

      # Ellipses
      stat_ellipse(data = raw_combined,
                   aes(x = mean_competence, y = mean_warmth,
                       color = Group, fill = Group),
                   geom = "polygon", alpha = 0.25, level = 0.68, linewidth = 0.8) +

      # Mean points
      geom_point(data = means_combined,
                 aes(x = mean_competence, y = mean_warmth, color = Group,
                     text = paste("Group:", Group,
                                  "<br>Mean Warmth:", round(mean_warmth, 4),
                                  "<br>Mean Competence:", round(mean_competence, 4),
                                  "<br>Documents:", n_docs)),
                 size = 6) +

      # Labels fanned per group
      geom_text(data = means_combined,
                aes(x = lx, y = ly, label = Group, color = Group),
                fontface = "bold", size = 4.5, family = "serif") +

      # Quadrant annotations (appear on both facets automatically)
      annotate("text", x =  0.14,  y =  0.1, label = "High Warmth\nHigh Competence\n (Admiration)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.017, y =  0.1, label = "High Warmth\nLow Competence\n (Pity)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +
      annotate("text", x =  0.14,  y = -0.035, label = "Low Warmth\nHigh Competence\n (Envy / Threat)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 1) +
      annotate("text", x = -0.017, y = -0.035, label = "Low Warmth\nLow Competence\n (Contempt)",
               color = "gray40", fontface = "italic", size = 3.5, hjust = 0) +

      facet_wrap(~comparison, ncol = 2, scales = "fixed") +

      scale_color_manual(values = pal_all) +
      scale_fill_manual(values  = pal_all) +
      scale_x_continuous(limits = c(-0.025, 0.15), breaks = seq(-0.10, 0.15, by = 0.025)) +
      scale_y_continuous(limits = c(-0.05,  0.11), breaks = seq(-0.10, 0.15, by = 0.025)) +
      coord_fixed(ratio = 1) +

      labs(x = "Competence", y = "Warmth",
           subtitle = "Mean positions in the Stereotype Content Model with 68% confidence ellipses (2000–2024)") +

      theme_minimal(base_family = "serif", base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position  = "none",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc"),
        strip.text       = element_text(size = 12, face = "bold", color = "#003366"),
        text             = element_text(color = "black", family = "serif"),
        axis.text        = element_text(color = "#555555")
      )

    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        font          = list(family = "Times New Roman, serif", color = "black"),
        showlegend    = FALSE,
        annotations = list(
          list(
            x = 0, y = 1.05,
            text = "<b>AI Framing in U.S. News Coverage by Medium and Political Orientation</b><br><sup>Mean positions in the Stereotype Content Model with 68% confidence ellipses (2000–2024)</sup>",
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            xanchor = "left", yanchor = "bottom",
            align = "left",
            font = list(size = 16, color = "black", family = "Times New Roman, serif")
          )
        ),
        margin = list(t = 80)
      )
    })
  })


  # Plot 2: Trend Line Plot
  output$trend_plot <- renderPlotly({
    doc_data <- master_df %>%
      filter(Source %in% input$trend_source_select) %>%
      group_by(Year) %>%
      summarise(
        Warmth     = mean(mean_warmth,     na.rm = TRUE),
        Competence = mean(mean_competence, na.rm = TRUE),
        n_docs     = n(),
        .groups    = "drop"
      ) %>%
      pivot_longer(cols = c(Warmth, Competence), names_to = "Dimension", values_to = "Score") %>%
      mutate(Dimension = factor(Dimension, levels = c("Competence", "Warmth"))) %>%
      filter(!is.na(Score), Year >= 2000, Year <= 2024)

    if (nrow(doc_data) == 0) {
      return(plotly_empty() %>% layout(plot_bgcolor = "white", paper_bgcolor = "white"))
    }

    p <- ggplot(doc_data, aes(x = Year, y = Score, color = Dimension, group = Dimension,
                              text = paste("Dimension:", Dimension,
                                           "<br>Year:", Year,
                                           "<br>Mean Score:", round(Score, 4),
                                           "<br>Docs in average:", n_docs))) +
      geom_line(linewidth = 0.7, alpha = 0.3) +
      geom_point(size = 2, alpha = 0.3) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 0.9, span = 0.4) +
      scale_color_manual(values = c("Warmth" = "#FA8027", "Competence" = "#0E4C92"),
                         name = "Dimension") +
      scale_x_continuous(limits = c(2000, 2026), breaks = seq(2000, 2024, by = 4)) +
      scale_y_continuous(
        limits = c(0.02, 0.11),
        breaks = seq(-0.15, 0.15, by = 0.01),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      geom_vline(xintercept = c(2002, 2011, 2016, 2022),
                 linetype = "dashed", color = "gray50") +
      annotate("text", x = 2021, y = 0.02, label = "ChatGPT\nRelease", 
               hjust = 0, size = 3, fontface = "italic", color = "grey40") +
      annotate("text", x = 2015, y = 0.02, label = "Transformers", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+
      annotate("text", x = 2010, y = 0.02, label = "Watson", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+
      annotate("text", x = 2001, y = 0.02, label = "Roomba", 
               hjust = 1, size = 3, fontface = "italic", color = "grey40")+
      theme_minimal(base_family = "serif") +
      theme(
        legend.position = "right",
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.minor.y = element_line(color = "grey90", linetype = "dotted"),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.minor.x = element_line(color = "grey90", linetype = "dotted"),
        text             = element_text(color = "black", family = "serif"),
        axis.text        = element_text(color = "#555555"),
        axis.title = element_text(family = "serif", face = "bold"),
        axis.text.y = element_text(vjust = -0.5, margin = margin(l = 0, r = -20)),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black")
      ) +
      labs(x = "Year", y = "Mean Score")

    suppressWarnings({
      ggplotly(p, tooltip = "text") %>% layout(
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        font          = list(family = "Times New Roman, serif", color = "black"),
        legend        = list(y = 0.5, yanchor = "middle", bgcolor = "white",
                             bordercolor = "black", borderwidth = 1),
        title = list(
          text = paste0("<b>Shift in AI Framing in U.S. News Coverage Shifted Over 25 Years</b>",
                        "<br>",
                        "<sup>Diachronic analysis of annual mean warmth and competence scores aggregated across selected U.S. news organizations (2000–2024)</sup>"),
          x = 0
        ),
        margin = list(t = 80)
      )
    })

  })
  output$bar_plot <- renderPlotly({
    data <- master_df %>%
      filter(Year >= input$pol_year_select[1] & Year <= input$pol_year_select[2]) %>%
      filter(!is.na(Political_Orientation)) %>%
      group_by(Political_Orientation) %>%
      summarise(
        Avg_Warmth = mean(mean_warmth, na.rm = TRUE),
        Avg_Competence = mean(mean_competence, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Avg_Warmth, Avg_Competence), names_to = "Dimension", values_to = "Score")
      
    if(nrow(data) == 0) return(plotly_empty() %>% layout(plot_bgcolor="white", paper_bgcolor="white"))
    
    p <- ggplot(data, aes(x = Political_Orientation, y = Score, fill = Dimension)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Avg_Warmth" = "#003366", "Avg_Competence" = "#00FFFF")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#cccccc", linetype="dashed"),
        panel.grid.minor = element_blank(),
        text = element_text(color = "black"),
        axis.text = element_text(color = "#cccccc")
      ) +
      labs(x = "Political Orientation", y = "Average Score")
      
    ggplotly(p) %>% layout(plot_bgcolor = "white", paper_bgcolor = "white", font = list(color = "black"))
  })
}

shinyApp(ui, server)
