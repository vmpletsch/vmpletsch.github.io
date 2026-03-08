library(plotly)
library(ggplot2)
library(scales)

#Bubble Plot (note that i ended up going for a different plot type because bubble didn't work but i didn't want to rename it so i left it as bubble)
make_bubble_plot <- function(data_to_plot, x_var = "income_ppp") {
  plot_ly(
    data_to_plot,
    x = ~get(x_var),
    y = ~babies_per_woman,
    size = ~population,
    color = ~religion,
    colors = c("#2E0854", "#FF00FF", "#DDA0DD", "#C71585"),
    type = "scatter",
    mode = "markers",
    sizes = c(15, 65),
    text = ~paste0("<b>", name, "</b><br>",
                   "Year: ", year, "<br>",
                   "Fertility: ", babies_per_woman, "<br>",
                   "Income: $", scales::comma(income_ppp)),
    hoverinfo = "text",
    marker = list(opacity = 0.7, sizemode = 'diameter', line = list(width = 1, color = '#FFFFFF'))) %>%
    layout(
      xaxis = list(
        title = list(text = "<b>Average Daily Income (PPP)</b>"),
        type = "linear",
        tickprefix = "$",
        linecolor = 'black',
        linewidth = 1,
        mirror = TRUE,
        showline = TRUE,
        range = c(0, 100),
        dtick = 10),
      yaxis = list(
        title = list(text = "<b>Average Fertility Rate (Births per Woman)</b>"), 
        range = c(0, 9), 
        dtick = 1,
        linecolor = 'black',
        linewidth = 1,
        mirror = TRUE,
        showline = TRUE),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      margin = list(l = 50, r = 50, t = 50, b = 50)) %>%
    config(displayModeBar = FALSE)
}

#Correlation plot, scatter
make_correlation_plot <- function(data_to_plot, x_var, x_label, log_x = FALSE) {
  p <- ggplot(data_to_plot, aes(x = .data[[x_var]], y = babies_per_woman, color = religion)) +
    geom_point(alpha = 0.6, size = 3, aes(text = paste0("<b>", name, "</b><br>",
                                                         "Religion: ", religion, "<br>",
                                                         if(x_label == "Average Educational Attainment (in years)") {
                                                           paste0("Education: ", round(.data[[x_var]], 2), " years")
                                                         } else if(x_label == "Daily Income") {
                                                           paste0("Daily Income: ", scales::comma(round(.data[[x_var]], 2)), " PPP")
                                                         } else {
                                                           paste0(x_label, ": ", round(.data[[x_var]], 2))
                                                         }, "<br>",
                                                         "Fertility: ", babies_per_woman))) +
    scale_color_manual(values = c(
      "Christian" = "#2E0854",   # Midnight Purple
      "Muslim" = "#FF00FF",      # Vivid Magenta
      "Eastern" = "#DDA0DD",     # Plum
      "Unaffiliated" = "#C71585" # Deep Rose
    )) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
    labs(
      x = x_label,
      y = "Average Fertility Rate (Births per Woman)") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"))

  if (log_x) {
    p <- p + scale_x_log10(labels = scales::comma)
  } else if (x_label == "Average Educational Attainment (in years)") {
    p <- p + scale_x_continuous(breaks = seq(0, 15, by = 1), limits = c(0, 15))
  } else if (x_label == "Daily Income") {
    p <- p + scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100))
  } else {
    p <- p + scale_x_continuous(labels = scales::comma, limits = c(0, NA))
  }
  
  p <- p + scale_y_continuous(breaks = seq(0, 9, by = 1), limits = c(0, 9))

  ggplotly(p, tooltip = "text") %>%
    layout(
      xaxis = list(linecolor = 'black', linewidth = 1, mirror = TRUE, showline = TRUE),
      yaxis = list(linecolor = 'black', linewidth = 1, mirror = TRUE, showline = TRUE),
      margin = list(l = 50, r = 20, t = 40, b = 50),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      legend = list(orientation = "h", y = -0.2)
    ) %>%
    config(displayModeBar = FALSE)
}

#Line graph for time
make_religion_line_plot <- function(data_to_plot) {
  p <- ggplot(data_to_plot, aes(x = year, y = babies_per_woman, group = name, color = name)) +
    geom_line(alpha = 0.5) +
    geom_point(aes(text = paste0("<b>", name, "</b><br>",
                                 "Year: ", year, "<br>",
                                 "Religion: ", religion, "<br>",
                                 "Fertility: ", babies_per_woman, "<br>",
                                 "Income: $", scales::comma(income_ppp), "<br>",
                                 "Gender Equality: ", round(idea, 2))),
               size = 0.5) +
    labs(
      x = "Year",
      y = "Average Fertility Rate (Births per Woman)",
      color = "Country") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")) +
    scale_y_continuous(breaks = seq(0, 9, by = 1), limits = c(0, 9)) +
    scale_x_continuous(breaks = seq(1800, 2030, by = 20)) +
    scale_color_manual(values = c(
      "#FF69B4", "#BA55D3", "#F08080", "#DB7093", "#DA70D6",
      "#C71585", "#FFB6C1", "#D8BFD8", "#DDA0DD", "#EE82EE",
      "#9370DB", "#BA55D3", "#E06666", "#D5A6BD", "#A64D79"
    ))

  ggplotly(p, tooltip = "text") %>%
    layout(
      xaxis = list(linecolor = 'black', linewidth = 1, mirror = TRUE, showline = TRUE),
      yaxis = list(linecolor = 'black', linewidth = 1, mirror = TRUE, showline = TRUE),
      margin = list(l = 50, r = 20, t = 40, b = 50),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa"
    ) %>%
    config(displayModeBar = FALSE)
}
#scatter plot for gender equality
make_idea_plot <- function(data_to_plot) {
  p <- ggplot(data_to_plot, aes(x = idea, y = babies_per_woman, color = religion)) +
    geom_point(alpha = 0.6, size = 3, aes(text = paste0("<b>", name, "</b><br>",
                                                         "Religion: ", religion, "<br>",
                                                         "Idea: ", round(idea, 2), "<br>",
                                                         "Fertility: ", babies_per_woman))) +
    scale_color_manual(values = c(
      "Christian" = "#2E0854",   # Midnight Purple
      "Muslim" = "#FF00FF",      # Vivid Magenta
      "Eastern" = "#DDA0DD",     # Plum
      "Unaffiliated" = "#C71585" # Deep Rose
    )) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
    labs(
      x = "Gender Equality Index (IDEA)",
      y = "Average Fertility Rate (Births per Woman)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    ) +
    scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + # IDEA is 0-100
    scale_y_continuous(breaks = seq(0, 9, by = 1), limits = c(0, 9))

  ggplotly(p, tooltip = "text") %>%
    layout(
      xaxis = list(linecolor = 'black', linewidth = 1, mirror = TRUE, showline = TRUE),
      yaxis = list(linecolor = 'black', linewidth = 1, mirror = TRUE, showline = TRUE),
      margin = list(l = 50, r = 20, t = 40, b = 50),
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#fafafa",
      legend = list(orientation = "h", y = -0.2)
    ) %>%
    config(displayModeBar = FALSE)
}
