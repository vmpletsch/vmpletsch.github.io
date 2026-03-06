library(shiny)
library(plotly)
library(tidyverse)
library(bslib)
library(shinythemes)
library(scales)
library(ggplot2)

source("Plots.R")

#Load data, forgot to clean 
master_df <- read_csv("gapminder_master.csv") %>%
  rename(name = name.x) %>%
  mutate(
    year = as.numeric(year),
    population = 1,
    religion = case_when(
      main_religion_2008 == "muslim" ~ "Muslim",
      main_religion_2008 == "christian" ~ "Christian",
      main_religion_2008 == "eastern_religions" ~ "Eastern",
      main_religion_2008 == "" ~ "Unaffiliated",
      TRUE ~ "Other"))

#Add unique country names 
countries_list <- master_df %>% 
  pull(name) %>% 
  unique() %>% 
  sort()

religions_list <- c("Christian", "Muslim", "Eastern", "Unaffiliated")

# Helper function for the color key in the sidebar
religion_color_key <- function() {
  div(
    hr(),
    h5("Religion Color Key:"),
    tags$ul(style = "list-style: none; padding-left: 0;",
      tags$li(tags$span(style="display:inline-block; width:12px; height:12px; border-radius:50%; background-color:#2E0854; margin-right:8px;"), "Christian"),
      tags$li(tags$span(style="display:inline-block; width:12px; height:12px; border-radius:50%; background-color:#FF00FF; margin-right:8px;"), "Muslim"),
      tags$li(tags$span(style="display:inline-block; width:12px; height:12px; border-radius:50%; background-color:#DDA0DD; margin-right:8px;"), "Eastern"),
      tags$li(tags$span(style="display:inline-block; width:12px; height:12px; border-radius:50%; background-color:#C71585; margin-right:8px;"), "Unaffiliated")
    ),
    hr()
  )
}

#UI
ui <- page_navbar(
  title = "Global Fertility Drivers: 1800–2026",
  theme = bs_theme(
    version = 5,
    bootswatch = "united",
    primary = "#800080" # Purple color for sliders and accents
  ) %>% 
    bs_add_rules(
      ".navbar-brand { font-weight: bold; font-size: 1.3rem; }
       .navbar-nav .nav-link { 
         color: #800080 !important;
         border: 1px solid rgba(128,0,128,0.2); 
         border-radius: 5px; 
         margin: 0 5px; 
         padding: 8px 15px !important;}
       .navbar-nav .nav-link.active { 
         background-color: rgba(128,0,128,0.1); 
         border-color: #800080;}"),
  
  #About page for narrative 
  
  #Tab 1: Education 
  nav_panel(
    title = "Education",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("The Impact of Education on Fertility Rates"),
        p("See the correlation between female education and fertility across religious groups."),
        selectInput(
          inputId = "religion_select_edu",
          label = "Select Religions:",
          choices = c("Christian", "Muslim", "Eastern", "Unaffiliated"),
          selected = c("Christian", "Muslim"),
          multiple = TRUE),
        div(
          class = "d-flex justify-content-between mb-2",
          actionButton("select_all_edu", "Select All", class = "btn-sm btn-outline-primary"),
          actionButton("deselect_all_edu", "Deselect All", class = "btn-sm btn-outline-secondary")),
        sliderInput(
          inputId = "year_select_edu",
          label = "Select Year (1970-2015):",
          min = 1970,
          max = 2015,
          value = 2010,
          step = 1,
          sep = ""),
        religion_color_key(),
        helpText("Dashed line shows the trend for all selected groups."),
        h5("Variable Key:"),
        p(strong("Fertility:"), "Number of children born to a woman, i.e., fertility rates. (Source: http://gapm.io/dtfr)"),
        p(strong("Education:"), "The average number of years of school attended by all females between 15 to 24 years old. (Source: Global Educational Attainment Distributions, IHME).")),
      card(
        card_header("The Impact of Education on Fertility Rates"),
        plotlyOutput("edu_plot", height = "600px"),
        div(style = "text-align: right; font-style: italic; font-size: 0.8rem; color: #666; padding: 5px 10px;", "(Source: Gapminder)")))),
  
  #Tab 2: Income
  nav_panel(
    title = "Income",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("The Impact of Income on Fertility Rates"),
        p("See the correlation between income and fertility across religious groups."),
        selectInput(
          inputId = "religion_select_wealth",
          label = "Select Religions:",
          choices = c("Christian", "Muslim", "Eastern", "Unaffiliated"),
          selected = c("Christian", "Muslim"),
          multiple = TRUE),
        div(
          class = "d-flex justify-content-between mb-2",
          actionButton("select_all_wealth", "Select All", class = "btn-sm btn-outline-primary"),
          actionButton("deselect_all_wealth", "Deselect All", class = "btn-sm btn-outline-secondary")),
        sliderInput(
          inputId = "year_select_wealth",
          label = "Select Year (1800-2025):",
          min = 1800,
          max = 2025,
          value = 2010,
          step = 1,
          sep = ""),
        religion_color_key(),
        helpText("Dashed line shows the trend for all selected groups."),
        h5("Variable Key:"),
        p(strong("Fertility:"), "Number of children born to a woman, i.e., fertility rates. (Source: http://gapm.io/dtfr)"),
        p(strong("Income:"), "Mean daily household per capita income or Purchasing Power Parity (PPP). (Source: http://gapm.io/dmincpcap_cppp).")),
      card(
        card_header("The Impact of Income on Fertility Rates"),
        plotlyOutput("wealth_plot", height = "600px"),
        div(style = "text-align: right; font-style: italic; font-size: 0.8rem; color: #666; padding: 5px 10px;", "(Source: Gapminder)")))),
  
  #Tab 3: Gender Equality 
  nav_panel(
    title = "Gender Equality",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("The Impact of Gender Equality on Fertility Rates"),
        p("Explore the relationship between the Gender Equality Index (IDEA) and fertility rates."),
        selectInput(
          inputId = "religion_select_idea",
          label = "Select Religions:",
          choices = c("Christian", "Muslim", "Eastern", "Unaffiliated"),
          selected = c("Christian", "Muslim", "Eastern", "Unaffiliated"),
          multiple = TRUE),
        div(
          class = "d-flex justify-content-between mb-2",
          actionButton("select_all_idea", "Select All", class = "btn-sm btn-outline-primary"),
          actionButton("deselect_all_idea", "Deselect All", class = "btn-sm btn-outline-secondary")),
        sliderInput(
          inputId = "year_select_idea",
          label = "Select Year (1975-2015):",
          min = 1975,
          max = 2015,
          value = 2010,
          step = 1,
          sep = "",
          animate = animationOptions(interval = 1000, loop = TRUE)),
        religion_color_key(),
        helpText("Gender Equality Index (IDEA) is scaled from 0 to 100, with 100 meaning female representation, access, and participation within society is equal to that of men."),
        helpText("Dashed line shows the trend for all selected groups."),
        h5("Variable Key:"),
        p(strong("Fertility:"), "Number of children born to a woman, i.e., fertility rates. (Source: http://gapm.io/dtfr)"),
        p(strong("Gender Equality:"), "As determined by Inclusion, Diversity, Equity, and Access or Accessibility, (IDEA). (Source: http://gapm.io/ddemocrix_idea).")),
      card(
        card_header("The Impact of Gender Equality on Fertility Rates"),
        plotlyOutput("idea_plot", height = "600px"),
        div(style = "text-align: right; font-style: italic; font-size: 0.8rem; color: #666; padding: 5px 10px;", "(Source: Gapminder)")))),
  
  #Tab 4: Trend Over Time 
  nav_panel(
    title = "Trend Over Time",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Comparison of Country Fertility Rates Over Time"),
        p("Select specific countries to see how their fertility rates have changed over time."),
        selectInput(
          inputId = "country_select",
          label = "Select Countries:",
          choices = countries_list,
          selected = c("Italy", "Philippines"),
          multiple = TRUE),
        div(
          class = "d-flex justify-content-between mb-2",
          actionButton("select_all_countries", "Select All", class = "btn-sm btn-outline-primary"),
          actionButton("deselect_all_countries", "Deselect All", class = "btn-sm btn-outline-secondary")),
        hr()),
      card(
        card_header("Comparison of Country Fertility Rates Over Time"),
        plotlyOutput("country_line_plot", height = "600px"),
        div(style = "text-align: right; font-style: italic; font-size: 0.8rem; color: #666; padding: 5px 10px;", "(Source: Gapminder)")))),

nav_panel(
  title = "About",
  layout_column_wrap(
    width = 1,
    card(
      h2("Narrative"),
      h3("Introduction"),
      p("Across the globe birth rates are in decline, with fertility rates hitting record lows. Many countries such as the US and South Korea are beginning to show concern as fertility rates dip below the replacement-level fertility rate of 2.1, or the number of births per woman needed to sustain population stability (https://publichealth.jhu.edu/2026/is-the-us-birth-rate-declining). So, why are so many women of childbearing limiting the number of children they have, with some forgoing motherhood altogether? This is a question that has not only captivated global leaders, but it is now garnering the attention of regular civilians as the consequences of low fertility rates become more apparent."),
      p("It has long been speculated, and studied, by historians that fertility rates are affected by a decline in child mortality; that is the odds of a child surviving past the age of five (UNICEF). Additionally, fertility rates have been considered a byproduct of socioeconomic development, such as urbanization (the move away from an agricultural dominant society), increased GDP, and decreased rates of poverty. But, what else drives fertility rates? And, what can we learn from examining the effect educational attainment, income, gender equality, religion, and time has on fertility levels?"),
      h3("The Relation Between Education, Income, Gender Equality, and Time"),
      p("The main relationship that I tried to highlight with my variables and graphs was that the drop in fertility rates is correlated to an increase in the quality of women’s lives. Thus, I chose three main variables that have increase, women’s educational attainment, women’s purchasing power (daily income), and female equality (IDEA). Due to each graph being isolated in it’s own separate tab, I intended to maintain the variable’s connection through my design choices. Thus, I not only chose scatterplots as the primary graph type. Not only for cohesion and connection, but because scatterplots allow for multiple variables to be visualized at once. This way, all of the variables I chose to analyze are able to be inserted into the graph (either by the interactive slider/selector or hover textbox) even when the graph is highlighting its key variable. With the final line graph, I hoped to bring together all variables so that they could be analyzed together while maintaining a new perspective on the data (time) and interactivity for the user. In this way, I hoped to visualize that although fertility rates are decreasing, so are the variables that marginalize women."),
      h3("Conclusion"),
      p("The purpose of women has long been framed as childbearing. Societal doctrine has reinforced the belief that women innately choose motherhood, and in such case, also innately desire to have as many children as possible. The idea that this may not be the case frames women who chose alternatively as defective. Yet as the number of women choosing to forgo motherhood increases and fertility rates decline, those in positions of power have responded by villainizing women who exercise their autonomy. In 2022, the late Pope Francis referred to individuals who choose not to have children as “selfish.” Similarly, in 2021, Vice President JD Vance’s remark describing women who opt out of motherhood as “childless cat ladies”, with the intent to incite slander, went viral."),
      p("Beyond simply low fertility rates, it is the consequences of such that are often framed as a woman’s failure to society and a problem they need to solve by bearing more children. However, if what the data suggests is true, and declining fertility is correlated to greater female empowerment, then why is it framed as a problem women need to solve? Declining fertility correlates with higher daily purchasing power, greater gender equality, and increased educational attainment. Rather than signaling societal decay, low fertility rates indicate expanded opportunity and female empowerment."),
      p("If fertility rates below replacement level indicate economic, labor market, or socio-structural challenges, those are issues for government around the world to address through policy and institutional reform. It can be speculated that low fertility rates as a “societal crisis” appear to only be a problem after the industrial revolution and introduction of the capitalist system. With economic growth, labor markets, and social stability being dependent on limiting women’s autonomy, ensuring they bear more children. Perhaps it is time for governments and the cultures they shape to confront the structural insecurities that low fertility highlights, rather than placing responsibility on women’s reproductive choices."),
      hr(),
      h2("Design Choice"),
      p("The design of this Shiny App Dashboard was intentionally structured to visually communicate why women are choosing to forgo motherhood and the factors shaping those decisions. To reinforce my argument that the consequences of low fertility rates are not solely the responsibility of women of childbearing age, I prioritized interactivity throughout the dashboard. By requiring users to actively filter, compare, and explore the data themselves, the design intends to push the idea that declining fertility is a broader societal issue and one that requires collective engagement rather than only one gender bearing the responsibility. I hope that users leave the dashboard feeling connected to both the data and the larger structural problem. The goal of the dashboard was to allow users to push pass being passive observers and instead be active participants in understanding the problem."),
      p("I selected scatterplots as the primary visualization because they most effectively display the relationships among overlapping variables and their association with fertility rates. Given that the main problem addressed in this project is the long term decline in fertility, I also included a timeline graph that allows the user to examine trends over the past two centuries. In alignment with the data visualization principle of data to ink ratio, I tried to keep the visualizations as simple as possible considering how data heavy they were."),
      p("As for how interactive I designed this Shiny App to be, I tried to ensure the user experience was the focal point. Thus, I incorporated as many interactive features as I could to enhance engagement, insight and clarity. Rather than presenting all visualizations on a single static homepage, I designed the app as a dashboard with multiple tabs, that way the user could flip back and forth, reducing boredom. For the education, income, and gender equality visualizations, I added two interactive features, whereas users can filter the data by selected religions and/or year. In addition to manual year selection, users also have the option to “play” an automatic year-by-year progression to watch trends over time. Recognizing that manually selecting or deselecting all religions (or countries in the trend over time graph) can be time consuming, I included a button that allows users to do so automatically. To further enhance clarity and insight, I incorporated a hover/popup box for each data point. This is to allow users to see additional and specific details of the data. For variety and comparative analysis, I included a final visualization that enables users to either examine a single country’s birth rate trend over time or compare multiple countries at the same time (for a little fun and chaos, they can select all countries at once). Finally, to minimize confusion and improve clarity, I added a variable key in the side panel that defines each measure displayed in the dashboard.")))))

#Server
server <- function(input, output, session) {
  
  #Plot 1: Education
  filtered_data_edu <- reactive({
    master_df %>%
      filter(religion %in% input$religion_select_edu) %>%
      filter(year == input$year_select_edu) %>%
      filter(!is.na(mean_years), !is.na(babies_per_woman))})
  
  output$edu_plot <- renderPlotly({
    make_correlation_plot(filtered_data_edu(), "mean_years", "Average Educational Attainment (in years)", log_x = FALSE)})
  
  #Plot 2: Income
  filtered_data_wealth <- reactive({
    master_df %>%
      filter(religion %in% input$religion_select_wealth) %>%
      filter(year == input$year_select_wealth) %>%
      filter(!is.na(income_ppp), !is.na(babies_per_woman))})
  
  output$wealth_plot <- renderPlotly({
    make_correlation_plot(filtered_data_wealth(), "income_ppp", "Daily Income", log_x = FALSE)})
  
  #Plot 3: time line
  filtered_data_country <- reactive({
    master_df %>%
      filter(name %in% input$country_select) %>%
      filter(year <= 2023) })
  
  
  output$country_line_plot <- renderPlotly({
    make_religion_line_plot(filtered_data_country())})
  
  #Add feature to select/deselect all counries
  observeEvent(input$select_all_countries, {
    updateSelectizeInput(session, "country_select", selected = countries_list)})
  
  observeEvent(input$deselect_all_countries, {
    updateSelectizeInput(session, "country_select", selected = character(0))})
  
  #select/deselect all religions
  observeEvent(input$select_all_edu, {
    updateSelectInput(session, "religion_select_edu", selected = religions_list)})
  observeEvent(input$deselect_all_edu, {
    updateSelectInput(session, "religion_select_edu", selected = character(0))})
  
  observeEvent(input$select_all_wealth, {
    updateSelectInput(session, "religion_select_wealth", selected = religions_list)})
  observeEvent(input$deselect_all_wealth, {
    updateSelectInput(session, "religion_select_wealth", selected = character(0))})
  
  observeEvent(input$select_all_idea, {
    updateSelectInput(session, "religion_select_idea", selected = religions_list)})
  observeEvent(input$deselect_all_idea, {
    updateSelectInput(session, "religion_select_idea", selected = character(0))})
  
  #Plot 4: gender equality
  filtered_data_idea <- reactive({
    master_df %>%
      filter(religion %in% input$religion_select_idea) %>%
      filter(year == input$year_select_idea) %>%
      filter(!is.na(idea), !is.na(babies_per_woman))})
  
  output$idea_plot <- renderPlotly({
    make_idea_plot(filtered_data_idea())})}

# Run the app
shinyApp(ui, server)
