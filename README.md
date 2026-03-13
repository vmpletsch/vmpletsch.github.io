# Veronica Pletsch - Professional Portfolio & Data Visualizations

This repository contains the source code for my professional portfolio and personal website, built using [Quarto](https://quarto.org/) and [R](https://www.r-project.org/). It features a collection of data visualizations, writing samples, and professional experience.

## Project Description

The website serves as a central hub for my research projects, data visualization examples, and professional background. It includes interactive plots built with `ggplot2` and `plotly` to explore demographic and social science datasets (such as global income, fertility rates, gender equality indices, and religious affiliation). 

## Getting Started

To run this code and build the website locally on your own computer, follow the instructions below.

### Prerequisites & Software

You will need to install the following software:
1. **[R](https://cran.r-project.org/)**: The programming language used for data processing and visualizations.
2. **[RStudio](https://posit.co/download/rstudio-desktop/)** (Recommended): An IDE that makes working with R and Quarto seamless.
3. **[Quarto CLI](https://quarto.org/docs/get-started/)**: The open-source scientific and technical publishing system used to render the site.

### Required R Packages

You must install the necessary R packages before rendering the site. Open your R console and run the following command:

```R
install.packages(c("plotly", "ggplot2", "scales"))
```

### Data

All necessary dataset files are included directly in this repository (e.g., `gapminder_master.csv`, `gendereq_idea.csv`, `ppp.csv`, `religion.csv`). You do not need to download or obtain them separately.

### Execution Instructions

1. **Clone or Download the Repository**:
   Download this repository to your local machine and navigate into the project folder.

2. **Open the Project**:
   Open the project folder in RStudio.

3. **Render the Website**:
   To render and preview the Quarto website locally, use one of the following methods:
   - **In RStudio**: Open any `.qmd` file (e.g., `index.qmd`) and click the **Render** button at the top of the editor.
   - **Terminal / Command Line**: Open your terminal, navigate to the project directory, and run:
     ```bash
     quarto preview
     ```
     This will build the site and open a live preview in your default web browser.

   To build the final static HTML files for deployment, run:
   ```bash
   quarto render
   ```
   The compiled files will be output to the `docs/` directory, as configured in the `_quarto.yml` file.