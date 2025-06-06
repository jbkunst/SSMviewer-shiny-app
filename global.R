# packages ----------------------------------------------------------------
cli::cli_h1("Packages")
suppressMessages({
  # shiny
  library(shiny)
  library(bslib)
  library(fontawesome)
  library(bsicons)
  library(stringr)
  library(scales)
  library(cli)
  library(highcharter)
  library(leaflet)
  library(forcats)
  library(shinyWidgets)
  library(markdown)
  
  # data
  library(duckdb)
  library(dplyr)
  library(dbplyr)
  library(pool)
  library(tidyr)
  library(tibble)
  library(purrr)
  
  # network
  library(sigmajs)
  library(igraph)
  library(visNetwork)
  
  # extra
  library(sessioninfo)
  
})

source("R/app-helpers.R")

# parameters & options ----------------------------------------------------
app_parameters <- list(
  # data
  pool = get_con(),
  # theme
  fonts = list(
    base = "Inria Sans"
  ),
  colors =  list(
    fg      = "#5F5758",
    bg      = "#FFFFFF",
    primary = "#004F6D",
    danger  = "#FF3057"
  ),
  color_palette = c("#004f6d", "#ec9929", "#8d7257", "#4780a1", "#19d6ff", "#5F5758", "#FF3057")
)

# highcharter
hcopts <- getOption("highcharter.opts")
hcopts$colors <- app_parameters$color_palette

hcopts_lang  <- getOption("highcharter.lang")
hcopts_lang$thousandsSep <- ","
hcopts_lang$decimalPoint <- "."

options(
  highcharter.color_palette = app_parameters$color_palette, # just for data_to_hierarchical
  highcharter.opts  = hcopts,
  highcharter.lang  = hcopts_lang,
  highcharter.theme = hc_theme_smpl(
    colors = list(app_parameters$colors$primary),
    chart = list(style = list(fontFamily = app_parameters$fonts$base)),
    plotOptions = list(
      series = list(
        states = list(inactive = list(opacity = 0.2)),
        marker = list(symbol = "circle"),
        dataLabels = list(style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE))
        ),
      line = list(marker = list(symbol = "circle")),
      area = list(marker = list(symbol = "circle"))
    )
  )
)

# theme -------------------------------------------------------------------
app_theme <-  bs_theme(
  base_font = font_google(app_parameters$fonts$base),
  bg = app_parameters$colors$bg,
  fg = app_parameters$colors$fg,
  primary = app_parameters$colors$primary,
  ) |> 
  bs_add_rules(str_glue(".bslib-value-box.default .value-box-showcase>svg.bi {{ fill: {app_parameters$colors$primary} !important; }}")) |>
  identity()

gender_palette <- c("female" = "#8624f5", "male" = "#1fc3aa", "indeterminate" = "#bcbcbc")

# sidebar -----------------------------------------------------------------
opts_years <- table("papers_with_summaries") |> 
  count(year) |> 
  collect() |> 
  pull(year) |> 
  sort()

# https://github.com/anespinosa/sna-ssla/issues/5
opts_years <- 1990:2021 

# harcodeados dado que no se actualizará la data
opts_quinquenios <- c("1990-1994", "1995-1999", "2000-2004", "2005-2009",
                      "2010-2014", "2015-2019", "2020-2021")

opts_top_fields <- table("papers_fields") |> 
  filter(field_type == "Top") |>
  # glimpse() |> 
  count(field_name) |> 
  collect() |>
  arrange(desc(n)) |> 
  # arrange(field_name) |> 
  pull(field_name)

opts_regions <- table("affiliations") |> 
  count(region) |> 
 # arrange(desc(n)) |> 
  arrange(region) |> 
  collect() |> 
  pull(region) |> 
  na.omit() |> 
  as.character()

app_sidebar <- bslib::sidebar(
  # width = 300,
  
  conditionalPanel(
    condition = "!['Collab', 'Content', 'Multidiscipline'].includes(input.navsection)",
    shiny::sliderInput(
      "years", "Years",
      min = min(opts_years), max = max(opts_years),
      value = c(min(opts_years), max(opts_years)),
      sep = "",
      step = 1,
      ticks = FALSE
    ),
    selectizeInput(
      "top_fields", "Top Fields",
      choices = opts_top_fields,
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = "(All top fields)")
    ),
    selectizeInput(
      "regions", "Regions",
      choices = opts_regions,
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = "(All regions)")
    ),
    # Selector dinámico para países, basado en las regiones seleccionadas
    uiOutput("country_selector"),
    # Selector dinámico para instituciones, basado en los países seleccionados
    uiOutput("affiliation_selector")
  ),
  conditionalPanel(
    condition = "['Collab', 'Content', 'Multidiscipline'].includes(input.navsection)",
    sliderTextInput(
      "years2", "Years (5 yr period)",
      choices = opts_quinquenios,
      selected = "2020-2021"
    ),
    selectizeInput(
      "top_fields2", "Top Fields",
      choices = opts_top_fields,
      multiple = FALSE,
      selected = first(opts_top_fields),
      # options = list(placeholder = "(All top fields)")
    )
  ),
  actionButton("filtrar", "Apply filters", class = "btn-primary"),
  div(style = "height: 20px;"),
  div(
    class = "sidebar-logo",
    img(src = "logo-fondecyt.png", 
        style = "max-width: 100%; height: auto; display: block; margin: 0 auto;",
        alt = "Logo FONDECYT")
  ),
  tags$style(HTML("
    .bslib-sidebar-layout > .sidebar {
      position: relative !important;
      padding-bottom: 80px !important;
    }
    .sidebar-logo {
      position: absolute !important;
      bottom: 25px !important;
      left: 50% !important;
      transform: translateX(-50%) !important;
      width: calc(100% - 20px) !important;
      text-align: center !important;
    }
    .sidebar-logo img {
      max-height: 200px !important;
      max-width: 100% !important;
    }
  "))
)

# partials ----------------------------------------------------------------
card <- purrr::partial(bslib::card, full_screen = TRUE)

# other options -----------------------------------------------------------
lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      useHTML = TRUE,
      verticalAlign = "top",
      style = list(
        fontSize = "16px", 
        textOutline = FALSE,
        color = "#FFFFFF",
        fontWeight = "normal"
      )
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.75),
    dataLabels = list(enabled = TRUE),
    useHTML = TRUE,
    style = list(
      fontSize = "8px",
      textOutline = FALSE, 
      color = "white", 
      fontWeight = "normal"
    )
  )
)


