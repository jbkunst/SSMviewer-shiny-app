# source("global.R")
# input <- list(
#   years = c(2020, 2020),
#   top_fields = c("Environmental science"),

#   years2 = "2000-2004",
#   top_fields2 = "Environmental science",

#   regions = NULL,
#   collab_n_top = 250,
#   collab_net_color = "genre",
#   collab_top_metric = "Weighted Degree"
# )
# input <- list(years = c(1990, 2021), top_fields = c("Business", "Environmental science"), regions = "Northern America", countries = c("Canada"))

function(input, output, session) {
  
  # UI ----------------------------------------------------------------------
  # Observer para los selectores
  observe({
    
    # toma los 4 inputs
    
    # los originales
    multiple_sel <- input$top_fields
    years_range <- input$years

    # los simplificados
    single_sel <- input$top_fields2
    selected_quinquenio <- input$years2

    # dependerá de la sección en qué sentido actualizar
    section <- isolate(input$navsection)


    if(section %in% c("Collab", "Content", "Multidiscipline")) {
      # si estamos en pestañas de collab o content,
      # actualizamos los originales      
      updateSelectizeInput(session, "top_fields", selected = single_sel)
      
      years_range <- as.numeric(strsplit(selected_quinquenio, "-")[[1]])
      updateSliderInput(session, "years", value = c(years_range[1], years_range[2]))
    
    } else {
      # en caso contrario actualizamos los simplificados
      sel <-if(is_null(multiple_sel)) first(opts_top_fields) else first(multiple_sel)
      updateSelectizeInput(session, "top_fields2", selected = sel)
      
      closest_quinquenio <- find_closest_quinquenio(
        years_range[1],
        years_range[2],
        opts_quinquenios
      )
      updateSliderTextInput(session, "years2", selected = closest_quinquenio)
      
    }
    
  })
  
  # Selector de países dinámico basado en las regiones seleccionadas
  output$country_selector <- renderUI({
    if (is.null(input$regions) || length(input$regions) == 0) {
      # updateSelectizeInput(session, "countries", selected = NULL)
      return(NULL)
    }
    
    # Filtrar los países según las regiones seleccionadas
    selected_countries <- table("affiliations") |> 
      filter(region %in% input$regions) |> 
      distinct(country_name) |> 
      arrange(country_name) |> 
      collect() |> 
      pull()
    
    # Crear el selector de países
    selectizeInput(
      "countries",
      "Countries",
      choices = selected_countries,
      multiple = TRUE,
      options = list(placeholder = "(All countries)")
      )
  })
  
  # Selector de afiliaciones dinámico basado en los países seleccionados
  output$affiliation_selector <- renderUI({
    # si no hay regiones/paises seleccionadas no mostrar nada
    if (is.null(input$regions) || length(input$regions) == 0 || 
        is.null(input$countries) || length(input$countries) == 0
      ) {
      # updateSelectizeInput(session, "affiliations", selected = NULL)
      return(NULL)
    } 
    
    # Filtrar affiliation según los países seleccionados
    selected_affiliations <- table("affiliations") |> 
      filter(country_name %in% input$countries) |> 
      distinct(affiliation_name) |> 
      arrange(affiliation_name) |> 
      collect() |> 
      pull()
    
    # Crear el selector de instituciones
    selectizeInput(
      "affiliations",
      "Affiliations",
      choices = selected_affiliations,
      multiple = TRUE,
      options = list(placeholder = "(All affiliations)")
      )
  })
  
  # Data --------------------------------------------------------------------
  
  ## Data papers -----
  dat_papers2 <- reactive({
    
    cli::cli_inform("reactive `dat_papers`")
    
    # showNotification(cli::ansi_strip("Loading data") , type = "message")
    
    dat_papers <- table("papers_with_summaries")
    
    # asigno, pues en parquet hay error al usar filter(input$y1...)
    y1 <- input$years[1]
    y2 <- input$years[2]
    
    dat_papers <- dat_papers |> 
      filter(y1 <= year, year <= y2)
    
    # filter by top field
    if(!is.null(input$top_fields)){
      
      dat_papers_fields <- table("papers_fields")
      
      dat_papers_fields <- dat_papers_fields |> 
        filter(field_name %in% input$top_fields) |> 
        select(paper_id, field_id, field_name, field_type)
      
      dat_papers <- semi_join(
        dat_papers,
        dat_papers_fields,
        by = join_by(paper_id)
        )
      
    }
    
    # filter by regions/countries/affiliations
    if(!is.null(input$regions)){
      
      # cli_h1("filter by regions/countries/affiliations")
      # print(reactiveValuesToList(input))
      
      dat_affiliations <- table("affiliations") |> 
        select(affiliation_id, affiliation_name, country_name, region)
      
      # filtra segun orden: afiliacion, pais, region, toma el primero no nulo 
      if(!is.null(input$regions) && !is.null(input$countries) && !is.null(input$affiliations)) {
        # cli_h1("filtering by affiliations")
        dat_affiliations <- filter(dat_affiliations, affiliation_name %in% input$affiliations)
      } else if(!is.null(input$regions) && !is.null(input$countries)) {
        # cli_h1("filtering by countries")
        dat_affiliations <- filter(dat_affiliations, country_name %in% input$countries)
      } else if(!is.null(input$regions)) {
        # cli_h1("filtering by regions")
        dat_affiliations <- filter(dat_affiliations, region %in% input$regions)
      }
      
      dat_papers_auth_aff <- table("papers_auth_aff")
      
      # papers donde almenos UN autos es de la afiliaciones seleccionadas
      dat_papers_auth_aff <- inner_join(
        dat_papers_auth_aff, # contiene paper y autor con su afiliacion 
        dat_affiliations,    # contiene afiliaciones de interes
        by = join_by(affiliation_id)
      )
      
      # filtro dat_papers con el resultado previamente obtenido
      dat_papers <- semi_join(
        dat_papers,
        dat_papers_auth_aff,
        by = join_by(paper_id)
      )
    
    }
    
    dat_papers
      
  })
  
  # dat_papers <- debounce(dat_papers2, 1000)
  dat_papers <- eventReactive(input$filtrar, {
    if(input$filtrar == 0){
      showNotification(cli::ansi_strip("Loading data") , type = "message", duration = 2)
    } else {
      showNotification(cli::ansi_strip("Applying filters") , type = "message", duration = 2)
    }
    
    dat_papers2()
    
    }, ignoreNULL = FALSE)
  
  ## Data Collab (tablas precalculadas) -----
  dat_collab <- eventReactive(input$filtrar, {
    
    tf <- input$top_fields2
    ys <- input$years2
    
    f <- str_glue("data/net_rds/{tf}-{ys}.rds")
    
    cli::cli_inform("reactive `dat_collab`, file {f}")
    
    dat_collab <- readRDS(f)
    
    dat_collab
    
  }, ignoreNULL = FALSE)
  
  ## Data Content (tablas precalculadas) -----
  
  dat_content <- eventReactive(input$filtrar, {
    
    tf <- input$top_fields2
    ys <- input$years2
    
    f <- str_glue("data/nouns_rds/{tf}-{ys}.rds")
    
    cli::cli_inform("reactive `dat_content`, file {f}")
    
    dat_content <- readRDS(f)
    
    dat_content
    
  }, ignoreNULL = FALSE)
  
  
  ## Data Coocurrence (tablas precalculadas)
  
  ### Edges
  
  dat_edges_coocc <- eventReactive(input$filtrar, {
    
    tf <- input$top_fields2
    ys <- input$years2
    
    f <- str_glue("data/cooccurrence_rds/edges_{tf}-{ys}.rds")
    
    cli::cli_inform("reactive `dat_edges_coocc`, file {f}")
    
    dat_edges_coocc <- readRDS(f)
    
    dat_edges_coocc
    
  }, ignoreNULL = FALSE)
  
  ### Nodes
  
  dat_nodes_coocc <- eventReactive(input$filtrar, {
    
    tf <- input$top_fields2
    ys <- input$years2
    
    f <- str_glue("data/cooccurrence_rds/nodes_{tf}-{ys}.rds")
    
    cli::cli_inform("reactive `dat_nodes_coocc`, file {f}")
    
    dat_nodes_coocc <- readRDS(f)
    
    dat_nodes_coocc
    
  }, ignoreNULL = FALSE)
  
  ## Data Multidiscipline ----
  
  dat_auth_fields <- eventReactive(input$filtrar, {
    
    tf <- input$top_fields2
    ys <- input$years2
    
    f <- str_glue("data/fields_auth_rds/{tf}-{ys}.rds")
    
    cli::cli_inform("reactive `dat_auth_fields`, file {f}")
    
    dat_auth_fields <- readRDS(f)
    
    dat_auth_fields
    
  }, ignoreNULL = FALSE)
  

  ## Titles ----
  
  dat_titles <- reactive({
    
    cli::cli_inform("reactive `dat_titles`")
    
    dat_papers <- dat_papers()
    
    dat_titles <- inner_join(
      table("papers_titles"),
      dat_papers |> select(paper_id),
      by = join_by(paper_id)
    ) |> 
      collect()
    
    dat_titles
    
  })
  
  ## Papers summary ----
  
  dat_papers_summary <- reactive({
    
    cli::cli_inform("reactive `dat_papers_summary`")
    
    # showNotification(cli::ansi_strip("Calculating summaries") , type = "message")
    
    dat_papers <- dat_papers()
    
    d1 <- dat_papers |> 
      summarise(
        papers_n = as.numeric(n()),
        papers_avg_year = round(n()/n_distinct(year)),
        papers_avg_n_auth = round(mean(author_count), 2),
        papers_avg_n_aff = round(mean(affiliation_count), 2),
        papers_avg_citation = mean(citation_count),
        papers_avg_reference = mean(reference_count),
        journal_n = n_distinct(journal_id),
        original_venue_n = n_distinct(original_venue)   
      ) |> 
      collect() |> 
      mutate(across(everything(), as.numeric)) 
    
    dat_papers_auth_aff <- table("papers_auth_aff") |>
      select(paper_id, author_id, affiliation_id)
    
    dat_affiliations <- table("affiliations") |>
      select(affiliation_id, country_iso)
    
    d2 <- dat_papers |>
      select(paper_id) |> 
      left_join(dat_papers_auth_aff, by = join_by(paper_id)) |>
      left_join(dat_affiliations,by = join_by(affiliation_id)) |> 
      collect() |> 
      # glimpse() |> 
      summarise(
        auth_n          = n_distinct(author_id),
        aff_n           = n_distinct(affiliation_id),
        aff_countries_n = n_distinct(country_iso)
      ) |> 
      mutate(across(everything(), as.numeric))
    
    d3 <- dat_papers |> 
      count(year) |> 
      collect() |> 
      arrange(year)
    
    d31 <- d3 |> 
      summarise(
        # n_last = last(n),
        annual_growth_rate = (last(n)/first(n))^(1 / (n() - 1)) - 1
        # serie_anual = list(n)
      )
    
    dat_papers_summary <- bind_cols(d1, d2, d31, tibble(data_year = list(d3))) |> 
      as.list()
      
    dat_papers_summary
    
  })


  # Value boxes -------------------------------------------------------------
  output$value_boxes <- renderUI({
    
    cli::cli_inform("output `value_boxes`")
    
    ## Data ------
    dat_papers_summary <- dat_papers_summary()
    # check values
    dat_papers_summary |> 
      enframe() |>
      filter(map_lgl(value, is.numeric)) |>
      mutate(value = map(value, as.double)) |> 
      unnest(value)
    
    # Value boxes ----
    
    # Cada bloque controla un value box 
    vb_papers <- value_box(
      title = "Documents",
      value = comma(dat_papers_summary$papers_n),
      showcase = bs_icon("file-earmark-text"),
      tags$small(str_glue("Average of {comma(dat_papers_summary$papers_avg_year)} articles annually"))
    )
    
    vb_journals <- value_box(
      title = "Journals",
      value = comma(dat_papers_summary$journal_n),
      showcase = bs_icon("journal-bookmark")
    )
    
    vb_authors <- value_box(
      title = "Authors",
      value =  comma(dat_papers_summary$auth_n),
      showcase = bs_icon("person-fill")
      )
    
    vb_coauthors <- value_box(
      title = "Coauthorting",
      value =  comma(dat_papers_summary$papers_avg_n_auth, accuracy = 0.1),
      showcase = bs_icon("people-fill"),
      tags$small(str_glue("Average authors per document"))
    )
    
    vb_affiliations <- value_box(
      title = "Affiliations",
      value = comma(dat_papers_summary$aff_n),
      showcase = bs_icon("bank2")
      # str_glue("From {comma(dat_papers_summary$aff_countries_n)} countries")
      )
    
    vb_coaffiliation <- value_box(
      title = "Co-affiliation",
      value = comma(dat_papers_summary$papers_avg_n_aff, accuracy = 0.1),
      showcase = bs_icon("buildings"),
      tags$small(str_glue("Average affiliations per document"))
    )
    
    vb_countries <- value_box(
      title = "Countries",
      value = comma(dat_papers_summary$aff_countries_n),
      showcase = bs_icon("globe"),
      tags$small(str_glue("According to the authors' affiliation"))
    )
    
    sprk <- dat_papers_summary$data_year[[1]] |> 
      set_names(c("x", "y")) |> 
      hchart(
        if_else(nrow(dat_papers_summary$data_year[[1]]) <= 5, "column", "area"),
        showInLegend = FALSE, name = "Documents", fillOpacity = 0.25
        ) |>
      hc_yAxis(endOnTick = FALSE) |> 
      hc_plotOptions(series = list(animation = FALSE)) |> # Desactiva la animación 
      hc_responsive(
        rules = list(
          list(
            condition = list(maxWidth = 300),
            chartOptions = list(
              tooltip = list(enabled = FALSE),
              title = list(text = NULL),
              subtitle = list(text = NULL),
              xAxis = list(visible = FALSE),
              yAxis = list(visible = FALSE)
            )
          ),
          list(
            condition = list(minWidth = 300),
            chartOptions = list(
              tooltip = list(enabled = TRUE),
              title = list(text = "Annual Scientific Production"),
              xAxis = list(visible = TRUE, title = list(text = "Year")),
              yAxis = list(visible = TRUE, title = list(text= "Documents")),
              caption = list(text = "The annual growth rate shows the average yearly growth over a given period. It compares the final value to the initial value, distributing the total growth evenly across the years, giving an idea of consistent yearly progress.")
            )
          )
        )
      )
    
    sprk
    
    vb_growth <- value_box(
      title = "Annual growth rate",
      value = percent(dat_papers_summary$annual_growth_rate, accuracy = .1),
      showcase = sprk,
      full_screen = TRUE
    )
    
    vb_citation <- value_box(
      title = "Citations",
      value = comma(dat_papers_summary$papers_avg_citation, accuracy = .1),
      showcase = bs_icon("quote"),
      tags$small(str_glue("Average citations per document"))
    )
    
    vb_reference <- value_box(
      title = "References",
      value = comma(dat_papers_summary$papers_avg_reference, accuracy = .1),
      showcase = bs_icon("search"),
      tags$small(str_glue("Average references per document"))
    )
    
    # layout_column_wrap(width = 1/4,
    layout_columns(col_widths = 4,
      vb_papers,
      vb_journals,
      vb_authors,
      vb_coauthors,
      vb_affiliations,
      vb_coaffiliation,
      vb_countries,
      vb_growth,
      vb_citation,
      vb_reference
    )
    
  })

  # Fields ------------------------------------------------------------------
  ## Data ----
  dat_papers_fields <- reactive({

    cli::cli_inform("reactive `dat_hierarchical`")
        
    dat_papers <- dat_papers()
    
    dat_papers_fields <- inner_join(
      table("papers_fields") |> select(paper_id, field_name, field_type),
      dat_papers |> select(paper_id, year),
      by = join_by(paper_id)
      ) |> 
      # ordeno, importate para siguiente paso
      arrange(paper_id, desc(field_type), field_name)
  
    # dat_papers_fields |> collect() |> glimpse()
    dat_papers_fields
    
  })
  
  ## Treemap ----
  output$fields_hc_dist <- renderHighchart({

    cli::cli_inform("output `fields_hc_dist`")
    
    dat_papers_fields <- dat_papers_fields()
    
    # Selecciono un top y un sub esto es para que el total cuadre con los 
    # documentos seleccionados (dat_papers)
    dat_fields_top <- dat_papers_fields |> 
      filter(field_type == "Top") |> 
      select(paper_id, field_name) |> 
      collect()
    
    dat_fields_sub <- dat_papers_fields |> 
      filter(field_type == "Sub") |> 
      select(paper_id, field_name) |> 
      collect()
    
    # partir de dat_papers para cuadrar conteos 
    dat_fields_topsub <- full_join(
      dat_fields_top,
      dat_fields_sub,
      by = join_by(paper_id),
      relationship = "many-to-many",
      suffix = c("_top", "_sub")
      ) |> 
      # mutate(field_name_sub = coalesce(field_name_sub, "No subfield")) |>
      collect() |> 
      mutate(
        # field_name_top = forcats::fct_lump_prop(field_name_top, prop = 0.01, other_level = "Other Field"),
        # n = 5 by number of colors palette's length
        field_name_top = forcats::fct_lump_n(field_name_top, n = 6, other_level = "Other Field"),
        field_name_sub = forcats::fct_lump_prop(field_name_sub, prop = 0.01, other_level = "Other Subfield"),
      )
    
    dat_hierarchical <- dat_fields_topsub |> 
      mutate(val = 1) |> 
      highcharter::data_to_hierarchical(c(field_name_top, field_name_sub), val)
    
    dat_hierarchical
    
    hc <- hchart(
      dat_hierarchical,
      type = "treemap",
      name = "Fields",
      allowDrillToNode = TRUE,
      levels = lvl_opts,
      dataLabels = list(color = "white"), # this is the option!!
      tooltip = list(valueDecimals = FALSE)
    ) 
    
    hc
  
  })
  
  ## Trend (line)  ---- 
  output$fields_hc_time <- renderHighchart({
    
    cli::cli_inform("output `fields_hc_time`")
    
    dat_papers_fields <- dat_papers_fields()
    
    daux <- dat_papers_fields |> 
      filter(field_type == "Top") |> 
      collect() |> 
      distinct(paper_id, field_name, .keep_all = TRUE) |> 
      count(field_name, year) |> 
      mutate(field_name = fct_reorder(field_name, n, .fun = sum, .desc = TRUE))
    
    num_categories <- length(levels(daux$field_name))
    
    tf_vector <- if (num_categories > 5) {
      c(rep(TRUE, 5), rep(FALSE, num_categories - 5))
    } else {
      rep(TRUE, num_categories)
    }
    
    daux |> 
      select(x = year, y = n, group = field_name) |> 
      hchart(
        if_else(nrow(daux) <= 5, "column", "line"),
        # "line",
        hcaes(x, y, group = group),
        visible = tf_vector
        ) |> 
      hc_colors(getOption("highcharter.color_palette")) |> 
      hc_tooltip(table = TRUE, sort = TRUE) |>
      hc_legend(layout = "proximate", align = "right") |> 
      hc_xAxis(title = list(text = "Year")) |> 
      hc_yAxis(title = list(text = "Documents"))
    
  })
  
  ## Geo ------
  dat_papers_country <- reactive({
    
    cli::cli_inform("reactive `dat_papers_country`")
    
    dat_papers <- dat_papers()
    
    dat_papers_country <- inner_join(
      table("papers_auth_aff") |> select(paper_id, affiliation_id),
      dat_papers |> select(paper_id, year),
      by = join_by(paper_id)
    ) |> 
      left_join(
        table("affiliations") |> select(affiliation_id, country_iso, country_name, region),
        by = join_by(affiliation_id)
      )
    
    # dat_papers_country |> collect() |> glimpse()
    dat_papers_country
    
  })
  
  output$geo_hc_chro <- renderHighchart({
    
    cli::cli_inform("output `geo_hc_chro`")
    
    dat_papers_country <- dat_papers_country()
    
    daux <- dat_papers_country |> 
      group_by(country_iso) |>
      # value name for hcjs
      summarise(value = n()) |> 
      collect() 
    
    daux
    
    color_stops_index <- color_stops(colors = c("#EEEEEE", app_parameters$colors$primary))
    
    # hcmap(
    #   "custom/world-robinson-lowres",
    #   data = daux,
    #   name = "Documents by country",
    #   value = "value",
    #   borderWidth = 0.1,
    #   joinBy = c("hc-a2" , "country_iso"),
    #   nullColor = "#FFFFFF"
    # ) |>
    #   hc_colorAxis(stops = color_stops_index, type = "logarithmic") |>
    #   hc_legend(enabled = TRUE) |>
    #   hc_mapNavigation(enabled = FALSE) |>
    #   hc_credits(enabled = FALSE)

    highchart(type = "map") %>% 
      hc_add_series(
        mapData = JS("Highcharts.maps['custom/world-robinson-highres']"), 
        data = daux, 
        name = "Documents by country",
        joinBy = c("hc-a2" , "country_iso"),
        borderWidth = 0.1
      ) %>% 
      hc_chart(zoomType = "xy") %>% 
      hc_colorAxis(stops = color_stops_index, type = "logarithmic") |>
      hc_legend(symbolWidth = 500, align = "center", verticalAlign = "bottom") |>
      hc_mapNavigation(enabled = FALSE) |>
      hc_credits(enabled = FALSE)
    
  }) 
  
  output$geo_hc_dist <- renderHighchart({
    
    cli::cli_inform("output `geo_hc_dist`")
    
    dat_papers_country <- dat_papers_country()
    
    daux <- dat_papers_country |> 
      group_by(region, country_name) |> 
      summarise(n = n()) |> 
      collect() |> 
      ungroup() |> 
      filter(!is.na(region)) |> 
      # fix NA
      mutate(
        # region = coalesce(region, "No region"),
        # country_name = coalesce(country_name, "No country"),
      )
    
    dat_hierarchical <- daux |> 
      # mutate(val = 1) |> 
      highcharter::data_to_hierarchical(
        c(region, country_name),
        n
        # colors = viridis_pal()((daux |> distinct(region) |> nrow()))
        )
    
    dat_hierarchical
    
    hc <- hchart(
      dat_hierarchical,
      type = "treemap",
      name = "Regions",
      allowDrillToNode = TRUE,
      levels = lvl_opts,
      dataLabels = list(color = "white"), # this is the option!!
      tooltip = list(valueDecimals = FALSE)
    ) 
    
    hc
    
  })
  
  output$geo_hc_time <- renderHighchart({
    
    cli::cli_inform("output `geo_hc_time`")
    
    dat_papers_country <- dat_papers_country()
    
    # dat_papers_country |> filter(is.na(country_name)) |> collect()
    # 
    daux <- dat_papers_country |> 
      count(country_name, year) |> 
      collect() |> 
      # no considerar que no tiene pais/aff
      filter(!is.na(country_name)) |> 
      # bloque para quedarnos con 10 mas po
      mutate(
        # country_name = fct_na_value_to_level(country_name, "No affilation/country"),
        country_name = fct_reorder(as.character(country_name), n, .fun = sum, .desc = TRUE),
        ) |> 
      filter(country_name %in% head(levels(country_name), 10)) |> 
      mutate(
        country_name = fct_reorder(as.character(country_name), n, .fun = sum, .desc = TRUE),
      ) |> 
      count(country_name, year, wt = n)
      
    num_categories <- length(levels(daux$country_name))
    
    tf_vector <- if (num_categories > 5) {
      c(rep(TRUE, 5), rep(FALSE, num_categories - 5))
    } else {
      rep(TRUE, num_categories)
    }
    
    daux |> 
      select(x = year, y = n, group = country_name) |> 
      hchart("line", hcaes(x, y, group = group), visible = tf_vector) |> 
      hc_colors(getOption("highcharter.color_palette")) |> 
      hc_tooltip(table = TRUE, sort = TRUE) |>
      hc_legend(layout = "proximate", align = "right") |> 
      hc_xAxis(title = list(text = "Year")) |> 
      hc_yAxis(title = list(text = "Documents")) |> 
      hc_title(text = str_glue("Top {num_categories} of countries"))
    
  })
  
  output$geo_map_net <- renderLeaflet({
    
    cli::cli_inform("output `geo_map_net`")
    
    dat_papers <- dat_papers()
    
    dat_collab <- table("collab_institutions") |> 
      semi_join(dat_papers, by = join_by(paper_id)) |> 
      count(institution_1, institution_2, name =  "n", sort = TRUE) |>
      left_join(
        table("affiliations") |> select(affiliation_id, country_name_1 = country_name),
        by = join_by(institution_1 == affiliation_id)
      ) |> 
      left_join(
        table("affiliations") |> select(affiliation_id, country_name_2 = country_name),
        by = join_by(institution_2 == affiliation_id)
      ) |> 
      group_by(country_name_1, country_name_2) |> 
      summarise(n = sum(n)) |>
      arrange(desc(n)) |> 
      collect() |> 
      ungroup()
    
    dat_collab
    
    # dcord <- readr::read_csv("https://gist.githubusercontent.com/metal3d/5b925077e66194551df949de64e910f6/raw/c5f20a037409d96958553e2eb6b8251265c6fd63/country-coord.csv")
    # Write the data frame to a Parquet file
    # write_parquet(dcord, "data/country_coord.parquet")
    dcord <- table("country_coord") |> 
      collect() |> 
      janitor::clean_names() |> 
      rename(lat = latitude_average, lon = longitude_average)
    
    dat_collab <- dat_collab |>
      left_join(
        dcord, by = join_by(country_name_1 == country)
        ) |>
      rename(lat_1 = lat, lon_1 = lon) |> 
      left_join(
        dcord, by = join_by(country_name_2 == country)
      ) |>
      rename(lat_2 = lat, lon_2 = lon) 
      
    dat_collab <- dat_collab |>
      filter(country_name_1 != country_name_2) 
    
    # filter
    quantile(dat_collab$n)
    dat_collab_f <- dat_collab |> 
      filter(n > quantile(n, 0.90)) 
      # filter(n > 1)
      # head(50)
    
    trfn <- log
    trfn <- identity
    
    dat_collab_f <- dat_collab_f |> 
      mutate(
        width = rescale(trfn(n), to = c(1, 10)),
        opactity = rescale(trfn(n), to = c(.25, 1)), # reverse
        lbl = str_glue("{country_name_1} - {country_name_2}: {comma(n)}")
        ) |> 
      mutate(rn = row_number())
    
    map <- leaflet() |> 
      addTiles(
        # "https://tile.openstreetmap.es/{z}/{x}/{y}.png"
        "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
      )
    
    for(r in 1:nrow(dat_collab_f)){
      # cli::cli_progress_step(r)
      values <- dat_collab_f |> 
       filter(rn == r) |> 
        as.list()
      
      map <- map |>
       addPolylines(
         lng = c(values$lon_1, values$lon_2),
         lat = c(values$lat_1, values$lat_2),
         color = app_parameters$colors$primary,
         weight = values$width,
         opacity = values$opactity,
         smoothFactor = 1,
         label = values$lbl,
         labelOptions = labelOptions(
           style = list(
             "font-family"  = app_parameters$fonts$base,
             # "font-weight" = "bold",
             padding = "3px 8px"),
           textsize = "15px",
           direction = "auto"
         )
       )
     
    }
    
    dat_collab_c <- dat_collab_f |> 
      select(country_name_1, country_name_2, n) |> 
      pivot_longer(cols = -(n)) |> 
      count(value, wt = n, sort = TRUE) |> 
      mutate(lbl = str_glue("{value}: {comma(n)}")) |>  
      left_join(dcord, by = join_by(value == country)) |> 
      mutate(sz = rescale(n, to = c(5, 20)))
    
    map <- map |> 
      addCircleMarkers(
        data = dat_collab_c,
        ~lon,
        ~lat,
        popup = ~lbl,
        label = ~lbl,
        radius = dat_collab_c$sz,
        color = app_parameters$colors$danger,
        fillOpacity = .9,
        weight = 2,
        labelOptions = labelOptions(
          style = list(
            "font-family"  = app_parameters$fonts$base,
            # "color" = "red",
            # "font-weight" = "bold",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        ),
        options = markerOptions(zIndexOffset = 1000)  # Ensures circles stay on top
      )
    
    map
    
  })
  
  # Collab ------------------------------------------------------------------
  ## Net ------
  output$collab_net_title <- renderText({
    str_glue("Network’s {input$collab_n_top} most central nodes based on {str_to_title(input$collab_top_metric)}")
  })
  
  # output$collab_net_chart <- renderSigmajs({
  #   
  #   cli::cli_inform("reactive `collab_net_chart`")
  #   
  #   showNotification(cli::ansi_strip("Calculating network") , type = "message", duration = 5)
  #   
  #   print(sessioninfo::session_info())
  #   
  #   dat_collab <- dat_collab()
  #   
  #   g <- dat_collab$g
  #   g
  #   
  #   
  #   if(input$collab_top_metric == "Betweenness"){
  #     important_nodes <- order(V(g)$betweenness, decreasing = TRUE)[1:input$collab_n_top]
  #   } else if (input$collab_top_metric == "Weighted Degree"){
  #     important_nodes <- order(V(g)$weighted_degree, decreasing = TRUE)[1:input$collab_n_top]
  #   } else if (input$collab_top_metric == "Degree"){
  #     important_nodes <- order(V(g)$degree, decreasing = TRUE)[1:input$collab_n_top]
  #   } else {
  #     important_nodes <- order(V(g)$pagerank, decreasing = TRUE)[1:input$collab_n_top]
  #   }
  # 
  #   g_sub <- induced_subgraph(g, vids = important_nodes)
  #   
  #   nodes <- igraph::as_data_frame(g_sub, what = "vertices") |>
  #     as_tibble()
  #   
  #   dat_auth_name <- table("authors_with_extra_data") |> 
  #     select(author_id, author_name, field_name, region, p_gf) |> 
  #     mutate(author_id = as.character(author_id)) |> 
  #     mutate(author_id = str_remove(author_id, "\\.0$")) |> 
  #     collect() |> 
  #     mutate(p_gf_lbl = ifelse(p_gf < 0.5, "M", "F"))
  #   
  #   nodes <- nodes |> 
  #     left_join(dat_auth_name, join_by(name == author_id))
  #   
  #   # se ordena y setea tamaño 
  #   if(input$collab_top_metric == "Betweenness"){
  #     nodes <- nodes |> arrange(desc(betweenness)) |> mutate(size = rescale(betweenness, to = c(1, 5)))
  #   } else if (input$collab_top_metric == "Weighted Degree"){
  #     nodes <- nodes |> arrange(desc(weighted_degree)) |> mutate(size = rescale(weighted_degree, to = c(1, 5)))
  #   } else if (input$collab_top_metric == "Degree"){
  #     nodes <- nodes |> arrange(desc(degree)) |> mutate(size = rescale(degree, to = c(1, 5)))
  #   } else {
  #     nodes <- nodes |> arrange(desc(pagerank)) |> mutate(size = rescale(pagerank, to = c(1, 5)))
  #   }
  #   
  #   # se colorea
  #   if(input$collab_net_color == "field"){
  #     nodes <- nodes |> mutate(color = colorize(field_name))
  #   } else if (input$collab_net_color == "region"){
  #     nodes <- nodes |> mutate(color = colorize(region))
  #   } else {
  #     nodes <- nodes |> mutate(color = colorize(p_gf_lbl))
  #   }
  #   
  #   nodes <- nodes |>
  #     mutate(id = name, label = row_number(), .before = 1) |> 
  #     select(id, label, name, size, color, betweenness, weighted_degree, degree, pagerank)
  #   
  #   edges <- as_data_frame(g_sub, what = "edges") |>
  #     as_tibble() |>
  #     mutate(id = row_number()) |>
  #     rename(source = from, target = to)
  #   
  #   sigmajs() |>
  #     sg_nodes(nodes, id, label, size, color)  |>
  #     sg_edges(edges, id, source, target) |>
  #     # sg_force() |>
  #     sg_layout(layout = igraph::layout_nicely) |>
  #     # sg_layout(layout = ForceAtlas2::layout.forceatlas2) |>
  #     # sg_layout(layout = layout_igraph_stress_custom) |>
  #     sg_neighbours() |> 
  #     sg_events("clickNode") |> 
  #     identity()
  #   
  # })
  
  ## Table top authors ----
  # esto es para hacer click en la tabla DT definirla y luego consultarla en el observeEvent
  dat_collab_vertices <- reactive({
    cli::cli_inform("reactive `dat_collab_vertices`")
    
    dat_collab <- dat_collab()
    
    dat_collab_vertices <- dat_collab$vertices
    
    if(input$collab_top_metric == "Betweenness"){
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(betweenness))
    } else if (input$collab_top_metric == "Weighted Degree"){
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(weighted_degree))
    } else if (input$collab_top_metric == "Degree"){
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(degree))
    } else {
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(pagerank))
    }
    
    dat_auth_name <- table("authors_with_extra_data") |> 
      select(author_id, author_name, field_name, affiliation_name, region) |> 
      mutate(author_id = as.character(author_id)) |> 
      mutate(author_id = str_remove(author_id, "\\.0$")) |>  
      collect()
    
    dat_collab_vertices <- dat_collab_vertices |> 
      head(as.numeric(input$collab_n_top)) |> 
      select(name, betweenness, weighted_degree, degree, pagerank) |> 
      left_join(dat_auth_name, join_by(name == author_id)) |> 
      select(id = name, author_name, field_name, affiliation_name, region, everything()) 
    
    if(input$collab_top_metric == "Betweenness"){
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(betweenness))
    } else if (input$collab_top_metric == "Weighted Degree"){
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(weighted_degree))
    } else if (input$collab_top_metric == "Degree"){
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(degree))
    } else {
      dat_collab_vertices <- dat_collab_vertices |> arrange(desc(pagerank))
    }
    
    # id debe ser primero para la defincion de visible
    dat_collab_vertices <- dat_collab_vertices |> 
      mutate(rank = row_number(), .after = id)  |> 
      mutate(across(c(betweenness, weighted_degree, degree, pagerank), round_by_range))
      
    dat_collab_vertices
    
  })
  
  output$collab_net_top_authors <- DT::renderDT({
    cli::cli_inform("output `collab_net_top_authors`")
    
    dat_collab_vertices <- dat_collab_vertices()
    
    dat_collab_vertices |> 
      rename_all(str_replace_all, "_", " ") |> 
      rename_all(str_to_title) |> 
      datatable_sim(
        columnDefs = list(list(visible = FALSE, targets = 0))
      )
    
  })
  
  ## Table net metrics ----
  output$collab_net_metrics <- DT::renderDT({
    cli::cli_inform("output `collab_net_metrics`")
    
    dat_collab <- dat_collab()
    
    d_collab_metrics <- dat_collab$metrics
    d_collab_metrics <- d_collab_metrics |> 
      mutate(
        value = scales::comma(value, accuracy = 0.01),
        value = str_remove(value, "\\.00")
      ) 

    d_collab_metrics <- d_collab_metrics |> 
      pmap_df(function(metric, value, description){
        
        # description <- "Loremp ipsum"
        # metric <- "Metric"
        
        po <- tooltip(
          tags$span(metric, bs_icon("info-circle")),
          # title = metric,
          title = NULL,
          description
        )
        
        tibble(
          Metric = as.character(
            # tags$span(
            #   tags$span(metric, style = "padding-right: 5px;"),
            #   po
            # )
            po
          ),
          Value = value
        )
        
      })
    
    d_collab_metrics |> datatable_sim()
    
  })
  
  observeEvent(input$collab_net_chart_click_node, {
    
    print(input$collab_net_chart_click_node)
    
    cli::cli_inform("clicking node {input$collab_net_chart_click_node$label}")
    print(input$collab_net_chart_click_node$label)
    
    showModal(modal_author(input$collab_net_chart_click_node$id))
    
  })
  
  observeEvent(input$collab_net_top_authors_rows_selected, {
    
    print(input$collab_net_top_authors_rows_selected)
    
    rn <- as.numeric(input$collab_net_top_authors_rows_selected)
    
    cli::cli_inform("clicking row/rank {rn}")
    
    dat_collab_vertices <- dat_collab_vertices()
    
    id <- dat_collab_vertices |> 
      filter(rank == rn) |> 
      pull(id)
    
    showModal(modal_author(id))
    
  })
  
  # Content ------------------------------------------------------------------
  ## Wordcloud -----
  output$content_wordcloud <- renderHighchart({
    
    cli::cli_inform("output `content_wordcloud`")
    
    dat_content <- dat_content()
    
    wordcloud <- dat_content |> 
      slice_max(n, n = 60) |> 
      hchart("wordcloud", hcaes(name = lemma, weight = n)) |> 
      hc_tooltip(pointFormat = "{point.weight}") |> 
      hc_plotOptions(
        series = list(
          rotation = list(
            orientations = 1  
          ),
          minFontSize = 8,
          maxFontSize = 50
          )
      )
    
    wordcloud
    
  })
  
  ## Co-ocurrence plot -----
  output$content_cooccurrence <- renderVisNetwork({
    cli::cli_inform("output `content_cooccurrence`")
    
    dat_nodes_coocc <- dat_nodes_coocc()
    
    dat_edges_coocc <- dat_edges_coocc()
    
    # Nodes and edges pre-processing 
    
    min_log <- min(dat_nodes_coocc$value_log)
    max_log <- max(dat_nodes_coocc$value_log)
    
    dat_nodes_coocc$size <- 10 + 40 * (dat_nodes_coocc$value_log - min_log) / (max_log - min_log)
    dat_nodes_coocc$font.size <- 14 + 14 * (dat_nodes_coocc$value_log - min_log) / (max_log - min_log)
    dat_nodes_coocc$color.background <- ifelse(dat_nodes_coocc$id == "sustainability", "#004f6d", "#4780a1")
    dat_nodes_coocc$color.border <- dat_nodes_coocc$color.background
    dat_nodes_coocc$font.color <- "white"
    dat_nodes_coocc$shape <- "box"
    dat_nodes_coocc$shapeProperties <- map(1:nrow(dat_nodes_coocc), ~ list(borderRadius = 10))

    central_index <- which(dat_nodes_coocc$id == "sustainability")
    if(length(central_index) > 0) {
      dat_nodes_coocc$size[central_index] <- dat_nodes_coocc$size[central_index] * 1.4
    }
    
    # The plot
    
    visNetwork(dat_nodes_coocc, dat_edges_coocc) %>%
      visNodes() %>%
      visEdges(scaling = list(min = 1, max = 12)) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
      visLayout(randomSeed = 1234) |> 
      visPhysics(
        solver = "repulsion",
        repulsion = list(centralGravity = 0.3)
      )
  })

  
  
  ## Frecuency table -----
  output$content_top_nouns <- DT::renderDT({
    cli::cli_inform("output `content_top_nouns`")
    dat_content <- dat_content()
    colnames(dat_content) <- c("Noun (lemma)", "Absolute frequency", "Relative frequency") 
    DT::datatable(dat_content) |> 
      DT::formatPercentage("Relative frequency", digits = 2)
    })
  
  # Gender ------------------------------------------------------------------
  ## Data -----
  dat_papers_gender <- reactive({
    
    cli::cli_inform("reactive `dat_papers_gender`")
    
    dat_papers <- dat_papers()

    dat_papers_gender <- inner_join(
      table("papers_gender"),
      dat_papers |> select(paper_id, year),
      by = join_by(paper_id)
    ) |> 
      collect()
    
    dat_papers_gender
    
  })

  ## Tab: team composition ----
  ### % authors by gender -----
  output$authors_gender <- renderHighchart({
    cli::cli_inform("output `authors_gender`")
    dat_papers_gender <- dat_papers_gender()
    
    total_authors <- sum(dat_papers_gender$n)
    
    gender_total <- dat_papers_gender |> 
      group_by(name_gender) |> 
      summarise(n = sum(n)) |> 
      mutate(percent = round((n / total_authors) * 100, 2), 
             color = gender_palette[name_gender]) |> 
      hchart("column", hcaes(x = name_gender, y = percent, color = color)) |> 
      hc_yAxis(
        labels = list(format = "{value}%"), 
        title = list(text = ""),
        min = 0,
        max = 100
      ) |> 
      hc_xAxis(
        title = list(text = NULL)
      ) |>  
      hc_tooltip(
        pointFormat = "<br>{point.percent:.1f}%<br>(n = {point.n:,.0f})"
      )
    
      gender_total
    
  })
  
  ### % of first authors by gender -----
  
  output$first_author_gender <- renderHighchart({
    cli::cli_inform("output `first_author_gender`")
    dat_papers_gender <- dat_papers_gender()
    
    total_papers <- n_distinct(dat_papers_gender$paper_id)
    
    first_author_gender <- dat_papers_gender |> 
      distinct(paper_id, gender_first_auth) |> 
      count(gender_first_auth) |> 
      drop_na() |> 
      mutate(percent = round((n / total_papers) * 100, 2), 
             color = gender_palette[gender_first_auth]) |> 
      hchart("column", hcaes(x = gender_first_auth, y = percent, color = color)) |> 
      hc_yAxis(
        labels = list(format = "{value}%"), 
        title = list(text = ""),
        min = 0,
        max = 100
      ) |> 
      hc_xAxis(
        title = list(text = NULL)
      ) |>  
      hc_tooltip(
        pointFormat = "<br>{point.percent:.1f}%<br>(n = {point.n:,.0f})"
      )
    
    first_author_gender
    
  })
  
  ### Women in collabs -----
  output$women_in_collabs <- renderHighchart({
    cli::cli_inform("output `women_collab`")
    dat_papers_gender <- dat_papers_gender()
    
    
    total_papers <- dat_papers_gender |> 
      distinct(paper_id, .keep_all = TRUE) |> 
      filter(is_collab == TRUE) |> 
      n_distinct()
    
    women_percent <- dat_papers_gender |>
      filter(is_collab == TRUE, name_gender == "female") |>
      mutate(percent = round((n / n_authors) * 100, 2),
             women_team = if_else(percent >= 50, "50% or more", "fewer than 50%")) |> 
      count(women_team) |> 
      mutate(percent = round((n / total_papers) * 100, 1),
             color = ifelse(women_team == "50% or more", "#8624f5", "#f2f2f2"))
    
    
    women_in_collabs <- women_percent|> 
      hchart("pie", 
             hcaes(
               x = women_team, 
               y = percent,
               color = color), 
             innerSize = "50%"
      ) |> 
      hc_plotOptions(
        pie = list(
          dataLabels = list(enabled = TRUE, format = "Papers with {point.name} <br>women in the team:<br>{point.y}%")
        )
      ) |>  
      hc_tooltip(
        pointFormat = "<br>{point.percent:.1f}%<br>(n = {point.n:,.0f})"
      )
    
    women_in_collabs
    
    # # Histogram
    # women <- dat_papers_gender |>
    #   filter(is_collab == TRUE, name_gender == "female") |>
    #   mutate(percent = round((n / n_authors) * 100, 2))
    # 
    # women_in_collabs <- hchart(women$percent, color = "#8624f5") |> 
    #   hc_xAxis(
    #     title = list(text = "Percentage of women authors"),
    #     labels = list(format = "{value}%")  # Agrega el símbolo de porcentaje
    #   ) |> 
    #   hc_yAxis(
    #     title = list(text = "Number of papers")  # Agrega el símbolo de porcentaje
    #   )|> 
    #   hc_legend(enabled = FALSE) 
    # 
    # women_in_collabs
    # 
    
    # Density? 
    
    # hchart(density(women$percent), type = "area", color = "#8624f5") |> 
    #   hc_xAxis(
    #     title = list(text = "Percentage of women authors"),
    #     labels = list(format = "{value}%")  # Agrega el símbolo de porcentaje
    #   ) |> 
    #   hc_tooltip(enabled = FALSE)
    
    
  })
  
  
  ### All-male papers ----
  output$allmale_papers <- renderHighchart({
    cli::cli_inform("output `all_male_papers`")
    dat_papers_gender <- dat_papers_gender()
    
    total_papers <- n_distinct(dat_papers_gender$paper_id)
    
    all_male_data <- dat_papers_gender |> 
      filter(name_gender == "male" & n == n_authors) |> 
      count(name_gender)  |> 
      (\(df) bind_rows(df, data.frame(name_gender = "other", n = total_papers - df$n)))() |> 
      mutate(
        percent = round((n / total_papers) * 100, 1),
        name_gender = ifelse(name_gender == "male", "all-male papers", "other"),
        color = ifelse(name_gender == "all-male papers", "#1fc3aa", "#f2f2f2")
      ) 
    
    all_male <- all_male_data|> 
      hchart("pie", 
             hcaes(
               x = name_gender, 
               y = percent,
               color = color), 
             innerSize = "50%"
      ) |> 
      hc_plotOptions(
        pie = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y}%")
        )
      ) |> 
      hc_tooltip(
        pointFormat = "<br>{point.percent:.1f}%<br>(n = {point.n:,.0f})"
      )
    
    all_male
  })

  ## Tab: Collabs ----
  ### International collabs ----
  output$int_collabs <- renderHighchart({
    
    cli::cli_inform("output `int_collabs`")
    
    dat_papers_gender <- dat_papers_gender()
    
    int_collabs <- table("papers_with_summaries") |>
      select(paper_id, is_int_collab) |>
      collect()

    total_papers_gender <- dat_papers_gender |>
      distinct(paper_id, gender_first_auth) |>
      count(gender_first_auth, name = "total_gender")
    
      collabs <- dat_papers_gender |>
        distinct(paper_id, gender_first_auth) |> 
        left_join(int_collabs, by = "paper_id") |> 
        count(gender_first_auth, is_int_collab) |> 
        left_join(total_papers_gender) |> 
        drop_na() |>
        mutate(
          percent = round((n / total_gender) * 100, 2),
          color = gender_palette[gender_first_auth],
          color = if_else(is_int_collab == TRUE, color, "#e2e2e2"),
          is_int_collab = if_else(is_int_collab == TRUE, "International Collaboration", "National Collaboration"),
          is_int_collab = factor(is_int_collab, levels = c("National Collaboration", "International Collaboration"))
          )
      
      collabs |> 
        hchart(
          "column",
          hcaes(
            x = gender_first_auth, 
            y = percent, 
            color = color, 
            group = is_int_collab)
          ) |> 
        hc_plotOptions(
          series = list(stacking = "normal")
          ) |> 
        hc_legend(enabled = FALSE) |> 
        hc_xAxis(
               title = list(text = "")
               ) |> 
        hc_tooltip(
          pointFormat = "{series.name}<br>{point.percent:.1f}%<br>(n = {point.n:,.0f})"
        )

    ### One more card ----
     
      # TBD
      
  })
  
  ## Tab: impact ----
  
  ### H-Index  ----
  output$h_index <- DT::renderDT({
    
    cli::cli_inform("output `h_index`")
    

    
    h_index <- table("authors_with_extra_data") |>
      select(author_id, author_name, h_index_auth, name_gender) |>
      filter(name_gender == "female") |>
      mutate(author_id = as.character(author_id)) |> 
      mutate(author_id = str_remove(author_id, "\\.0$"))  # esto hubo que hacerlo porque en esta tabla la variable author_id qudó como dbl. Y no se puede convertir a int directamente porque duckdb tiene problema con los INT32
      
    
    
    dat_papers <- dat_papers()
    

    paper_author <- dat_papers |>
       select(paper_id, year) |>
       inner_join(table("papers_auth_aff") |>
                    select(paper_id, author_id)) 
    
    top_20_h_index <- paper_author |> 
      inner_join(h_index, by = "author_id") |>
      distinct(author_name, h_index_auth) |> 
      slice_max(h_index_auth, n = 20) |> 
      rename(`Author name` = author_name, `H-Index`= h_index_auth) |> 
      collect()

    datatable_sim(top_20_h_index)
    
  })
  
  ### Citations  ----
  
  output$citations_table <- DT::renderDT({
    
    dat_titles <- dat_titles()
    
    dat_papers_gender <- dat_papers_gender() 
    
    dat_papers <- dat_papers()
    

    female_led_papers <- dat_papers_gender |> 
      filter(gender_first_auth == "female") |> 
      select(paper_id) 
    
    top_20_citations <- dat_papers |> 
      select(paper_id, doi, year, citation_count) |> 
      collect() |> 
      inner_join(female_led_papers) |> 
      distinct(paper_id, .keep_all = TRUE) |> 
      slice_max(citation_count, n = 20) |>
      left_join(dat_titles) |>
       mutate(paper_title = str_glue('<a href="https://doi.org/{doi}" target="_blank">{paper_title}</a>')) |>
      arrange(desc(citation_count)) |> 
     select(Title = paper_title, Year = year, `Nº Citations`= citation_count) 

    
    datatable_sim(top_20_citations,
                  options = list(pageLength = 20, scrollX = TRUE),
                  columnDefs = list(
                    list(width = '65%', targets = 0),  
                    list(width = '10%', targets = 1),  
                    list(width = '25%', targets = 2)   
                  ))
  })
  
  
  # Multidiscipline -----
  
  ## Data
  
  dat_diversity_index <- reactive({
    
    cli::cli_inform("reactive `dat_diversity_index`")
    
    # years in range
    start <- as.numeric(str_split(input$years2, "-")[[1]][1])
    finish <- as.numeric(str_split(input$years2, "-")[[1]][2])
    
    dat_papers <- dat_papers() 
    
    dat_papers_fields <- dat_papers_fields()
    
    dat_diversity_index <- inner_join(
      table("papers_multidicipline_index"),
      dat_papers |> select(paper_id, doi, year),
      by = join_by(paper_id)
    ) |> 
       filter(year >= start & year <= finish) |> 
      inner_join(dat_papers_fields |> 
                  filter(field_type == "Top")) |> 
      filter(field_name == input$top_fields2) |> 
      collect() 
    
    dat_diversity_index
    
  })
  
  ## Table ----

  output$diversity_table_title <- renderText({
    paste("Top 20", input$top_fields2, "Papers with Highest Diversity Index")
  })
  
  output$diversity_index <- DT::renderDT({
    
    dat_diversity_index <- dat_diversity_index()
    
    dat_titles <- dat_titles()
    
    top_20_diversity <- dat_diversity_index |> 
      slice_max(multidisciplinarity_messure, n = 20) |> 
      left_join(dat_titles) |> 
      mutate(paper_title = str_glue('<a href="https://doi.org/{doi}" target="_blank">{paper_title}</a>'),
             multidisciplinarity_messure = round(multidisciplinarity_messure, 2)) |> 
      select(Title = paper_title, Year = year, `Shannon Index`= multidisciplinarity_messure)


    datatable_sim(top_20_diversity,
                  options = list(pageLength = 20, scrollX = TRUE),
                  columnDefs = list(
                    list(width = '70%', targets = 0),  
                    list(width = '10%', targets = 1),  
                    list(width = '20%', targets = 2)   
                  ))
    
  })
  

  
  
  ## Dependency Wheel ----
  
  output$wheel_title <- renderText({
    paste("Disciplines Collaborating with", input$top_fields2, "Authors")
  })
  
  output$discipline_wheel <- renderHighchart({
    
    cli::cli_inform("output `discipline_wheel`")
    
    dat_auth_fields <- dat_auth_fields()
    
    color_palette_fields <- c("#004f6d", "#ec9929", "#8d7257", "#4780a1", "#19d6ff", 
                          "#5F5758", "#FF3057", "#2E8B57", "#9370DB", "#FF6347",
                          "#20B2AA", "#DAA520", "#708090", "#FF1493", "#32CD32",
                          "#4169E1", "#FF8C00", "#8B4513", "#00CED1", "#DC143C")
    
    dat_auth_fields$color <- rep(color_palette_fields, length.out = nrow(dat_auth_fields))
    
    highchart() |> 
      hc_chart(type = "dependencywheel") |> 
      hc_add_series(
        data = list_parse(dat_auth_fields),
        keys = c("from", "to", "weight", "color"),
        name = "Authors' fields"
      ) |> 
      hc_plotOptions(
        dependencywheel = list(
          dataLabels = list(
            enabled = TRUE,
            color = "#2c3e50",
            backgroundColor = "rgba(255, 255, 255, 0.8)",
            borderRadius = 2,
            padding = 3,
            style = list(
              fontSize = "11px",
              fontWeight = "normal"
            )
          )
        )
      )
    
  })
  

}