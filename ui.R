bslib::page_navbar(
  title = "Sustainability Science Viewer",
  id = "navsection",
  window_title = "SSV",
  theme = app_theme,
  sidebar = app_sidebar,
  # nav_panel home ----------------------------------------------------------
  bslib::nav_panel(
    title = "Home",
    icon = icon("gauge"),
    tags$script(src = "https://code.highcharts.com/mapdata/custom/world-robinson-highres.js"),
    useBusyIndicators(),
    uiOutput("value_boxes")
    ),
  # nav_panel fields --------------------------------------------------------
  bslib::nav_panel(
    title = "Fields",
    icon = bs_icon("diagram-2"),
    layout_columns(
      col_widths = 12,
      card(card_header("Trends Over Time"), highchartOutput("fields_hc_time")),
      card(card_header("Distribution"), highchartOutput("fields_hc_dist"))
    )
  ),
  # nav_panel geo -----------------------------------------------------------
  bslib::nav_panel(
    title = "Geo",
    icon = bs_icon("globe"),
    layout_columns(
      col_widths = 6,
      card(card_header("Publication Count"), highchartOutput("geo_hc_chro")),
      card(card_header("Distribution"), highchartOutput("geo_hc_dist")),
      card(card_header("Trends Over Time"), highchartOutput("geo_hc_time")),
      card(card_header("Map network"), leafletOutput("geo_map_net", height = "100%"))
    )
  ),
  
  # nav_panel gender --------------------------------------------------------
  
  bslib::nav_panel(
    title = "Gender",
    icon = bs_icon("gender-ambiguous"),
    navset_card_underline(
      nav_panel(
        "Team composition",
        layout_columns(
          col_widths = 6,
          card(card_header("% authors by gender"), highchartOutput("authors_gender")),
          card(card_header("% of first authors by gender"), highchartOutput("first_author_gender")),
          card(card_header("Women in papers with two or more authors"), highchartOutput("women_in_collabs")),
          card(card_header("% of all-male papers"), highchartOutput("allmale_papers")),
        )
      ),
      nav_panel(
        "Collaboration",
        layout_columns(
          col_widths = 6,
          card(card_header("International collaborations by gender"), highchartOutput("int_collabs")),
         # card(card_header("One more card")),
        )
      ),
      nav_panel(
        "Impact",
        layout_columns(
          col_widths = 6,
          card(card_header("Top 20 Women with highest h-index"), DT::DTOutput("h_index")),
          card(card_header("Top 20 most cited papers led by women"), DT::DTOutput("citations_table"))
        )
      )
    )
  ),
  # nav_panel collab --------------------------------------------------------
  bslib::nav_panel(
    title = "Collab",
    icon = bs_icon("people-fill"),
    layout_columns(
      col_widths = c(6, 6, 12),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tooltip(tags$span(textOutput("collab_net_title", inline = TRUE), bs_icon("info-circle")), title = NULL, "The labels correspond to the ranking based on the selected centrality metric."),
          popover(
            tags$span(tags$small("Filters"), bsicons::bs_icon("gear-fill", title = "Settings")),
            selectInput("collab_net_color", "Color by", choices = c("Top field" = "field", Region = "region", Genre = "genre"), selectize = TRUE),
            selectInput("collab_top_metric", "Centrality", choices = c("Betweenness", "Weighted Degree", "Degree", "Page Rank")),
            radioButtons("collab_n_top", "Show top", c(50, 100, 250), inline = FALSE)
          ),
        ),
        sigmajsOutput("collab_net_chart", height = "100%")
        ),
      card(card_header("Total Network Statistics"), DT::DTOutput("collab_net_metrics")),
      card(card_header("Top authors by Centrality Index"), DT::DTOutput("collab_net_top_authors"))
      )
    ),
  # nav_panel content -------------------------------------------------------
  bslib::nav_panel(
    title = "Content",
    icon = bs_icon("chat-quote-fill"),
    layout_columns(
      navset_underline(
        nav_panel("Nouns wordcloud", highchartOutput("content_wordcloud")),
        nav_panel("Co-occurrence plot", visNetworkOutput("content_cooccurrence", height = "80vh")),
        nav_panel("Nouns frecuency table", DT::DTOutput("content_top_nouns"))
      )
    )
  ),
  
  # nav_panel multidiscipline -------------------------------------------------------
  bslib::nav_panel(
    title = "Multidiscipline",
    icon = bs_icon("book-half"),
    layout_columns(
      col_widths = 6,
      card(textOutput("wheel_title"), highchartOutput("discipline_wheel")),
      card(textOutput("diversity_table_title"), DT::DTOutput("diversity_index"))
      
    )
  ),
  
 

  # nav_panel about ---------------------------------------------------------
  bslib::nav_spacer(),
  bslib::nav_panel(
    title = "About",
    icon = bs_icon("info-square-fill"),
    fillable = TRUE,
    width = 1/1,
    column(
      width = 10,
      offset = 1,
      includeMarkdown("md/about.md")
    )
  )

)
