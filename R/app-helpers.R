get_con <- function() {

  # pool <- dbPool(
  #   drv = RPostgres::Postgres(),
  #   host = Sys.getenv("HOST"),
  #   port = 5432,
  #   dbname = Sys.getenv("DB"), 
  #   user = Sys.getenv("USER"),
  #   password = Sys.getenv("PWD")
  # )
  
  pool <- dbPool(
    drv = duckdb::duckdb(),
    dbdir = "data/data_base.duckdb", 
    read_only = TRUE
  )  
  
  pool
  
}

table <- function(table_name = "papers_fields"){
  
  # if(source == "parquet"){
  #   
  #   if(table_name == "abstracts") return(arrow::open_dataset("data/abstracts/"))
  #   
  #   t <- arrow::open_dataset(str_glue("data/{table_name}.parquet"))
  #   
  # } else if (source == "postgres"){
  #   
  #   t <- tbl(app_parameters$pool, table_name)
  #   
  # } else {
  #   
  #   stop("source needs to be parquet or postgres")
  #   
  # }
  # 
  # return(t)
  
  tbl(app_parameters$pool, table_name)
  
}

get_network_metrics <- function(g){

  # g <- make_ring(10) # Ejemplo de grafo en anillo
  
  num_authors <- vcount(g)
  num_collabs <- ecount(g)
  network_density <- edge_density(g)
  network_diameter <- diameter(g, directed = FALSE)
  avg_path_length <- mean_distance(g, directed = FALSE)
  clustering_coeff <- transitivity(g, type = "global")
  num_components <- components(g)$no
  degree_assortativity <- assortativity_degree(g)
  avg_degree_centrality <- mean(degree(g))
  avg_closeness_centrality <- mean(closeness(g))
  avg_betweenness_centrality <- mean(betweenness(g))
  
  pr_values       <- page_rank(g, directed = FALSE)$vector
  avg_pagerank    <- mean(pr_values)
  
  # sp_matrix       <- distances(g)  
  # reachable_pairs <- sum(sp_matrix < Inf & row(sp_matrix) != col(sp_matrix))
  # connectedness   <- reachable_pairs / (vcount(g) * (vcount(g) - 1))
  # fragmentation   <- 1 - connectedness
  
  comp            <- igraph::components(g, mode = "weak")
  comp_entropy    <- entropy::entropy.empirical(comp$csize, unit = "log2")

  network_metrics <- tribble(
    ~metric, ~value, ~description,
    "Number of Authors",               num_authors,                "Total number of nodes (or vertices) in the network.",
    "Number of Collabs",               num_collabs,                "Total number of edges (or connections) in the network.",
    "Network Density",                 network_density,            "Ratio of the number of edges to the maximum possible number of edges.",
    "Degree Assortativity",            degree_assortativity,       "Correlation of nodes' degree with the degree of neighboring nodes.",
    "Average Degree Centrality",       avg_degree_centrality,      "Average number of connections per node.",
    "Average Closeness Centrality",    avg_closeness_centrality,   "Average closeness of nodes to each other in the network.",
    "Average Betweenness Centrality",  avg_betweenness_centrality, "Average extent to which nodes lie on the shortest paths between others.",
    "Network Diameter",                network_diameter,           "Longest shortest path between any two nodes in the network.",
    "Average Path Length",             avg_path_length,            "Average shortest path length between any two nodes in the network.",
    "Clustering Coefficient",          clustering_coeff,           "Measure of the degree to which nodes tend to cluster together.",
    "Number of Components",            num_components,             "Number of disconnected components (subnetworks) within the network.",
    "Average PageRank",                avg_pagerank,               "Mean PageRank value of all nodes in the network.",
    # "Connectedness",                   connectedness,              "Proportion of node pairs that are mutually reachable.",
    # "Fragmentation",                   fragmentation,              "1 - Connectedness.",
    "Entropy of Component Sizes",      comp_entropy,               "Shannon entropy (bits) of the distribution of component sizes."
  )
  
  # ### CONNECTEDNESS (WHOLE NETWORK: UCINET):
  # (sum((sna::reachability(network::as.network(g)))))/(vcount(g)*(vcount(g)-1)) # same as UCINET!
  # 
  # ### FRAGMENTATION:
  # 1-(sum((sna::reachability(network::as.network(SC)))))/(vcount(g1sc)*(vcount(g1sc)-1))
  # 
  # h_index = function(cites) {
  #   if(max(cites) == 0) return(0) # assuming this is reasonable
  #   cites = cites[order(cites, decreasing = TRUE)]
  #   tail(which(cites >= seq_along(cites)), 1)
  # }
  # 
  # library(entropy)
  # entropy.empirical(components(g1sc, mode=c("weak"))$csize, unit="log2")

  network_metrics
    
}

get_top_nodes <- function(g, top = 50){
  
  degree_centrality <- igraph::degree(g, mode = "all")
  closeness_centrality <- igraph::closeness(g, mode = "all", normalized = TRUE)
  betweenness_centrality <- igraph::betweenness(g, normalized = TRUE)
  eigenvector_centrality <- igraph::eigen_centrality(g)$vector
  
  node_metrics <- tibble(
    author = V(g)$name,  # Asume que los nodos tienen un atributo 'name' que contiene el nombre del autor
    degree_centrality = degree_centrality,
    closeness_centrality = closeness_centrality,
    betweenness_centrality = betweenness_centrality,
    eigenvector_centrality = eigenvector_centrality
  )
  
  node_metrics <- node_metrics |> 
    arrange(desc(degree_centrality)) |> 
    mutate(rank = row_number(), .before = 1)
 
  top_nodes <- head(node_metrics, top)
  
  # Mostrar el tibble
  top_nodes
  
}

datatable_sim <- function(data, ...){
  DT::datatable(
    data,
    options = list(
      ...,
      paging = FALSE,        # Desactivar paginación
      searching = FALSE,     # Desactivar barra de búsqueda
      lengthChange = FALSE,  # Ocultar selector de ítems por página
      info = FALSE           # Eliminar la frase "Showing X to Y of Z entries"
    ),
    rownames = FALSE,
    escape = FALSE,
    selection = "single"    # Let user select a single row at a time
  )
}

layout_igraph_stress_custom <- function(g){
  
  g |>
    graphlayouts::layout_igraph_stress() |> 
    select(1, 2) |> 
    setNames(NULL) |>
    as.matrix() 
  
}

modal_author <- function(id = 2122291097){
  
 cli:: cli_inform("modal_author - entra")

  id <- as.numeric(id)
  
  cli:: cli_inform("modal_author - dauth_id")

  dauth_id <- table("authors_with_extra_data") |> 
    filter(author_id == id) |> 
    collect() |> 
    mutate(across(everything(), as.character)) |> 
    pivot_longer(cols = everything())

  cli:: cli_inform("modal_author - dauth_id 2")

  dauth_id <- dauth_id |>
    # elimino lo que es _id
    filter(!str_detect(name, "_id$")) |> 
    filter(name != "author_main_field") |> 
    mutate(
      
      name = case_when(
        name == "subregion_un" ~ "subregion",
        name == "name_gender" ~ "gender",
        TRUE ~ name
      ),
      name = str_replace_all(name, "_", " "),
      name = str_to_title(name),
      
      value = map_chr(value, function(x){ 
        # if(str_detect(x, "^[-+]?\\d*\\.?\\d+$")) {
        if(!is.na(as.numeric(x))) {

          x <- as.numeric(x)
          x <- scales::comma(x, accuracy = 0.01)
          x <- str_remove(x, "\\.00")
        } else{
          x
        }
        x
      })
    ) |> 
    setNames(c("Attribute", "Value"))

  glimpse(dauth_id)

  dauth_id_list <- as.list(deframe(dauth_id))

  cli:: cli_inform("modal_author - dpapers_auth_id")
  
  dpapers_auth_id <- table("papers_auth_aff") |> 
    filter(as.character(author_id) == as.character(id))
  
  cli:: cli_inform("modal_author - d1")
  
  # hc1: cantidad de paper por año
  d1 <- table("papers_with_summaries") |> 
    semi_join(dpapers_auth_id) |> 
    # collect() |> 
    count(year) |> 
    select(x = year, y = n) |> 
    arrange(x) |> 
    collect() 
  hc1 <-  hchart(d1, ifelse(nrow(d1) < 4, "column", "line"), name = str_glue("Paper by year"))
  
  # hc2: paper field
  hc2 <- table("papers_fields") |>
    select(paper_id, field_id, field_name) |> 
    inner_join(dpapers_auth_id |> select(paper_id), by = join_by(paper_id)) |> 
    collect() |> 
    mutate(
      field_name_top = forcats::fct_lump_n(field_name, n = 6, other_level = "Other Fields"),
      # field_name_top = forcats::fct_lump(field_name, other_level = "Other Fields"),
    ) |> 
    count(field_name_top, sort = TRUE) |> 
    select(name = field_name_top, y = n) |> 
    hchart("pie", , name = "Papers by Top Field", colorByPoint = TRUE) |> 
    hc_colors(getOption("highcharter.color_palette"))
  
  content <- layout_columns(
    col_widths = c(6, 6, 12),
    row_heights = "250px",
    card(card_header("Papers by year"), hc1),
    card(card_header("Papers by Top Field"), hc2),
    card(card_header("Information"), datatable_sim(dauth_id), height = "350px")
    )
  
  # content
  # dauth_id_list
  
  modalDialog(
    title = str_glue("{dauth_id_list$author_name}"),
    size = "xl",
    content,
    footer = NULL,
    easyClose = TRUE
    )
  
}

# quinquenios -------------------------------------------------------------
# Función auxiliar para encontrar el quinquenio más cercano
get_center_max <- function(vec) {
  # Encuentra el valor máximo en el vector
  max_val <- max(vec)
  # Encuentra las posiciones donde ocurre el valor máximo
  max_positions <- which(vec == max_val)
  # Calcula la posición "media"
  center_pos <- round(mean(max_positions))
  # Devuelve la posición central
  return(center_pos)
}

find_closest_quinquenio <- function(min_year, max_year, quinquenios) {
  # min_year <- 1997
  # max_year <- 2012
  # quinquenios <- opts_quinquenios
  # Convertimos los quinquenios a rangos numéricos
  ranges <- strsplit(quinquenios, "-")
  scores <- sapply(ranges, function(x) {
    # x <-  c("2005", "2009")
    q_min <- as.numeric(x[1])
    q_max <- as.numeric(x[2])
    # Calculamos cuántos años se solapan
    overlap <- length(intersect(q_min:q_max, min_year:max_year))
    return(overlap)
  })
  
  # Retornamos el quinquenio con mayor solapamiento
  # quinquenios[which.max(scores)]
  quinquenios[get_center_max(scores)]
}


round_by_range <- function(x) {
  rng <- diff(range(x, na.rm = TRUE))
  
  digits <- case_when(
    # 1) If the range is extremely small (< 1e-4), show 7 decimals
    rng < 1e-3 ~ 10,
    # 2) If range < 1, show 3 decimals
    rng < 1    ~ 3,
    # 3) If range < 10, show 2 decimals
    rng < 10   ~ 2,
    # 4) If range < 100, show 1 decimal
    rng < 100  ~ 1,
    # 5) Otherwise, show 0 decimals
    TRUE       ~ 0
  )
  
  round(x, digits = digits)
}


