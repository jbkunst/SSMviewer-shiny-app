# packages ----------------------------------------------------------------
library(pool)
library(DBI)
library(dbplyr)
library(dplyr)
library(fs)
library(cli)
library(purrr)
library(stringr)

pool <- dbPool(
  drv = RPostgres::Postgres(),
  host = Sys.getenv("HOST"),
  port = 5432,
  dbname = Sys.getenv("DB"), 
  user = Sys.getenv("USER"),
  password = Sys.getenv("PWD")
)

pool

# test --------------------------------------------------------------------
# purrr::safely(pool::dbRemoveTable)(pool, "collab_authors")
purrr::safely(pool::dbRemoveTable)(pool, "papers")

pool::dbWriteTable(
  pool,
  "papers",
  arrow::open_dataset("data/papers.parquet") |> collect(),
  overwrite = TRUE
)

tbl(pool, "papers") |> 
  count()

# upload tables -----------------------------------------------------------
fs::dir_ls("data", regexp = "parquet$") |> 
  # block to remove tables if is required
  map_chr(function(p = "data/collab_authors.parquet"){

    table_name <- tools::file_path_sans_ext(basename(p))

    cli::cli_inform("Deleting table {table_name}")

    purrr::safely(pool::dbRemoveTable)(pool, table_name)

    return(p)

  }) |>
  walk(function(p = "data/country_coord.parquet"){
    
    table_name <- tools::file_path_sans_ext(basename(p))
    
    cli::cli_h1("{table_name}")
    
    if(pool::dbExistsTable(pool, table_name)) return(TRUE)
    #  purrr::safely(pool::dbRemoveTable)(pool, table_name)
    
    df <- p |> 
      arrow::open_dataset() |> 
      collect()
    
    # eliminando columnas de tipo lista
    df <- df |> 
      select(-where(is.list)) |> 
      as.data.frame()
    
    if(table_name == "country_coord") {
      # para normalizar nombres y crear una columna id ficticia para
      # que mantenga la estrucutra del resto de tablas
      df <- df |> 
        janitor::clean_names() |> 
        mutate(country_id = alpha_2_code, .before = 1)
    }
    
    glimpse(df)
    
    cli::cli_inform("Uploading table {table_name}")
    
    pool::dbWriteTable(
      pool,
      table_name,
      df,
      overwrite = TRUE
    )
    
    cli::cli_inform("Creating index on table {table_name}")
    
    cols_idx <- names(df) |> 
      str_subset("_id$")  
    
    name_idx <- cols_idx |> 
      str_remove("_id") |> 
      str_c(collapse = "_") 
    name_idx <- str_c("idx_", name_idx, "_id")
    name_idx <- str_c("table_", table_name, "_", name_idx)
    # CREATE INDEX idx_paper_author_id ON papers(paper_id, author_id);
    
    q <- str_glue(
      "CREATE INDEX {name_idx} ON {table_name}({cols});",
      cols = str_c(cols_idx, collapse = ", ")
      )
    
    cli::cli_inform(q)
    
    pool::dbGetQuery(pool, q)
    
    # pool::dbGetQuery(pool, "select * from pg_indexes where tablename = 'affiliations'")
    
    n1 <- nrow(df)
    n2 <- (tbl(pool, table_name) |> count() |> pull() |> as.integer())
    
    stopifnot(n1 == n2)
    
    rm(df)
    
    gc()
    
  })

fs::dir_ls("data", regexp = "parquet$") |> 
  basename() |> 
  tools::file_path_sans_ext() |> 
  purrr::walk(function(t){ 
    cli::cli_h1(t)
    tbl(pool, t) |> glimpse() |> count()
  })
  
# upload abstracts --------------------------------------------------------
# No es necesario subirlos dado que Riva preprocesó la información 
# en nounds_rds.

# purrr::safely(pool::dbRemoveTable)(pool, "abstracts")
# 
# df_abstracts <- arrow::open_dataset("data/abstracts/") |> 
#   collect()
# 
# df_abstracts |> 
#   count(year) |> 
#   pull(year) |> 
#   sort() |> 
#   walk(function(y = 2000){
#     
#     cli::cli_progress_step(y)
#     
#     pool::dbWriteTable(
#       pool,
#       "abstracts",
#       df_abstracts |> filter(year == y),
#       append = TRUE
#     )
#     
#     Sys.sleep(1)
#     
#   }) 
# 
# n1 <- nrow(df_abstracts)
# n2 <- (tbl(pool, "abstracts") |> count() |> pull() |> as.integer())
# 
# stopifnot(n1 == n2)
# 
# rm(df_abstracts, n1, n2, pool)
# 
# gc()


