read_data <- function() read_excel("data/readings.xlsx")

page_and_year_df <- function(readings) {
  genres <- c()
  for (row in seq_len(nrow(readings))) {
    list_genres <- strsplit(readings[["Genre"]], ", ")
    genres <- c(genres, ifelse("Nonfiction" %in% list_genres[[row]],
                               "Nonfiction", "Fiction"))
  }
  df <- data.frame(year = readings[["Year"]], size = readings[["Pages"]])
  df <- df %>% mutate(genre = genres)
  df
}

table_df <- function(readings, col) {
  val <- unlist(strsplit(readings[[col]], ", "))
  df <- as.data.frame(table(val))
  df <- df %>% arrange(Freq)
  df
}

date_df <- function(readings) {
  dates <- data.frame(finished = year(readings[['Date']]))
  df <- as.data.frame(table(dates), stringsAsFactors = FALSE)
  df <- rbind.data.frame(df, data.frame(finished = "2017", Freq = 0))
  df <- df %>% arrange(finished)
  df
}

pages_per_year_df <- function(readings) {
  df <- data.frame(pages = readings[['Pages']], year = year(readings[['Date']]))
  df <- df %>% group_by(year) %>% dplyr::summarise(pages = sum(pages))
  df <- rbind.data.frame(df, data.frame(pages = 0, year = 2017))
  df <- df %>% arrange(year)
  df
}

get_data_frames <- function() {
  readings <- read_data()

  df_page_year <- page_and_year_df(readings)
  df_genres <- table_df(readings, "Genre")
  df_authors <- table_df(readings, "Author")
  df_dates <- date_df(readings)
  df_places <- table_df(readings, "Place")
  df_pages_per_year <- pages_per_year_df(readings)

  list(
    df_page_year = df_page_year,
    df_genres = df_genres,
    df_authors = df_authors,
    df_dates = df_dates,
    df_places = df_places,
    df_pages_per_year = df_pages_per_year
  )
}
