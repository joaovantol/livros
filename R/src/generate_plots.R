generate_plots <- function(dfs) {
  plt <- horizontal_barplot(dfs$df_genres, "Genre")
  ggsave("plots/barplot_genres.pdf", plt)

  plt <- horizontal_barplot(dfs$df_places, "Place")
  ggsave("plots/barplot_places.pdf", plt)

  plt <- horizontal_barplot(dfs$df_authors, "Author")
  ggsave("plots/barplot_authors.pdf", plt)

  plt <- vertical_barplot(dfs$df_dates, "Books")
  ggsave("plots/barplot_books.pdf", plt)

  plt <- vertical_barplot(dfs$df_pages_per_year, "Pages")
  ggsave("plots/barplot_pages.pdf", plt)

  df <- read_data()
  plt <- cumulated_series(df, "Book_series")
  ggsave("plots/cumulated_books.pdf", plt)

  plt <- cumulated_series(df, "Page_series")
  ggsave("plots/cumulated_pages.pdf", plt)
}
