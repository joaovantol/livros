generate_plots <- function(dfs) {
  plt <- horizontal_barplot(dfs$df_genres, "Genre")
  ggsave("plots/barplot_genres.pdf", plt, width = 1800, height = 2000, units = "px")

  plt <- horizontal_barplot(dfs$df_places, "Place")
  ggsave("plots/barplot_places.pdf", plt, width = 1800, height = 2000, units = "px")

  plt <- horizontal_barplot(dfs$df_authors, "Author")
  ggsave("plots/barplot_authors.pdf", plt, width = 3750, height = 3250, units = "px")

  plt <- vertical_barplot(dfs$df_dates, "Books")
  ggsave("plots/barplot_books.pdf", plt, width = 1800, height = 2000, units = "px")

  plt <- vertical_barplot(dfs$df_pages_per_year, "Pages")
  ggsave("plots/barplot_pages.pdf", plt, width = 1800, height = 2000, units = "px")

  df <- read_data()
  plt <- cumulated_series(df, "Book_series")
  ggsave("plots/cumulated_books.pdf", plt, width = 1800, height = 2000, units = "px")

  plt <- cumulated_series(df, "Page_series")
  ggsave("plots/cumulated_pages.pdf", plt, width = 1800, height = 2000, units = "px")
}
