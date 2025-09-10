get_period_str <- function() {
  first_date <- as.Date(read_data()[["Date"]][1])
  first_month <- month(first_date, label = TRUE, abbr = FALSE,
                       locale = "en_US.UTF-8")
  first_year <- year(first_date)

  last_date <- as.Date(tail(read_data()[["Date"]],1))
  last_month <- month(last_date, label = TRUE, abbr = FALSE,
                       locale = "en_US.UTF-8")
  last_year <- year(last_date)

  str <- paste0("Reading period: from ", first_month, " ", first_year,
                " to ", last_month, " ", last_year)

  str
}

get_title_and_subtitle <- function(col) {
  titles <- list(
    Place = "Countries",
    Genre = "Genres/Themes",
    Author = "Authors",
    Books = "Reading record",
    Pages = "Reading record",
    Book_series = "Reading progression",
    Page_series = "Reading progression"

  )
  subtitles <- list(
    Place = "Number of books by place of publication",
    Genre = "Number of books by genre or theme",
    Author = "Number of books by author",
    Books = "Number of books read by year",
    Pages = "Number of pages read by year",
    Book_series = "Cumulated number of books read",
    Page_series = "Cumulated number of pages read"
  )

  c(titles[[col]], subtitles[[col]])
}

make_grob_tree <- function(plt, bottom) {
  g_rect <- grid.rect(
    x = 0,
    y = 1,
    width = 1,
    height = 0.010,
    just = c("left", "top"),
    gp = gpar(fill = "#e5001c", lwd = 0)
  )

  if (bottom) {
    g_text <- grid.text(
      get_period_str(),
      x = 0.005,
      y = 0.02,
      just = c("left", "bottom"),
      gp = gpar(
        col = "grey50",
        fontsize = 14
      )
    )
    return(grobTree(ggplotGrob(plt), g_rect, g_text))
  }

  return(grobTree(ggplotGrob(plt), g_rect))
}

horizontal_barplot <- function(df, col) {
  if (col == "Author") df <- df %>% filter(Freq >= 2)

  names <- df[["val"]]
  count <- df[["Freq"]]
  data <- data.frame(
    count = count,
    name = factor(names, levels = names),
    y = seq_along(names) * 0.9
  )

  titles <- get_title_and_subtitle(col)

  barwidth <- 0.6
  upperlimit <- max(data$count) + 20
  steps <- 20

  if (col == "Author") {
    barwidth <- 0.9
    upperlimit <- max(data$count) + 0.5
    steps <- 1
  }

  plt <- ggplot(data) +
    geom_col(aes(count, name), fill = "#076fa2", width = barwidth) +
    scale_x_continuous(
      limits = c(0, upperlimit),
      breaks = seq(0, upperlimit, by = steps),
      expand = c(0, 0),
      position = "top"
    ) +
    scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
      axis.ticks.length = unit(0, "mm"),
      axis.title = element_blank(),
      axis.line.y.left = element_line(color = "black"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 16)
    ) +
    labs(
      title = titles[1],
      subtitle = titles[2]
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        size = 15
      )
    ) +
    theme(
      plot.margin = margin(0.025, 0, 0.05, 0.01, "npc")
    )

  if (col != "Author") {
    plt <- plt +
      geom_shadowtext(
        data = data,
        aes(count + 1, y = name, label = name),
        hjust = 0,
        nudge_x = 0.3,
        nudge_y = 0.1,
        colour = "#076fa2",
        bg.colour = "white",
        bg.r = 0.2,
        size = 4
      )
  } else {
    plt <- plt +
      geom_text(
        data = data,
        aes(0, y = name, label = name),
        hjust = 0,
        nudge_x = 0.3,
        colour = "white",
        size = 3
      )
  }

  make_grob_tree(plt, bottom = TRUE)
}

vertical_barplot <- function(df, col) {
  year_str <- ifelse(col == "Books", "finished", "year")
  count_str <- ifelse(col == "Books", "Freq", "pages")

  years <- df[[year_str]]
  count <- df[[count_str]]
  data <- data.frame(
    count = count,
    name = factor(years, levels = years),
    x = seq(length(years)) * 0.9
  )

  titles <- get_title_and_subtitle(col)

  upperlimit <- ifelse(col == "Books", 82.5, 21000)
  upperbreak <- ifelse(col == "Books", 80, 20000)
  breakstep <- ifelse(col == "Books", 10, 2000)
  ynudge <- ifelse(col == "Books", 2.5, 500)

  plt <-ggplot(data) +
    geom_col(aes(name, count), fill = "#076fa2", width = 0.6) +
    scale_y_continuous(
      limits = c(0, upperlimit),
      breaks = seq(0, upperbreak, by = breakstep),
      expand = c(0, 0)
    ) +
    scale_x_discrete() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
      axis.ticks.length = unit(0, "mm"),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 16),
      plot.margin = margin(0.025, 0, 0.05, 0.01, "npc"),
      plot.title = element_text(
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        size = 15
      )
    ) +
    geom_shadowtext(
      data = data,
      aes(x = name, count, label = count),
      hjust = "centre",
      nudge_x = 0,
      nudge_y = ynudge,
      colour = "#076fa2",
      bg.colour = "white",
      bg.r = 0.2,
      size = 5
    ) +
    labs(
      title = titles[1],
      subtitle = titles[2]
    )

  make_grob_tree(plt, bottom = TRUE)
}

cumulated_series <- function(df, col) {
  df <- df %>%
    mutate(Book_series = 1:nrow(df), Page_series = cumsum(Pages)) %>%
    select(Date, Book_series, Page_series) %>%
    group_by(Date) %>%
    dplyr::summarize(Book_series = max(Book_series),
                     Page_series = max(Page_series)) %>%
    ungroup() %>%
    complete(Date = seq(min(Date), max(Date), by = "1 day"))

  for (idx in seq_along(2:nrow(df))) {
    if (is.na(df[["Book_series"]][idx]))
      df[["Book_series"]][idx] <- df[["Book_series"]][idx - 1]
    if (is.na(df[["Page_series"]][idx]))
      df[["Page_series"]][idx] <- df[["Page_series"]][idx - 1]
  }

  vals <- df[[col]]
  colorscale <- rev(gray((vals - min(vals)) / (max(vals) - min(vals))))

  # blue_palette <- colorRampPalette(c("blue", "darkblue"))
  # blue_scale <- blue_palette(length(vals))

  titles <- get_title_and_subtitle(col)

  if (col == "Book_series") {
    plt <- ggplot(df, aes(x = as.Date(Date), y = Book_series))
    upperlimit <- max(df[[col]]) + 5
    breakstep <- 25
  }
  if (col == "Page_series") {
    plt <- ggplot(df, aes(x = as.Date(Date), y = Page_series))
    upperlimit <- max(df[[col]]) + 2000
    breakstep <- 10000
  }

  plt <- plt +
    geom_segment(aes(xend = as.Date(Date), yend = 0, colour = vals),
                 linewidth = 0.2,
                 alpha = 0.1) +
    geom_line() +
    scale_colour_gradient(
      low = colorscale[1],
      high = tail(colorscale, 1)) +
    scale_y_continuous(
      limits = c(0, upperlimit),
      breaks = seq(0, upperlimit, by = breakstep),
      expand = c(0, 0),
      position = "left"
    ) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%Y-%b"
    ) +
    labs(
      y = element_blank(),
      title = titles[1],
      subtitle = titles[2]
    ) +
    theme(
      plot.margin = margin(0.025, 0, 0.01, 0.01, "npc"),
      plot.title = element_text(
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        size = 15
      ),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      axis.title.x = element_blank())

  make_grob_tree(plt, bottom = FALSE)
}
