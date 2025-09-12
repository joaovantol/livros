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
    Genre = "Genres / Themes",
    Author = "Authors",
    Books = "Reading record",
    Pages = "Reading record",
    Book_series = "Reading progression",
    Page_series = "Reading progression"

  )
  subtitles <- list(
    Place = "Books read by place of publication",
    Genre = "Books read by genre or theme",
    Author = "Books read by author",
    Books = "Books read by year",
    Pages = "Pages read by year",
    Book_series = "Cumulated books read",
    Page_series = "Cumulated pages read"
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
        fontsize = 10
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

  if (col == "Genre") {
    barwidth <- 0.6
    upperlimit <- max(data$count) + 30
    lowerlimit <- 0
    steps <- 40
  }

  if (col == "Place") {
    barwidth <- 0.6
    upperlimit <- max(data$count) + 35
    lowerlimit <- 0
    steps <- 20
  }

  if (col == "Author") {
    barwidth <- 0.9
    upperlimit <- max(data$count) + 0.5
    lowerlimit <- -0.8
    steps <- 1
  }

  plt <- ggplot(data) +
    geom_col(aes(count, name), fill = "#076fa2", width = barwidth) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(
      limits = c(lowerlimit, upperlimit),
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
      geom_shadowtext(
        aes(count, y = name, label = name),
        hjust = 1,
        nudge_x = -0.1,
        colour = "white",
        bg.colour = "black",
        bg.r = 0.2,
        size = 2.8
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

  plt <- ggplot(data) +
    geom_col(aes(name, count), fill = "#076fa2", width = 0.6) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#A8BAC4", linewidth = 0.3),
      axis.ticks.length = unit(0, "mm"),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust=1),
      plot.margin = margin(0.025, 0, 0.05, 0.01, "npc"),
      plot.title = element_text(
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(
        size = 15
      )
    ) +
    scale_y_continuous(
      limits = c(0, upperlimit),
      breaks = seq(0, upperbreak, by = breakstep),
      expand = c(0, 0)
    ) +
    scale_x_discrete() +
    geom_shadowtext(
      data = data,
      aes(x = name, count, label = count),
      hjust = "centre",
      nudge_x = 0,
      nudge_y = ynudge,
      colour = "#076fa2",
      bg.colour = "white",
      bg.r = 0.2,
      size = 3.5
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
  blue_palette <- colorRampPalette(c("lightblue", "#076fa2"))
  colorscale <- blue_palette(length(vals))

  titles <- get_title_and_subtitle(col)

  if (col == "Book_series") {
    plt <- ggplot(df, aes(x = as.Date(Date), y = Book_series)) +
      geom_segment(
        aes(
          x = as.Date(Date),
          xend = as.Date(Date),
          y = Book_series, yend = 0,
          colour = vals),
        inherit.aes = FALSE,
        linewidth = 0.5,
        alpha = 0.05)
    upperlimit <- max(df[[col]]) + 5
    breakstep <- 25
    breaks <- c(0, 100, 200, 300)
  }
  if (col == "Page_series") {
    plt <- ggplot(df, aes(x = as.Date(Date), y = Page_series)) +
      geom_segment(
        aes(
          x = as.Date(Date),
          xend = as.Date(Date),
          y = Page_series, yend = 0,
          colour = vals),
        inherit.aes = FALSE,
        linewidth = 0.5,
        alpha = 0.05)
    upperlimit <- max(df[[col]]) + 2000
    breakstep <- 10000
    breaks <- c(0, 30000, 60000, 90000)
  }

  limit <- max(df[[col]])
  first_date <- as.Date(df[["Date"]][1]) + 90
  last_date <- as.Date(tail(df[["Date"]], 1))

  plt <- plt +
    geom_line() +
    scale_colour_gradient(
      low = colorscale[1],
      high = tail(colorscale, 1),
      limits = c(0, limit),
      breaks = c(breaks, limit),
      labels = c(breaks, limit)) +
    scale_y_continuous(
      limits = c(0, upperlimit),
      breaks = seq(0, upperlimit, by = breakstep),
      expand = c(0, 0),
      position = "left"
    ) +
    scale_x_date(
      breaks = c(seq.Date(first_date, last_date, by = '6 months')),
      date_labels = "%Y-%b",
      expand = c(0,0)
    ) +
    labs(
      y = element_blank(),
      title = titles[1],
      subtitle = titles[2],
      colour = NULL
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
      axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
      axis.title.x = element_blank(),
      legend.position = "bottom") +
    guides(colour = guide_colourbar(
      barwidth = unit(10, "cm"))
    )

  make_grob_tree(plt, bottom = FALSE)
}
