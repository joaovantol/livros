library("readxl")
library("dplyr")
library("lubridate")
library("ggplot2")
library("shadowtext")
library("grid")
library("plyr")
library("tidyr")

source("R/src/generate_plots.R")
source("R/src/read_data.R")
source("R/src/plot_functions.R")

dfs <- get_data_frames()
generate_plots(dfs)









# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"

# Histograma dos anos de lançamento
plot <- ggplot(df.years, aes(x = year)) + 
  geom_histogram(aes(y=..density..), colour="blue", fill="lightblue", binwidth = 5) +
  geom_density(alpha=.2, fill=GREY) +
  geom_vline(aes(xintercept = mean(.data[['year']])), color="#990000", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept = median(.data[['year']])), color="#FF0000", linetype="dashed", linewidth=1) +
  scale_x_continuous(
    limits = c(1610, 2030),
    breaks = seq(1600, 2040, by = 20),
    expand = c(0, 0)
  ) +
  labs(
    title = "Histograma",
    subtitle = "Ano de lançamento dos livros lidos",
    x = "Ano de lançamento",
    y = "Densidade"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 15
    )
  )


  
#plot
grid.text(
  paste0("Média = ", round(mean(df.years[['year']]),1)), 
  x = 0.08, 
  y = 0.86, 
  just = c("left", "top"),
  gp = gpar(
    col = "#990000",
    fontsize = 13
  )
)

grid.text(
  paste0("Mediana = ", median(df.years[['year']])), 
  x = 0.08, 
  y = 0.80, 
  just = c("left", "top"),
  gp = gpar(
    col = "#FF0000",
    fontsize = 13
  )
)
plot <- grid.grab()
plot <- plot_grid(plot)
ggsave("plots/histograma.png")


# Histograma dos anos de lançamento (separado por gênero)
mu <- ddply(df.years, "genero", summarise, grp.mean=mean(year))
med <- ddply(df.years, "genero", summarise, grp.med=median(year))

plot <- ggplot(df.years, aes(x = year, fill = genero, color = genero)) + 
  geom_histogram(binwidth = 5, alpha=0.5) +
  geom_vline(data=mu,aes(xintercept = grp.mean, color=genero, linetype="media"), linewidth=1, show_guide=T) +
  geom_vline(data=med,aes(xintercept = grp.med, color=genero, linetype="mediana"), linewidth=1, show_guide=T) +
  scale_linetype_manual(values=c("media"="longdash","mediana"="dashed"))+
  theme(legend.key.size = unit(1.2, 'cm'),
        legend.position="bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_x_continuous(
    limits = c(1610, 2030),
    breaks = seq(1600, 2040, by = 20),
    expand = c(0, 0)
  ) +
  labs(
    title = "Histograma",
    subtitle = "Ano de lançamento dos livros lidos",
    x = "Ano de lançamento",
    y = "Frequência"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 15
    )
  )


plot
grid.text(
  paste0("Média (Ficção) = ", round(mu[1,2],1)), 
  x = 0.07, 
  y = 0.86, 
  just = c("left", "top"),
  gp = gpar(
    col = "red",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)
grid.text(
  paste0("Mediana (Ficção) = ", med[1,2]), 
  x = 0.07, 
  y = 0.81, 
  just = c("left", "top"),
  gp = gpar(
    col = "red",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)

grid.text(
  paste0("Média (Não Ficção) = ", round(mu[2,2],1)), 
  x = 0.07, 
  y = 0.76, 
  just = c("left", "top"),
  gp = gpar(
    col = "blue",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)
grid.text(
  paste0("Mediana (Não Ficção) = ", med[2,2]), 
  x = 0.07, 
  y = 0.71, 
  just = c("left", "top"),
  gp = gpar(
    col = "blue",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)


# Histograma dos tamanhos dos livros
plot <- ggplot(df.pages, aes(x = size)) + 
  geom_histogram(aes(y=..density..), colour="blue", fill="lightblue", binwidth = 10) +
  geom_density(alpha=.2, fill=GREY) +
  geom_vline(aes(xintercept = mean(.data[['size']])), color="#990000", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept = median(.data[['size']])), color="#FF0000", linetype="dashed", linewidth=1) +
  scale_x_continuous(
    limits = c(35, 1970),
    breaks = seq(0, 2000, by = 100),
    expand = c(0, 0)
  ) +
  labs(
    title = "Histograma",
    subtitle = "Número de páginas dos livros lidos",
    x = "Número de páginas",
    y = "Densidade"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 15
    )
  )

plot
grid.text(
  paste0("Média = ", round(mean(df.pages[['size']]),1)), 
  x = 0.98, 
  y = 0.86, 
  just = c("right", "top"),
  gp = gpar(
    col = "#990000",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)

grid.text(
  paste0("Mediana = ", median(df.pages[['size']])), 
  x = 0.98, 
  y = 0.80, 
  just = c("right", "top"),
  gp = gpar(
    col = "#FF0000",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)





# Histograma dos tamanhos dos livros (separado por gênero)
mu <- ddply(df.pages, "genero", summarise, grp.mean=mean(size))
med <- ddply(df.pages, "genero", summarise, grp.med=median(size))

plot <- ggplot(df.pages, aes(x = size, fill = genero, color = genero)) + 
  geom_histogram(binwidth = 10, alpha=0.5) +
  geom_vline(data=mu,aes(xintercept = grp.mean, color=genero, linetype="media"), linewidth=1, show_guide=T) +
  geom_vline(data=med,aes(xintercept = grp.med, color=genero, linetype="mediana"), linewidth=1, show_guide=T) +
  scale_linetype_manual(values=c("media"="longdash","mediana"="dashed"))+
  theme(legend.key.size = unit(1.2, 'cm'),
        legend.position="bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_x_continuous(
    limits = c(35, 1970),
    breaks = seq(0, 2000, by = 100),
    expand = c(0, 0)
  ) +
  labs(
    title = "Histograma",
    subtitle = "Número de páginas dos livros lidos",
    x = "Número de páginas",
    y = "Frequência"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 15
    )
  )


plot

grid.text(
  paste0("Média (Ficção) = ", round(mu[1,2],1)), 
  x = 0.98, 
  y = 0.86, 
  just = c("right", "top"),
  gp = gpar(
    col = "red",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)
grid.text(
  paste0("Mediana (Ficção) = ", med[1,2]), 
  x = 0.98, 
  y = 0.81, 
  just = c("right", "top"),
  gp = gpar(
    col = "red",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)


grid.text(
  paste0("Média (Não Ficção) = ", round(mu[2,2],1)), 
  x = 0.98, 
  y = 0.76, 
  just = c("right", "top"),
  gp = gpar(
    col = "blue",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)
grid.text(
  paste0("Mediana (Não Ficção) = ", med[2,2]), 
  x = 0.98, 
  y = 0.71, 
  just = c("right", "top"),
  gp = gpar(
    col = "blue",
    fontsize = 13,
    fontfamily = "Econ Sans Cnd"
  )
)


















