
require(tidyverse)
require(readxl)
library(ggbeeswarm)
library(geobr)
library(wesanderson)
library(reshape2)
library(cowplot)
library(extrafont)

### Carregando fontes:
  
### CARREGANDO A TABELA DE ESTABELECIMENTOS, pulamos as 5 primeiras linhas e renomeamos as demais: 

df_estab <- read_xlsx("data/tabela6770-2.xlsx", sheet = 1, skip = 5) |> 
  select(-c(2, 3)) |> 
  rename(municipio = 1, 
         estab_agro_negocio = 2, 
         estab_agro_familiar = 3) |> 
  slice(-1)

### CARREGANDO A TABELA DE ÁREAS, pulamos as 5 primeiras linhas e renomeamos as demais: 

df_area <- read_xlsx("data/tabela6770-2.xlsx", sheet = 2, skip = 5) |> 
  select(-c(2, 3)) |> 
  rename(municipio = 1, 
         area_agro_negocio = 2, 
         area_agro_familiar = 3) |> 
  slice(-1)

### Juntando os bancos de dados usando a variavel municipio: 

df <- left_join(df_area, df_estab)

## Separando o nome dos municipios e a sigla dos Estados

pattern <- "^(.*?) \\((\\w{2})\\)$"
df_matched <- str_match(df$municipio, pattern)
df$municipio <- tolower(df_matched[, 2])
df$uf <- df_matched[, 3]

### Carregando banco de dados dos modulos fiscais

modulos_fiscais <- read_csv("data/modulos_fiscais.csv") |> 
  mutate(municipio = tolower(municipio)) |> 
  rename(uf = UF, 
         modulos_fiscais = 3) |> 
  select(-c(4, 5))

### Ajustes finais : União dos bancos de dados e criação de indicadores

df <- left_join(df, modulos_fiscais) |> 
  filter(modulos_fiscais != 0) |> 
  mutate(area_media_agro_familiar = area_agro_familiar/ estab_agro_familiar , 
         area_media_agro_negocio = area_agro_negocio/ estab_agro_negocio ) |> 
  mutate(modulos_fiscais_familiar = area_media_agro_familiar/modulos_fiscais, 
         modulos_fiscais_agro_negocio = area_media_agro_negocio/modulos_fiscais) 

#### Gráfico: 

p1 <- df |> 
  select(municipio, modulos_fiscais_familiar, modulos_fiscais_agro_negocio) |> 
  reshape2::melt() |> 
  rename(Tipo = 2, 
         "Módulos Fiscais" = 3) |> 
  mutate(Tipo = ifelse(Tipo == "modulos_fiscais_familiar", 
                       "Agricultura familiar", "Agricultura não familiar")) |> 
  ggplot(aes(x = `Módulos Fiscais`, fill = Tipo)) + 
  geom_histogram(cex = 0.5, alpha = 0.6, col = "white") +
  scale_x_log10(breaks = c(1, 4, 14)) + 
  scale_fill_manual(values = c("#DC863B", "#C93312")) + 
  theme_bw() + 
  labs(y = "", fill = "") +
  theme(text = element_text(family = "Tahoma")) + 
  theme(axis.text.x = element_text(angle = 90, size = 20, 
                                   face="bold"), 
        axis.text.y = element_text(angle = 0, size = 12, 
                                   face="bold"), 
        axis.title.y = element_text(angle = 90, size = 20, 
                                    face="bold"), 
        plot.title = element_text(size=22)) 

p2 <- df |> 
  select(municipio, modulos_fiscais_familiar, modulos_fiscais_agro_negocio) |> 
  reshape2::melt() |> 
  rename(Tipo = 2, 
         "Módulos Fiscais" = 3) |> 
  mutate(Tipo = ifelse(Tipo == "modulos_fiscais_familiar", 
                       "Agricultura familiar", "Agricultura não familiar")) |> 
  ggplot(aes(x = `Módulos Fiscais`, 
             y = Tipo, col = Tipo)) + 
  geom_quasirandom(cex = 0.5, show.legend = F, alpha = 0.7) +
  scale_colour_manual(values = c("#DC863B", "#C93312")) + 
  scale_x_log10() + 
  theme_bw() + 
  scale_x_log10(breaks = c(1, 4, 14)) + 
  labs(y = "", col = "") + 
  theme(text = element_text(family = "Tahoma")) +
  theme(axis.text.x = element_text(angle = 90, size = 20, 
                                   face="bold"), 
        axis.text.y = element_text(angle = 0, size = 12, 
                                   face="bold"), 
        axis.title.y = element_text(angle = 90, size = 20, 
                                    face="bold"), 
        plot.title = element_text(size=22)) 


### faendo o plot

plot_row <- plot_grid(p1, p2)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Distribuição da área média de estabelecimentos rurais, agricultura familiar ou não, por município",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )



g1 <- plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
) +
  theme_half_open() +
  background_grid() # always place this after the theme


ggsave(g1, file = "resultado/grafico1.jpeg",
       dpi = 1000, width = 20, height = 14)  
