#### Define working directory #### 

setwd("~/Documents/Projetos/Gastos")

#### Load packages and functions #### 

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(ggplot2)
  library(gghighlight)
  library(ggthemes)
  library(lubridate)
  library(rmarkdown)
})

source("Scripts/Functions.R", encoding = "utf8") 

#### Import data ####

dados <- lerArquivo(4)
dados$Data <- convertToDate(dados$Data)

mes <- month(floor_date(min(dados$Data)), label = TRUE, abbr = FALSE)

#### Make analysis ####

maior_gasto <- aggregate(Valor ~ Categoria, dados, function(x) c(Maior = max(x)))
dados_categoria <- aggregate(Valor ~ Categoria, dados, function(x) c(Soma = sum(x)))

#### Save data for Markdown rendering #### 

save.image(file = "Dados/Data & Functions.RData") 

#### Render R Markdown ####

render(input = "Scripts/README.Rmd",
       output_file = "README.html",
       output_format = html_document(),
       knit_root_dir = getwd(),
       encoding = "UTF-8",
       quiet = TRUE)

