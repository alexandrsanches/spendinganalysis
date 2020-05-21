#### Funções ####

## Importar arquivo

lerArquivo <- function(mes) {
  
  require(lubridate)
  require(openxlsx)
  
  if(mes == month(Sys.Date())) {
    
    dados <- read.xlsx("~/Documents/Projetos/Gastos/Arquivos/Orçamento.xlsm", sheet = "Despesas")
    
  } else { 
    
    mes <- month(mes, label = TRUE, abbr = FALSE)
    dados <- read.xlsx(paste0("~/Documents/Projetos/Gastos/Arquivos/Meses anteriores/Gastos - ", mes, ".xlsm")
                       , colNames = TRUE, sheet = "Despesas")
    
    }
}

## Gráfico de barra por categoria de gasto

gasto_categoria <- function(gasto = "total") {
  
  if(gasto == "total") {
  
  dados_categoria <- aggregate(Valor ~ Categoria, dados, function(x) c(Soma = sum(x)))  
    
  dados_categoria %>%
    ggplot(aes(x = Categoria, y = Valor)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = Valor), vjust = -0.15) +
    labs(x = "", y = "valor (R$)", title = paste0("Gastos por categoria - ", mes)) +
    ggthemes::scale_colour_economist() +
    theme(panel.background = element_rect(fill = "white", colour = "grey10"))
  
  } else if(gasto == "maior") {
    
  maior_gasto <- aggregate(Valor ~ Categoria, dados, function(x) c(Maior = max(x)))
  
  maior_gasto %>%
    ggplot(aes(x = Categoria, y = Valor)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = Valor), vjust = -0.15) +
    labs(x = "", y = "valor (R$)", title =  paste0("Maior gasto por categoria - ", mes)) +
    ggthemes::scale_colour_economist() +
    theme(panel.background = element_rect(fill = "white", colour = "grey10")) 
  
  }
}

## Gráfico de dispersão por dia da semana

dia_semana <- function(grafico = "col") {

  dados$Dia <- format(dados$Data, format = "%u")
  dados$Dia <- factor(dados$Dia, labels = c("qua", "qui", "sex", "sab", "dom")) ### REVISAR APÓS ABRIl c("seg", "ter", "qua", "qui", "sex", "sab", "dom")
  
  dados_agg <- aggregate(Valor ~ Dia, dados, function(x) c(Mean = mean(x), Median = median(x)))
  dados_agg <- cbind(dados_agg[1], dados_agg[[2]])

# Dispersão

  if(grafico == "disp") {
    
    dados %>%
        ggplot(aes(x = Data, y = Valor, group = Dia, color = Categoria)) +
        geom_point(na.rm = TRUE) +
        geom_hline(data = dados_agg, 
                          aes(yintercept = Mean, group = Dia),
                          color = "#8c8c8c") + 
        scale_x_date(date_labels = "%b %d", date_breaks = "10 day") +
        theme(legend.position = "bottom") +
        labs(x = "", y = "valor (R$)", title = paste0("Gastos por categoria e dia da semana - ", mes), color = "") +
        facet_wrap( ~ Dia, ncol = 7) +
      ggthemes::scale_colour_economist() +
      theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
      theme(panel.grid.major = element_line(colour = "gray", linetype = "dashed"), plot.subtitle = element_text(size = 11))
    
  } else if(grafico == "col") { 
    
    dados %>%
      ggplot(aes(x = Data, y = Valor, group = Dia, color = Categoria)) +
      geom_col(na.rm = TRUE) +
      geom_hline(data = dados_agg, 
                 aes(yintercept = Mean, group = Dia),
                 color = "#8c8c8c") +
      scale_x_date(date_labels = "%b %d", date_breaks = "10 day") +
      theme(legend.position = "bottom") +
      labs(x = "", y = "valor (R$)", title = paste0("Gastos por categoria e dia da semana - ", mes), color = "") +
      facet_wrap( ~ Dia, ncol = 7) +
      ggthemes::scale_colour_economist() +
      theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
      theme(panel.grid.major = element_line(colour = "gray", linetype = "dashed"), plot.subtitle = element_text(size = 11))
    
  }
}

## Gráfico de dispersão de gastos

disp <- function() {
  
  dados %>%
    ggplot(aes(x = Data, y = Valor)) +
    geom_point() +
    geom_smooth(method = "loess", color = "#1395d5") +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 day") +
    labs(x = "", y = "valor (R$)", title = paste0("Gastos ao longo do mês - ", mes)) +
    ggthemes::scale_colour_economist() +
    theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
    theme(panel.grid.major = element_line(colour = "gray", linetype = "dashed"), plot.subtitle = element_text(size = 11))
  
}
  
## 

