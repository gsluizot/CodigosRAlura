path <- "C:/Users/conta/OneDrive/Documentos/GitHub/dados_r_alura/dataviz_uma_var-dados/googleplaystore.csv"
path2 <- "C:/Users/conta/OneDrive/Documentos/GitHub/dados_r_alura/dataviz_uma_var-dados/user_reviews.csv"

# dados <- read.csv(file = path)

# View(dados)

# head(dados)

# tail(dados)

# str(dados)

dados <- read.csv(file = path, stringsAsFactors = FALSE)

#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# hist(dados$Rating)

# table(dados$Rating)

# hist(dados$Rating, xlim = c(1,5))

rating.Histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5)) + xlim(c(1,5))
rating.Histogram
# rating.Histogram # Isto executa o gráfico

ggplot(data = dados) + geom_bar(mapping = aes(x = Category), stat = "count") + coord_flip()

# gerando 5 categorias mais presentes em gráfico

category.Freq <- data.frame(table(dados$Category))

ggplot(data = category.Freq) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()

category.Top10 <- category.Freq[(order(-category.Freq$Freq)), ][1:10, ]
freq.Category.Plot <- ggplot(data = category.Top10) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()

dados_2 <- dados %>% filter(Category != "1.9")

# min(dados_2$Rating)
# max(dados_2$Rating)

# dados_2 %>% filter(is.na(Rating)) %>% count()

# summary(dados_2$Rating)

# média nos nulos

# dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>% count() %>% summarize(media = mean(Rating))

mean.Category <- dados_2 %>% filter(!is.na(Rating)) %>% group_by(Category) %>% summarize(media = mean(Rating))

for (i in  1:nrow(dados_2)){
  if(is.na(dados_2[i, "Rating"])) {
    dados_2[i, "newRating"] <- mean.Category[mean.Category$Category == dados_2[i, "Category"], "media"]
  } else {
    dados_2[i, "newRating"] <- dados_2[i, "Rating"]
  }
}

# summary(dados_2$newRating)

dados_2 <- dados_2 %>% mutate(rating_class = if_else(newRating < 2, "Ruim", if_else(newRating > 4, "Bom", "Regular")))

rating_class_plot <- ggplot(dados_2) + geom_bar(aes(rating_class), stat = "count")

type.Freq <- data.frame(table(dados_2$Type))

type.Plot <- ggplot(type.Freq) + 
  geom_bar(aes(x = "", Freq, fill = Var1), stat = 'identity') + 
  coord_polar(theta = "y", start = 0)

type.Plot

freq.Size <- data.frame(table(dados_2$Size))

dados_2$Size[1]

## Ocorrencia de K ou M nos registros

grepl(pattern = "M", x = dados_2$Size[1])

## conversão elimitar a letra m ou k

gsub(pattern = "M", replacement = "--", x = dados_2$Size[1])

## loop com sapply
dados_2$kb <- sapply(X = dados_2$Size, FUN = function(y){
  if (grepl("M", y, ignore.case = T)){
    y <- as.numeric(gsub(pattern = "M", replacement = "", x= y)) * 1024
  } else if(grepl("k|\\+", y, ignore.case = T)) {
    y <- gsub("k|\\+", replacement = "", x = y)
  } else{
    y <- "nd"
  }
})

## conversão de mega para kilobite

1* 1024

hist(as.numeric(dados_2$kb))

options(scipen = 999)

size.app <- dados_2 %>% filter(kb != "nd") %>% mutate(kb = as.numeric(kb))

size.App.Plot <- ggplot(size.app) + geom_histogram(aes(kb))

size.App.Plot

# Anotar a partir daqui

install.packages("lubridate")
library(lubridate)

lubridate:: # isso permite ver todas as funções desse pacote
  
dmy('05-07-2018') # transforma string em variavel tipo date
# nome intuitivo d = dia, m = mes,  y = ano
ymd('20020510') # outro exemplo

ymd_hms("2002/05/10 10:00:00")

data_hora = '2018-12-25 14:05:15'
data_hora <- ymd_hms(data_hora)
data_hora

## extrair apenas o mês

year(data_hora) # extrai o ano

month(data_hora)

mday(data_hora) # extrai o dia

wday(data_hora, label = T) # pega o dia da semana. Retire o label = T e tenha
                           # apenas o número do dia referente a data

month(data_hora, label = T) # label = T retorna o nome do mês

notas = read.csv(path2)

notas$data_2 <- ymd_hms(notas$data)
str(notas)

ggplot(notas) + geom_line(aes(x = data_2, y = Sentiment_Polarity))

# pega apenas o ano e mês das datas com data e hora
notas$data_2 <- parse_date_time( format(notas$data_2, "%Y-%m"), "ym" )

# calcula média da nota por dia
media_notas <- notas %>% group_by(data_2) %>% summarize(media = mean(Sentiment_Polarity))

line.Plot <- ggplot(media_notas) + geom_line(aes(x = data_2, y = media))

rating.Histogram <- rating.Histogram + ggtitle("Histograma Rating") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
rating.Histogram

freq.Category.Plot <- freq.Category.Plot + xlab("Categoria") + ylab("Quantidade") + geom_bar(aes(Var1, Freq), fill = "darkcyan", stat = "identity") + theme_bw()

# define varias coisas no tema para branco
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

# mostra porcentagens nas partes do gráfico
library(scales)
type.Plot + blank_theme + geom_text(aes(x = "", y = Freq/2, label = percent(Freq/sum(Freq))), size = 5) + scale_fill_discrete(name = "Tipo App") + ggtitle("Tipos de apps") + theme(plot.title = element_text(hjust = 0.5))


size.App.Plot <- size.App.Plot + ggtitle("Histograma tamanho dos aplicativos") + geom_histogram(aes(kb, , fill = after_stat(x))) + scale_fill_gradient(low = "blue", high= "yellow") + guides(fill = FALSE) + xlab("Tamanho em kb") + ylab("Quantidade de Apps")

nota_plot <- rating_class_plot + ggtitle("Categoria de notas do app") + xlab("Categoria") + ylab("Quantidade") + geom_bar(aes(rating_class), fill = c("green4", "yellow2", "red2"))

install.packages("gridExtra")
library(gridExtra)

grid.arrange(rating.Histogram, 
             freq.Category.Plot, 
             type.Plot, 
             size.App.Plot, 
             line.Plot, 
             nota_plot, 
             ncol = 3, 
             nrow = 2)
