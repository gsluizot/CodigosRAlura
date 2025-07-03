getwd()
setwd("C:/Users/conta/OneDrive/Documentos/GitHub/dados_r_alura/dataviz_multivar_dados/dataviz_mult_var-dados")

library(data.table)
library(dplyr)
library(ggplot2)
library(bit64)

enem_2010 <- fread("enem_2010.csv", encoding = "UTF-8")
enem_2011 <- fread("enem_2011.csv", encoding = "UTF-8")
enem_2012 <- fread("enem_2012.csv", encoding = "UTF-8")
enem_2013 <- fread("enem_2013.csv", encoding = "UTF-8")
enem_2014 <- fread("enem_2014.csv", encoding = "UTF-8")
enem_2015 <- fread("enem_2015.csv", encoding = "UTF-8")
enem_2016 <- fread("enem_2016.csv", encoding = "UTF-8")
enem_2017 <- fread("enem_2017.csv", encoding = "UTF-8")

merge_enem <- rbind(enem_2010, enem_2011, enem_2012, enem_2013, enem_2014, enem_2015, enem_2016, enem_2017, fill = TRUE)

colunas <- colnames(merge_enem)

enem <- merge_enem %>% select(all_of(colunas))

str(enem)

table(enem$SEXO)
# VALOR 1 = FEM
# VALOR 2 = MAS

enem$SEXO <- gsub("1", "FEMININO", enem$SEXO)
enem$SEXO <- gsub("^F$", "FEMININO", enem$SEXO)
enem$SEXO <- gsub("0", "MASCULINO", enem$SEXO)
enem$SEXO <- gsub("^M$", "MASCULINO", enem$SEXO)

table(enem$TIPO_LINGUA)

enem$TIPO_LINGUA <- gsub("0", "INGLÊS", enem$TIPO_LINGUA)
enem$TIPO_LINGUA <- gsub("1", "ESPANHOL", enem$TIPO_LINGUA)

length(table(enem$UF_PROVA))
rm(enem_2010, enem_2011, enem_2012, enem_2013, enem_2014, enem_2015, enem_2016, enem_2017, merge_enem, colunas)

table(enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("1", "CONCLUÍDO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("2", "CONCLUIRÁ NO ANO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("3", "CONCLUIRÁ APÓS(ANO)", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("4", "NÃO CONC. NÃO CURSANDO", enem$SITUACAO_CONCLUSAO)

summary(enem$NOTA_CIENCIAS_HUMANAS) 
# TRAZ RESUMO DE COLUNAS NUMÉRICAS. COMO A COLUNA ESTÁ EM CHAR, RESULTADO
# RETORNADO DIFERE.

enem$NOTA_CIENCIAS_HUMANAS <- as.numeric(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_CIENCIAS_NATUREZA <- as.numeric(enem$NOTA_CIENCIAS_NATUREZA)
enem$NOTA_MATEMATICA <- as.numeric(enem$NOTA_MATEMATICA)
enem$NOTA_LINGUAGENS_CODIGOS <- as.numeric(enem$NOTA_LINGUAGENS_CODIGOS)
enem$NOTA_REDACAO <- as.numeric(enem$NOTA_REDACAO)

ggplot(data = enem) + geom_bar(aes(x = TIPO_LINGUA), stat = "count")

tp_lingua_sexo <- enem %>% filter(TIPO_LINGUA != ".") %>% select(all_of(c('SEXO', 'TIPO_LINGUA')))

# GRAFICO AQUI
plot.idioma.sexo <- ggplot(data = tp_lingua_sexo) + geom_bar(aes(x = SEXO, fill= TIPO_LINGUA), stat = 'count', position = position_dodge())

plot.idioma.sexo <- plot.idioma.sexo + ggtitle("Idioma por sexo") + xlab("Sexo") + ylab("Quantidade")

plot.idioma.sexo <- plot.idioma.sexo + theme_linedraw() + theme(plot.title = element_text(hjust = 0.5))
plot.idioma.sexo

ggplot(data = enem) + geom_bar(aes(x = UF_PROVA), stat = 'count')

options(scipen = 9999) # tira notação ciêntífica

uf_prova <- enem %>% filter(UF_PROVA != "") %>% select(all_of(c("UF_PROVA", "SITUACAO_CONCLUSAO")))
uf_prova

plot_uf_conclusao <- ggplot(data = uf_prova) + geom_bar(aes(x = UF_PROVA, fill = SITUACAO_CONCLUSAO), position = position_dodge()) + facet_grid(SITUACAO_CONCLUSAO~.)

plot_uf_conclusao <- plot_uf_conclusao + ggtitle("Situação escolar por estado") + ylab("Quantidade") + xlab("Estado")

plot_uf_conclusao <- plot_uf_conclusao + theme_linedraw() + labs(fill = "Situação") + theme(plot.title = element_text(hjust = 0.5))

summary(enem$IDADE)

idade_uf <- enem %>% filter(!is.na(IDADE))

summary(idade_uf$IDADE)

media_idade_sexo_uf <- idade_uf%>% group_by(UF_PROVA, SEXO) %>% summarise(media = mean(IDADE))

media_idade_sexo_uf <- media_idade_sexo_uf %>% filter(UF_PROVA != "")
View(media_idade_sexo_uf)

ggplot(data = media_idade_sexo_uf) + geom_bar(aes(x = UF_PROVA, y = media, fill = SEXO), position = position_dodge(), stat = "identity") + coord_flip()

plot_piram_idade <- ggplot(data = media_idade_sexo_uf, aes(x = reorder(UF_PROVA, -media), y = ifelse(SEXO == 'MASCULINO', -media, media), fill = SEXO)) + geom_bar(stat = "identity") + coord_flip()

plot_piram_idade <- plot_piram_idade + scale_y_continuous(labels = abs)

plot_piram_idade <- plot_piram_idade + ggtitle("Média de idade por UF e Sexo") + ylab("Média Idade") + xlab("Estado") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual(values = c('hotpink', 'dodgerblue3')) + geom_text(aes(label = round(media, digits = 2), hjust = 0.5), size = 4.5, color = 'black', fontface = 'bold')
plot_piram_idade

notas_ciencias_humanas <- enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(IDADE) & IDADE > 17) 
notas_ciencias_humanas_idade <- notas_ciencias_humanas %>% group_by(IDADE) %>% summarize(media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS))

ggplot(data = notas_ciencias_humanas_idade) + geom_point(aes(x = IDADE, y = media_nota_ciencias_humanas))

notas_mt = enem %>% filter(!is.na(NOTA_MATEMATICA) & !is.na(IDADE) & IDADE > 17)
notas_matematica_idade <- notas_mt %>% group_by(IDADE) %>% summarize(media_nota_matematica = mean(NOTA_MATEMATICA))
ggplot(data = notas_matematica_idade) + geom_point(aes(x = IDADE, y = media_nota_matematica))

notas_ciencias_humanas_idade
notas_matematica_idade

notas_ciencias_humanas_matematica_idade <- merge(notas_ciencias_humanas_idade, notas_matematica_idade, all = T)

#install.packages("reshape2")
library(reshape2)

notas_ciencias_humanas_matematica_idade <- melt(notas_ciencias_humanas_matematica_idade, id.vars = 'IDADE')
View(notas_ciencias_humanas_matematica_idade)

plot_scatter_mt_ch <- ggplot(data = notas_ciencias_humanas_matematica_idade) + geom_point(aes(x = IDADE, y = value, color = variable))

plot_scatter_mt_ch <- plot_scatter_mt_ch + ggtitle("Média notas por idade e matéria") + xlab("Idade") + ylab("Nota(média)") + theme_bw() + scale_color_manual(name = 'Matéria', values = c('blue', 'red'), labels = c('Ciências\nHumanas', 'Matemática'))

media_anos <- enem %>% 
  filter(!is.na(NOTA_CIENCIAS_HUMANAS) & 
           !is.na(NOTA_CIENCIAS_NATUREZA) & 
           !is.na(NOTA_LINGUAGENS_CODIGOS) & 
           !is.na(NOTA_MATEMATICA) & 
           !is.na(NOTA_REDACAO)) %>% group_by(ANO) %>% 
  summarise(media_ch = mean(NOTA_CIENCIAS_HUMANAS), 
            media_cn = mean(NOTA_CIENCIAS_NATUREZA),
            media_lc = mean(NOTA_LINGUAGENS_CODIGOS),
            media_mt = mean(NOTA_MATEMATICA),
            media_red = mean(NOTA_REDACAO))

#View(media_anos)


media_anos_2 <- melt(data = media_anos, id.vars = 'ANO')
#View(media_anos_2)
plot_line_notas <- ggplot(data = media_anos_2) + geom_line(aes(x = ANO, y = value, color = variable))

plot_line_notas <- plot_line_notas + ggtitle("Média Notas por Matéria") + ylab("Média") + geom_point(aes(ANO, value, color = variable), size = 3) + geom_text(aes(x = ANO, y = value, color = variable, label = round(value, digits = 2), vjust = -0.15, hjust = 1.25))
plot_line_notas <- plot_line_notas + scale_color_discrete(name = "Matérias", labels = c("Ciênc. Natureza", "Ciência Humanas", "Matemática", "Letras/Códig", "Redação")) + theme_bw()
plot_line_notas

notas_matematica_redacao <- enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_MATEMATICA) & !is.na(NOTA_REDACAO) & !is.na(IDADE) & IDADE > 17 & UF_PROVA %in% c("CE", "DF", "MG", "RS")) %>% group_by(IDADE, UF_PROVA) %>% summarize(media_nota_matematica = mean(NOTA_MATEMATICA), media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS), media_nota_redacao = mean(NOTA_REDACAO))
plot_bolhas_uf_notas <- ggplot(data = notas_matematica_redacao) + geom_point(aes(x = media_nota_ciencias_humanas, y = media_nota_matematica, color = UF_PROVA, size = media_nota_redacao), alpha = 0.5)
plot_bolhas_uf_notas <- plot_bolhas_uf_notas + ggtitle("Médias matemática, ciências humanas e redação") + xlab("Média nota ciências humanas") + ylab("Media nota matemática")
plot_bolhas_uf_notas <- plot_bolhas_uf_notas + labs(color = "UF Prova", size = "Média nota redação") + theme_bw() + theme(legend.position = "bottom")
plot_bolhas_uf_notas

notas_redacao_uf <- enem %>% filter(UF_PROVA != "" & !is.na(NOTA_REDACAO)) %>% select(all_of(c("UF_PROVA", "NOTA_REDACAO")))
View(notas_redacao_uf)

plot_box_uf_redacao <- ggplot(data = notas_redacao_uf) + geom_boxplot(aes(x = UF_PROVA, y = NOTA_REDACAO))
dados <- plot_box_uf_redacao$data # posso acessar dados diretamente do grafico no caso do boxplot
dados <- dados %>% mutate(filial = if_else(UF_PROVA %in% c("CE", "DF", "MG", "RS"), T, F))
ggplot(data = dados) + geom_boxplot(aes(x = UF_PROVA, y = NOTA_REDACAO, fill = filial), outlier.color = 'red', outlier.size = 3.5) + xlab("UF PROVA") + ylab("Notas Redação") + theme_bw() + scale_fill_manual(name = "", values = c("chocolate3", "chartreuse3"), labels = c("Sem filial", "Com filial"))

media_redacao <- enem %>% filter(UF_PROVA != "" & !is.na(NOTA_REDACAO)) %>% mutate(media_nacional = mean(NOTA_REDACAO)) %>% group_by(UF_PROVA, media_nacional) %>% summarize(media_uf = mean(NOTA_REDACAO))

View(media_redacao)

plot_bar_erro <- ggplot(data = media_redacao, aes(x = reorder(UF_PROVA, media_uf), y = media_uf)) + geom_errorbar(aes(ymin = media_nacional/2, ymax = media_nacional), linewidth = 1) + geom_bar(stat = "identity") + coord_flip()
dados <- plot_bar_erro$data
dados <- dados %>% mutate(filial = if_else(UF_PROVA %in% c('CE', 'DF', 'MG', 'RS'), T, F))
View(dados)

plot_bar_erro <- ggplot(data = dados, aes(x = reorder(UF_PROVA, media_uf), y = media_uf)) + geom_errorbar(aes(ymin = media_nacional/2, ymax = media_nacional), linewidth = 1) + geom_bar(aes(fill = filial),stat = "identity") + coord_flip() + guides(fill = FALSE) + ggtitle("Média nota redação por UF/Nacional") + xlab("UF Prova") + ylab("Média Redação") + theme_bw()
plot_bar_erro


plot.idioma.sexo
plot_uf_conclusao
plot_piram_idade
plot_scatter_mt_ch
plot_line_notas
plot_bolhas_uf_notas
plot_box_uf_redacao
plot_bar_erro

library(gridExtra)
library(grid)

lay <- rbind(c(1,2,3,4), c(5,6,7,8))

lay <- rbind(c(1,2), c(3,3))

lay <- rbind(c(1,2), c(3,3), c(4,5), c(6,6))

lay <- rbind(c(1,1), c(2,3))

lay <- rbind(c(1,1), c(2,2))

lay

grid.arrange( 
             #plot_uf_conclusao, 
             #plot_scatter_mt_ch, 
             #plot_bolhas_uf_notas,
             #plot_line_notas, 
             #plot_box_uf_redacao, 
             plot.idioma.sexo,
             plot_piram_idade, 
             #plot_bar_erro, 
             layout_matrix = lay
             )

