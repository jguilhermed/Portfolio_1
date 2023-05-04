setwd("/home/jguilhermed/Documentos/Projetos_R/Despesas_Medicas")
getwd()


library(readr)
library(psych)


despesas <- read_csv("despesas.csv")
View(despesas)




hist(despesas$gastos, main = "Histograma", xlab = "Gastos (R$)")

table(despesas$regiao)

cor(despesas[c("idade", "bmi", "filhos", "gastos")])

# Nenhuma correlação da matriz é considerada forte
# Porém há algumas associações interessantes...
# Por exemplo, bmi e idade, bmi e gastos, filhos e também a idade e gastos...

plot(x = despesas$idade, y = despesas$gastos, main = "Scatterplot - Idade x Gastos",
     xlab = "Idade (anos)",
     ylab = "Gastos (R$)")
pairs(despesas[c("idade", "bmi", "filhos", "gastos")])

pairs.panels(despesas[c("idade", "bmi", "filhos", "gastos")])


modelo <- lm(gastos ~ ., data = despesas) 
#define gastos como target e todas as demais como preditoras

modelo

previsao1 <- predict(modelo)
View(previsao1)
previsao1 <- as.data.frame(previsao1)

despesasTeste <- read.csv("despesas-teste.csv")
View(despesasTeste)

previsao2 = predict(modelo, despesasTeste)
View(previsao2)
previsao2 <- as.data.frame(previsao2)

summary(modelo)

despesas$idade2 = despesas$idade^2
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1,0)

View(despesas)

modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + bmi30 * fumante + sexo + regiao,
                data = despesas)

summary(modelo_v2)
despesasTeste <- read_csv("despesas-teste.csv")
despesasTeste$idade2 <- despesasTeste$idade^2
despesasTeste$bmi30 <- ifelse(despesasTeste$bmi >= 30, 1,0)
previsao3 <- predict(modelo_v2, despesasTeste)
View(previsao3)


