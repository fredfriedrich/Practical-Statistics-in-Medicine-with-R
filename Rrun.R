# ==========================================================  # título da aula no script
# AULA PRÁTICA – ESTATÍSTICA DESCRITIVA E INFERENCIAL        # tema geral
# Dataset: asma vs controle (300 pacientes)                  # contexto do banco
# ==========================================================  # separador visual

# ----------------------------------------------------------  # seção de diretório
# 0) DEFINIR O DIRETÓRIO DE TRABALHO                          # onde estão seu .R e .xlsx
# ----------------------------------------------------------  # separador

setwd("/Users/fredericofriedrich/Desktop/Data Analyst : R/Practical Statistics in Medicine with R")  # defina seu diretório
getwd()                        # confirma o diretório atual
list.files()                   # lista arquivos na pasta (confira se o .xlsx está aqui)

# ----------------------------------------------------------  # seção de pacotes
# 1) INSTALAÇÃO E CARREGAMENTO DE PACOTES                    # alunos de 1ª vez no R
# ----------------------------------------------------------  # separador

install.packages("readxl")        # instala pacote para ler planilhas Excel (.xlsx)
install.packages("dplyr")         # instala pacote para manipulação e resumo de dados
install.packages("ggplot2")       # instala pacote para criar gráficos
# (opcionais para importações diversas, se desejar demonstrar futuramente)
# install.packages("haven")       # para SPSS/Stata/SAS
# install.packages(c("DBI","RSQLite"))  # para bancos SQL locais (SQLite)

library(readxl)                   # carrega o readxl (leitura de .xlsx)
library(dplyr)                    # carrega o dplyr (gramática de dados)
library(ggplot2)                  # carrega o ggplot2 (visualização)

# ----------------------------------------------------------  # seção de importação de exemplos
# 2) MODELOS DE IMPORTAÇÃO DE BANCOS DE DADOS                 # exemplos comuns no ambiente médico
# ----------------------------------------------------------  # separador
# Estes exemplos NÃO serão executados durante a aula.
# Servem apenas para ilustrar como ler diferentes formatos de arquivos.

# ---- Leitura de arquivos CSV (separados por vírgula) ----
# dados <- read.csv("arquivo.csv", sep = ",", header = TRUE)

# ---- Leitura de arquivos delimitados por ponto e vírgula (ex.: exportações do REDCap) ----
# dados <- read.csv2("arquivo.csv", sep = ";", header = TRUE)

# ---- Leitura de arquivos TXT (tabulados) ----
# dados <- read.table("arquivo.txt", sep = "\t", header = TRUE)

# ---- Leitura de arquivos SAV (SPSS) ----
# library(haven)
# dados <- read_sav("arquivo.sav")

# ---- Leitura de arquivos DTA (Stata) ----
# library(haven)
# dados <- read_dta("arquivo.dta")

# ---- Leitura de arquivos RDS (formato nativo do R) ----
# dados <- readRDS("arquivo.rds")

# ---- Leitura de bancos SQL (via DBI + RSQLite) ----
# library(DBI)
# con <- dbConnect(RSQLite::SQLite(), "banco.sqlite")
# dados <- dbReadTable(con, "tabela_nome")
# dbDisconnect(con)

# ----------------------------------------------------------  # seção de importação real
# 3) IMPORTAR O BANCO DE DADOS                                # ler o arquivo da aula
# ----------------------------------------------------------  # separador

dados <- read_excel("asthma_dataset_2025-10-15.xlsx", sheet = "data")   # importa a planilha "data"

head(dados)                        # visualiza as 6 primeiras linhas para checagem rápida
View(dados)                        # abre a base em visualização tipo planilha no RStudio

# ==========================================================  # seção de exploração
# 4) EXPLORAÇÃO INICIAL DO BANCO DE DADOS                    # conhecer a estrutura
# ==========================================================  # separador

names(dados)                       # lista os nomes das variáveis (colunas)
str(dados)                         # mostra tipos de dados e exemplo de valores por coluna
summary(dados)                     # resumo estatístico geral (mín, máx, média, quartis)
dim(dados)                         # retorna número de linhas (n) e colunas (p)
colSums(is.na(dados))              # conta quantos NA existem em cada coluna

# ==========================================================  # seção de resumos por grupo
# 5) ESTATÍSTICAS RESUMIDAS POR GRUPO                        # comparar asma vs controle
# ==========================================================  # separador

table(dados$group)                 # n de participantes por grupo (asma vs controle)

dados %>%                          # início do fluxo (pipe) sobre a base
  group_by(group) %>%              # agrupa linhas por grupo (condicional ao grupo)
  summarise(                       # inicia resumo por grupo
    media_idade = mean(age_years), # média da idade por grupo
    dp_idade = sd(age_years)       # desvio padrão da idade por grupo
  )                                # encerra o summarise

dados %>%                          # novo fluxo de resumo de IMC
  group_by(group) %>%              # agrupa por grupo
  summarise(                       # calcula medidas robustas e quartis
    mediana_bmi = median(bmi),     # mediana do IMC
    Q1_bmi = quantile(bmi, 0.25),  # primeiro quartil (25%)
    Q3_bmi = quantile(bmi, 0.75),  # terceiro quartil (75%)
    IQR_bmi = IQR(bmi)             # intervalo interquartil (Q3 - Q1)
  )                                # encerra o summarise

dados %>%                          # novo fluxo para proporções
  group_by(group) %>%              # agrupa por grupo
  summarise(                       # resumo por grupo
    fumantes_atuais_pct =          # nome da coluna de resultado
      mean(smoking_status == "Current") * 100  # % de fumantes atuais por grupo
  )                                # encerra o summarise

table(dados$group, dados$sex)      # tabela cruzada (grupo x sexo) em contagens

# ==========================================================  # seção de descritiva geral
# 6) ESTATÍSTICA DESCRITIVA GERAL                            # medidas simples
# ==========================================================  # separador

mean(dados$age_years)              # média da idade (todos os participantes)
median(dados$age_years)            # mediana da idade (robusta a outliers)
sd(dados$age_years)                # desvio padrão da idade (dispersão)

mean(dados$bmi)                    # média do IMC
median(dados$bmi)                  # mediana do IMC
quantile(dados$bmi, c(0.25,0.75))  # Q1 e Q3 do IMC (25% e 75%)
IQR(dados$bmi)                     # IQR do IMC (Q3 - Q1)

# Exemplo combinado de saída amigável: mediana [Q1 – Q3]
paste0("IMC: ", round(median(dados$bmi),1),
       " [", round(quantile(dados$bmi,0.25),1), " – ",
       round(quantile(dados$bmi,0.75),1), "]")     # string pronta para relatório

prop.table(                                        # calcula proporções por linha
  table(dados$group, dados$allergic_rhinitis),     # tabela grupo x rinite
  1                                                # margem 1 = por linha (dentro do grupo)
) * 100                                            # converte proporções em %

# ==========================================================  # seção de gráficos
# 7) GRÁFICOS SIMPLES                                        # visualizações básicas
# ==========================================================  # separador

boxplot(bmi ~ group, data = dados,                  # boxplot: IMC (y) por grupo (x)
        main = "IMC por grupo",                     # título do gráfico
        ylab = "IMC (kg/m²)",                       # rótulo do eixo y
        col = "gray")                               # cor cinza (evitar cores fortes)

hist(dados$age_years,                               # histograma da idade
     main = "Distribuição da idade",                # título do gráfico
     xlab = "Idade (anos)",                         # rótulo do eixo x
     col = "lightgray")                             # cor cinza clara

# (opcional com ggplot2) barra empilhada de sexo por grupo:
ggplot(dados, aes(x = group, fill = sex)) +         # mapeia grupo no x e sexo na cor
  geom_bar(position = "fill") +                     # barras empilhadas normalizadas (proporção)
  labs(y = "Proporção", x = "Grupo",                # rótulos dos eixos
       title = "Distribuição de sexo por grupo") +  # título do gráfico
  theme_bw()                                        # tema em preto-e-branco (limpo)

# ==========================================================  # seção de tabelas & proporções
# 8) TABELAS CRUZADAS E PROPORÇÕES                          # frequências relativas
# ==========================================================  # separador

table(dados$sex)                                    # contagem simples de sexo
table(dados$group, dados$allergic_rhinitis)         # tabela cruzada grupo x rinite (contagem)
prop.table(table(dados$group, dados$allergic_rhinitis), 1) * 100   # % por linha (grupo)

# ==========================================================  # seção de testes
# 9) TESTES ESTATÍSTICOS BÁSICOS                            # inferência introdutória
# ==========================================================  # separador

shapiro.test(dados$age_years[1:500])                # teste de normalidade (Shapiro); usa <=500 obs (LIMITADO)
hist(dados$age_years)                               # histograma da idade
qqnorm(dados$age_years)                             # gráfico Q-Q (quantis teóricos vs observados)
qqline(dados$age_years, col="red")                  # linha de referência da normalidade

t.test(fev1_percent_predicted ~ group, data = dados) # teste t: compara FEV1% entre grupos
wilcox.test(bmi ~ group, data = dados)              # Mann-Whitney: compara IMC entre grupos (não paramétrico)

tab <- table(dados$group, dados$allergic_rhinitis)  # cria tabela 2x2 de grupo x rinite
chisq.test(tab)                                     # teste qui-quadrado de associação
fisher.test(tab)                                    # teste exato de Fisher (se células pequenas)

cor.test(dados$fev1_percent_predicted,              # correlação entre FEV1% (numérico)
         dados$bmi, method = "spearman")            # e IMC (numérico) via Spearman (não paramétrica)

cor.test(dados$fev1_percent_predicted,              # correlação entre FEV1%
         dados$age_years, method = "pearson")       # e idade via Pearson (assume normalidade)

# ==========================================================  # seção de modelos logísticos
# 10) REGRESSÃO LOGÍSTICA                                   # desfecho binário (asma vs controle)
# ==========================================================  # separador

dados$asma <- ifelse(dados$group == "Asthma", 1, 0) # cria variável binária: 1=asma, 0=controle

modelo_log1 <- glm(asma ~ allergic_rhinitis,        # modelo logístico simples: rinite → asma
                   data = dados, family = "binomial")  # família binomial = regressão logística
summary(modelo_log1)                                 # exibe coeficientes (log-OR), erros, p-valores
round(
  exp(cbind(OR = coef(modelo_log1),                  # transforma coeficientes em OR
            confint(modelo_log1))), 2)              # e calcula IC95% das OR (arredonda em 2 casas)

modelo_log2 <- glm(asma ~ allergic_rhinitis +       # modelo logístico ajustado (controle de confusão)
                     age_years + sex + bmi,         # ajusta por idade, sexo e IMC
                   data = dados, family = "binomial")  # família binomial (logística)
summary(modelo_log2)                                 # resultados do modelo ajustado
exp(cbind(OR = coef(modelo_log2),                   # OR ajustadas
          confint(modelo_log2)))                    # IC95% das OR ajustadas

# Interpretação: OR > 1 aumenta chance de asma; OR < 1 reduz chance de asma  # dica didática

# ==========================================================  # seção de modelos lineares
# 11) REGRESSÃO LINEAR                                      # desfecho numérico (FEV1% previsto)
# ==========================================================  # separador

modelo_lin1 <- lm(fev1_percent_predicted ~ bmi,     # modelo linear simples: FEV1% ~ IMC
                  data = dados)                     # usa toda a base
summary(modelo_lin1)                                 # coeficientes, erro, t, p e R²

modelo_lin2 <- lm(fev1_percent_predicted ~          # modelo linear ajustado: inclui covariáveis
                    bmi + age_years + sex +         # IMC, idade, sexo
                    smoking_status,                 # tabagismo
                  data = dados)                     # base de dados
summary(modelo_lin2)                                 # resultados com ajuste

plot(dados$bmi, dados$fev1_percent_predicted,       # gráfico de dispersão (IMC x FEV1%)
     main = "Relação entre IMC e FEV1%",            # título
     xlab = "IMC (kg/m²)", ylab = "FEV1 (% previsto)",  # rótulos
     pch = 19, col = "gray")                        # pontos cinza (visual neutro)
abline(modelo_lin1, col = "black", lwd = 2)         # adiciona a reta do modelo simples

# Dicas: coeficiente do IMC indica variação média no FEV1% por 1 unidade de IMC     # interpretação
# p-valor testa se o coeficiente é diferente de 0; R² indica proporção explicada     # interpretação

# ==========================================================  # exercícios
# 12) EXERCÍCIOS PRÁTICOS                                   # para os alunos praticarem
# ==========================================================  # separador

# 1. Calcule a média e a mediana da idade por grupo.         # exercício de resumo por grupo
# 2. Crie um boxplot de FEV1% por grupo.                      # exercício de visualização
# 3. Teste se há diferença de IMC entre asmáticos e controles.# teste não paramétrico (ou t-test se normal)
# 4. Teste se há associação entre asma e refluxo gastroesofágico. # qui-quadrado ou Fisher
# 5. Ajuste um modelo logístico com 2 variáveis preditoras.    # prática de glm binomial
# 6. Ajuste um modelo linear com FEV1% como desfecho.          # prática de lm
