# Pacotes necessários
library(naniar)
library(dplyr)
library(magick)  # Para combinar imagens

# 1. Simular os dados
set.seed(123)
n <- 500
age <- rnorm(n, 50, 10)
income <- 30000 + 1000 * age + rnorm(n, 0, 5000)
education <- rnorm(n, 16, 2)
data_full <- data.frame(age, income, education)

# 2. Criar os três tipos de missing
data_mcar <- data_full %>%
  mutate(
    income = ifelse(runif(n) < 0.2, NA, income),
    education = ifelse(runif(n) < 0.1, NA, education)
  )

data_mar <- data_full %>%
  mutate(
    income = ifelse(age > 55 & runif(n) < 0.5, NA, income),
    education = ifelse(income > 70000 & runif(n) < 0.4, NA, education)
  )

data_mnar <- data_full %>%
  mutate(
    income = ifelse(income > quantile(income, 0.75) & runif(n) < 0.5, NA, income),
    education = ifelse(education < 14 & runif(n) < 0.5, NA, education)
  )

# 3. Salvar os gráficos como imagens
png("mcar.png", width = 800, height = 600)
gg_miss_upset(data_mcar, nsets = 3)
dev.off()

png("mar.png", width = 800, height = 600)
gg_miss_upset(data_mar, nsets = 3)
dev.off()

png("mnar.png", width = 800, height = 600)
gg_miss_upset(data_mnar, nsets = 3)
dev.off()

# 4. Usar magick para unir as imagens
img_mcar <- image_read("mcar.png")
img_mar  <- image_read("mar.png")
img_mnar <- image_read("mnar.png")

# Unir lado a lado
combined <- image_append(c(img_mcar, img_mar, img_mnar))
print(combined)  # Mostrar na tela
