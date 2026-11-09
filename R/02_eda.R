# 02_eda.R
# Utforskende analyse av treningsdataene.
# Bruker bare train.rds, så testsettet holdes urørt til modellevalueringen.
# Resultat: figurer i output/figures/

library(dplyr)
library(ggplot2)

train <- readRDS("data/train.rds")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# --- Oversikt -----------------------------------------------------------------

str(train)
summary(train)

# Andel forsikringer med 0, 1, 2, ... skader
round(prop.table(table(train$ClaimNb)), 4)

# Total skadefrekvens: skader per forsikringsår
sum(train$ClaimNb) / sum(train$Exposure)

# --- Fordeling av eksponering -------------------------------------------------

p <- ggplot(train, aes(Exposure)) +
  geom_histogram(bins = 50) +
  labs(x = "Eksponering (år)", y = "Antall forsikringer")
ggsave("output/figures/exposure.png", p, width = 6, height = 4)

# --- Observert skadefrekvens per variabel -------------------------------------
# Frekvens = sum(skader) / sum(eksponering) i hver gruppe.
# Kontinuerlige variabler deles i intervaller først.

train <- train |>
  mutate(
    DrivAgeGr    = cut(DrivAge, c(17, 21, 26, 31, 41, 51, 71, Inf)),
    VehAgeGr     = cut(VehAge, c(-1, 0, 2, 5, 10, 15, Inf)),
    BonusMalusGr = cut(BonusMalus, c(49, 50, 60, 80, 100, Inf)),
    DensityGr    = cut(log(Density), 6)
  )

freq_by <- function(var) {
  train |>
    group_by(.data[[var]]) |>
    summarise(
      frekvens   = sum(ClaimNb) / sum(Exposure),
      eksponering = sum(Exposure)
    )
}

plot_freq <- function(var) {
  tab <- freq_by(var)
  p <- ggplot(tab, aes(.data[[var]], frekvens)) +
    geom_col() +
    labs(x = var, y = "Skader per forsikringsår") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0("output/figures/freq_", var, ".png"), p, width = 6, height = 4)
  tab
}

vars <- c("DrivAgeGr", "VehAgeGr", "BonusMalusGr", "DensityGr",
  "VehPower", "Area", "VehGas", "VehBrand", "Region")

for (v in vars) print(plot_freq(v))
