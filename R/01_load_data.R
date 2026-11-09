# 01_load_data.R
# Laster freMTPL2freq, rydder dataene og deler i trening og test.
# Resultat: data/train.rds og data/test.rds

library(CASdatasets)

data(freMTPL2freq)
df <- freMTPL2freq

# --- Rydding ----------------------------------------------------------------

# Sikre riktige typer (formatet har variert mellom versjoner av pakken)
df$ClaimNb  <- as.integer(df$ClaimNb)
df$Exposure <- as.numeric(df$Exposure)

# Eksponering er andel av et år. Noen få verdier er over 1, som trolig er feil.
# Skadeantall over 4 er svært sjeldne og trolig feil. Begge kappes,
# som i Noll, Salzmann og Wüthrich (2018).
cat("Exposure > 1:", sum(df$Exposure > 1), "\n")
cat("ClaimNb > 4: ", sum(df$ClaimNb > 4), "\n")
df$Exposure <- pmin(df$Exposure, 1)
df$ClaimNb  <- pmin(df$ClaimNb, 4L)

# Kategoriske variabler som faktorer
for (v in c("Area", "VehBrand", "VehGas", "Region")) {
  df[[v]] <- factor(df[[v]])
}

# IDpol er bare en ID og brukes ikke i modellene
df$IDpol <- NULL

# Grupper kontinuerlige variabler (brukes i EDA, GLM og rapport)
df$DrivAgeGr <- cut(df$DrivAge, c(17, 21, 26, 31, 41, 51, 71, Inf),
  labels = c("18–21", "22–26", "27–31", "32–41",
    "42–51", "52–71", "72+"))
df$VehAgeGr <- cut(df$VehAge, c(-1, 0, 2, 5, 10, 15, Inf),
  labels = c("0", "1–2", "3–5", "6–10", "11–15", "16+"))
df$BonusMalusGr <- cut(df$BonusMalus, c(49, 50, 60, 80, 100, Inf),
  labels = c("50", "51–60", "61–80", "81–100", "101+"))
df$DensityGr <- cut(df$Density, c(0, 5, 30, 165, 900, 4900, Inf),
  labels = c("1–5", "6–30", "31–165", "166–900",
    "901–4900", "4900+"))

# --- Oppsummering -------------------------------------------------------------

cat("Antall forsikringer:", nrow(df), "\n")
cat("Total eksponering (år):", round(sum(df$Exposure)), "\n")
cat("Skadefrekvens per år:", round(sum(df$ClaimNb) / sum(df$Exposure), 4), "\n")
print(table(df$ClaimNb))

# --- Trening og test ----------------------------------------------------------

# 80/20, samme splitt for alle modeller
set.seed(42)
idx   <- sample(nrow(df), size = round(0.8 * nrow(df)))
train <- df[idx, ]
test  <- df[-idx, ]

dir.create("data", showWarnings = FALSE)
saveRDS(train, "data/train.rds")
saveRDS(test,  "data/test.rds")

cat("Trening:", nrow(train), " Test:", nrow(test), "\n")
