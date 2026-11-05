# 01_load_data.R
# Laster freMTPL2freq, rydder dataene og deler i trening og test.
# Resultat: data/train.rds og data/test.rds
#
# Installer CASdatasets én gang (ligger ikke på CRAN):
#   install.packages(c("xts", "zoo", "sp"))
#   install.packages("CASdatasets",
#                    repos = "https://cas.uqam.ca/pub/", type = "source")
#   renv::snapshot()

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

# --- Oppsummering -------------------------------------------------------------

cat("Antall forsikringer:", nrow(df), "\n")
cat("Total eksponering (år):", round(sum(df$Exposure)), "\n")
cat("Skadefrekvens per år:", round(sum(df$ClaimNb) / sum(df$Exposure), 4), "\n")
print(table(df$ClaimNb))

# --- Trening og test -----------------------------------------------------------

# 80/20, samme splitt for alle modeller
set.seed(42)
idx   <- sample(nrow(df), size = round(0.8 * nrow(df)))
train <- df[idx, ]
test  <- df[-idx, ]

dir.create("data", showWarnings = FALSE)
saveRDS(train, "data/train.rds")
saveRDS(test,  "data/test.rds")

cat("Trening:", nrow(train), " Test:", nrow(test), "\n")
