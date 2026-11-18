# 03_glm.R
# Poisson-GLM for skadefrekvens.
# Tilpasses på treningsdataene og evalueres på testdataene.
# Resultat: output/glm_results.rds (tabeller og mål til rapporten)

train <- readRDS("data/train.rds")
test  <- readRDS("data/test.rds")

# --- Forberedelse -------------------------------------------------------------

prepare <- function(d) {
  # Effektklasse som kategorisk variabel, siden sammenhengen ikke er lineær.
  # Klasse 12–15 har få forsikringer og slås sammen til "12+".
  d$VehPowerF <- factor(
    pmin(d$VehPower, 12),
    levels = 4:12,
    labels = c(4:11, "12+"))

  # Tetthet 1–5 har svært få forsikringer og fikk et veldig bredt
  # konfidensintervall i første tilpasning. Slås sammen med 6–30.
  levels(d$DensityGrouped)[levels(d$DensityGrouped) %in% c("1–5", "6–30")] <- "1–30"

  return(d)
}

train <- prepare(train)
test  <- prepare(test)

# Referansenivå = gruppen med størst eksponering. Da tolkes koeffisientene
# mot en "typisk" kunde. Samme referanse brukes i test.
factors <- c(
  "DrivAgeGrouped",
  "BonusMalusGrouped",
  "VehAgeGrouped",
  "VehPowerF",
  "VehGas",
  "VehBrand",
  "DensityGrouped",
  "Region")

# opprinnelig rekkefølge, brukes i tabeller og figurer
orig_levels <- list()

for (v in factors) {
  orig_levels[[v]] <- levels(train[[v]])
  exposure_per_level <- tapply(train$Exposure, train[[v]], sum)
  ref <- names(which.max(exposure_per_level))
  train[[v]] <- relevel(train[[v]], ref = ref)
  test[[v]]  <- relevel(test[[v]],  ref = ref)
  cat(v, "- referanse:", ref, "\n")
}

# --- Modeller ----------------------------------------------------------------
# log(Exposure) som offset: forventet antall skader er frekvens * eksponering,
# så log(E[N]) = log(Exposure) + lineær prediktor.

# Nullmodell: samme frekvens for alle (sammenligningsgrunnlag)
m_null <- glm(
  ClaimNb ~ 1 + offset(log(Exposure)),
  family = poisson, data = train)

# Bare førervariabler
m_driver <- glm(
  ClaimNb ~ DrivAgeGrouped + BonusMalusGrouped + offset(log(Exposure)),
  family = poisson, data = train)

# Full modell. Area er utelatt fordi den måler nesten det samme som tettheten.
m_glm <- glm(
  ClaimNb ~ DrivAgeGrouped +
    BonusMalusGrouped +
    VehAgeGrouped +
    VehPowerF +
    VehGas +
    VehBrand +
    DensityGrouped +
    Region +
    offset(log(Exposure)),

  family = poisson, data = train)

# --- Overdispersjon ----------------------------------------------------------
# Poisson antar at varians = forventning. Quasi-Poisson tillater
# varians = phi * forventning. Koeffisientene blir de samme, men
# standardfeilene skaleres med sqrt(phi). Brukes til intervaller og tester.
m_quasi <- update(m_glm, family = quasipoisson)
summary(m_quasi)

dispersion <- summary(m_quasi)$dispersion
cat("Dispersjon (phi):", round(dispersion, 3), "\n")

# --- Bidrar hver variabel? ----------------------------------------------------
# F-test (quasi-Poisson-versjonen av likelihood ratio-testen): fjerner én
# variabel om gangen og tester om modellen blir signifikant dårligere.
# Tar et par minutter, siden modellen tilpasses på nytt for hver variabel.
f_tests <- drop1(m_quasi, test = "F")
print(f_tests)

# --- Relative risikoer --------------------------------------------------------
# Med log-lenke er exp(koeffisient) den multiplikative effekten på frekvensen
# sammenlignet med referansegruppen. 1,5 betyr 50 % høyere skadefrekvens.
# Wald-intervaller med quasi-Poisson-standardfeil.
coefs <- coef(m_quasi)
ci <- confint.default(m_quasi)

rr_list <- list()
for (v in factors) {
  terms <- paste0(v, orig_levels[[v]])
  i_coef <- match(terms, names(coefs))  # NA for referansenivået
  i_ci   <- match(terms, rownames(ci))
  rr_list[[v]] <- data.frame(
    variable  = v,
    level     = orig_levels[[v]],
    rr        = ifelse(is.na(i_coef), 1, exp(coefs[i_coef])),
    lower     = ifelse(is.na(i_ci), 1, exp(ci[i_ci, 1])),
    upper     = ifelse(is.na(i_ci), 1, exp(ci[i_ci, 2])),
    reference = is.na(i_coef)
  )
}
relative_risk <- do.call(rbind, rr_list)
rownames(relative_risk) <- NULL
print(relative_risk, digits = 3)

# Skadefrekvens for en kunde i alle referansegruppene
baseline <- exp(coefs[["(Intercept)"]])
cat("Frekvens for referansekunden:", round(baseline, 4), "\n")

# --- Evaluering på testdata ---------------------------------------------------
# Gjennomsnittlig Poisson-devians: riktig mål for antall-data. Lavere er bedre.
# Ledd med y = 0 bidrar bare med mu, siden y * log(y / mu) -> 0.
poisson_deviance <- function(y, mu) {
  2 * mean(ifelse(y == 0, 0, y * log(y / mu)) - (y - mu))
}

models <- list(
  "Nullmodell" = m_null,
  "Førervariabler" = m_driver,
  "Full GLM" = m_glm)

results <- data.frame(
  modell     = names(models),
  parametere = sapply(models, function(m) length(coef(m))),
  aic        = sapply(models, AIC),
  dev_train  = sapply(models, function(m) {
    poisson_deviance(train$ClaimNb, fitted(m))
  }),

  dev_test   = sapply(models, function(m) {
    poisson_deviance(test$ClaimNb, predict(m, newdata = test, type = "response"))
  }),
  row.names = NULL
)
results$forbedring <- 1 - results$dev_test / results$dev_test[1]
print(results)

# --- Kalibrering på testdata --------------------------------------------------
# Treffer modellen riktig nivå, totalt og per gruppe?
test$pred <- predict(m_glm, newdata = test, type = "response")

totals <- c(observert = sum(test$ClaimNb), predikert = sum(test$pred))
print(totals)

calibrate <- function(var) {
  exposure <- tapply(test$Exposure, test[[var]], sum)
  data.frame(
    level     = names(exposure),
    observert = tapply(test$ClaimNb, test[[var]], sum) / exposure,
    predikert = tapply(test$pred, test[[var]], sum) / exposure,
    row.names = NULL
  )
}

calibration <- list(
  age = calibrate("DrivAgeGrouped"),
  bonus_malus = calibrate("BonusMalusGrouped")
)
print(calibration)

# --- Lagre resultater ---------------------------------------------------------
# Selve modellobjektet er stort (inneholder alle treningsdata), så bare
# resultatene lagres. De leses inn i rapporten.
dir.create("output", showWarnings = FALSE)
saveRDS(
  list(
    results       = results,
    f_tests       = f_tests,
    dispersion    = dispersion,
    relative_risk = relative_risk,
    baseline      = baseline,
    totals        = totals,
    calibration   = calibration
  ),
  "output/glm_results.rds"
)
