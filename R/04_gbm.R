# 04_gbm.R
# Gradient boosting (xgboost) for skadefrekvens, som sammenligning med GLM-en.
# Samme trening/test-splitt og samme mål som i 03_glm.R.
# Resultat: output/gbm_results.rds (tabeller og mål til rapporten)
#
# Kjøretid: kryssvalideringen tar typisk 5–15 minutter.

library(xgboost)

train <- readRDS("data/train.rds")
test  <- readRDS("data/test.rds")

# --- Forberedelse -------------------------------------------------------------
# Rå variabler i stedet for grupperte: trærne finner ikke-lineære
# sammenhenger og samspill selv. Tetthet trenger ingen log-transformasjon,
# siden splitter i trær ikke påvirkes av monotone transformasjoner.
# Area er utelatt av samme grunn som i GLM-en.
features <- c(
  "DrivAge",
  "BonusMalus",
  "VehAge",
  "VehPower",
  "Density",
  "VehGas",
  "VehBrand",
  "Region")

# Faktorer gjøres om til dummyvariabler (one-hot)
make_matrix <- function(d) {
  model.matrix(~ . - 1, data = d[, features])
}

X_train <- make_matrix(train)
X_test  <- make_matrix(test)

# log(Exposure) som base_margin: xgboost sin versjon av offset.
# Må settes på BÅDE trening og test, ellers blir prediksjonene
# skader per år i stedet for forventet antall skader.
dtrain <- xgb.DMatrix(
  X_train, label = train$ClaimNb,
  base_margin = log(train$Exposure))

dtest  <- xgb.DMatrix(
  X_test, label = test$ClaimNb,
  base_margin = log(test$Exposure))

# --- Tuning med kryssvalidering ---------------------------------------------
# Testsettet brukes ikke her. 5-fold CV på treningsdataene velger
# tredybde og antall trær (early stopping).
set.seed(42)

params_base <- list(
  objective        = "count:poisson",
  eval_metric      = "poisson-nloglik",
  eta              = 0.05,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 10
)

depths <- c(2, 3, 4, 5)
cv_results <- data.frame(max_depth = depths, nrounds = NA, cv_nloglik = NA)

for (i in seq_along(depths)) {
  params <- c(params_base, max_depth = depths[i])
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1500,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = FALSE)

  # Beste runde leses fra loggen (fungerer i både gamle og nye xgboost-versjoner)
  log_cv <- cv$evaluation_log
  best   <- which.min(log_cv$test_poisson_nloglik_mean)
  cv_results$nrounds[i]    <- best
  cv_results$cv_nloglik[i] <- log_cv$test_poisson_nloglik_mean[best]
  cat("max_depth", depths[i], "- trær:", best,
    "- CV n_log_likelihood:", round(log_cv$test_poisson_nloglik_mean[best], 5), "\n")
}
print(cv_results)

best_row    <- which.min(cv_results$cv_nloglik)
best_depth  <- cv_results$max_depth[best_row]
best_rounds <- cv_results$nrounds[best_row]

# --- Endelig modell ----------------------------------------------------------
set.seed(42)
gbm <- xgb.train(
  params = c(params_base, max_depth = best_depth),
  data = dtrain,
  nrounds = best_rounds,
  verbose = 0)

# --- Evaluering på testdata --------------------------------------------------
# Samme devians som i 03_glm.R
poisson_deviance <- function(y, mu) {
  2 * mean(ifelse(y == 0, 0, y * log(y / mu)) - (y - mu))
}

# predict() bruker base_margin fra DMatrix, så dette er forventet antall skader
pred_train <- predict(gbm, dtrain)
pred_test  <- predict(gbm, dtest)

# Nullmodellen: samme frekvens for alle (som m_null i 03_glm.R)
null_freq <- sum(train$ClaimNb) / sum(train$Exposure)
dev_null_test <- poisson_deviance(test$ClaimNb, null_freq * test$Exposure)

# Samme kolonner som results i glm_results.rds, så tabellene kan slås sammen.
# Parametere og AIC er ikke meningsfulle for en GBM.
results <- data.frame(
  modell     = "GBM",
  parametere = NA,
  aic        = NA,
  dev_train  = poisson_deviance(train$ClaimNb, pred_train),
  dev_test   = poisson_deviance(test$ClaimNb, pred_test)
)
results$forbedring <- 1 - results$dev_test / dev_null_test
print(results)

# --- Kalibrering på testdata -------------------------------------------------
# Grupperes etter de grupperte variablene, som i GLM-en, selv om modellen
# bruker de rå variablene.
test$pred <- pred_test

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

# --- Variabelviktighet -------------------------------------------------------
# Gain: hvor mye hver variabel forbedrer tapsfunksjonen totalt.
# Dummyvariablene (f.eks. RegionR24) summeres tilbake til variabelen.
imp <- xgb.importance(model = gbm)
imp$variable <- sapply(imp$Feature, function(f) features[startsWith(f, features)][1])
importance <- aggregate(Gain ~ variable, data = imp, FUN = sum)
importance <- importance[order(-importance$Gain), ]
rownames(importance) <- NULL
print(importance)

# --- Partial dependence ------------------------------------------------------
# Gjennomsnittlig predikert frekvens når én variabel settes til en fast verdi
# for alle, og resten beholder sine faktiske verdier. Et utvalg brukes for fart.
# base_margin = 0 gir frekvens per år (eksponering 1).
set.seed(42)
pd_data <- train[sample(nrow(train), 10000), ]

partial_dependence <- function(var, values, fixed = list()) {
  d <- pd_data
  for (v in names(fixed)) d[[v]] <- fixed[[v]]
  sapply(values, function(x) {
    d[[var]] <- x
    dm <- xgb.DMatrix(make_matrix(d), base_margin = rep(0, nrow(d)))
    mean(predict(gbm, dm))
  })
}

ages <- 18:85
bms  <- seq(50, 150, by = 5)

pd_age <- data.frame(DrivAge = ages,
  frekvens = partial_dependence("DrivAge", ages))
pd_bm  <- data.frame(BonusMalus = bms,
  frekvens = partial_dependence("BonusMalus", bms))

# Samspill alder x bonus/malus: aldersprofilen for ulike bonus/malus-nivåer.
# Hvis kurvene har ulik form, er effekten av alder avhengig av bonus/malus,
# noe GLM-en uten samspillsledd ikke kan fange opp.
pd_age_bm <- do.call(rbind, lapply(c(50, 80, 100), function(b) {
  data.frame(
    DrivAge    = ages,
    BonusMalus = b,
    frekvens   = partial_dependence("DrivAge", ages, fixed = list(BonusMalus = b))
  )
}))

# --- Lagre resultater --------------------------------------------------------
dir.create("output", showWarnings = FALSE)
saveRDS(
  list(
    results     = results,
    cv_results  = cv_results,
    best_depth  = best_depth,
    best_rounds = best_rounds,
    totals      = totals,
    calibration = calibration,
    importance  = importance,
    pd_age      = pd_age,
    pd_bm       = pd_bm,
    pd_age_bm   = pd_age_bm
  ),
  "output/gbm_results.rds"
)
