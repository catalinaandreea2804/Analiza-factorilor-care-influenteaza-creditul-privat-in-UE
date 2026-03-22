# ============================================================
# ECONOMETRIE 2025-2026 — APLICATIA 1 (TRANSVERSAL + ML)
# Split: 80/20 + output vizibil + export grafice in /plots
# ============================================================

# 0) Pachete --------------------------------------------------
packs <- c("readxl","dplyr","ggplot2","corrplot","car","lmtest","sandwich",
           "Metrics","glmnet","psych","stargazer","caret","janitor","stringr")
to_install <- packs[!packs %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packs, library, character.only = TRUE))

set.seed(123)

# 1) Citire Excel ---------------------------------------------
file_name <- "set_date_state_UE (1).xlsx"
stopifnot(file.exists(file_name))

sheets <- readxl::excel_sheets(file_name)
raw <- readxl::read_excel(file_name, sheet = sheets[1]) %>% as.data.frame()
raw <- janitor::clean_names(raw)

cat("\n--- Foi Excel ---\n"); print(sheets)
cat("\n--- Coloane dupa clean_names() ---\n"); print(names(raw))

# 2) Auto-detect coloane (fara sa redenumesti manual) ----------
find_col <- function(nms, patterns) {
  for(p in patterns) {
    hit <- nms[stringr::str_detect(nms, stringr::regex(p, ignore_case = TRUE))]
    if(length(hit) > 0) return(hit[1])
  }
  return(NA_character_)
}
nms <- names(raw)

col_country <- find_col(nms, c("^country$", "tara", "stat", "name"))
col_code    <- find_col(nms, c("^code$", "^cod$", "iso", "country_code"))

col_y       <- find_col(nms, c("dom.*credit", "credit.*priv", "private.*credit", "credit.*gdp", "domestic_credit"))
col_gdpc    <- find_col(nms, c("gdp.*cap", "pib.*loc", "gdp_per_cap", "gdp_capita", "gdppc"))
col_lr      <- find_col(nms, c("lending.*rate", "loan.*rate", "interest.*rate", "doband", "rata.*credit"))
col_inf     <- find_col(nms, c("^inflation$", "hicp", "cpi", "infl"))
col_unemp   <- find_col(nms, c("unemployment", "somaj", "u_rate", "ur"))
col_finlit  <- find_col(nms, c("fin.*lit", "financial.*literacy", "literacy", "educ.*fin"))
col_hpi     <- find_col(nms, c("^hpi", "house.*price", "pret.*locuin", "housing.*price", "residential.*price"))

mapping <- data.frame(
  target = c("Country","Code","DomCredit_GDP","GDP_Capita","LendingRate","Inflation","Unemployment","FinLit_Score","HPI_Change"),
  found  = c(col_country, col_code, col_y, col_gdpc, col_lr, col_inf, col_unemp, col_finlit, col_hpi)
)

cat("\n--- Mapare detectata (target -> Excel) ---\n")
print(mapping)

if(any(is.na(mapping$found))) {
  stop("Nu am identificat toate coloanele. Trimite-mi tabelul mapping (ce are NA) si il fixez.")
}

# 3) Dataset standard ------------------------------------------
data <- raw %>%
  transmute(
    Country       = .data[[col_country]],
    Code          = .data[[col_code]],
    DomCredit_GDP = .data[[col_y]],
    GDP_Capita    = .data[[col_gdpc]],
    LendingRate   = .data[[col_lr]],
    Inflation     = .data[[col_inf]],
    Unemployment  = .data[[col_unemp]],
    FinLit_Score  = .data[[col_finlit]],
    HPI_Change    = .data[[col_hpi]]
  )

# 3.a) Curata numeric: %, virgule -> punct
num_cols <- c("DomCredit_GDP","GDP_Capita","LendingRate","Inflation","Unemployment","FinLit_Score","HPI_Change")
for(cc in num_cols){
  data[[cc]] <- as.numeric(gsub("%","", gsub(",", ".", as.character(data[[cc]]))))
}

cat("\n--- NA-uri dupa conversie numerica ---\n")
print(colSums(is.na(data[, c("Country","Code", num_cols)])))

# eliminam randurile incomplete (transparent)
data <- na.omit(data)

cat("\n--- Dimensiune finala dataset ---\n")
print(dim(data))

# 4) Transformari ---------------------------------------------
east_codes <- c("BGR","HRV","CZE","EST","HUN","LVA","LTU","POL","ROU","SVK","SVN")

data <- data %>%
  mutate(
    log_GDP = log(GDP_Capita),
    DummyEast = ifelse(Code %in% east_codes, 1, 0),
    LR_x_East = LendingRate * DummyEast,
    LendingRate_sq = LendingRate^2
  )

# 5) Export grafice ----------------------
if(!dir.exists("plots")) dir.create("plots")

save_plot <- function(filename, plot_obj, w=8, h=5){
  ggplot2::ggsave(filename = file.path("plots", filename),
                  plot = plot_obj, width = w, height = h, dpi = 200)
}

# 6) EDA -------------------------------------------------------
vars_eda <- c("DomCredit_GDP","log_GDP","LendingRate","Inflation","Unemployment","FinLit_Score","HPI_Change")


cat("\n================ EDA =================\n")
print(psych::describe(data[, vars_eda]))
stargazer(as.data.frame(data[, vars_eda]),
          type="text", title="Statistici descriptive - APLICATIA 1", digits=3)

# Corelatii (imagine)
cor_mat <- cor(data[, vars_eda], use = "pairwise.complete.obs")
png(file.path("plots","correlatii_heatmap.png"), width=900, height=700)
corrplot(cor_mat, method="color", addCoef.col="black", type="upper", diag=FALSE,
         title="Matrice corelatii (UE)", mar=c(0,0,2,0))
dev.off()

# Grafice EDA (salvate in /plots)
p_hist <- ggplot(data, aes(x = DomCredit_GDP)) +
  geom_histogram(bins=10) + theme_bw() +
  ggtitle("Distributia DomCredit_GDP (% PIB)")
save_plot("hist_domcredit.png", p_hist)

p1 <- ggplot(data, aes(x = log_GDP, y = DomCredit_GDP)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + ggtitle("DomCredit_GDP vs log(GDP_Capita)")
save_plot("scatter_loggdp_domcredit.png", p1)

p2 <- ggplot(data, aes(x = LendingRate, y = DomCredit_GDP)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + ggtitle("DomCredit_GDP vs LendingRate")
save_plot("scatter_lendingrate_domcredit.png", p2)

p3 <- ggplot(data, aes(x = Inflation, y = DomCredit_GDP)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + ggtitle("DomCredit_GDP vs Inflation")
save_plot("scatter_inflation_domcredit.png", p3)

p4 <- ggplot(data, aes(x = Unemployment, y = DomCredit_GDP)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + ggtitle("DomCredit_GDP vs Unemployment")
save_plot("scatter_unemployment_domcredit.png", p4)

p5 <- ggplot(data, aes(x = HPI_Change, y = DomCredit_GDP)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + ggtitle("DomCredit_GDP vs HPI_Change")
save_plot("scatter_hpi_domcredit.png", p5)

p6 <- ggplot(data, aes(x = FinLit_Score, y = DomCredit_GDP)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  theme_bw() + ggtitle("DomCredit_GDP vs FinLit_Score")
save_plot("scatter_finlit_domcredit.png", p6)

cat("\nGrafice salvate in folderul: plots/\n")

# 7) Split 80/20 ----------------------------------------------
# cerinta: train/test :contentReference[oaicite:1]{index=1}
set.seed(123)
idx <- caret::createDataPartition(data$DomCredit_GDP, p = 0.80, list = FALSE)
train <- data[idx, ]
test  <- data[-idx, ]

cat("\n================ SPLIT 80/20 ================\n")
cat("train =", nrow(train), "| test =", nrow(test), "\n")

mape <- function(y, yhat) mean(abs((y - yhat) / y)) * 100

# 9) Regresie Simpla (DomCredit_GDP ~ Inflation) ---------------

# Creeaza modelul de regresie simpla (m_simplu)
# Y = DomCredit_GDP
# X = Inflation
m_simplu <- lm(DomCredit_GDP ~ Inflation, data = train)



# Afiseaza sumarul modelului pentru a vedea coeficientii si semnificatia
cat("\n--- m_simplu (DomCredit_GDP ~ Inflation) ---\n")
print(summary(m_simplu))

nls_model <- nls(DomCredit_GDP ~ a * exp(b * Inflation),
                 data = train,
                 start = list(a = 50, b = 0.01))

cat("\n--- Rezumat regresie neliniara ---\n")
print(summary(nls_model))

# ---------------------------------------------
# # Pseudo-R^2
# ---------------------------------------------
# RSS = suma patratelor reziduurilor
RSS <- sum(residuals(nls_model)^2)

# TSS = suma patratelor diferentelor fata de medie
TSS <- sum((train$DomCredit_GDP - mean(train$DomCredit_GDP))^2)

# # Pseudo-R^2
pseudo_R2 <- 1 - RSS/TSS
cat("Pseudo-R2:", round(pseudo_R2, 3), "\n")
# 8) OLS: baza + extins ---------------------------------------
m1 <- lm(DomCredit_GDP ~ log_GDP + LendingRate + Inflation + Unemployment + FinLit_Score + HPI_Change,
         data = train)

m2 <- lm(DomCredit_GDP ~ log_GDP + LendingRate + LendingRate_sq + Inflation + Unemployment +
           FinLit_Score + HPI_Change + DummyEast + LR_x_East,
         data = train)

cat("\n================ OLS SUMMARY ================\n")
cat("\n--- m1 (baza) ---\n"); print(summary(m1))
cat("\n--- m2 (extins) ---\n"); print(summary(m2))
cat("\nAIC/BIC:\n"); print(AIC(m1,m2)); print(BIC(m1,m2))

# 9) Diagnostice + robuste ------------------------------------
diag_block <- function(model, name){
  cat("\n================ DIAGNOSTICE:", name, "================\n")
  # VIF nu se aplica direct sau este 1 pentru regresia simpla, il excludem sau verificam:
  # if (length(coef(model)) > 2) { cat("\nVIF:\n"); print(car::vif(model)) } else { cat("\nVIF: N/A (Regresie simpla)\n") }
  cat("\nBreusch-Pagan:\n"); print(lmtest::bptest(model))
  cat("\nRESET:\n"); print(lmtest::resettest(model, power=2:3, type="fitted"))
  cat("\nCoeficienti cu SE robuste HC1:\n")
  print(lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type="HC1")))
}

# Rulam diagnosticele pentru modelul simplu
diag_block(m_simplu,"m_simplu")
diag_block <- function(model, name){
  cat("\n================ DIAGNOSTICE:", name, "================\n")
  cat("\nVIF:\n"); print(car::vif(model))
  cat("\nBreusch-Pagan:\n"); print(lmtest::bptest(model))
  cat("\nRESET:\n"); print(lmtest::resettest(model, power=2:3, type="fitted"))
  cat("\nCoeficienti cu SE robuste HC1:\n")
  print(lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type="HC1")))
}
diag_block(m1,"m1")
diag_block(m2,"m2")

# 10) Performanta pe TEST -------------------------------------
pred_m_simplu <- predict(m_simplu, newdata = test)
pred_m1 <- predict(m1, newdata = test)
pred_m2 <- predict(m2, newdata = test)

metrics_ols <- data.frame(
  Model = c("OLS_m_simplu","OLS_m1_baza","OLS_m2_extins"),
  RMSE  = c(Metrics::rmse(test$DomCredit_GDP, pred_m_simplu),
            Metrics::rmse(test$DomCredit_GDP, pred_m1),
            Metrics::rmse(test$DomCredit_GDP, pred_m2)),
  MAE   = c(Metrics::mae(test$DomCredit_GDP, pred_m_simplu),
            Metrics::mae(test$DomCredit_GDP, pred_m1),
            Metrics::mae(test$DomCredit_GDP, pred_m2)),
  MAPE  = c(mape(test$DomCredit_GDP, pred_m_simplu),
            mape(test$DomCredit_GDP, pred_m1),
            mape(test$DomCredit_GDP, pred_m2))
)
# Alegerea celui mai bun model dupa RMSE
best_ols <- if(metrics_ols$RMSE[3] < metrics_ols$RMSE[1] & metrics_ols$RMSE[3] < metrics_ols$RMSE[2]) m2 else if(metrics_ols$RMSE[1] < metrics_ols$RMSE[2]) m_simplu else m1
best_name <- if(identical(best_ols, m2)) "OLS_m2_extins" else if(identical(best_ols, m_simplu)) "OLS_m_simplu" else "OLS_m1_baza"

# Afisare rezultate
cat("\n================ PERF OLS (TEST) =================\n")
print(metrics_ols)
cat("\nModel OLS ales dupa RMSE:", best_ols$name, "\n")

cat("\n================ PERF OLS (TEST) ================\n")
print(metrics_ols)
cat("Model OLS ales dupa RMSE:", best_name, "\n")

# 11) Scenariu (Romania) --------------------------------------
country_code <- "ROU"
base_row <- data %>% filter(Code == country_code)

pred_base <- NA; pred_scen <- NA
if(nrow(base_row) == 1){
  scen_row <- base_row %>%
    mutate(LendingRate = LendingRate - 1.5,
           Inflation   = Inflation - 2.0,
           HPI_Change  = HPI_Change + 3.0,
           LendingRate_sq = LendingRate^2,
           LR_x_East      = LendingRate * DummyEast)
  
  pred_base <- as.numeric(predict(best_ols, newdata = base_row))
  pred_scen <- as.numeric(predict(best_ols, newdata = scen_row))
  
  cat("\n================ SCENARIU (ROU) ================\n")
  cat("Baseline:", round(pred_base,3), "\n")
  cat("Scenariu :", round(pred_scen,3), "\n")
  cat("Delta    :", round(pred_scen - pred_base,3), "\n")
} else {
  cat("\nNu am gasit exact 1 rand pentru ROU (verifica Code in Excel).\n")
}

# 12) Regularizare: Ridge / Lasso / ElasticNet ----------------
x_train <- model.matrix(DomCredit_GDP ~ log_GDP + LendingRate + LendingRate_sq + Inflation + Unemployment +
                          FinLit_Score + HPI_Change + DummyEast + LR_x_East, data = train)[, -1]
y_train <- train$DomCredit_GDP

x_test <- model.matrix(DomCredit_GDP ~ log_GDP + LendingRate + LendingRate_sq + Inflation + Unemployment +
                         FinLit_Score + HPI_Change + DummyEast + LR_x_East, data = test)[, -1]
y_test <- test$DomCredit_GDP

set.seed(123)

cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
ridge <- glmnet(x_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
pred_ridge <- as.numeric(predict(ridge, newx = x_test))

cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
lasso <- glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
pred_lasso <- as.numeric(predict(lasso, newx = x_test))

cv_enet <- cv.glmnet(x_train, y_train, alpha = 0.5)
enet <- glmnet(x_train, y_train, alpha = 0.5, lambda = cv_enet$lambda.min)
pred_enet <- as.numeric(predict(enet, newx = x_test))

pred_best <- as.numeric(predict(best_ols, newdata = test))

metrics_ml <- data.frame(
  Model = c("OLS_best","Ridge","Lasso","ElasticNet"),
  RMSE  = c(Metrics::rmse(y_test, pred_best),
            Metrics::rmse(y_test, pred_ridge),
            Metrics::rmse(y_test, pred_lasso),
            Metrics::rmse(y_test, pred_enet)),
  MAE   = c(Metrics::mae(y_test, pred_best),
            Metrics::mae(y_test, pred_ridge),
            Metrics::mae(y_test, pred_lasso),
            Metrics::mae(y_test, pred_enet)),
  MAPE  = c(mape(y_test, pred_best),
            mape(y_test, pred_ridge),
            mape(y_test, pred_lasso),
            mape(y_test, pred_enet))
)

cat("\n================ PERF OLS vs ML (TEST) ================\n")
print(metrics_ml)

cat("\n--- Coeficienti LASSO (selectie variabile) ---\n")
print(coef(lasso))

# 13) REZUMAT FINAL----------------
cat("\n\n================ REZUMAT FINAL================\n")
cat("Observatii totale:", nrow(data), "\n")
cat("Train/Test (80/20): train =", nrow(train), "| test =", nrow(test), "\n\n")

cat("Performanta OLS pe TEST:\n"); print(metrics_ols); cat("\n")
cat("Model OLS ales dupa RMSE:", best_name, "\n\n")

cat("Comparatie OLS_best vs Regularizare (TEST):\n"); print(metrics_ml); cat("\n")

if(!is.na(pred_base) && !is.na(pred_scen)){
  cat("Scenariu (", country_code, "): baseline=", round(pred_base,3),
      " scenariu=", round(pred_scen,3),
      " delta=", round(pred_scen - pred_base,3), "\n", sep="")
}

cat("\nGraficele sunt salvate in folderul: plots/ (PNG).\n")
cat("====================================================================\n")
