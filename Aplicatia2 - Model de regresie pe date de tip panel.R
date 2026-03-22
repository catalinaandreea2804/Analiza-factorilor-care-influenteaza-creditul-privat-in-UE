library(plm)
library(lmtest)
library(sandwich)
library(dplyr)

# Dataset existent
df <- SetDate2

# Structura panel
pdata <- pdata.frame(df, index = c("Country","Year"))

# Transformare
pdata$lnGDPpc <- log(pdata$GDP_Capita)

# Specificatie model
form <- DomCredit_GDP ~ lnGDPpc + LendingRate + Inflation +
  Unemployment + FinLit_Score + HPI_Change

# Modele
pool <- plm(form, data = pdata, model = "pooling")
fe   <- plm(form, data = pdata, model = "within", effect = "individual")
re   <- plm(form, data = pdata, model = "random", effect = "individual")

# Rezultate modele
summary(pool)
summary(fe)
summary(re)

# Test Breusch-Pagan pentru efecte panel (Pooled vs Panel)
plmtest(pool, type = "bp")

# Test Hausman (FE vs RE)
phtest(fe, re)

# Test F pentru efecte fixe (Pooled vs FE)
pFtest(fe, pool)

# Test Breusch-Pagan pentru heteroscedasticitate
bptest(pool)

# Test Breusch-Pagan LM pentru efecte aleatorii
plmtest(pool, type = "bp")



library(dplyr)
library(ggplot2)

means_country <- pdata %>%
  group_by(Country) %>%
  summarise(mean_credit = mean(DomCredit_GDP))

ggplot(means_country, aes(x = Country, y = mean_credit)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Heterogenitate intre tari: media creditului privat (% PIB)",
       x = "Tara",
       y = "DomCredit_GDP mediu")


means_year <- pdata %>%
  group_by(Year) %>%
  summarise(mean_credit = mean(DomCredit_GDP))

ggplot(means_year, aes(x = Year, y = mean_credit)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  labs(title = "Evolutia medie a creditului privat in timp",
       x = "An",
       y = "DomCredit_GDP mediu")


num <- pdata %>% 
  select(DomCredit_GDP, GDP_Capita, LendingRate, Inflation,
         Unemployment, FinLit_Score, HPI_Change)

round(cor(num, use = "pairwise.complete.obs"), 2)


# ---------------------------------------------------------
# TESTAREA AUTOCORELARII
# ---------------------------------------------------------

# Testul Breusch-Godfrey/Wooldridge pentru autocorelare seriala
# H0: Nu exista autocorelare
# H1: Exista autocorelare
pbgtest(fe)

# Testul Durbin-Watson pentru panel 
pdwtest(fe)

# Pentru modelul Random Effects (RE)
coeftest(re, vcov = vcovHC(re, method = "arellano"))

# Pentru modelul Fixed Effects (FE)
coeftest(fe, vcov = vcovHC(fe, method = "arellano"))


