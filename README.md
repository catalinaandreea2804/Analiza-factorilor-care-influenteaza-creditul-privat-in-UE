# Analiza factorilor care influenteaza creditul privat in UE

Acest proiect de econometrie analizeaza factorii macroeconomici si financiari care determina nivelul creditului intern acordat sectorului privat in tarile din Uniunea Europeana. Analiza a fost realizata in R si cuprinde atat modele cross-sectional pentru anul 2023 (pe 28 de state), cat si analiza pe date de tip panel.

## Variabile analizate
- **Variabila dependenta:** Creditul intern acordat sectorului privat (ca % din PIB).
- **Variabile independente:**
  - PIB pe cap de locuitor (GDP_Capita).
  - Rata dobanzii la credite (Lending Rate).
  - Inflatia (Inflation).
  - Rata somajului (Unemployment).
  - Alfabetizarea financiara (FinLit_Score).
  - Modificarea indicelui preturilor locuintelor (HPI_Change).

## Metode si modele econometrice utilizate
- Analiza de tip cross-section utilizand regresii OLS (de la modele simple la specificatii multifactoriale complexe cu variabile Dummy si termeni de interactiune).
- Evaluarea performantei predictive utilizand algoritmi de tip Machine Learning (Ridge, Lasso, Elastic Net).
- Analiza pe date de tip Panel (Pooled OLS, Fixed Effects, Random Effects).
- Testarea validitatii si a autocorelarii erorilor (ex: Breusch-Pagan, Breusch-Godfrey, Durbin-Watson) si utilizarea de erori standard robuste (Arellano).

## Concluzii principale
- Inflatia actioneaza ca un inhibitor major al creditarii, existand o corelatie negativa puternica intre instabilitatea preturilor si volumul creditelor private.
- Dinamica preturilor locuintelor (HPI_Change) are un impact semnificativ asupra nivelului creditului privat.
- Din punct de vedere predictiv, modelul OLS extins a fost cel mai robust (oferind cea mai mica eroare RMSE), validand faptul ca relatia este explicata mai bine de interactiunile economice decat de selectia automata a trasaturilor (Lasso/Ridge).

## Cum se ruleaza
Scriptul este scris in R. Pentru a reproduce rezultatele, asigura-te ca ai pachetele necesare instalate si ruleaza fisierul principal .R sau .Rmd. Setul de date complet si scriptul se afla in acest repository.
