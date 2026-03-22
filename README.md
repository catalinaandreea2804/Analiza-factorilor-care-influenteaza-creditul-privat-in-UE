# Analiza factorilor care influenteaza creditul privat in UE

[cite_start]Acest proiect de econometrie analizeaza factorii macroeconomici si financiari care determina nivelul creditului intern acordat sectorului privat in tarile din Uniunea Europeana[cite: 1, 4, 5]. [cite_start]Analiza a fost realizata in R si cuprinde atat modele cross-sectional pentru anul 2023 (pe 28 de state), cat si analiza pe date de tip panel[cite: 21, 79].

## Variabile analizate
- [cite_start]**Variabila dependenta:** Creditul intern acordat sectorului privat (ca % din PIB)[cite: 24].
- **Variabile independente:**
  - [cite_start]PIB pe cap de locuitor (GDP_Capita)[cite: 26].
  - [cite_start]Rata dobanzii la credite (Lending Rate)[cite: 28].
  - [cite_start]Inflatia (Inflation)[cite: 29].
  - [cite_start]Rata somajului (Unemployment)[cite: 31].
  - [cite_start]Alfabetizarea financiara (FinLit_Score)[cite: 33].
  - [cite_start]Modificarea indicelui preturilor locuintelor (HPI_Change)[cite: 69].

## Metode si modele econometrice utilizate
- [cite_start]Analiza de tip cross-section utilizand regresii OLS (de la modele simple la specificatii multifactoriale complexe cu variabile Dummy si termeni de interactiune)[cite: 21, 22].
- [cite_start]Evaluarea performantei predictive utilizand algoritmi de tip Machine Learning (Ridge, Lasso, Elastic Net)[cite: 23].
- [cite_start]Analiza pe date de tip Panel (Pooled OLS, Fixed Effects, Random Effects)[cite: 84, 90, 95].
- [cite_start]Testarea validitatii si a autocorelarii erorilor (ex: Breusch-Pagan, Breusch-Godfrey, Durbin-Watson) si utilizarea de erori standard robuste (Arellano)[cite: 63, 96].

## Concluzii principale
- [cite_start]Inflatia actioneaza ca un inhibitor major al creditarii, existand o corelatie negativa puternica intre instabilitatea preturilor si volumul creditelor private[cite: 35].
- [cite_start]Dinamica preturilor locuintelor (HPI_Change) are un impact semnificativ asupra nivelului creditului privat[cite: 77].
- [cite_start]Din punct de vedere predictiv, modelul OLS extins a fost cel mai robust (oferind cea mai mica eroare RMSE), validand faptul ca relatia este explicata mai bine de interactiunile economice decat de selectia automata a trasaturilor (Lasso/Ridge)[cite: 36, 76].

## Cum se ruleaza
Scriptul este scris in R. Pentru a reproduce rezultatele, asigura-te ca ai pachetele necesare instalate si ruleaza fisierul principal `.R` sau `.Rmd`. Setul de date complet si scriptul se afla in acest repository.
