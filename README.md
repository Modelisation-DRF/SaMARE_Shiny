# Système de déploiement SaMARE

**Version:** 1.0  
**Date:** Décembre 2025

---

## Vue d'ensemble

Ce système permet de lancer l'application Shiny SaMARE en double-cliquant sur un fichier, sans avoir besoin d'ouvrir RStudio.

---

## Structure des fichiers

```
SaMARE_Application/
│
├── Lancer_SaMARE.vbs          ← Double-cliquer pour lancer
├── _launcher.bat              ← Script batch (appelé par VBS)
├── runShinyApp.R              ← Script de démarrage R
│
├── app/                       ← Dossier de l'application Shiny
│   ├── app.R                  ← Application principale 
│   ├── app_diagnostic.R       ← Application de diagnostic/installation
│   └── www/                   ← Ressources web 
│       ├── SaMARE_MRNF_Final.jpg
│       ├── Aide2.webp
│       ├── Aide application R du simulateur SaMARE.pdf
│       ├── Test2500m2.csv
│       └── GaulesTest2500m2.csv
│
└── README.md                  ← Ce fichier
```

---

## Installation

### Étape 1 : Préparer la structure

1. Créer un dossier `SaMARE_Application` (ou autre nom)
2. Y placer tous les fichiers fournis selon la structure ci-dessus

### Étape 2 : Packages requis

Les packages suivants doivent être installés. S'ils sont manquants, l'application de diagnostic s'ouvrira automatiquement pour les installer.

**Packages CRAN :**
```r
install.packages(c(
  "shiny", "shinyjs", "shinyFiles", "shinydashboard", "shinythemes",
  "shinyWidgets", "dplyr", "tidyverse", "readr", "readxl", "sas7bdat",
  "rlang", "mvtnorm", "miceadds", "rockchalk", "doFuture", "doRNG",
  "future.apply", "sf", "reticulate"
))
```

**Packages GitHub :**
```r
install.packages("remotes")
remotes::install_github("Modelisation-DRF/SimulateurSaMARE")
remotes::install_github("Modelisation-DRF/BillonnagePetro")
remotes::install_github("Modelisation-DRF/ExtractMap")
remotes::install_github("Modelisation-DRF/OutilsDRF")
```

**Note :** Pour les packages GitHub sous Windows, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) est requis.

---

## Utilisation

### Lancement normal

1. **Double-cliquer** sur `Lancer_SaMARE.vbs`
2. Une fenêtre console s'ouvre brièvement (minimisée)
3. Le navigateur s'ouvre avec l'application SaMARE
4. Utiliser l'application normalement
5. Cliquer sur "Fermer l'application" pour quitter proprement

### Si des packages sont manquants

1. L'application de diagnostic s'ouvre automatiquement
2. Cliquer sur "Installer automatiquement"
3. Attendre la fin de l'installation
4. Fermer l'application de diagnostic
5. Relancer `Lancer_SaMARE.vbs`

---

## Modifications apportées à app.R

| Modification | Raison |
|--------------|--------|
| Suppression de l'installation automatique | Géré par `app_diagnostic.R` maintenant |
| Suppression de `stopSimulation()` avec `taskkill` | **DANGER** : tuait TOUS les processus R |
| Ajout détection contexte (RStudio vs VBS) | Fermeture adaptée au contexte |
| Ajout protection JavaScript | Avertit si fermeture pendant simulation |
| Modification de la fermeture | Fermeture propre sans `taskkill` |
| Chemins `WWW` → `www` | Convention standard Shiny |

