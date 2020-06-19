# Détection en temps réel des points de retournement

L’analyse du cycle économique, et en particulier la détection rapide des points de retournement d'une série, est un sujet de première importance dans l'analyse de la conjoncture économique. Les moyennes mobiles ou les filtres linéaires sont omniprésents dans les méthodes d'extraction du cycle économique et d'ajustement saisonnier. Ainsi la méthode de désaisonnalisation X12-Arima utilise des moyennes mobiles de Henderson et des moyennes mobiles composites pour estimer les principales composantes d'une série chronologique, alors que Tramo-Seats utilise des filtres de Wiener-Kolmogorov. Des filtres symétriques sont appliqués au centre de la série, mais lorsqu'il s'agit d'estimer les points les plus récents, toutes ces méthodes doivent s'appuyer sur des filtres asymétriques. X12-Arima ou Tramo-Seats appliquent ainsi des moyennes symétriques sur les prévisions obtenues à partir d'une modélisation ARIMA de la série. Comme les valeurs prévues sont des combinaisons linéaires de valeurs passées, ces méthodes utilisent bien des moyennes mobiles asymétriques à la fin de la série.

Si ces moyennes mobiles asymétriques ont de bonnes propriétés concernant la taille des révisions futures induites par le processus de lissage, voir par exemple Pierce (1980), elles induisent également des déphasages qui ont généralement un impact sur l'estimation en temps réel des points de retournement.

Le calcul et les propriétés des moyennes mobiles symétriques et asymétriques ont été étudiés, en particulier dans Macaulay (1931), Musgrave (1964), Dagum (1982), Laniel (1985), Grun-Rehomme et Ladiray (1994), Gray et Thomson (1996) et Gray et Thomson (2002). Des études importantes ont été réalisées sur l'estimation de la tendance-cycle au cours des 20 dernières années dans plusieurs directions :

- En modifiant les filtres de Henderson, pour améliorer la détection précoce des points de retournement, Dagum (1996) introduit un estimateur non-linéaire de la tendance-cycle (NLDF). Dagum, Luati (2009) ont aussi proposé un filtre linéaire en cascade (CLF) proche du NLDF ;

- Considérant le problème général de l'estimation de la tendance-cycle en temps réel au moyen de polynômes locaux Proietti et Luati (2008) proposent une famille générale de filtres asymétriques qui minimisent la moyenne quadratique des révisions sous contraintes de conservation locale de polynômes ;

- D'autres stratégies ont été proposées pour estimer les points récents de la tendance : Kyung-Joon et Schucany (1998) utilisent une régression non paramétrique par noyau, Vasyechko et Grun-Rehomme (2014) proposent des filtres basés sur le noyau d'Epanechnikov et les critères de minimisation de Henderson, etc.

L’objectif du stage est de faire une comparaison théorique de ces filtres et d’évaluer leurs performances sur données réelles.

# Avancée du stage

- 16/06 : lecture de Proietti, Luati, Real Time Estimation in Local Polynomial Regression, with Application to Trend-Cycle Analysis (2008), https://aqlt.github.io/AsymmetricFilters/06:15%20-%20Proietti%20%26%20Luati/Proietti-Luati.pdf
