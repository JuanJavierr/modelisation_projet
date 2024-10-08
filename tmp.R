# Données de l'étude supp. 5
# de Sharma, Tully, et Cryder (2021)
data(STC21_SS5, package = "hecedsm")
# La fonction 'aov' sert à ajuste des ANOVA
# Équivalent à "lm" avec variables catégorielles, contrasts somme nulle
mod1 <- aov(likelihood ~ purchase * debttype,
    data = STC21_SS5
)
# Calculer le décompte par sous-catégorie (données débalancées)
xtabs(~ purchase + debttype, data = STC21_SS5)
##                debttype
## purchase        credit loan
##   discretionary    392  359
##   need             361  389
# Calcul de la moyenne globale/lignes/colonnes/cellules
moy_groupes <- model.tables(mod1, type = "means")


emmeans::emmeans(mod1,
    # 'specs': quelle(s) variable(s) conserver
    specs = "debttype",
    contr = "pairwise"
)
