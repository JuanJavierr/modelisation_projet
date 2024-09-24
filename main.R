library(tidyverse)
library(gridExtra)

# On charge les données
data_filename <- "MATH60604-projet-bixi_partie1_equipe1.csv"
raw_data <- read.csv(data_filename)

# On convertit les colonnes aux types appropriés
data <- raw_data %>%
    mutate(
        date = ymd_hms(dep),
        jour = as.factor(jour),
        mem = as.logical(mem),
        pointe = as.factor(pointe)
    ) %>%
    select(-c(dep))

# --- Analyse Exploratoire ---
summary(data)

# Combien de déplacements par jour de la semaine?
raw_data %>%
    group_by(jour) %>%
    summarise(n = n())

# Combien de déplacements par heure de la journée?
raw_data %>%
    group_by(heure) %>%
    summarise(n = n())

# Combien de déplacements par jour?
raw_data %>%
    group_by(date) %>%
    summarise(n = n())


# --- QUESTION 1.1 : En moyenne, les membres de BIXI effectuent-ils des trajets plus courts que les non-membres?  ---

# On crée un graphique pour visualiser la distribution des distances
ggplot(data, aes(x = dur, fill = mem)) +
    geom_histogram(bins = 30) +
    labs(
        title = "Distribution des distances des déplacements",
        x = "Durée (s)", y = "Nombre de déplacements"
    ) +
    theme_minimal()


# --- QUESTION 1.2 : Les résultats sont-ils les mêmes si l’on tient compte de l’utilisation en fin de semaine ou en semaine? ---
# On crée un graphique pour visualiser la distribution des distances
week_ends <- data %>%
    filter(jour %in% c("samedi", "dimanche"))

week_days <- data %>%
    filter(jour %in% c("lundi", "mardi", "mercredi", "jeudi", "vendredi"))


plot1 <- ggplot(week_ends, aes(x = dur, fill = mem)) +
    geom_histogram(bins = 30) +
    labs(
        title = "Distribution des distances des déplacements en fin de semaine",
        x = "Durée (s)", y = "Nombre de déplacements"
    ) +
    theme_minimal()

plot2 <- ggplot(week_days, aes(x = dur, fill = mem)) +
    geom_histogram(bins = 30) +
    labs(
        title = "Distribution des distances des déplacements en semaine",
        x = "Durée (s)", y = "Nombre de déplacements"
    ) +
    theme_minimal()

grid.arrange(plot1, plot2, ncol = 1)



# --- QUESTION 2.1 : Est-ce que la durée des trajets est influencée par la météo? ---
# On crée un nuage de points pour visualiser la relation entre les précipitations et la durée des déplacements
ggplot(data, aes(x = temp, y = dur)) +
    geom_point() +
    labs(
        title = "Relation entre les précipitations et la durée des déplacements",
        x = "Précipitations (mm)", y = "Durée (s)"
    ) +
    theme_minimal()


# --- QUESTION 2.2 : Au vu du résultat que vous obtenez,est-ce que vos modèles initiaux devraient être revisités? ---


# --- QUESTION 3.1 : Les durées de trajets sont-elles différentes selon que l’on se trouve aux heures de pointe ou non en semaine? ---


# --- QUESTION 3.2 : Existe-t-il des différences entre l’utilisation pour les heures de pointes en semaine le matin ou le soir? ---
