source("R/fonctions_arrondis.R", encoding = "UTF-8")


# Simulation sur un seul tableau à 2 dimensions 
#dont les 2 variables croisées sont ventilées en 2 modalités distinctes (+ total):

set.seed(1234)
ecarts_1tab_2x2 <- simulations_ecarts_arrondi_original(1000, 2, 2, 10)

plot(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, mean))/(1:1000), type = "l", ylim = c(-9,5))
lines(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, max))/(1:1000), type = "l", col = "purple")
lines(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, min))/(1:1000), type = "l", col = "purple")
lines(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, quantile, 0.05))/(1:1000), type = "l", col = "forestgreen")
lines(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, quantile, 0.95))/(1:1000), type = "l", col = "forestgreen")
lines(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, quantile, 0.25))/(1:1000), type = "l", col = "steelblue")
lines(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, quantile, 0.75))/(1:1000), type = "l", col = "steelblue")


summary(purrr::map_int(ecarts_1tab_2x2, max))
summary(purrr::map_int(ecarts_1tab_2x2, min))

ecarts_cell_par_cell_1tab_2x2 <- unlist(purrr::map(ecarts_1tab_2x2, c))
hist(ecarts_cell_par_cell_1tab_2x2)
boxplot(ecarts_cell_par_cell_1tab_2x2, horizontal = TRUE)
sum(ecarts_cell_par_cell_1tab_2x2 > -2 & ecarts_cell_par_cell_1tab_2x2 < 2)/length(ecarts_cell_par_cell_1tab_2x2)

#pour tableaux 2*2 => 40% de cellules à + ou -1
#pour tableaux 3*3 => 30% de cellules à + ou -1


# Simulation sur un seul tableau à 2 dimensions 
#dont les 2 variables croisées sont ventilées en 3 modalités distinctes (+ total):

set.seed(1234)

ecarts_1tab_3x3 <- simulations_ecarts_arrondi_original(1000, 3, 3, 10)

summary(purrr::map_int(ecarts_1tab_3x3, max))
summary(purrr::map_int(ecarts_1tab_3x3, min))

ecarts_cell_par_cell <- unlist(purrr::map(ecarts_1tab_3x3, c))
hist(ecarts_cell_par_cell)

sum(ecarts_cell_par_cell > -2 & ecarts_cell_par_cell < 2)/length(ecarts_cell_par_cell)


# Simulation sur deux tableaux liés à 2 dimensions 
# dont les 2 variables croisées du tableau 1 sont ventilées en 3 modalités distinctes (+ total):
# et dont la variable colonne du tableau 2 est ventilée en 2 modalités distinctes (+ total):

set.seed(1234)

ecarts_2tab_lies_2x2 <- simulations_ecarts_arrondi_original(1000, 2, 2, 10, lies = TRUE, ventil_mod = c(0.2,0.8))

summary(purrr::map_int(ecarts_2tab_lies_2x2, max))
summary(purrr::map_int(ecarts_2tab_lies_2x2, min))

ecarts_cell_par_cell <- unlist(purrr::map(ecarts_2tab_lies_2x2, c))
hist(ecarts_cell_par_cell)

sum(ecarts_cell_par_cell > -2 & ecarts_cell_par_cell < 2)/length(ecarts_cell_par_cell)


# Simulation sur deux tableaux liés à 2 dimensions 
# dont les 2 variables croisées du tableau 1 sont ventilées en 3 modalités distinctes (+ total):
# et dont la variable colonne du tableau 2 est ventilée en 2 modalités distinctes (+ total):

set.seed(1234)

ecarts_2tab_lies_3x3 <- simulations_ecarts_arrondi_original(1000, 3, 3, 10, lies = TRUE, ventil_mod = c(0.2,0.8))

summary(purrr::map_int(ecarts_2tab_lies_3x3, max))
summary(purrr::map_int(ecarts_2tab_lies_3x3, min))

ecarts_cell_par_cell <- unlist(purrr::map(ecarts_2tab_lies_3x3, c))
hist(ecarts_cell_par_cell)

sum(ecarts_cell_par_cell > -2 & ecarts_cell_par_cell < 2)/length(ecarts_cell_par_cell)

