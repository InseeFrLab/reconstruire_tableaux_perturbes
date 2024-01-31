library(ptable)
library(dplyr)
library(tidyr)
library(purrr)
library(lpSolve)

source("R/fonctions_arrondis.R", encoding = "UTF-8")

#' Construire tableau perturbé par une ckm et les matrices inf et sup correspondantes
#'
#' @param tableau 
#' @param ptab résultat de ptable::create_cnt_ptable(D = D, V = V)
#' @param D 
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1234)
#' tableau <- construire_tableau(c(15,38,27,120), 2, 2)
#' tableau_pert <- construire_tableau_ckm(tableau, 10, 6.25)
#' 
#' arguments_lp <- do.call("preparer_lp", args = tableau_pert[2:3])
#' prog_sol <- do.call("lp", args = arguments_lp)
#' prog_sol$solution - c(t(tableau))
construire_tableau_ckm <- function(tableau, ptab, D = 10){
  
  tableau_lg <- tableau %>% 
    as.data.frame() %>%
    mutate(X = LETTERS[1:nrow(tableau)]) %>% 
    pivot_longer(-X, names_to = "Y", values_to =  "orig") %>% 
    mutate(val2 = ifelse(orig > D, D, orig)) 
  
  
  tableau_pert <- tableau_lg %>% 
    mutate(
      deviation = sapply(
        val2,
        \(x){
          sample(x - as.numeric(colnames(ptab@tMatrix)), size = 1, prob = ptab@tMatrix[x + 1,])
        }
      )) %>% 
    mutate(pert = orig + deviation) %>% # %T>% print() %>% 
    select(X,Y,pert) %>% 
    pivot_wider(names_from = Y, values_from = pert) %>% 
    select(-X) %>% 
    as.matrix()
  
  lp <- D
  up <- D
  
  list(
    tableau_pert = tableau_pert,
    tableau_min = apply(tableau_pert, 2, FUN = function(a) ifelse(a - lp < 0, 0, a - lp)),
    tableau_max = apply(tableau_pert, 2, FUN = function(a) ifelse(a + up < 0, 0, a + up))
  )
}


#' Title
#'
#' @param nc 
#' @param nr 
#' @param D 
#' @param V 
#'
#' @return
#' @export
#'
#' @examples
#' ptab <- create_cnt_ptable(D = 10, V = 6.25)
#' solve_lp_tableau_ckm_simule(3,3, ptab, 10)
solve_lp_tableau_ckm_simule <- function(nc, nr, ptab, D = 10){
  
  require(lpSolve)
  
  tableau_original = construire_tableau(sample.int(1000, nc*nr), nc, nr)
  tableau_pert = construire_tableau_ckm(tableau_original, ptab, D)
  arguments_lp <- do.call("preparer_lp", args = tableau_pert[2:3])
  
  prog_sol <- do.call("lp", args = arguments_lp)
  ecarts <- if(all(prog_sol$solution == 0)) NULL else prog_sol$solution- c(t(tableau_original))
  
  return(
    list(orig = tableau_original,
         prog_sol = prog_sol,
         sol = matrix(prog_sol$solution, ncol = nc + 1, byrow = FALSE),
         ecarts = ecarts)
  )
}


#' Title
#'
#' @param nrep 
#' @param nc 
#' @param nr 
#' @param D 
#' @param V 
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1234)
#' ecarts_1tab_2x2 <- simulations_ecarts_ckm_original(1000, 2, 2, 10, 6.25)
#' base::plot(1:1000, cumsum(purrr::map_dbl(ecarts_1tab_2x2, mean))/(1:1000), type = "l")
simulations_ecarts_ckm_original <- function(
    nrep = 1000, nc, nr, D = 10, V = 6.25
){
  ptab <- create_cnt_ptable(D = D, V = V)
  ecarts <- compact(
    replicate(
      n = nrep,
      expr = {
        res <- solve_lp_tableau_ckm_simule(nc, nr, ptab, D)
        res$ecarts
      },
      simplify = FALSE
    ))
  
  return(ecarts)
}



