#############################################################################
## Résoudre un tableau arrondi à la dizaine
#############################################################################

## Objectif: Retrouver le tableau original à partir d'un tableau arrondi à la 
## dizaine

#
#' Construit une matrice de taille `n_mod_row + 1`, `n_mod_col + 1` dont les cellules 
#' internes (cad les cellules `1:n_mod_row`, `1:n_mod_col`) prennent les valeurs 
#' de `cellules_internes` et dont les cellules de la dernière colonne et de la 
#' dernière ligne sont les marges des cellules internes. 
#'
#' @param cellules_internes vecteur des cellules internes du tableau (lecture ligne par ligne) 
#' @param n_mod_col nb de modalités de la variable en colonnes (hors tot)
#' @param n_mod_row nb de modalités de la variable en lignes (hors tot)
#'
#' @return matrix de dimension n_mod_row + 1, n_mod_col + 1
#' @export
#'
#' @examples
#' construire_tableau(c(15,38,27,120), 2, 2)
construire_tableau <- function(cellules_internes, n_mod_col, n_mod_row){
  
  nc = n_mod_col + 1
  nr = n_mod_row + 1
  tableau <- matrix(0, ncol = nc, nrow = nr)
  tableau[1:n_mod_row, 1:n_mod_col] <- matrix(cellules_internes, ncol = n_mod_col, byrow=TRUE)
  marge_lignes <- colSums(tableau)[-nr]
  marge_colonnes <- rowSums(tableau)[-nc]
  marge <- sum(tableau)
  tableau[nr, 1:n_mod_col] <- marge_lignes
  tableau[1:n_mod_row, nc] <- marge_colonnes
  tableau[nr, nc] <- marge
  return(tableau)
}

#' Construction d'un tableau lié au tableau renseigné par les marges lignes 
#' (=> dernière colonne identique). Le tableau lié aura donc le même nb de lignes
#' que le tableau de départ. On lui donne 3 colonnes (~ventilation sur 2 modalités)
#' Ventilation de type 0.2 / 0.8 et 0.4 / 0.6 par défaut
#'
#' @param tableau 
#' @param ventil_mod min - max pour ventilations de la marge ligne sur la 1ere colonne
#'
#' @return matrix de dimensions : nb de lignes de tableau et 3 colonnes
#' @export
#'
#' @examples
#' construire_tableau_lie(tableau = construire_tableau(c(15,38,27,120), 2, 2))
construire_tableau_lie <- function(tableau, ventil_mod = c(0.2,0.4)){
  
  nc = ncol(tableau)
  nr = nrow(tableau)
  
  nc2 = 3 # nb col du tableau lié
  tableau2 <- matrix(0, ncol = nc2, nrow = nr)
  tableau2[,nc2] <- tableau[,nc]
  tableau2[1:(nr-1), 1] <- round(tableau2[1:(nr-1),nc2] * runif(nr-1, min = ventil_mod[1], max = ventil_mod[2]))
  tableau2[1:(nr-1), 2] <- tableau2[1:(nr-1),nc2] - tableau2[1:(nr-1), 1]
  
  tableau2[nr,] <- colSums(tableau2[1:(nr-1),])
  
  return(tableau2)
}


#' Arrondi qui permet d'attribur la valeur modale à l'arrondi supérieur systématiquement
#'
#' @param x 
#' @param arrondi 
#'
#' @return vecteur de même taille que `x`
#' @export
#'
#' @examples
arrondir <- function(x, arrondi){
  
  li <- 0:(ceiling(arrondi/2)-1)
  ui <- 0:(arrondi-1)
  ui <- ui[ui > li[length(li)]]
  
  ifelse(x %% arrondi %in% li, floor(x/arrondi)*arrondi, ceiling(x/arrondi)*arrondi)
  
}


#' Arrondi le tableau et fourni bornes inf et sup de l'intervalle des possibles 
#' pour chaque valeur
#'
#' @param tableau 
#' @param arrondi 
#'
#' @return liste de 3 matrices de mêmes tailles: tableau_arrondi, tableau_min (bornes inf) 
#' et tableau_max (bornes sup)
#' 
#' @export
#'
#' @examples
arrondir_tableau <- function(tableau, arrondi){
  
  tableau_arrondi = apply(tableau, 2, arrondir, arrondi = arrondi)
  lp <- round(arrondi/2)
  up <- round(arrondi/2) - 1
  
  return(
    list(
      tableau_arrondi = tableau_arrondi,
      tableau_min = apply(tableau_arrondi, 2, FUN = function(a) ifelse(a - lp < 0, 0, a - lp)),
      tableau_max = apply(tableau_arrondi, 2, FUN = function(a) ifelse(a + up < 0, 0, a + up))
    )
  )
}

#' Prépare les arguments pour résoudre un lp à partir des matrices 
#' de bornes inf et sup
#'
#' @param tableau_min 
#' @param tableau_max 
#'
#' @return liste d'arguments pour la fonction lpSolve::lp()
#' @export
#'
#' @examples
preparer_lp <- function(tableau_min, tableau_max){
  
  nc <- ncol(tableau_min); nr <- nrow(tableau_min)
  n_inconnues <- nc * nr
  
  obj_fun_mat <- matrix(1, ncol = nc, nrow = nr)
  obj_fun_mat[1:(nr-1), nc] <- 0
  obj_fun_mat[nr, 1:(nc-1)] <- 0
  obj_fun_mat[nr, nc] <- -1
  
  obj_fun <- c(t(obj_fun_mat))
  
  constr_egalites_mat <- matrix(0, ncol = n_inconnues, nrow = nr + nc)
  for(i in 1:nr){
    constr_egalites_mat[i, ((i-1)*nc + 1):((i-1)*nc + nc - 1)] <- 1
    constr_egalites_mat[i, (i-1)*nc + nc] <- -1
  }
  for(i in (nr+1):(nr+nc)){
    j_mod <- seq(i %% nc,n_inconnues, nc)
    constr_egalites_mat[i, j_mod] <- 1
    constr_egalites_mat[i, j_mod[length(j_mod)]] <- -1
  }
  
  constr <- rbind(
    diag(1, n_inconnues),
    diag(1, n_inconnues),
    constr_egalites_mat
  )
  
  constr_dir <- c(rep("<=", n_inconnues), rep(">=", n_inconnues), rep("=", nc + nr))
  rhs <- c(c(t(tableau_max)) , c(t(tableau_min)), rep(0, nc + nr))
  
  return(
    list(
      direction = "min",
      objective.in = obj_fun,
      const.mat = constr,
      const.dir = constr_dir,
      const.rhs = rhs,
      all.int = TRUE
    )
  )
}

#' Prépare les arguments pour lpSolve::lp() dans le cas de 2 tableaux liés
#'
#' @param tableau_min1 
#' @param tableau_max1 
#' @param tableau_min2 
#' @param tableau_max2 
#'
#' @return
#' @export
#'
#' @examples
#' tableau1 = construire_tableau( cellules_internes = c(8,451,578,487,234,896,266,428,795), 3, 3)
#' tableau2 = construire_tableau_lie(tableau1, ventil_mod = c(0.2,0.8))
#' tableau_arrondi1 = arrondir_tableau(tableau1, 10)
#' tableau_arrondi2 = arrondir_tableau(tableau2, 10)
#' args_prep <- c(tableau_arrondi1[2:3],  tableau_arrondi2[2:3])
#' names(args_prep) <- paste0(names(args_prep), c(1,1,2,2))
#' prepa_tableaux_lies <- do.call("preparer_lp_tableaux_lies", args = args_prep)
preparer_lp_tableaux_lies <- function(
    tableau_min1, tableau_max1,
    tableau_min2, tableau_max2
){
  
  nc1 <- ncol(tableau_min1); nr1 <- nrow(tableau_min1)
  nc2 <- ncol(tableau_min2); nr2 <- nrow(tableau_min2)
  
  arguments_lp1 <- do.call(
    "preparer_lp",
    args = list(
      tableau_min = tableau_min1,
      tableau_max = tableau_max1
    )
  )
  
  arguments_lp2 <- do.call(
    "preparer_lp",
    args = list(
      tableau_min = tableau_min2,
      tableau_max = tableau_max2
    )
  )
  
  obj_fun_mat <- c(arguments_lp1$objective.in, arguments_lp2$objective.in)
  obj_fun <- c(t(obj_fun_mat))
  
  constr_egalites_liens_mat <- matrix(
    0,
    ncol = ncol(arguments_lp1$const.mat) + ncol(arguments_lp2$const.mat),
    nrow = nr1
  )
  for(i in 1:nr1){
    constr_egalites_liens_mat[i, i*nc1] <- 1
    constr_egalites_liens_mat[i, nr1*nc1 + i*nc2] <- -1
  }
  constr <- rbind(
    cbind(
      arguments_lp1$const.mat, 
      matrix(0, nrow = nrow(arguments_lp1$const.mat), ncol = ncol(arguments_lp2$const.mat))
    ),
    cbind(
      matrix(0, nrow = nrow(arguments_lp2$const.mat), ncol = ncol(arguments_lp1$const.mat)),
      arguments_lp2$const.mat
    )
  ) |>
    rbind(constr_egalites_liens_mat)
  
  constr_dir_liens <- rep("=", nr1)
  constr_dir <- c(arguments_lp1$const.dir, arguments_lp2$const.dir, constr_dir_liens)
  
  rhs_liens <- rep(0, nr1)
  rhs <- c(arguments_lp1$const.rhs, arguments_lp2$const.rhs, rhs_liens)
  
  return(
    list(
      direction = "min",
      objective.in = obj_fun,
      const.mat = constr,
      const.dir = constr_dir,
      const.rhs = rhs,
      all.int = TRUE
    )
  )
}

#' Résout le lp pour un tableau simple généré aléatoirement
#'
#' @param nc correspond au n_mod_col de construire_tableau
#' @param nr correspond au n_mod_row de construire_tableau
#' @param arrondi 
#'
#' @return liste avec table de départ, table solution, écarts, résultat de lp()
#' @export
#'
#' @examples
#' solve_lp_tableau_simule(3,3,10)
solve_lp_tableau_simule <- function(nc, nr, arrondi = 10){
  
  require(lpSolve)
  
  tableau_original = construire_tableau(sample.int(1000, nc*nr), nc, nr)
  tableau_arrondi = arrondir_tableau(tableau_original, arrondi)
  arguments_lp <- do.call("preparer_lp", args = tableau_arrondi[2:3])
  
  prog_sol <- do.call("lp", args = arguments_lp)
  ecarts <- if(all(prog_sol$solution == 0)) NULL else matrix(prog_sol$solution, ncol = nc + 1, byrow = TRUE) - tableau_original
  
  return(
    list(orig = tableau_original,
         prog_sol = prog_sol,
         sol = matrix(prog_sol$solution, ncol = nc + 1, byrow = FALSE),
         ecarts = ecarts)
  )
}



#' Résout le lp pour deux tableaux liés et générés aléatoirement
#'
#' @param nc correspond au n_mod_col de construire_tableau
#' @param nr correspond au n_mod_row de construire_tableau
#' @param arrondi 
#'
#' @return liste avec tables de départs, tables solutions, écarts, résultat de lp()
#' @export
#'
#' @examples
#' solve_lp_tableaux_lies_simules(3,3,10)
solve_lp_tableaux_lies_simules <- function(nc, nr, arrondi = 10, ventil_mod = c(0.2,0.8)){
  
  require(lpSolve)
  
  tableau1 = construire_tableau(sample.int(1000, nc*nr), nc, nr)
  dim1 <- dim(tableau1)
  tableau2 = construire_tableau_lie(tableau1, ventil_mod = ventil_mod)
  dim2 <- dim(tableau2)
  tableau_arrondi1 = arrondir_tableau(tableau1, arrondi)
  tableau_arrondi2 = arrondir_tableau(tableau2, arrondi)
  args_prep <- c(tableau_arrondi1[2:3],  tableau_arrondi2[2:3])
  names(args_prep) <- paste0(names(args_prep), c(1,1,2,2))
  
  arguments_lp <- do.call("preparer_lp_tableaux_lies", args = args_prep)
  prog_sol <- do.call("lp", args = arguments_lp)
  
  if(all(prog_sol$solution == 0)){
    ecarts <- NULL
  }else{
    
    ecarts <- prog_sol$solution - c( c( t( tableau1 ) ), c( t( tableau2 ) ) )
    tableau_solution1 <- matrix(prog_sol$solution[1:(dim1[1]*dim1[2])], ncol = dim1[2], byrow = TRUE)
    tableau_solution2 <- matrix(prog_sol$solution[(dim1[1]*dim1[2] + 1):length(prog_sol$solution)], ncol = dim2[1], byrow = TRUE)
  }
  
  return(
    list(
      orig1 = tableau1,
      orig2 = tableau2,
      sol1 = tableau_solution1,
      sol2 = tableau_solution2,
      prog_sol = prog_sol,
      ecarts = ecarts
    )
  )
}

#' Lance des simulations de reconstruction de tableaux 
#'
#' @param nrep 
#' @param nc 
#' @param nr 
#' @param arrondi 
#' @param lies 
#' @param ventil_mod 
#'
#' @return liste de matrice des écarts
#' @export
#'
#' @examples
#' set.seed(1234)
#' ecarts_1tab_2x2 <- simulations_ecarts_arrondi_original(1000, 2, 2, 10)
simulations_ecarts_arrondi_original <- function(
    nrep = 1000, nc, nr, arrondi, lies = FALSE,  ventil_mod = c(0.2,0.8)
){
  
  require(purrr)
  
  if(!lies){
    ecarts <- compact(
      replicate(
        n = nrep,
        expr = {
          res <- solve_lp_tableau_simule(nc, nr, arrondi)
          res$ecarts
        },
        simplify = FALSE
      ))
  }else{
    ecarts <- compact(
      replicate(
        n = nrep,
        expr = {
          res <- solve_lp_tableaux_lies_simules(nc, nr, arrondi, ventil_mod = ventil_mod)
          res$ecarts
        },
        simplify = FALSE
      ))
  }
  
  return(ecarts)
}


