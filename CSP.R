#' Operators and wrappers for the cyclic problem.
#' @author Tom Decroos, Daan Seynaeve

csp_lahc <- function(instance, Lfa, max_iterations, p, shift_k, replace_k, replace_l, verbose) {
    stopifnot(length(p)==length(fnlist_neighbourhood))
    return(lahc(instance,Lfa,max_iterations,init_solution,fnlist_neighbourhood,p,verbose))
}

init_solution <- function(instance) {
    sol <- rbind(gsp_new_assigment(instance))
    return(gsp_repair(sol,instance))
}

#' --------------------------------
#' Neighbourhoods
#' --------------------------------

#' CSP Neighbourhood function
#' shifts an employee assignment by 1
csp_lshift <- function(sol, instance) {
    len <- dim(sol)[2]
    e <- sample(1:len,1)
    sol[e,] <- c(sol[e,2:len],sol[e,1])
    return(sol)
}

#' CSP Neighbourhood function
#' Shift the assignment of a random employee k times
csp_kl-replace <- function(sol, instance, k, l) {
    
}

#' CSP Neighbourhood function
#' Shift the assignment of a random employee k times
csp_k-shift <- function(sol, instance, k) {
    len <- dim(sol)[2]
    e <- sample(1:len,1)
    
    sol[e,] <- (rep(c(sol[e,]),3))[]
}


#' --------------------------------
#' Helper functions
#' --------------------------------

