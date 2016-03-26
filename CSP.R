#' Operators and wrappers for the cyclic problem.
#' @author Tom Decroos, Daan Seynaeve

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

#' --------------------------------
#' Helper functions
#' --------------------------------