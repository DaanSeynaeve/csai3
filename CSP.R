#' Operators and wrappers for the cyclic problem.
#' @author Tom Decroos, Daan Seynaeve

csp_lahc <- function(instance, Lfa, max_iterations, p, replace_k, replace_l, verbose) {
    fnlist_neighbourhood <- c(
        function(sol, inst) {csp_shift(sol,inst)},
        function(sol, inst) {csp_kl_replace(sol,inst,replace_k,replace_l)}
    )
    stopifnot(length(p)==length(fnlist_neighbourhood))
    init_solution <- function(x) {return(csp_init_solution(x,TRUE))}
    return(lahc(instance,Lfa,max_iterations,init_solution,fnlist_neighbourhood,p,verbose))
}

#' Generate an initial solution
#' If not random, each new employee assigment is obtained by shifting just 1 time to the left
csp_init_solution <- function(instance, random) {
    n <- function() {return(if (random) sample(1:instance$t,1) else 1)}
    sol <- rbind(n_lshift(instance$assignment,n()))
    i <- 1
    while (!check_solution(sol,instance)) {
        i <- i + 1
        sol <- rbind(sol,n_lshift(sol[i-1,],n()))
    }
    return(sol)
}

#' --------------------------------
#' Neighbourhoods
#' --------------------------------

#' Neighbourhood - CSP Shift
#' shifts a random employee assignment left or right
csp_shift <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    sol[e,] <- rbind(n_lshift(sol[e,],sample(c(-1,1),1)))
    if (check_solution(sol,instance)) return(sol) else return(NULL)
}

#' Neighbourhood - kl replacement
#' Maybe produce a new solution by deleting k assignments and adding l assignments
csp_kl_replace <- function(sol, instance, k, l) {
    if (k < dim(sol)[1]) {
        if (k > 0) sol <- rbind(sol[-sample(1:dim(sol)[1],k),])
        if (l > 0) {
            offsets <- matrix(sample(1:dim(sol)[2],l))
            sol <- rbind(sol,t(apply(offsets,1,function(n) {n_lshift(instance$assignment,n)})))
        }
        if (check_solution(sol, instance)) return(sol) else return(NULL)
    } else {
        return(NULL)
    }
}

#' --------------------------------
#' Helper functions
#' --------------------------------

# Shift an assignment n times to the left
n_lshift <- function(a,n) {
    i <- ((n-1) %% length(a)) + 1
    return(c(a[-c(1:i)],a[1:i]))
}

#' Checks whether a given solutions solves the instance
check_solution <- function(sol,instance) {
    return(all(colSums(sol) >= instance$b))
}