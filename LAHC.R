#' LAHC algorithm
#' @authors Tom Decroos, Daan Seynaeve 2016

#' Runs the LAHC algorithm.
#' @param instance; the problem instance
#' @param Lfa; length of the fitness array
#' @param max_iterations; number of iterations to perform
#' @param fn_init; function that generates an initial valid solution
#' @param neighbourhoods; list of neighbourhoods
#' @param p; vector of probabilities correspondig to the neighbourhoods
#' @param verbose; set to FALSE to surpress output
lahc <- function(instance, Lfa, max_iterations, init, neighbourhoods, p, verbose) {
    fn_mut = function(x) {neighbourhood_weighted(x,instance,neighbourhoods,p)}
    fn_c = cost
    
    s <- init(instance)
    f <- rep(fn_c(s),Lfa)
    f_pos <- 0
    if (verbose) {
        print('------ init ------')
        print(fn_c(s))
        start.time <- Sys.time()
        print('------ start ------')
    }
    i <- 0
    while (i < max_iterations) {
        if (i %% (max_iterations / 10) == 0 && verbose) {
            print(i)
        }
        s_new <- fn_mut(s)
        if (!is.null(s_new)) {
            v <- ((f_pos-1) %% Lfa) + 1
            
            if (fn_c(s_new) <= f[v] || fn_c(s_new) <= fn_c(s)) {
                s <- s_new
            }
            f[v] <- fn_c(s)
            f_pos = f_pos + 1
            # print("tick")
        }
        i <- i + 1
        # print("tock")
    }
    if (verbose) {
        print('------ finished ------')
        print(c("fitness:",fn_c(s)))
        print(c("#candidates:",f_pos))
        print(Sys.time() - start.time)
    }
    return(list(sol=s,res=fn_c(s)))
}

#' Apply a random neighbourhood selected from a given set with given probabilities
neighbourhood_weighted <- function(sol, instance, neighbourhoods, p) {
    pcum <- cumsum(p)
    # x <- sample(1:tail(pcum,n=1),1)
    x <- runif(1,0,tail(pcum,n=1))
    fn = min(which(pcum>x))
    # print(c("neighbourhood:",fn))
    return((neighbourhoods[[fn]])(sol, instance))
}

#' GSP/CSP Cost function
#' @return the number of assignments
cost <- function(sol, type) {
    return(sum(sol))
}

#' GSP/CSP Cost function
#' @return the number of employees
cost2 <- function(sol, type) {
    return(dim(sol)[1])
}