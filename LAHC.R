#' Run the LAHC algorithm on a GDODOSP instance.
#' @param instance; the problem instance
#' @param Lfa; length of the fitness array
#' @param max_iterations; number of iterations to perform
lahc <- function(instance, Lfa, max_iterations) {
    fn_mut = function(x) { replace_assignment(x,instance) }
    c = cost2 # fitness
    
    s <- init_solution(instance)
    f <- rep(c(s),Lfa)
    i <- 0
    print('init')
    print(f)
    print('start iteration')
    while (i < max_iterations) {
        if (i %% (max_iterations / 10) == 0) {
            print(i)
            # print(c(s))
        }
        s_new <- fn_mut(s)
        v <- (i %% Lfa) + 1
        if (c(s_new) <= f[v] || c(s_new) <= c(s)) {
            s <- s_new
        }
        f[v] <- c(s)
        i <- i + 1
    }
    print('finished')
    print(f)
    return(s)
}

#' Generate an initial solution by generating new employee assignments
#' until every timeslot is sufficiently filled.
init_solution <- function(instance) {
    sol <- rbind(new_assignment(instance))
    while (!check_solution(sol, instance)) {
        assignment <- new_assignment(instance)
        sol <- rbind(sol,assignment)
    }
    return(sol)
}

#' Generates a random assignment for the GDODOSP
new_assignment <- function(instance) {
    assignment = rep(FALSE,instance$t)
    working <- sample(c(TRUE,FALSE),1)
    day <- 1
    while (day <= instance$t) {
        if (working) {
            min = instance$on_min
            max = instance$on_max
        } else {
            min = instance$off_min
            max = instance$off_max
        }
        if (day==1) { min <- 1 }
        dur = sample(min:max,1)
        assignment[day:min(day-1+dur,instance$t)] = working
        day <- day + dur
        working <- !working
    }
    return(assignment)
}

#' Checks whether a given solutions solves the instance
check_solution <- function(sol,instance) {
    return(all(colSums(sol) >= instance$b))
}

#' Checks whether a given solutions consists of valid assignments
check_assignments <- function(sol,instance) {
    chk <- function(assignment) {check_assignment(assignment,instance)}
    return(all(apply(check_assignment,sol)))
}

#' Check whether a given assignment is valid
#' padding is added to the edges during the calculation of minima requirements
check_assignment <- function(assignment,instance) {
    pad_on <- rep(TRUE,instance$on_min)
    pad_off <- rep(FALSE,instance$off_min)
    freq <- function(x) { return(table(cumsum(x)[x==FALSE])) }
    if (length(freq(!assignment)) > 0) {
        c1 <- max(freq(!assignment)) <= instance$on_max
        c2 <- min(freq(!c(pad_on,assignment,pad_on))) >= instance$on_min
    } else {
        c1 <- TRUE
        c2 <- TRUE
    }
    if (length(freq(assignment)) > 0) {
        c3 <- max(freq(assignment)) <= instance$off_max
        c4 <- min(freq(c(pad_off,assignment,pad_off))) >= instance$off_min
    } else {
        c3 <- TRUE
        c4 <- TRUE
    }
    return(all(c(c1,c2,c3,c4)))
}

#' --------------------------------
#' Cost functions
#' --------------------------------

#' GDODOSP Cost function
#' @return the number of assignments
cost = function(sol, type) {
    return(sum(sol))
}

#' GDODOSP Cost function
#' @the number of employees
cost2 = function(sol, type) {
    return(dim(sol)[1])
}

#' --------------------------------
#' Neighbourhoods
#' --------------------------------

#' GDODOSP Neighbourhood function
#' deletes a random employee assignment and generate new assignments
#' until a feasible solution is reached
replace_assignment = function(sol, instance) {
    r = sample(1:dim(sol)[1],1)
    sol2 = sol[-r,]
    while (!check_solution(sol2, instance)) {
        assignment <- new_assignment(instance)
        sol2 <- rbind(sol2,assignment)
    }
    return(sol2)
}