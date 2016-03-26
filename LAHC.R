#' Run the LAHC algorithm on a GDODOSP instance.
#' @param instance; the problem instance
#' @param Lfa; length of the fitness array
#' @param max_iterations; number of iterations to perform
#' @param verbose; set to FALSE to surpress output
lahc <- function(instance, Lfa, max_iterations, p, verbose) {
    fn_mut = function(x) {gsp_neighbour(x,instance,p)}
    fn_c = cost
    
    s <- init_solution(instance)
    f <- rep(fn_c(s),Lfa)
    i <- 0
    if (verbose) {
        print('------ init ------')
        print(fn_c(s))
        print('------ start ------')
    }
    while (i < max_iterations) {
        if (i %% (max_iterations / 10) == 0 && verbose) {
            print(i)
        }
        s_new <- fn_mut(s)
        v <- (i %% Lfa) + 1
        if (fn_c(s_new) <= f[v] || fn_c(s_new) <= fn_c(s)) {
            s <- s_new
        }
        f[v] <- fn_c(s)
        i <- i + 1
    }
    if (verbose) {
        print('------ finished ------')
        print(fn_c(s))
    }
    return(s)
}

#' Generate an initial solution by generating new employee assignments
#' until every timeslot is sufficiently filled.
init_solution <- function(instance) {
    sol <- rbind(new_assignment(instance))
    return(repair(sol,instance))
}

#' --------------------------------
#' Cost functions
#' --------------------------------

#' GDODOSP Cost function
#' @return the number of assignments
cost <- function(sol, type) {
    return(sum(sol))
}

#' GDODOSP Cost function
#' @the number of employees
cost2 <- function(sol, type) {
    return(dim(sol)[1])
}

#' --------------------------------
#' Neighbourhoods
#' --------------------------------

#' Combines several neighbourhoods for the GDODOSP
gsp_neighbour <- function(sol, instance, p) {
    # and_ <- function(x){apply(x,2,all)}
    # or_ <- function(x){apply(x,2,any)}
    # xor_ <- function(x){apply(x,2,function(y) {any(y) && !all(y)})}
    neighbourhoods <- c(
        function() {gsp_replace(sol,instance)},
        function() {gsp_optimize(sol,instance)},
        function() {gsp_lshift2(sol,instance)}
    )
    pcum <- cumsum(p)
    # x <- sample(1:tail(pcum,n=1),1)
    x <- runif(1,0,tail(pcum,n=1))
    fn = min(which(pcum>x))
    # print(fn)
    return((neighbourhoods[[fn]])())
}

#' GDODOSP Neighbourhood function
#' deletes a random employee assignment and
#' generates new assignments until a feasible solution is reached
gsp_replace <- function(sol, instance) {
    r <- sample(1:dim(sol)[1],1)
    sol2 <- sol[-r,]
    return(repair(sol2, instance))
}

#' GDODOSP Neighbourhood function
#' combines two employees assignments into a new one and
#' generates new assignments until a feasible solution is reached
#' @param fn_combine; function that takes a matrix of 2 rows and combines them
gsp_combine <- function(sol, instance, fn_combine) {
    a <- sol[sample(1:dim(sol)[1],2),]
    b <- fn_combine(a)
    if (check_assignment(b,instance)) {
        # print('succes')
        sol2 = rbind(sol[c(-a[1],-a[2]),],b)
        return(repair(sol2, instance))
    } else {
        # print('fail')
        return(sol)
    }
}

#' GDODOSP Neighbourhood function
#' swap the assignment of a timeslot between two employees
gsp_vswap <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],2)
    d <- sample(1:dim(sol)[2],1)
    
    sol2 <- sol
    sol2[e[2],d] <- sol[e[1],d]
    sol2[e[1],d] <- sol[e[2],d]
    
    if(check_assignment(sol2[e[1],],instance) && check_assignment(sol2[e[2],], instance)) {
        if (check_solution(sol2[-e[1],],instance)) {
            return(sol2[-e[1],])
        } else if (check_solution(sol2[-e[2],],instance)) {
            return(sol2[-e[2],])
        } else {
            return(sol2)
        }
    } else {
        return(sol)
    }
}

#' GDODOSP Neighbourhood function
#' randomly changes the assignment of an employee in a timeslot
gsp_bitflip <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    d <- sample(1:dim(sol)[2],1)
    
    sol2 <- sol
    sol2[e,d] <- !sol[e,d]
    
    if (check_assignment(sol2[e,],instance) && check_solution(sol2,instance)) {
        return(sol2)
    } else {
        return(sol)
    }
}

#' KAPOT
#' GDOD0SP Neighbourhood functon
#' shifts an existing employee assignment
#' introduces a new assignment on the last day that fits the constraints
gsp_lshift <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    len <- dim(sol)[2]
    last <- sol[e,len]
    if (last) {
        x <- tail(sol,instance$on_max)
    } else {
        x <- tail(sol,instance$off_max)
    }
    if (all(x==last)) {
        new_last <- !last
    } else {
        new_last <- sample(c(TRUE,FALSE),1)
    }
    sol[e,] <- c(sol[e,2:len],new_last)
    return(sol)
}

gsp_lshift2 <- function(sol, instace) {
    e <- sample(1:dim(sol)[1],1)
    len <- dim(sol)[2]
    sol2 <- sol
    sol2[e,] <- c(sol[e,2:len],sample(c(TRUE,FALSE),1))
    if (check_solution(sol2,instace)) {
        return(sol2)
    } else {
        return(sol)
    }
}

#' CDODOSP Neighbourhood function
#' shifts an employee assignment by 1
csp_lshift <- function(sol, instance) {
    len <- dim(sol)[2]
    e <- sample(1:len,1)
    sol[e,] <- c(sol[e,2:len],sol[e,1])
    return(sol)
}

#' GDODOSP Neighbourhood function
#' pick a random employee and remove him if possible
gsp_optimize <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    if (check_solution(sol[-e,], instance)) {
        return(sol[-e,])
    } else {
        return(sol)
    }
}

#' --------------------------------
#' Helper functions
#' --------------------------------

#' generates new assignments until a feasible solution is reached
repair <- function(sol, instance) {
    sol2 <- sol
    while (!check_solution(sol2, instance)) {
        employee <- new_assignment(instance)
        sol2 <- rbind(sol2,employee)
    }
    return(sol2)
}

#' Generates a random assignment for the GDODOSP
new_assignment <- function(instance) {
    assignment <- rep(FALSE,instance$t)
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