#' Operators and wrappers for the general problem.
#' @authors Tom Decroos, Daan Seynaeve 2016

#' Wrapper function for running the LAHC algorithm on a GSP
gsp_lahc <- function(instance, Lfa, max_iterations, p, verbose) {
    and_ <- function(x){apply(x,2,all)}
    or_ <- function(x){apply(x,2,any)}
    xor_ <- function(x){apply(x,2,function(y) {any(y) && !all(y)})}
    fnlist_neighbourhood <- c(
        function(sol, inst) {gsp_replace(sol,inst)},
        function(sol, inst) {gsp_shrink(sol,inst)},
        function(sol, inst) {gsp_lshift(sol,inst)},
        function(sol, inst) {gsp_combine(sol,inst,and_)},
        function(sol, inst) {gsp_combine(sol,inst,or_)},
        function(sol, inst) {gsp_combine(sol,inst,xor_)},
        function(sol, inst) {gsp_vswap(sol,inst)},
        function(sol, inst) {gsp_bitflip(sol,inst)}
    )
    stopifnot(length(p)==length(fnlist_neighbourhood))
    return(lahc(instance,Lfa,max_iterations,gsp_init_solution,fnlist_neighbourhood,p,verbose))
}

#' Generate an initial solution by generating new employee assignments
#' until every timeslot is sufficiently filled.
gsp_init_solution <- function(instance) {
    sol <- rbind(gsp_new_assigment(instance))
    return(gsp_repair(sol,instance))
}

#' --------------------------------
#' Neighbourhoods
#' --------------------------------

#' Neighbourhood - Replace
#' Always produces a new solution by randomly deleting an employee assignment
#' and generating new assignments to replace it
gsp_replace <- function(sol, instance) {
    sol <- rbind(sol[-sample(1:dim(sol)[1],1),])
    return(gsp_repair(sol, instance))
}

#' Neighbourhood - Combine
#' Maybe produce a new solution by combining two employee assignments into a new one
#' @param fn_combine; function that takes a matrix of 2 rows and combines them
gsp_combine <- function(sol, instance, fn_combine) {
    if (dim(sol)[1] < 2) return(NULL)
    parent_idx <- sample(1:dim(sol)[1],2)
    e_new <- fn_combine(sol[parent_idx,])
    if (check_assignment(e_new,instance)) {
        sol <- rbind(sol[-parent_idx,],e_new)
        if (check_solution(sol,instance)) {
            return(sol)
        } else {
            return(NULL)
        }
    } else {
        return(NULL)
    }
}

#' Neighbourhood - Vertical Swap
#' Maybe produce a new solution by swapping the assignment of a timeslot between two employees
gsp_vswap <- function(sol, instance) {
    if (dim(sol)[1] < 2) return(NULL)
    e <- sample(1:dim(sol)[1],2)
    d <- sample(1:dim(sol)[2],1)
    
    tmp <- sol[e[2],d]
    sol[e[2],d] <- sol[e[1],d]
    sol[e[1],d] <- tmp
    
    if(check_assignment(sol[e[1],],instance) && check_assignment(sol[e[2],], instance)) {
        if (check_solution(rbind(sol[-e[1],]),instance)) {
            return(rbind(sol[-e[1],]))
        } else if (check_solution(rbind(sol[-e[2],]),instance)) {
            return(rbind(sol[-e[2],]))
        } else {
            return(NULL)
        }
    } else {
        return(NULL)
    }
}

#' Neighbourhood - Bitflip
#' Maybe produce a new solution by randomly changing
#' the assignment of an employee in a timeslot
gsp_bitflip <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    d <- sample(1:dim(sol)[2],1)
    
    sol[e,d] <- !sol[e,d]
    
    if (check_assignment(sol[e,],instance) && check_solution(sol,instance)) {
        return(sol)
    } else {
        return(NULL)
    }
}

#' (TODO: FIXME) Neighbourhood - Left shift
#' Always produce a new solution by shifting an existing employee assignment
gsp_lshift2 <- function(sol, instance) {
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

#' Neighbourhood - GSP Shift
#' Maybe produce a new solution by shifting an existing employee assignment
gsp_lshift <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    len <- dim(sol)[2]
    a <- sample(c(TRUE,FALSE),1)
    if (sample(c(TRUE,FALSE),1)) {
        sol[e,] <- c(sol[e,2:len],a)
    } else {
        sol[e,] <- c(a,sol[e,1:len-1])
    }
    if (check_assignment(sol[e,], instance) && check_solution(sol,instance)) {
        return(sol)
    } else {
        return(NULL)
    }
}

#' Neighbourhood - Shrink
#' Maybe produce a new solution by removing a random employee assignment
gsp_shrink <- function(sol, instance) {
    e <- sample(1:dim(sol)[1],1)
    if (check_solution(rbind(sol[-e,]), instance)) {
        return(rbind(sol[-e,]))
    } else {
        return(NULL)
    }
}

#' --------------------------------
#' Helper functions
#' --------------------------------

#' generates new assignments until a feasible solution is reached
gsp_repair <- function(sol, instance) {
    while (!check_solution(sol, instance)) {
        employee <- gsp_new_assigment(instance)
        sol <- rbind(sol,employee)
    }
    return(sol)
}

#' Generates a random assignment for the GSP
gsp_new_assigment <- function(instance) {
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
    return(all(apply(sol,1,chk)))
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