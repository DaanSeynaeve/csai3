init <- function(instance) {
    print("hey")
}

new_dude <- function(instance) {
    print("making a new dude")
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
        dude[day:day-1+dur] = working
        day <- day + dur
        working <- !working
    }
    return(assignment)
}
  
lahc <- function() {
    
}

sol <- function(sol)
    return(sol)