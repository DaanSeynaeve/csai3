# Return a GDODOSP based on the proxy delhaize where Daan used to work.
# The time unit is half days and the period is a full week.
delhaize <- function() {
  list(t=14,
       on_min=2,
       on_max=6,
       off_min=2,
       off_max=4,
       b=c(2,2,
           2,2,
           3,3,
           2,2,
           2,3,
           3,3,
           4,3))
}

# Return a GDODOSP based on the restaurant 'Royal' where Tom used to work.
# The time unit is hours and the period is a full working day.
royal <- function() {
  list(t=16,
       on_min=3,
       on_max=8,
       off_min=3,
       off_max=8,
       b=c(10,10,15,15,
           20,20,20,15,
           15,15,20,20,
           20,20,15,10))
}

# Return a random GDODOSP that should resemble a real world scheduling problem
delhaize_royal_spectrum <- function(t=NA,b_min=NA,b_max=NA) {
  if(is.na(t)) t = sample(10:20,1)
  if(is.na(b_min)) b_min = sample(1:20,1)
  if(is.na(b_max)) b_max = sample(b_min:(2*b_min),1)
  list(t=t,
       on_min=sample(1:4,1),
       on_max=sample(4:10,1),
       off_min=sample(1:4,1),
       off_max=sample(4:8,1),
       b = sample(b_min:b_max,t,replace=TRUE))
}

# Return a random GDODOSP that was solved by a randomly generated binary matrix.
random_binary_matrix <- function(rows=NA,cols=NA) {
  if(is.na(rows)) rows = sample(10:20,1)
  if(is.na(cols)) cols = sample(10:20,1)
  
  m = matrix(sample(c(FALSE,TRUE),rows*cols,replace=TRUE),rows,cols)
  
  b = apply(m,2,sum)
  t = length(b)
  
  on_min = min(apply(m,1,function(x) find_oomm(x,TRUE,min)))
  on_max = max(apply(m,1,function(x) find_oomm(x,TRUE,max)))
  off_min = min(apply(m,1,function(x) find_oomm(x,FALSE,min)))
  off_max = max(apply(m,1,function(x) find_oomm(x,FALSE,max)))
  
  list(t=t,
       on_min=on_min,
       on_max=on_max,
       off_min=off_min,
       off_max=off_max,
       b=b)
  
}

# Get the lengths of maximal subsequences from x of the given bool
# and apply the given filter such as min or max
find_oomm <- function(x,bool,filter) {
  if(!bool) x = !x
  filter(table(cumsum(x)[!x]))
}

# Take a gdodosp and convert it to a a cdodosp
# with a randomly generated basic assignment
convert_to_cyclic_problem <- function(gdodosp) {
  assignment = c()
  on = sample(c(TRUE,FALSE),1)
  while(length(assignment) < gdodosp$t) {
    if(on){
      lower = gdodosp$on_min
      upper = gdodosp$on_max
    } else {
      lower = gdodosp$off_min
      upper = gdodosp$off_max
    }
    assignment = c(assignment, rep(on,sample(lower:upper,1)))
    on = !on
  }
  list(b = gdodosp$b,
       t = gdodosp$t,
       assignment = assignment[1:gdodosp$t])
}

generate_and_save_examples <- function(n=100,t=NA,examplesdir="instances_t100") {
  dir.create(examplesdir)
  for (i in 1:n) {
    example <- delhaize_royal_spectrum(t)
    file <- paste(examplesdir,"/",as.character(i),".rds",sep="")
    saveRDS(example,file=file)
  }
}