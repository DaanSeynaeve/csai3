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

delhaize_royal_spectrum <- function(t=NA,b_min=NA,b_max=NA) {
  if(is.na(t)) t = sample(10:20,1)
  if(is.na(b_min)) b_min = sample(1:20,1)
  if(is.na(b_max)) b_max = sample(b_min:2*b_min,1)
  
  list(t=t,
       on_min=sample(1:4,1),
       on_max=sample(4:10,1),
       off_min=sample(1:4,1),
       off_max=sample(4:8,1),
       b = sample(b_min:b_max,t,replace=TRUE))
}

random_binary_matrix <- function(rows=NA,cols=NA) {
  if(is.na(rows)) rows = sample(10:20,1)
  if(is.na(cols)) cols = sample(10:20,1)
  
  m = matrix(sample(0:1,rows*cols,replace=TRUE),rows,cols)
  
  b = apply(m,2,sum)
  t = length(b)
  
  on_min = min(apply(m,1,function(x) find_oomm(x,1,min)))
  on_max = max(apply(m,1,function(x) find_oomm(x,1,max)))
  off_min = min(apply(m,1,function(x) find_oomm(x,0,min)))
  off_max = max(apply(m,1,function(x) find_oomm(x,0,max)))
  
  list(t=t,
       on_min=on_min,
       on_max=on_max,
       off_min=off_min,
       off_max=off_max,
       b=b)
  
}

find_oomm <- function(x,find=0,filter) {
  if(find != 0) x = 1-x
  filter(table(cumsum(x)[x==0]))
}
  