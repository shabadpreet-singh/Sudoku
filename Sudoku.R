setwd("F:/Sudoku_Solver")

#Attempt at a simulated annealing sudoku solver

#Too easy one
s=matrix(NA,ncol=9,nrow=9)
s[1,c(6,8)]=c(6,4)
s[2,c(1:3,8)]=c(2,7,9,5)
s[3,c(2,4,9)]=c(5,8,2)
s[4,3:4]=c(2,6)
s[6,c(3,5,7:9)]=c(1,9,6,7,3)
s[7,c(1,3:4,7)]=c(8,5,2,4)
s[8,c(1,8:9)]=c(3,8,5)
s[9,c(1,7,9)]=c(6,9,1)


# s=matrix(NA,ncol=9,nrow=9)
# s[1,c(1,6,7)]=c(8,1,2)
# s[2,c(2:3)]=c(7,5)
# s[3,c(5,8,9)]=c(5,6,4)
# s[4,c(3,9)]=c(7,6)
# s[5,c(1,4)]=c(9,7)
# s[6,c(1,2,6,8,9)]=c(5,2,9,4,7)
# s[7,c(1:3)]=c(2,3,1)
# s[8,c(3,5,7,9)]=c(6,2,1,9)

# s=matrix(NA,ncol=9,nrow=9)
# s[1,c(1,5,6,8)]=c(4,9,7,8)
# s[2,2]=2
# s[3,c(3,8)]=c(6,4)
# s[4,9]=9
# s[5,c(2,4,7)]=c(1,4,6)
# s[6,c(8,3,6,8,9)]=c(8,3,2,5,7)
# s[7,1:3]=c(5,9,7)
# s[8,5]=1
# s[9,5:7]=c(2,6,7)

score <- function(x){
  ans <- 0
  
  for(k in 1:9){
    ans <- ans - length(unique(x[k, ])) - length(unique(x[, k]))
  }
  return(ans)
} # A -1 for each unique number in row and column so min score = -162
# fill randomly in matrix s such that each small box has only one from 1-9
fill_empty <- function(s)
{ 
  isfilled <- !is.na(s)
  
  for(i in 0:2){
    num <- c(1:9)
    
    temp <- s[(i*3 + 1):((i+1)*3), 1:3]
    
    filled <- temp[isfilled[(i*3 + 1):((i+1)*3), 1:3]]
    num <- num[-(filled)]
    
    #randomly fill empty spaces
    temp[!isfilled[(i*3 + 1):((i+1)*3), 1:3]] <- sample(num, length(num))
    
    s[(i*3 + 1):((i+1)*3), 1:3] <- temp
    
    ##########################
    
    num <- c(1:9)
    
    temp <- s[(i*3 + 1):((i+1)*3), 4:6]
    
    filled <- temp[isfilled[(i*3 + 1):((i+1)*3), 4:6]]
    num <- num[-(filled)]
    
    #randomly fill empty spaces
    temp[!isfilled[(i*3 + 1):((i+1)*3), 4:6]] <- sample(num, length(num))
    
    s[(i*3 + 1):((i+1)*3), 4:6] <- temp
    
    ##########################
    
    num <- c(1:9)
    
    temp <- s[(i*3 + 1):((i+1)*3), 7:9]
    
    filled <- temp[isfilled[(i*3 + 1):((i+1)*3), 7:9]]
    num <- num[-(filled)]
    
    #randomly fill empty spaces
    temp[!isfilled[(i*3 + 1):((i+1)*3), 7:9]] <- sample(num, length(num))
    
    s[(i*3 + 1):((i+1)*3), 7:9] <- temp
  }
  return(s)
}


# find which elements are filled
isfilled <- !is.na(s)
original <- s
s <- fill_empty(s)


curr_score <- score(s)
prev_score <- curr_score
min_score <- -162 # 9*9*2 = 162

T <- 0.5
count <- 0
similar <- 0

###############################
while(count < 1e6){
  if(count%%1e4 == 0) cat("Iteration number", count, "score is ", curr_score, "\n")

while(1){
  
  block <- sample(1:9, 1)
  block_index <- c(( (block-1) %/% 3 )*3 + 1,((block - 1)%%3)*3 + 1)
  
  # Now selecting numbers to be exchanged 
  temp <- s[block_index[1]:(block_index[1] + 2), block_index[2]:(block_index[2] + 2)]
  empty <- which(!isfilled[block_index[1]:(block_index[1] + 2), block_index[2]:(block_index[2] + 2)])
  
  if(length(empty) > 2){ break }
}
  
#2 random numbers 
num <- sample(empty, 2)
cacahe <- temp[num[1]]
temp[num[1]] <- temp[num[2]]
temp[num[2]] <- cacahe


candidate <- s
candidate[block_index[1]:(block_index[1] + 2), block_index[2]:(block_index[2] + 2)] <- temp

candidate_score <- score(candidate)

delta_s <- curr_score - candidate_score

if(exp(delta_s/T) - runif(1) > 0){
  
  curr_score <- candidate_score
  s <- candidate
}

if(curr_score == prev_score) similar <- similar + 1
else{
  prev_score <- curr_score
  similar <- 0
}

if(candidate_score == min_score){
  s <- candidate
  cat("Iteration number", count, "score is ", candidate_score, "\n")
  break
}

if(curr_score == min_score) break

if(similar > 1e4){
  s <- original
  s <- fill_empty(s)
  
  similar <- 0
  T <- 0.5
  
  curr_score <- score(s)
  prev_score <- curr_score
  
}
  
count <- count + 1
T <- T*0.99
}

save(s, curr_score, file = "ans.RData")
