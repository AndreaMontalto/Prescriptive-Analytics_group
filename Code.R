## Loading Library ##
library(gurobi)

#normal distribution, mean of 155 and sd = 45 to generate daily paitients over a week
patients <-rnorm(7,155,45)

#Shifts in a day
Svec <- list(s1 = (0:8), 
            s2 = (6:14), 
            s3 = (12:20), 
            s4 = (16:24))
#Number of days 
Tvec <- 1:7
#number of periods in a day
Ivec <- list(i1 = (0:4), 
             i2 = (4:8), 
             i3 = (8:12), 
             i4 = (12:16), 
             i5 = (16:20), 
             i6 = (20:24))
#type of doctor
Dvec <- 1:3

#Cost matrix -- each type of doctor per shift 
Pmat<-matrix(c(120,347,283,
               80,231,189,
               80,231,189,
               80,231,189), nrow=length(Dvec), ncol = length(Svec))

#demand per each period i in a day T
Nmat<-matrix(0, nrow = length(Tvec), ncol = length(Ivec))

generate_random_demand <- function(patients,Nmat) {
  for (i in 1:length(patients)) {
    random_numbers <- runif(6)
    normalized_numbers <- random_numbers / sum(random_numbers)
    scaled_numbers <- normalized_numbers * patients[i]
    Nmat[i,]<-round(scaled_numbers,0)
  }
  return(Nmat)
}

Nmat<-generate_random_demand(patients,Nmat)
colnames(Nmat)<-c("i1","i2","i3","i4","i5","i6")

#matrix to determine if shift s covers period i
Amat<-matrix(0, nrow=length(Svec), ncol = length(Ivec))
colnames(Amat)<-c("i1","i2","i3","i4","i5","i6")

#filling up Amat
for (i in 1:length(Ivec)){
  for (s in 1:length(Svec)){ 
    binary <- sum(unlist(Ivec[i]) %in% unlist(Svec[s]))
    if (as.numeric(binary) == 5){ 
      Amat[s, i] <- 1
      }
    else if (as.numeric(binary) == 3){ 
      Amat[s, i] <- 0.5
      }
    else{ 
      Amat[s, i] <- 0}
  }
}

#### GUROBI MODEL ####



