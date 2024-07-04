library(gurobi)

patients<-rnorm(7,155,45)

Svec<-1:4
Tvec<-1:7
Ivec<-1:6
Dvec<-1:3
Pmat<-matrix(c(120,347,283,
               80,231,189,
               80,231,189,
               80,231,189), nrow=length(Dvec), ncol = length(Svec))

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

Amat<-matrix(0, nrow=length(Svec), ncol = length(Ivec))
