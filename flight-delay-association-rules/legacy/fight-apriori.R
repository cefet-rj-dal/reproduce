#dataset flight_apriori

flight_apriori[sapply(flight_apriori, is.character)] <- 
  lapply(flight_apriori[sapply(flight_apriori, is.character)], as.factor)
flight_apriori[sapply(flight_apriori, is.numeric)] <- 
  lapply(flight_apriori[sapply(flight_apriori, is.numeric)], as.factor)


aa<-min(sum(flight_apriori$delay_dep==1),
        sum(flight_apriori$delay_arr==1))
a<-aa/nrow(flight_apriori)


a<-sum(flight_apriori$delay_dep==1)/nrow(flight_apriori)


for (s in 1:16){
  
  b<-365*6*s/nrow(flight_apriori)
  
  for (n in 2:4) {
    
    tic()  
    
    rules_delay<-apriori(flight_apriori, 
                         parameter=list(supp = b, conf = a, minlen=2, maxlen= n, target = "rules"), 
                         appearance=list(rhs = c("delay_dep=1"),default="lhs"), 
                         control=NULL)
    
    exectime <- toc()
    
    rules_delay_a<-as(rules_delay, "data.frame")
    
    DT<-data.table(rules_delay_a)
    
    rules_DT<-DT[!(rules %like% "NA")]
    
    nrules<-nrow(rules_DT)
    mlift<-max(rules_DT$lift)
    exectime <- exectime$toc - exectime$tic
    resumo<-data.frame(s,n,nrules,mlift,exectime)
    print(resumo)
    
    
  }
}
