best<-function(state,outcome){
  
  setwd("~/Documents/R-programming/ProgAssignment3")
  data1<-read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  data<-as.data.frame(lapply(data1, function(x){replace(data1,data1=="Not Available","NA")}))
  
  #check sate
  states<-data1$State
  
  if(length(states[states==state]) <= 0){
    stop("invalid state")
  }  
  
  #namesCol<-names(data1[seq(from = 11, to = 29, by = 6)])
  seq<-c(11,17,23)
  namesCol<-names(data1[seq])
  outcome2<-gsub(" ",".",outcome)
  ncol<-""
  nucol<-0
  for (x in 1:length(namesCol)){
    if(length(grep(outcome2,namesCol[x],ignore.case=TRUE))>0){
      #ncol<-x*6+11
      ncol<-namesCol[x]
      nucol<-seq[x]
    }    
  }
  
  if(ncol==""){
    stop("invalid outcome")
  }
  
  mini<-min(as.numeric(data[which(data[,7] == state), nucol]),na.rm=TRUE)
  name<-data1[(as.numeric(data1[,nucol])==mini & data1[,7]==state),2]
  nameo<-sort(name)
  nameo[1]
}