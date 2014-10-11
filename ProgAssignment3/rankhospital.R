rankhospital<-function(state,outcome,num="best"){
  
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
  #check sate
  states<-data$State
  if(length(states[states==state]) <= 0){
    stop("invalid state")
  }  
  
  #give back name and number of refered column 
  seq<-c(11,17,23)
  namesCol<-names(data[seq])
  outcome2<-gsub(" ",".",outcome)
  ncol<-""
  nucol<-0
  for (x in 1:length(namesCol)){
    if(length(grep(outcome2,namesCol[x],ignore.case=TRUE))>0){
      ncol<-namesCol[x]
      nucol<-seq[x]
    }    
  }
  
  #check outcome
  if(ncol==""){
    stop("invalid outcome")
  }
  
  # subset info, by state 
  data<-data[data[,7]==state,]
  
  # Sort by outcome and by name
  data<-data[order(as.numeric(data[,nucol]),data[,2]),]
  
  # replace Not Available by NA
  data1<-as.data.frame(lapply(data, function(x){replace(data,data=="Not Available","NA")}))
  
  # rank subset
  rank<-c(1:nrow(data1))
  data1<-cbind(data1,rank)
  
  # eliminate NA
  data2<-data1[complete.cases(as.numeric(data1[,nucol])),]
  
  #Checkin it was actually eliminated
  #print(head(data2[,2]))
  
  if (num =="best"){
    data2[1,2]
  }else if(num =="worst"){
    data2[nrow(data2),2]
  }else if (num>4706 ||num<0){
    data2<-NA
  }else {
    data2[num,2]  
  }
}