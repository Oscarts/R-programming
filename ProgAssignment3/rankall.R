rankall<-function(outcome,num="best") {
  #testing num
  if ((num > 4706 ||num < 0)&& !is.character(num)){
    data2<-NA
    stop("what this number!")
  }
  
  #read data
  data<-read.csv("outcome-of-care-measures.csv") 
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
  # replace Not Available by NA
  data<-as.data.frame(lapply(data, function(x){replace(data,data=="Not Available","NA")}))
  
  # list of states
  tStates<-table(data[,7]) # one specific name: names(t[1])
  
  #dataframe to storage new data
  df<-data.frame() #hospital="", state=""
  hospital<-vector()
  state<-vector()
  
  # for each state find the hospital´s rank given
  for (x in 1:length(tStates)) {
    
    # subset info, by state 
    data2<-data[data[,7]==names(tStates[x]),]
    
    # Sort by outcome and by name
    data2<-data2[order(as.numeric(data2[,nucol]),data2[,2]),]  #
    
    # rank subset
    rank<-c(1:nrow(data2))
    data2<-cbind(data2,rank)
    
    # adding data
     if (num =="best"){
        df<-rbind(df,state=data.frame(hospital=data2[1,2],data2[1,7]))
        #state[length(state)+1]<-
        #hospital[length(hospital)+1]<-
     }else if(num =="worst"){
        df<-rbind(df,data.frame(hospital=data2[nrow(data2),2],state=data2[nrow(data2),7]))
        #state[length(state)+1]<-data2[nrow(data2),7]
        #hospital[length(hospital)+1]<-data2[nrow(data2),2]
     }else {
        df<-rbind(df,data.frame(hospital=data2[num,2],state=names(tStates[x])))
        #state[length(state)+1]<-data2[num,7]
        #hospital[length(hospital)+1]<-data2[num,2]
     }
  }
  #data frame hospital and state  
  #df$state<-state
  #df$hospital<-hospital
  #head(state)
  #head(hospital)
  df
}
