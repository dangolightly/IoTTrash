head(test_data)

#convert timestamp to datatime format
test_data$timestamp_edit<-as.POSIXct(test_data$timestamp,format="%Y-%m-%dT%H:%M:%OS")

#______________________________________________________________________________

#function to calculate time period between can trigger(going over 80%, 5 consecative times) and 
#emptying (5 consectative readings below 20) per container
time_to_empty<-function(df, containerID){
  t_start=NA
  t_end=NA
  overflow=NA
  k=0
  m=0
  df$concec80[is.na(df$concec80)]<-"NA"
  df$concec20[is.na(df$concec20)]<-"NA"
  for(i in 1:length(df$fill_level_percentage)){
    if(df$concec80[i]=="trigger_full" & m==0){
      k=k+1
      m=1
      t_start[k]=df$timestamp_edit[i]
    }
    if(df$concec100[i]=="trigger_overflow" & m==1){
      overflow[k]=1
    }
    if(df$concec20[i]=="trigger_empty" & m==1){
      t_end[k]=df$timestamp_edit[i]
      m=0
    }
  }
  t_start<-as.POSIXct(t_start,format="%Y-%m-%dT%H:%M:%OS", origin="1970-01-01T00:00:00")
  t_end<-as.POSIXct(t_end,format="%Y-%m-%dT%H:%M:%OS", origin="1970-01-01T00:00:00")
  p1<-difftime(t_end,t_start,units="mins")
  result<-as.data.frame(cbind(t_start,t_end,p1,overflow))
  result$t_start<-as.POSIXct(result$t_start,format="%Y-%m-%dT%H:%M:%OS", origin="1970-01-01T00:00:00")
  result$t_end<-as.POSIXct(result$t_end,format="%Y-%m-%dT%H:%M:%OS", origin="1970-01-01T00:00:00")
  result$overflow[is.na(result$overflow)]<-0
  result$containerID<-containerID
  return(result)
}


## iterate through all containers and generate a df (time_df) for emptying times per can  

conatiner_list<-unique(test_data$container_id)
time_df<-data.frame(matrix(ncol = 5, nrow = 0))
colnames(time_df) = c('t_start','t_end','p1','overflow','containerID')

#calculate per container
for(i in 1:length(conatiner_list)){
  df<-test_data[test_data$container_id==conatiner_list[i],] %>% 
    arrange(timestamp_edit) %>% 
    mutate(threshold = ifelse(fill_level_percentage>80, "over80", "notover80")) %>% 
    mutate(empty = ifelse(fill_level_percentage<20, "empty", "not_empty")) %>% 
    mutate(overflow = ifelse(fill_level_percentage>99, "overflow", "not_overflow")) %>% 
    mutate(concec80 = ifelse(threshold=="over80" & 
                               lag(threshold,1)=="over80" &
                               lag(threshold,2)=="over80" &
                               lag(threshold,3)=="over80" &
                               lag(threshold,4)=="over80", "trigger_full", "notrigger")) %>% 
    mutate(concec20 = ifelse(empty=="empty" & 
                               lag(empty,1)=="empty" &
                               lag(empty,2)=="empty" &
                               lag(empty,3)=="empty" &
                               lag(empty,4)=="empty", "trigger_empty", "notrigger")) %>% 
    mutate(concec100 = ifelse(overflow=="overflow" & 
                               lag(overflow,1)=="overflow" &
                               lag(overflow,2)=="overflow" &
                               lag(overflow,3)=="overflow" &
                               lag(overflow,4)=="overflow", "trigger_overflow", "notrigger"))
  time_df<-bind_rows(time_df,time_to_empty(df,conatiner_list[i]))
}

#remove datapoints where cans not emptied yet (these result in negative time differences)
time_df_edit<-time_df[!time_df$p1<0,]

write.csv(time_df_edit, "time_to_empty_test.csv")





