getData<-function(){
  data<-read.csv("activity.csv", stringsAsFactors = FALSE)
  data$date<-as.Date(data$date)
  data
}

completeData<-function(method="mean"){
  data<-read.csv("activity.csv", stringsAsFactors = FALSE)
  data$date<-as.Date(data$date)
  
  ##Calculate and report the total number of missing values in the dataset 
  print(paste("Missing rows:",nrow(data[is.na(data$steps),])))
  
  ##Devise a strategy for filling in all of the missing values in the dataset. 
  ##Use mean of each interval to fill NA.
  if (method=="mean"){
    by_interval<-group_by(data,interval)
    steps_by_interval<-summarise(by_interval,avgSteps=mean(steps,na.rm = TRUE))
    na_interval<-distinct(data[is.na(data$steps),],interval)
    for (i in 1:nrow(na_interval)){
      data$steps[is.na(data$steps)&data$interval==na_interval$interval]<-steps_by_interval$avgSteps[steps_by_interval$interval==na_interval$interval]
    }
  }
  data
  
}

plot1<-function(data=data.frame()){
  require(dplyr)
  require(ggplot2)
  
  if(nrow(data)==0){
    data<-getData()
  }
  
  by_day<-group_by(data, date)
  steps_by_day<-summarise(by_day,totalSteps=sum(steps,na.rm = TRUE))
  
  p<-ggplot(data = steps_by_day,aes(x = date, y = totalSteps)) +
      geom_bar(stat = "identity")
  print(p)
}

plot2<-function(data=data.frame()){
  require(dplyr)
  require(ggplot2)
  
  if(nrow(data)==0){
    data<-getData()
  }
  
  by_day<-group_by(data, date)
  steps_by_day<-summarise(by_day,meanSteps=mean(steps,na.rm = TRUE))
  
  p<-ggplot(data = steps_by_day,aes(x = date, y = meanSteps)) +
    geom_step()
  print(p)
}

plot3<-function(data=data.frame()){
  require(dplyr)
  require(ggplot2)
  
  if(nrow(data)==0){
    data<-getData()
  }
  
  by_day<-group_by(data, date)
  steps_by_day<-summarise(by_day,medianSteps=median(steps,na.rm = TRUE))
  
  p<-ggplot(data = steps_by_day,aes(x = date, y = medianSteps)) +
    geom_step()
  print(p)
}

plot4<-function(data=data.frame()){
  require(dplyr)
  require(ggplot2)
  
  if(nrow(data)==0){
    data<-getData()
  }
  
  by_interval<-group_by(data, interval)
  steps_by_interval<-summarise(by_interval,avgSteps=mean(steps,na.rm = TRUE))
  maxAvgSteps<-max(steps_by_interval$avgSteps)
  
  p<-ggplot(data = steps_by_interval,aes(x = interval, y = avgSteps)) +
    geom_line()+
    geom_point(data = filter(steps_by_interval, avgSteps==maxAvgSteps),colour="red")+
    geom_text(aes(label=interval),data = filter(steps_by_interval, avgSteps==maxAvgSteps),nudge_y = 10)
  print(p)
  
  steps_by_interval<-arrange(steps_by_interval, desc(avgSteps))
  i<-1
  for(i in 1:nrow(steps_by_interval))
  {
    if(steps_by_interval$avgSteps[i+1]<steps_by_interval$avgSteps[i]){
      break
    }
  }
  maxInterval<-as.character(steps_by_interval$interval[1])
  if(i>1){
    for(j in 2:i){
      maxInterval<-paste(maxInterval, ",", steps_by_interval$interval[i])
    }
  }
  print(paste("Interval", maxInterval, "contains the maximum number of steps."))
}

plot5<-function(data=data.frame()){
  require(dplyr)
  require(ggplot2)
  
  if(nrow(data)==0){
    data<-getData()
  }
  
  data<-mutate(data, week=as.factor(getWeek(date)))
  by_interval<-group_by(data, week, interval)
  steps_by_interval<-summarise(by_interval,avgSteps=mean(steps,na.rm = TRUE))
  
  p<-ggplot(data = steps_by_interval, aes(x = interval, y = avgSteps))+
    geom_line()+
    facet_grid(facets = week~.)
  print(p)
}

getWeek<-function(date=as.Date()){
  w<-as.character()
  for (i in 1:length(date)){
    if(weekdays(date[i])=="ÐÇÆÚÁù"|weekdays(date[i])=="ÐÇÆÚÈÕ"){
      w<-c(w,"weekend")
    }
    else{
      w<-c(w,"weekday")
    }
  }
  w
}