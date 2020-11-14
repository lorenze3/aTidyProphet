library(prophet)
library(tidyverse)

#set ggplot2 theme
theme_set(ggthemes::theme_tufte())

#pulling in prepared data -- shaped conveniently already
trendData<-read_csv('diet cranberry sauces.csv')


#make a future df, with the dates needed for forecasting
#last date in data

lastTrainingDate<-max(trendData$ds)
# "2020-11-01"

#number of cases
nCases<-length(unique(trendData$case))
#  5

#one set of dates
days=seq(lastTrainingDate+1,as.Date('2020-11-26'),by=1)
#prophet requires data frames with columns ds and y
future<-data.frame(case=rep(unique(trendData$case),each=length(days)),
         ds=rep(days,nCases),
          y=vector('numeric',length(days)*nCases)
         )

#using some of that famous tidyverse expressiveness and pipe!!
#this sort is completely unnecessary, but will make the a visual
#check of the table clearer
future<-future %>% arrange(case,ds)

#group_by all columns used for identifying a case or used to control the prophet function
nestedData <- trendData %>% group_by(case) %>% nest()
#data is the default name of the nested column created by nest -- but it's just a list of tibbles
#where each tibble has ds and y in it --
#you can see that by doing something like
#finalForm$data[[1]] and finalForm$future[[1]]

#we'll want to name the nested column different in the future
nestedFuture<- future %>% group_by(case) %>% nest(future=c(ds,y))

#join them together for later use; join here will use all columns in common
finalForm<-inner_join(nestedData,nestedFuture)

#data is the default name of the nested column created by nest -- but it's just a list of tibbles
#where each tibble has ds and y in it --
#you can see that by doing something like
#finalForm$data[[1]] and finalForm$future[[1]]

#we will need a holiday to properly model the thanksgiving bump!
turkeyDayUSA <- data_frame(
  holiday = 'USThanksGiving',
  ds = as.Date(c('2015-11-26','2016-11-24','2017-11-23',
                 '2018-11-22','2019-11-28','2020-11-26')),
  lower_window = -10,
  upper_window = 0
)
#NB: the second argument to map and pmap is the function name, no parentheses!
#the following arguments are those held constant for all cases in the list
#Remember our list of cases here is the tibble being passed in
nestedResults<-finalForm %>% mutate(model=map(data,prophet,holidays=turkeyDayUSA))

#And we can go ahead and keep using map functions to produce other outputs
#tidily stored with their input data; we'll add a forecast and a plot
#pmap is being used here to show the general form of map function
#see ?map for all options

nestedResults<-nestedResults%>% mutate( forecast=pmap(list(object=model,df=future),predict),
                                        chart=pmap(list(model,forecast),
                                               plot,
                                               xlabel='day',ylabel=unique(case))
                                    )


                                     