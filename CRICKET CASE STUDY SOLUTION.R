library(stringr)
library(dplyr)

CRICKETDATA=read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
STADIUMS=read.csv(file.choose(),sep=',',header=T,stringsAsFactors = F)
Cdata=CRICKETDATA
Sdata=STADIUMS
str(CRICKETDATA)
summary(CRICKETDATA)
View(CRICKETDATA)

# Q.1 Transform Margin Column into 2 Columns "Wickets" and "Runs"

for(i in 1:length(CRICKETDATA$Margin))
{
    if(str_detect(CRICKETDATA[i,'Margin'],"wickets"))
    {
      CRICKETDATA[i,'BYWICKETS']=grep("wickets",CRICKETDATA[i,'Margin'],value = T)
    }else if((str_detect(CRICKETDATA[i,'Margin'],"runs"))){
      CRICKETDATA[i,'BYRUNS']=grep("runs",CRICKETDATA[i,'Margin'],value = T)
    }else if((str_detect(CRICKETDATA[i,'Winner'],"tied"))){
      CRICKETDATA[i,'BYRUNS']="Tied"
      CRICKETDATA[i,'BYWICKETS']="Tied"
    }else {
      CRICKETDATA[i,'BYRUNS']="No Result"
      CRICKETDATA[i,'BYWICKETS']="No Result"
    }
}
View(CRICKETDATA)


# Q.2 Which Country have played most ODI in 2019

Team1=data.frame(CRICKETDATA%>%
  group_by(Country=Team.1)%>%
  summarise(MatchesPLayed=n())%>%
  arrange(desc(MatchesPLayed)))

Team2=data.frame(CRICKETDATA%>%
  group_by(Country=Team.2)%>%
  summarise(MatchesPLayed=n())%>%
  arrange(desc(MatchesPLayed)))

TotalMatches=union(Team1,Team2)

TOTALMATCHESBYCOUNTRY=data.frame(TotalMatches%>%
    group_by(Country)%>%
    select(Country,MatchesPLayed)%>%
    summarise(MatchesPLayed=sum(MatchesPLayed))%>%
    arrange(desc(MatchesPLayed)))

TOTALMATCHESBYCOUNTRY%>%
  top_n(1)

    
# Q.3 Top 3 Countries who won most ODIs

TOTALWONBYCOUNTRY=data.frame(CRICKETDATA%>%
    filter(Winner!='tied')%>%
    filter(Winner!='no result')%>%
    group_by(Country=Winner)%>%
    summarise(MatchesWon=n())%>%
    arrange(desc(MatchesWon)))
    
TOTALWONBYCOUNTRY%>%
  top_n(3)
  

# Q.4 Based On the map the Country with Stadium in the new column named as HOSTED COUNTRY


str(STADIUMS)

for(i in 1:length(CRICKETDATA$Ground))
{
  if((CRICKETDATA[i,'Ground'])%in%(STADIUMS$STADIUM))
  {
    CRICKETDATA[i,'HOSTCOUNTRY']=(unique(STADIUMS[which((STADIUMS$STADIUM)==unique(grep(CRICKETDATA[i,'Ground'],STADIUMS$STADIUM,value = T))),'COUNTRY']))
  }else{
    CRICKETDATA[i,'HOSTCOUNTRY']='COUNTRYNOTFOUND'
  }
}

View(CRICKETDATA)


# Q.5 Which Country Played Maximum matches at home ground

for(i in 1:length(CRICKETDATA$HOSTCOUNTRY))
{
  if((c(CRICKETDATA[i,'Team.1'],CRICKETDATA[i,'Team.2']))%in%(CRICKETDATA[i,'HOSTCOUNTRY']))
  {
    CRICKETDATA[i,'HOMEGROUND']='PLAYED AT HOME'
  }else{
    CRICKETDATA[i,'HOMEGROUND']='PLAYED OUTSIDE'
  }
}


CRICKETDATA%>%
  filter(HOMEGROUND=="PLAYED AT HOME")%>%
  group_by(HOSTCOUNTRY)%>%
  summarise(COUNTOFMATCHES=n())%>%
  arrange(desc(COUNTOFMATCHES))%>%
  top_n(3)


# Q.6 Top 3 Wins by Runs

CRICKETDATA%>%
  select(Winner,BYRUNS)%>%
  group_by(Winner,BYRUNS)%>%
  arrange(desc(BYRUNS))%>%
  filter(BYRUNS != 'Tied')%>%
  filter(BYRUNS != 'No Result')%>%
  head(3)

# Q.8 Month in which most ODIs were played

strptime(CRICKETDATA$Match.Date,"%d-%b-%y")
CRICKETDATA$MONTH=as.POSIXlt(as.POSIXct(CRICKETDATA$Match.Date,format="%d-%b-%y"))$mon+1
CRICKETDATA$MONTH=month.abb[CRICKETDATA$MONTH]


CRICKETDATA%>%
  group_by(MONTH)%>%
  summarize(COUNT_OF_MATCHES_PLAYED=n())%>%
  arrange(desc(COUNT_OF_MATCHES_PLAYED))%>%
  top_n(1)


# Q.9 Team Which Toured Most Foreign Countries

a=data.frame(CRICKETDATA%>%
   filter(HOMEGROUND=="PLAYED OUTSIDE")%>%
   group_by(Countryname=Team.1)%>%
   summarize(COUNT_OF_TOURS=n()))%>%
   arrange(desc(COUNT_OF_TOURS))
 
b=data.frame(CRICKETDATA%>%
  filter(HOMEGROUND=="PLAYED OUTSIDE")%>%
  group_by(Countryname=Team.2)%>%
  summarize(COUNT_OF_TOURS=n()))%>%
  arrange(desc(COUNT_OF_TOURS))

rbind(a,b)%>%
  group_by(Countryname)%>%
  summarize(COUNT_OF_TOURS=sum(COUNT_OF_TOURS))%>%
  top_n(1)

# Q.10 How many Cricket Matches were played every month

CRICKETDATA%>%
  group_by(MONTH)%>%
  summarize(COUNT_OF_MATCHES=n())%>%
  arrange(desc(COUNT_OF_MATCHES))

# Q.11 On Which Ground Most Games were played

CRICKETDATA%>%
  group_by(Ground)%>%
  summarize(COUNT_OF_MATCHES=n())%>%
  arrange(desc(COUNT_OF_MATCHES))%>%
  top_n(1)

# Q.12 Did India win mostly by Chasing or Playing First

for (i in 1:length(CRICKETDATA$BYRUNS)){
  if (complete.cases(CRICKETDATA[i,'BYRUNS'])){
    CRICKETDATA[i,'BATTINGOPTION']="BATTING FIRST"
  }else{
    CRICKETDATA[i,'BATTINGOPTION']="CHASED"
  }
}


CRICKETDATA%>%
  filter(Winner=="India")%>%
  group_by(BATTINGOPTION)%>%
  summarize(COUNT_OF_MATCHES=n())%>%
  arrange(desc(COUNT_OF_MATCHES))


# Q.13 Top 3 Countries who won most matches in 2018 and what was their winning%


WINNING_PERCENTAGE=inner_join(TOTALMATCHESBYCOUNTRY,TOTALWONBYCOUNTRY)
WINNING_PERCENTAGE=cbind(WINNING_PERCENTAGE,WinningPercentage=(WINNING_PERCENTAGE$MatchesWon/WINNING_PERCENTAGE$MatchesPLayed*100))

WINNING_PERCENTAGE%>%
  mutate(WinningPercentage=(WINNING_PERCENTAGE$MatchesWon/WINNING_PERCENTAGE$MatchesPLayed*100))%>%
  arrange(desc(MatchesWon))%>%
  head(3)
