


setwd("C:/Users/G/Desktop/NBA R Code/NBA R Code")

library(rvest)


library(rjson)


library(dplyr)


rm(list=ls())
freethrow1=NULL
freethrowhome1=NULL
freethrowaway1=NULL
checkitout=NULL
problem=NULL
problem2=NULL
problem3=NULL
problem4=NULL
problem5=NULL
problem6=NULL
problemd=NULL
problemd2=NULL
problemd3=NULL
problemd4=NULL
problemd5=NULL
problemd6=NULL
okayhome=NULL
okayaway=NULL
out=NULL
game_id=NULL
team_id=NULL
team_abbrev=NULL
team_city=NULL
player_id=NULL
player_name=NULL
start_position=NULL
comment=NULL
min=NULL
fgm=NULL
fga=NULL
fg_pct=NULL
fg3m=NULL
fg3a=NULL
fg3_pct=NULL
ftm=NULL
fta=NULL
ft_pct=NULL
oreb=NULL
dreb=NULL
reb=NULL
ast=NULL
stl=NULL
blk=NULL
to=NULL
fouls=NULL
pts=NULL
plus_minus=NULL




library(rvest)


library(rjson)

for(v in 1:1230){
  
  game_id=NULL
  team_id=NULL
  team_abbrev=NULL
  team_city=NULL
  player_id=NULL
  player_name=NULL
  start_position=NULL
  comment=NULL
  min=NULL
  fgm=NULL
  fga=NULL
  fg_pct=NULL
  fg3m=NULL
  fg3a=NULL
  fg3_pct=NULL
  ftm=NULL
  fta=NULL
  ft_pct=NULL
  oreb=NULL
  dreb=NULL
  reb=NULL
  ast=NULL
  stl=NULL
  blk=NULL
  to=NULL
  fouls=NULL
  pts=NULL
  plus_minus=NULL
  
  
  urlchanger=21500000+v
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=0000", sep="")
  
  # read url and convert to data.frame
  document = fromJSON(file=url, method = "C", unexpected.escape = "error")
  
  s=length(document$resultSets[[1]]$rowSet)
  
  for (p in 1:s){
    for(q in 1:28){
      if(is.null(document$resultSets[[1]]$rowSet[[p]][[q]])){
        document$resultSets[[1]]$rowSet[[p]][[q]]=0
      }else{
        document$resultSets[[1]]$rowSet[[p]][[q]]=document$resultSets[[1]]$rowSet[[p]][[q]]
      }
      
    }
  }
  
  for (j in 1:s){
    game_id[j]=document$resultSets[[1]]$rowSet[[j]][[1]]
    team_id[j]=document$resultSets[[1]]$rowSet[[j]][[2]]
    team_abbrev[j]=document$resultSets[[1]]$rowSet[[j]][[3]]
    team_city[j]=document$resultSets[[1]]$rowSet[[j]][[4]]
    player_id[j]=document$resultSets[[1]]$rowSet[[j]][[5]]
    player_name[j]=document$resultSets[[1]]$rowSet[[j]][[6]]
    start_position[j]=document$resultSets[[1]]$rowSet[[j]][[7]]
    comment[j]=document$resultSets[[1]]$rowSet[[j]][[8]]
    min[j]=document$resultSets[[1]]$rowSet[[j]][[9]]
    fgm[j]=document$resultSets[[1]]$rowSet[[j]][[10]]
    fga[j]=document$resultSets[[1]]$rowSet[[j]][[11]]
    fg_pct[j]=document$resultSets[[1]]$rowSet[[j]][[12]]
    fg3m[j]=document$resultSets[[1]]$rowSet[[j]][[13]]
    fg3a[j]=document$resultSets[[1]]$rowSet[[j]][[14]]
    fg3_pct[j]=document$resultSets[[1]]$rowSet[[j]][[15]]
    ftm[j]=document$resultSets[[1]]$rowSet[[j]][[16]]
    fta[j]=document$resultSets[[1]]$rowSet[[j]][[17]]
    ft_pct[j]=document$resultSets[[1]]$rowSet[[j]][[18]]
    oreb[j]=document$resultSets[[1]]$rowSet[[j]][[19]]
    dreb[j]=document$resultSets[[1]]$rowSet[[j]][[20]]
    reb[j]=document$resultSets[[1]]$rowSet[[j]][[21]]
    ast[j]=document$resultSets[[1]]$rowSet[[j]][[22]]
    stl[j]=document$resultSets[[1]]$rowSet[[j]][[23]]
    blk[j]=document$resultSets[[1]]$rowSet[[j]][[24]]
    to[j]=document$resultSets[[1]]$rowSet[[j]][[25]]
    fouls[j]=document$resultSets[[1]]$rowSet[[j]][[26]]
    pts[j]=document$resultSets[[1]]$rowSet[[j]][[27]]
    plus_minus[j]=document$resultSets[[1]]$rowSet[[j]][[28]]
    
    
    
  }
  
  
  c=data.frame(                 game_id,
                                team_id,
                                team_abbrev,
                                team_city,
                                player_id,
                                player_name,
                                start_position,
                                comment,
                                min,
                                fgm,
                                fga,
                                fg_pct,
                                fg3m,
                                fg3a,
                                fg3_pct,
                                ftm,
                                fta,
                                ft_pct,
                                oreb,
                                dreb,
                                reb,
                                ast,
                                stl,
                                blk,
                                to,
                                fouls,
                                pts,
                                plus_minus
                                
  )
  
  out=rbind(out,c)
  
  
  
  urlchanger=21500000+v
  
  
  
  url <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=00", urlchanger,"&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  document = fromJSON(file=url, method = "C", unexpected.escape = "error")
  s=length(document$resultSets[[1]]$rowSet)
  
  one=NA
  ggg=NA
  for(j in 1:s){
    
    play1=document$resultSets[[1]]$rowSet[[j]]
    
    g=rbind(play1)
    
    g3=data.frame(g)
    
    one=rbind(one,g3)
  }
  
  one <- data.frame(lapply(one, as.character), stringsAsFactors=FALSE)
  
  one$possesion=NA
  
  one$X5=as.numeric(one$X5)
  
  one=one[one$X5!=5 & one$X5!=6 & one$X5!=7 & one$X5!=8 & one$X5!=9 & one$X5!=10 & one$X5!=11 & one$X5!=12,]
  
  #### shots that resulted in free throws that were not counted as shots
  
  one$homefreethrow=0
  one$awayfreethrow=0
  
  yyy=which(grepl('S.FOUL',one$X10))
  
  
  j=which(!grepl('PTS',one$X8[yyy-1],ignore.case=TRUE))
  
  k=yyy[j]
  
  
  khjk=one[k,]
  
  y=one$X14
  
  y=unique(y)
  
  x=which(y==0|is.na(y))
  
  yy=y[-x]
  
  ssss=length(yy)
  ssss=as.numeric(ssss)
  
  khjk$numberitup=1
  
  finalfreethrow2=NULL
  
  for (z in 1:ssss){
    
    code=yy[z]
    
    
    
    playerindex=which(khjk$X21==code)
    
    codeindex=khjk[playerindex,]
    
    timesfouledshooting=sum(codeindex$numberitup)
    
    
    game_id=one$X1[3]
    player_id=code
    player_free_throw=timesfouledshooting
    
    
    finalfreethrow=cbind(game_id,player_id,player_free_throw)
    
    finalfreethrow2=rbind(finalfreethrow2,finalfreethrow)
    
  }
  
 
  
  freethrowfin1=data.frame(finalfreethrow2)
  
  
  freethrow1=rbind(freethrow1,freethrowfin1)
  
 
  

  
}

write.csv(freethrow1,'2105freethrow.csv')
