injuries=out[out$min==0,]

names=c('LeBron James','Joey Dorsey')

inj=injuries[injuries$player_name %in% names,  ] 

inj$yes=1
games=unique(out$game_id)
k=data.frame(games)


colnames(k) <- c("game_id")
m=merge(k,inj,by='game_id',all=T)





rm(list=ls())



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

for(i in 1:500){
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


  urlchanger=21600000+i
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2014-15&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  
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
}

out2=NULL
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



for(i in 501:1000){
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
  
  
  urlchanger=21400000+i
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2014-15&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  
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
  
  out2=rbind(out2,c)
}



out3=NULL
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






for(i in 1001:1230){
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
  
  
  urlchanger=21400000+i
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2014-15&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  
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
  
  out3=rbind(out3,c)
}

out14=rbind(out,out2,out3)

write.csv(out14,'out14.csv')



outya=NULL
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

for(i in 1:1230){
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
  
  
  urlchanger=21300000+i
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2013-14&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  
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
  
  outya=rbind(outya,c)
}

write.csv(outya,'out13.csv')







outya1=NULL
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

for(i in 1:1230){
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
  
  
  urlchanger=21200000+i
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2012-13&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  
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
  
  outya1=rbind(outya1,c)
}

write.csv(outya1,'out12.csv')




outya3=NULL
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

for(i in 1:1230){
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
  
  
  urlchanger=21500000+i
  
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=00",urlchanger,"&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  
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
  
  outya3=rbind(outya3,c)
}

write.csv(outya3,'out15.csv')
