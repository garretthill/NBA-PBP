
setwd("C:/Users/G/Desktop/NBA R Code/NBA R Code")

library(rvest)


library(rjson)


library(dplyr)


rm(list=ls())
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

### Find out who got possesion of the jump ball
y=which(grepl('Jump Ball',one$X8))
one$possesion[y]=one$X32[y]

for (i in y) {
  if (identical(one$X28[i],one$X16[3])) {
    
    one$possesion[i]=one$X18[3]
    
  } 
}

for (i in y) {
if (identical(one$X28[i],one$X23[3])) {
  
  one$possesion[i]=one$X25[3]
  
} 
}


### Find all missed shots by the home team, where the shot was not the first of 2 free throws or the first or second of three free throws
y=which(grepl('MISS',one$X8) & !grepl('1 of 2',one$X8) & !grepl('1 of 3',one$X8) & !grepl('2 of 3',one$X8) & !grepl('Technical',one$X8,ignore.case=TRUE))

### All plays directly after a miss by the hawks that leads to a live ball
x=y+1



one$changer=0

### Set the changer variable so that we can tell which plays were hawks misses leading to a live ball

one$changer[x]=1


### Find which missed hawks shots lead to a pistons rebound or loose ball foul on the 

m=which(grepl('REBOUND',one$X10,ignore.case=TRUE) & one$changer>0)

n=which(grepl('FOUL',one$X8,ignore.case=TRUE) & one$changer>0)

### Indicate that possesion is now pistons ball

one$possesion[m]=one$X25[3]




### Redo for pistons shots

y=which(grepl('MISS',one$X10) & !grepl('1 of 2',one$X10) & !grepl('1 of 3',one$X10) & !grepl('2 of 3',one$X10) & !grepl('Technical',one$X10,ignore.case=TRUE))

### All plays directly after a miss by the hawks that leads to a live ball
x=y+1



one$changer=0

### Set the changer variable so that we can tell which plays were hawks misses leading to a live ball

one$changer[x]=1


### Find which missed hawks shots lead to a pistons rebound or loose ball foul on the 

m=which(grepl('REBOUND',one$X8,ignore.case=TRUE) & one$changer>0)

n=which(grepl('FOUL',one$X10,ignore.case=TRUE) & one$changer>0)

### Indicate that possesion is now pistons ball

one$possesion[m]=one$X18[3]




### dealing with turnovers

y=which(grepl('Turnover',one$X10,ignore.case=TRUE))

one$possesion[y]=one$X18[3]

y=which(grepl('Turnover',one$X8,ignore.case=TRUE))

one$possesion[y]=one$X25[3]


### dealing with made shots by the pistons

y=which(grepl('shot|dunk|layup|jumper|running jump|floating jump',one$X10,ignore.case=TRUE) & !grepl('miss',one$X10,ignore.case=TRUE))
j=which(grepl('S.FOUL',one$X8[y+1],ignore.case=TRUE))

k=y[j]



one$possesion[y]=one$X18[3]
one$possesion[k]=NA


### dealing with made shots by the hawks

y=which(grepl('shot|dunk|layup|jumper|running jump|floating jump',one$X8,ignore.case=TRUE) & !grepl('miss',one$X8,ignore.case=TRUE))
j=which(grepl('S.FOUL',one$X10[y+1],ignore.case=TRUE))

k=y[j]


one$possesion[y]=one$X25[3]
one$possesion[k]=NA




### dealing with made free throws by the pistons, if free throw is last they will shoot that possesion

y=which(grepl('free throw',one$X10,ignore.case=TRUE) & !grepl('miss',one$X10,ignore.case=TRUE) & grepl('1 of 1|2 of 2|3 of 3',one$X10,ignore.case=TRUE))

one$possesion[y]=one$X18[3]

### dealing with made free throws by the hawks, if free throw is last they will shoot that possesion

y=which(grepl('free throw',one$X8,ignore.case=TRUE) & !grepl('miss',one$X8,ignore.case=TRUE) & grepl('1 of 1|2 of 2|3 of 3',one$X8,ignore.case=TRUE))

one$possesion[y]=one$X25[3]



### dealing with start of quarters


x=which(grepl('12',one$X3,ignore.case=TRUE) & grepl('2',one$X5,ignore.case=TRUE))
y=which(grepl('12',one$X3,ignore.case=TRUE) & grepl('3',one$X5,ignore.case=TRUE))
z=which(grepl('12',one$X3,ignore.case=TRUE) & grepl('4',one$X5,ignore.case=TRUE))

if (identical(one$X25[3],one$possesion[3])) {
  one$possesion[x]=one$X18[3]
  one$possesion[y]=one$X18[3]
  one$possesion[z]=one$X25[3]
} else {
  one$possesion[x]=one$X25[3]
  one$possesion[y]=one$X25[3]
  one$possesion[z]=one$X18[3]
}



### possesion tracker, who has the ball at every event

one$possesiontrack=one$possesion

for (i in 1:1000){
  k=which((is.na(one$possesiontrack)))
  
  l=k[-2]
  m=l[-1]
  
  one$possesiontrack[m]=one$possesion[m-i]
}


### Dealing with extra possesions caused by jump balls

y=which(grepl('Jump Ball',one$X8))

for (i in y) {
  if (identical(one$possesion[i],one$possesiontrack[i-1])) {
    
    one$possesion[i]=NA
    
  } 
}

y=which(one$X7=='0:05'|one$X7=='0:04'|one$X7=='0:03'|one$X7=='0:02'|one$X7=='0:01'|one$X7=='0:00')

one$possesion[y]=NA




one$homeplayer1=NA
one$homeplayer2=NA
one$homeplayer3=NA
one$homeplayer4=NA
one$homeplayer5=NA



one$awayplayer1=NA
one$awayplayer2=NA
one$awayplayer3=NA
one$awayplayer4=NA
one$awayplayer5=NA

out2=out[out$start_position!='',]


one$homeplayer1=out2$player_id[6]
one$homeplayer2=out2$player_id[7]
one$homeplayer3=out2$player_id[8]
one$homeplayer4=out2$player_id[9]
one$homeplayer5=out2$player_id[10]



one$awayplayer1=out2$player_id[1]
one$awayplayer2=out2$player_id[2]
one$awayplayer3=out2$player_id[3]
one$awayplayer4=out2$player_id[4]
one$awayplayer5=out2$player_id[5]

j=which(one$X7=='12:00' & one$X5=='2')

j=as.numeric(j)

j=min(j)

totallength=length(one$homeplayer1)

totallength=as.numeric(totallength)


for(i in 1:j){
  
if (one$X14[i]==one$homeplayer1[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
{
  one$homeplayer1[i:j]=one$X21[i]
  
}
  
  if (one$X14[i]==one$homeplayer2[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer3[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer4[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer5[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  if (one$X14[i]==one$awayplayer1[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer2[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer3[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer4[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer5[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer5[i:j]=one$X21[i]
    
  }
  
  
  
    
}


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
url

urlchanger=21500000+v

url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=7500&GameID=00",urlchanger,"&RangeType=2&Season=2014-15&SeasonType=Regular+Season&StartPeriod=1&StartRange=7300", sep="")
# read url and convert to data.frame
document = fromJSON(file=url, method = "C", unexpected.escape = "error")

s=length(document$resultSets[[1]]$rowSet)
if (s>0){
for (p in 1:s){
  for(q in 1:28){
    if(is.null(document$resultSets[[1]]$rowSet[[p]][[q]])){
      document$resultSets[[1]]$rowSet[[p]][[q]]=0
    }else{
      document$resultSets[[1]]$rowSet[[p]][[q]]=document$resultSets[[1]]$rowSet[[p]][[q]]
    }
    
  }
}

for (o in 1:s){
  game_id[o]=document$resultSets[[1]]$rowSet[[o]][[1]]
  team_id[o]=document$resultSets[[1]]$rowSet[[o]][[2]]
  team_abbrev[o]=document$resultSets[[1]]$rowSet[[o]][[3]]
  team_city[o]=document$resultSets[[1]]$rowSet[[o]][[4]]
  player_id[o]=document$resultSets[[1]]$rowSet[[o]][[5]]
  player_name[o]=document$resultSets[[1]]$rowSet[[o]][[6]]
  start_position[o]=document$resultSets[[1]]$rowSet[[o]][[7]]
  comment[o]=document$resultSets[[1]]$rowSet[[o]][[8]]
  min[o]=document$resultSets[[1]]$rowSet[[o]][[9]]
  fgm[o]=document$resultSets[[1]]$rowSet[[o]][[10]]
  fga[o]=document$resultSets[[1]]$rowSet[[o]][[11]]
  fg_pct[o]=document$resultSets[[1]]$rowSet[[o]][[12]]
  fg3m[o]=document$resultSets[[1]]$rowSet[[o]][[13]]
  fg3a[o]=document$resultSets[[1]]$rowSet[[o]][[14]]
  fg3_pct[o]=document$resultSets[[1]]$rowSet[[o]][[15]]
  ftm[o]=document$resultSets[[1]]$rowSet[[o]][[16]]
  fta[o]=document$resultSets[[1]]$rowSet[[o]][[17]]
  ft_pct[o]=document$resultSets[[1]]$rowSet[[o]][[18]]
  oreb[o]=document$resultSets[[1]]$rowSet[[o]][[19]]
  dreb[o]=document$resultSets[[1]]$rowSet[[o]][[20]]
  reb[o]=document$resultSets[[1]]$rowSet[[o]][[21]]
  ast[o]=document$resultSets[[1]]$rowSet[[o]][[22]]
  stl[o]=document$resultSets[[1]]$rowSet[[o]][[23]]
  blk[o]=document$resultSets[[1]]$rowSet[[o]][[24]]
  to[o]=document$resultSets[[1]]$rowSet[[o]][[25]]
  fouls[o]=document$resultSets[[1]]$rowSet[[o]][[26]]
  pts[o]=document$resultSets[[1]]$rowSet[[o]][[27]]
  plus_minus[o]=document$resultSets[[1]]$rowSet[[o]][[28]]
  
  
  
}
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

c$seconds=gsub(".*:","",c$min)

c$seconds=as.numeric(c$seconds)

if (nrow(c)!=10 & min(c$seconds)>5)
 {
   problem=c
 }

problem2=rbind(problem,problem2)

d=c[c$seconds>=max(c$seconds)-5,]

if (NROW(d)!=10 )
{
  problemd=d
}

if (NROW(d)==10) {
problemd2=rbind(problemd,problemd2)
one$awayplayer1[j:totallength]=d$player_id[1]
one$awayplayer2[j:totallength]=d$player_id[2]
one$awayplayer3[j:totallength]=d$player_id[3]
one$awayplayer4[j:totallength]=d$player_id[4]
one$awayplayer5[j:totallength]=d$player_id[5]
one$homeplayer1[j:totallength]=d$player_id[6]
one$homeplayer2[j:totallength]=d$player_id[7]
one$homeplayer3[j:totallength]=d$player_id[8]
one$homeplayer4[j:totallength]=d$player_id[9]
one$homeplayer5[j:totallength]=d$player_id[10]

jj=j

j=which(one$X7=='12:00' & one$X5=='3')

j=as.numeric(j)

j=min(j)



rowsnum=as.numeric(nrow(d))

if (rowsnum==10){

for(i in jj:j){
  
  if (one$X14[i]==one$homeplayer1[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer2[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer3[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer4[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer5[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  if (one$X14[i]==one$awayplayer1[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer2[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer3[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer4[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer5[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  
}
  
}

}


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
url

url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=14653&GameID=00",urlchanger,"&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=14423", sep="")
# read url and convert to data.frame
document = fromJSON(file=url, method = "C", unexpected.escape = "error")

s=length(document$resultSets[[1]]$rowSet)
s>0

if (s>0){

for (p in 1:s){
  for(q in 1:28){
    if(is.null(document$resultSets[[1]]$rowSet[[p]][[q]])){
      document$resultSets[[1]]$rowSet[[p]][[q]]=0
    }else{
      document$resultSets[[1]]$rowSet[[p]][[q]]=document$resultSets[[1]]$rowSet[[p]][[q]]
    }
    
  }
}

for (o in 1:s){
  game_id[o]=document$resultSets[[1]]$rowSet[[o]][[1]]
  team_id[o]=document$resultSets[[1]]$rowSet[[o]][[2]]
  team_abbrev[o]=document$resultSets[[1]]$rowSet[[o]][[3]]
  team_city[o]=document$resultSets[[1]]$rowSet[[o]][[4]]
  player_id[o]=document$resultSets[[1]]$rowSet[[o]][[5]]
  player_name[o]=document$resultSets[[1]]$rowSet[[o]][[6]]
  start_position[o]=document$resultSets[[1]]$rowSet[[o]][[7]]
  comment[o]=document$resultSets[[1]]$rowSet[[o]][[8]]
  min[o]=document$resultSets[[1]]$rowSet[[o]][[9]]
  fgm[o]=document$resultSets[[1]]$rowSet[[o]][[10]]
  fga[o]=document$resultSets[[1]]$rowSet[[o]][[11]]
  fg_pct[o]=document$resultSets[[1]]$rowSet[[o]][[12]]
  fg3m[o]=document$resultSets[[1]]$rowSet[[o]][[13]]
  fg3a[o]=document$resultSets[[1]]$rowSet[[o]][[14]]
  fg3_pct[o]=document$resultSets[[1]]$rowSet[[o]][[15]]
  ftm[o]=document$resultSets[[1]]$rowSet[[o]][[16]]
  fta[o]=document$resultSets[[1]]$rowSet[[o]][[17]]
  ft_pct[o]=document$resultSets[[1]]$rowSet[[o]][[18]]
  oreb[o]=document$resultSets[[1]]$rowSet[[o]][[19]]
  dreb[o]=document$resultSets[[1]]$rowSet[[o]][[20]]
  reb[o]=document$resultSets[[1]]$rowSet[[o]][[21]]
  ast[o]=document$resultSets[[1]]$rowSet[[o]][[22]]
  stl[o]=document$resultSets[[1]]$rowSet[[o]][[23]]
  blk[o]=document$resultSets[[1]]$rowSet[[o]][[24]]
  to[o]=document$resultSets[[1]]$rowSet[[o]][[25]]
  fouls[o]=document$resultSets[[1]]$rowSet[[o]][[26]]
  pts[o]=document$resultSets[[1]]$rowSet[[o]][[27]]
  plus_minus[o]=document$resultSets[[1]]$rowSet[[o]][[28]]
  
  
  
}
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

c$seconds=gsub(".*:","",c$min)

c$seconds=as.numeric(c$seconds)

if (nrow(c)!=10 & min(c$seconds)>5)
{
  problem3=c
}

problem4=rbind(problem3,problem4)

d=c[c$seconds>=5,]

if (NROW(d)!=10 )
{
  problemd3=d
}

problemd4=rbind(problemd3,problemd4)
if (NROW(d)==10) {
  
one$awayplayer1[j:totallength]=d$player_id[1]
one$awayplayer2[j:totallength]=d$player_id[2]
one$awayplayer3[j:totallength]=d$player_id[3]
one$awayplayer4[j:totallength]=d$player_id[4]
one$awayplayer5[j:totallength]=d$player_id[5]
one$homeplayer1[j:totallength]=d$player_id[6]
one$homeplayer2[j:totallength]=d$player_id[7]
one$homeplayer3[j:totallength]=d$player_id[8]
one$homeplayer4[j:totallength]=d$player_id[9]
one$homeplayer5[j:totallength]=d$player_id[10]

jjj=j

j=which(one$X7=='12:00' & one$X5=='4')

j=as.numeric(j)

j=min(j)


for(i in jjj:j){
  
  if (one$X14[i]==one$homeplayer1[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer2[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer3[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer4[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer5[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  if (one$X14[i]==one$awayplayer1[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer2[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer3[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer4[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer5[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  
}
}

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


url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=21895&GameID=00",urlchanger,"&RangeType=2&Season=2015-16&SeasonType=Regular+Season&StartPeriod=1&StartRange=21631", sep="")
# read url and convert to data.frame
document = fromJSON(file=url, method = "C", unexpected.escape = "error")

s=length(document$resultSets[[1]]$rowSet)
if (s>0){
for (p in 1:s){
  for(q in 1:28){
    if(is.null(document$resultSets[[1]]$rowSet[[p]][[q]])){
      document$resultSets[[1]]$rowSet[[p]][[q]]=0
    }else{
      document$resultSets[[1]]$rowSet[[p]][[q]]=document$resultSets[[1]]$rowSet[[p]][[q]]
    }
    
  }
}


for (o in 1:s){
  game_id[o]=document$resultSets[[1]]$rowSet[[o]][[1]]
  team_id[o]=document$resultSets[[1]]$rowSet[[o]][[2]]
  team_abbrev[o]=document$resultSets[[1]]$rowSet[[o]][[3]]
  team_city[o]=document$resultSets[[1]]$rowSet[[o]][[4]]
  player_id[o]=document$resultSets[[1]]$rowSet[[o]][[5]]
  player_name[o]=document$resultSets[[1]]$rowSet[[o]][[6]]
  start_position[o]=document$resultSets[[1]]$rowSet[[o]][[7]]
  comment[o]=document$resultSets[[1]]$rowSet[[o]][[8]]
  min[o]=document$resultSets[[1]]$rowSet[[o]][[9]]
  fgm[o]=document$resultSets[[1]]$rowSet[[o]][[10]]
  fga[o]=document$resultSets[[1]]$rowSet[[o]][[11]]
  fg_pct[o]=document$resultSets[[1]]$rowSet[[o]][[12]]
  fg3m[o]=document$resultSets[[1]]$rowSet[[o]][[13]]
  fg3a[o]=document$resultSets[[1]]$rowSet[[o]][[14]]
  fg3_pct[o]=document$resultSets[[1]]$rowSet[[o]][[15]]
  ftm[o]=document$resultSets[[1]]$rowSet[[o]][[16]]
  fta[o]=document$resultSets[[1]]$rowSet[[o]][[17]]
  ft_pct[o]=document$resultSets[[1]]$rowSet[[o]][[18]]
  oreb[o]=document$resultSets[[1]]$rowSet[[o]][[19]]
  dreb[o]=document$resultSets[[1]]$rowSet[[o]][[20]]
  reb[o]=document$resultSets[[1]]$rowSet[[o]][[21]]
  ast[o]=document$resultSets[[1]]$rowSet[[o]][[22]]
  stl[o]=document$resultSets[[1]]$rowSet[[o]][[23]]
  blk[o]=document$resultSets[[1]]$rowSet[[o]][[24]]
  to[o]=document$resultSets[[1]]$rowSet[[o]][[25]]
  fouls[o]=document$resultSets[[1]]$rowSet[[o]][[26]]
  pts[o]=document$resultSets[[1]]$rowSet[[o]][[27]]
  plus_minus[o]=document$resultSets[[1]]$rowSet[[o]][[28]]
  
  
  
}
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

c$seconds=gsub(".*:","",c$min)

c$seconds=as.numeric(c$seconds)

if (nrow(c)!=10 & min(c$seconds)>5)
{
  problem5=c 
}




problem6=rbind(problem5,problem6)

d=c[c$seconds>=5,]

if (NROW(d)!=10 )
{
  problemd=d
}



problemd6=rbind(problemd5,problemd6)


if (NROW(d)==10) {

one$awayplayer1[j:totallength]=d$player_id[1]
one$awayplayer2[j:totallength]=d$player_id[2]
one$awayplayer3[j:totallength]=d$player_id[3]
one$awayplayer4[j:totallength]=d$player_id[4]
one$awayplayer5[j:totallength]=d$player_id[5]
one$homeplayer1[j:totallength]=d$player_id[6]
one$homeplayer2[j:totallength]=d$player_id[7]
one$homeplayer3[j:totallength]=d$player_id[8]
one$homeplayer4[j:totallength]=d$player_id[9]
one$homeplayer5[j:totallength]=d$player_id[10]
jjjj=j

j=length(one$awayplayer5)

j=as.numeric(j)



for(i in jjjj:j){
  
  if (one$X14[i]==one$homeplayer1[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer2[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer3[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer4[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$homeplayer5[i] & grepl('sub',one$X8[i],ignore.case=TRUE) )
  {
    one$homeplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  if (one$X14[i]==one$awayplayer1[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer1[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer2[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer2[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer3[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer3[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer4[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer4[i:j]=one$X21[i]
    
  }
  
  if (one$X14[i]==one$awayplayer5[i] & grepl('sub',one$X10[i],ignore.case=TRUE) )
  {
    one$awayplayer5[i:j]=one$X21[i]
    
  }
  
  
  
  
}
}

home1=which(is.na(one$homeplayer1))
home2=which(is.na(one$homeplayer2))
home3=which(is.na(one$homeplayer3))
home4=which(is.na(one$homeplayer4))
home5=which(is.na(one$homeplayer5))
away1=which(is.na(one$awayplayer1))
away2=which(is.na(one$awayplayer2))
away3=which(is.na(one$awayplayer3))
away4=which(is.na(one$awayplayer4))
away5=which(is.na(one$awayplayer5))

one$homeplayer1[home1]=1
one$homeplayer2[home2]=1
one$homeplayer3[home3]=1
one$homeplayer4[home4]=1
one$homeplayer5[home5]=1

one$awayplayer1[away1]=1
one$awayplayer2[away2]=1
one$awayplayer3[away3]=1
one$awayplayer4[away4]=1
one$awayplayer5[away5]=1


one$awayposscounter=0

one$homeposscounter=0

home=which(one$possesion==one$X18[3])

away=which(one$possesion==one$X25[3])

one$awayposscounter[away]=1

one$homeposscounter[home]=1

x=c(one$awayplayer1,one$awayplayer2,one$awayplayer3,one$awayplayer4,one$awayplayer5)

x=unique(x)




q=which(grepl('MISS',one$X8) & !grepl('1 of 2',one$X8) & !grepl('1 of 3',one$X8) & !grepl('2 of 3',one$X8) & !grepl('Technical',one$X8,ignore.case=TRUE))

one$awaydefreboppcounter=0
one$awaydefreboppcounter[q]=1

q=which(grepl('MISS',one$X10) & !grepl('1 of 2',one$X10) & !grepl('1 of 3',one$X10) & !grepl('2 of 3',one$X10) & !grepl('Technical',one$X10,ignore.case=TRUE))

one$homedefreboppcounter=0
one$homedefreboppcounter[q]=1


one$awayoffreboppcounter=one$homedefreboppcounter

one$homeoffreboppcounter=one$awaydefreboppcounter






q=which(grepl('MISS',one$X8) & grepl('free throw',one$X8,ignore.case=TRUE) & !grepl('1 of 2',one$X8) & !grepl('1 of 3',one$X8) & !grepl('2 of 3',one$X8) & !grepl('Technical',one$X8,ignore.case=TRUE))

qq=one[q,]

one$awayftdefreboppcounter=0
one$awayftdefreboppcounter[q]=1

q=which(grepl('MISS',one$X10) & grepl('free throw',one$X10,ignore.case=TRUE) & !grepl('1 of 2',one$X10) & !grepl('1 of 3',one$X10) & !grepl('2 of 3',one$X10) & !grepl('Technical',one$X10,ignore.case=TRUE))

one$homeftdefreboppcounter=0
one$homeftdefreboppcounter[q]=1


one$awayftoffreboppcounter=one$homeftdefreboppcounter

one$homeftoffreboppcounter=one$awayftdefreboppcounter







y=c(one$homeplayer1,one$homeplayer2,one$homeplayer3,one$homeplayer4,one$homeplayer5)

y=unique(y)

ssss=length(y)
ssss=as.numeric(ssss)

final2=NULL

for (z in 1:ssss){
  
  code=y[z]
  
  
  
  playerindex=which(one$homeplayer1==code|one$homeplayer2==code|one$homeplayer3==code|one$homeplayer4==code|one$homeplayer5==code)
  
  codeindex=one[playerindex,]
  
  possesionsforcode=sum(codeindex$homeposscounter)
  
  
  game_id=one$X1[3]
  home_id=one$X16[3]
  home_name=one$X18[3]
  player_id=code
  player_poss=sum(codeindex$homeposscounter)
  player_defrebopp=sum(codeindex$homedefreboppcounter)
  player_ftdefrebopp=sum(codeindex$homeftdefreboppcounter)
  player_offrebopp=sum(codeindex$homeoffreboppcounter)
  player_ftoffrebopp=sum(codeindex$homeftoffreboppcounter)
  total_home_defrebopp=sum(one$homedefreboppcounter)
  total_home_ftdefrebopp=sum(one$homeftdefreboppcounter)
  total_home_offrebopp=sum(one$homeoffreboppcounter)
  total_home_ftoffrebopp=sum(one$homeftoffreboppcounter)
  total_home_poss=sum(one$homeposscounter)
  
  final=cbind(game_id,home_id,home_name,player_id,player_poss,total_home_poss,player_defrebopp,player_ftdefrebopp,player_offrebopp,player_ftoffrebopp,
              total_home_defrebopp,total_home_ftdefrebopp,total_home_offrebopp,
              total_home_ftoffrebopp)
  
  final2=rbind(final2,final)
  
 
  
}

y=c(one$awayplayer1,one$awayplayer2,one$awayplayer3,one$awayplayer4,one$awayplayer5)

y=unique(y)

ssss=length(y)
ssss=as.numeric(ssss)

final4=NULL

for (z in 1:ssss){
  
  code=y[z]
  
  
  
  playerindex=which(one$awayplayer1==code|one$awayplayer2==code|one$awayplayer3==code|one$awayplayer4==code|one$awayplayer5==code)
  
  codeindex=one[playerindex,]
  
  possesionsforcode=sum(codeindex$awayposscounter)
  
  reboundoppforcode=sum(codeindex$awayreboppcounter)
  
  ftreboundoppforcode=sum(codeindex$awayftreboppcounter)
  
  
  game_id=one$X1[3]
  away_id=one$X23[3]
  away_name=one$X25[3]
  player_id=code
  player_poss=sum(codeindex$awayposscounter)
  player_defrebopp=sum(codeindex$awaydefreboppcounter)
  player_ftdefrebopp=sum(codeindex$awayftdefreboppcounter)
  player_offrebopp=sum(codeindex$awayoffreboppcounter)
  player_ftoffrebopp=sum(codeindex$awayftoffreboppcounter)
  total_away_defrebopp=sum(one$awaydefreboppcounter)
  total_away_ftdefrebopp=sum(one$awayftdefreboppcounter)
  total_away_offrebopp=sum(one$awayoffreboppcounter)
  total_away_ftoffrebopp=sum(one$awayftoffreboppcounter)
  total_away_poss=sum(one$awayposscounter)
  
  final3=cbind(game_id,away_id,away_name,player_id,player_poss,total_away_poss,player_defrebopp,player_ftdefrebopp,player_offrebopp,player_ftoffrebopp,
               total_away_defrebopp,total_away_ftdefrebopp,total_away_offrebopp,
               total_away_ftoffrebopp)
  
  final4=rbind(final4,final3)
  
}


fin1=data.frame(final2)
fin2=data.frame(final4)

okayhome=rbind(okayhome,fin1)

okayaway=rbind(okayaway,fin2)

okayhome[] <- lapply(okayhome, as.character)

okayaway[] <- lapply(okayaway, as.character)




x=which(one$X14!=one$homeplayer1 & one$X14!=one$homeplayer2 & one$X14!=one$homeplayer3 & one$X14!=one$homeplayer4 & one$X14!=one$homeplayer5 & 
          one$X14!=one$awayplayer1 & one$X14!=one$awayplayer2 & one$X14!=one$awayplayer3 & one$X14!=one$awayplayer4 & one$X14!=one$awayplayer5)

check=one[x,]

x=which(check$X15!='NULL')

check2=check[x,]

x=which(check2$X3!=8)

check3=check2[x,]

check3=check3[-1,]

checkitout=rbind(checkitout,check3)

}

write.csv(okayhome,'a15a.csv')

write.csv(okayaway,'b15a.csv')

write.csv(checkitout,'checkitout15.csv')

#### 402



x=which(checkitout$X3==18)

checkiutoutagain=checkitout[-x,]


jjj=unique(checkiutoutagain$X1)
