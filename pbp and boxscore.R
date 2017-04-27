

setwd("C:/Users/G/Desktop/NBA R Code/NBA R Code")


rm(list=ls())

a=read.csv('a15a.csv')

b=read.csv('b15a.csv')

c=read.csv('out15.csv')

d=merge(a,c,by=c('game_id','player_id'))

e=merge(b,c,by=c('game_id','player_id'))

ff=read.csv('travel15.csv')

want=c(3,4,75,76,83,84,86,87)

f=ff[,want]

h=merge(d,f,by=c('game_id','team_id'))

i=merge(e,f,by=c('game_id','team_id'))

want2=c(1,2,5,6,8,44)

hh=h[,want2]

go=unique(hh)

want2=c(1,2,5,6,8,44)

ii=i[,want2]

go2=unique(ii)

gogo=merge(go,go2,by='game_id')

gogo$total_poss=gogo$total_home_poss+gogo$total_away_poss


out=NULL


gogo$date=as.Date(gogo$dates.y)

gogo$numberdates=gogo$date-as.Date("2015-10-27")




for (z in 0:200){
  
  z=200
  heyyo2=gogo[gogo$numberdates <=z,]
  
  
  
  mat=matrix(0,nrow=30,ncol=30)
  mat2=matrix(0,nrow=30,ncol=30)
  
  for (i in 1:30){
    for (j in 1:30){
      
      i=2
      j=1
      home=heyyo2[heyyo2$home_id==1610612736+i & heyyo2$away_id==1610612736+j,]
      
      away=heyyo2[heyyo2$away_id==1610612736+i & heyyo2$home_id==1610612736+j,]
      
      
      x=home$total_poss
      y=away$total_poss
      
      l=c(x,y)
      
      
      yo=sum(l)
      yo2=as.numeric(length(l))
      
      mat[i,j]=yo
      mat2[i,j]=yo2
    }
  }
  
  avgposs=sum(mat)/sum(mat2)
  
  tot=NA
  for (i in 1:30){
    
    tot[i]=sum(mat[i,1:30])
    
    
  }
  
  
  
  
  tot2=NA
  for (i in 1:30){
    
    tot2[i]=sum(mat2[i,1:30])
    
    
  }
  
  k=(tot/tot2)/avgposs
  
  first=k
  
  
  
  finalscore=NULL
  
  for ( i in 1:500){
    for (j in 1:30){
      x=i
      adj=mat[j,]/k
      
      adjsum=sum(adj)
      totgames=sum(mat2[j,])
      
      adjfinal=adjsum/totgames
      
      finalscore[j]=adjfinal/avgposs
    }
    
    k=finalscore
  }
  total3=c(finalscore*avgposs,z)
  
  out=rbind(out,total3)
  
}




