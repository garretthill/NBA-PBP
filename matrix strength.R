setwd("C:/Users/G/Desktop/NBA R Code/NBA R Code")

rm(list=ls())

out=NULL
joined=read.csv('joined15 new.csv')

joined$betterDates <- as.Date(joined$Date,
                                format = "%a %b %d %Y")

joined$numberdates=joined$betterDates-as.Date("2015-10-27")
max(joined$numberdates)

newdata <- joined[order(joined$numberdates),]

newdata$numberdates

for (z in 0:200){


heyyo2=joined[joined$numberdates <=z,]



mat=matrix(0,nrow=30,ncol=30)
mat2=matrix(0,nrow=30,ncol=30)

for (i in 1:30){
  for (j in 1:30){
    
    
home=heyyo2[heyyo2$home_team_id==1610612736+i & heyyo2$away_team_id==1610612736+j,]

away=heyyo2[heyyo2$away_team_id==1610612736+i & heyyo2$home_team_id==1610612736+j,]


x=home$home_team_pace
y=away$away_team_pace

l=c(x,y)


yo=sum(l)
yo2=as.numeric(length(l))

mat[i,j]=yo
mat2[i,j]=yo2
}
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



adjpace15=out
write.csv(adjpace15,'adjpace15.csv')
