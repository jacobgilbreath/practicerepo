library("readr")
urlfile="https://raw.githubusercontent.com/jacobgilbreath/practicerepo/main/high_diamond_ranked_10min.csv"
d=read.csv(url(urlfile))
head(d)
library("ggplot2")
library("tidyverse")
d$wins=as.factor(d$blueWins)
d$noisex=d$blueKills+(rnorm(9879)/100)
d$noisey=d$redKills+(rnorm(9879)/100)
d$redKDA=(d$redKills+d$redAssists)/d$redDeaths
d$blueKDA=(d$blueKills+d$blueAssists)/d$blueDeaths
d$kdadiff=d$blueKDA-d$redKDA

tower_perc=(nrow(blueWin)-nrow(blueWin[blueWin$blueTowersDestroyed==0,]))/nrow(blueWin)
one_tower_perc=nrow(blueWin[blueWin$blueTowersDestroyed==1,])/(nrow(blueWin)-nrow(blueWin[blueWin$blueTowersDestroyed==0,]))
tower_perc
one_tower_perc

#Red vs Blue Gold
ggplot(d,aes(x=blueTotalGold,y=redTotalGold))+geom_point(aes(color=blueWins))+
  geom_line(aes(x=seq(12500,22500,length.out=9879),y=seq(12500,22500,length.out=9879)),color="black")


#New DataFrame for Blue Win/Blue Loss
blueWin=d[d$blueWins==1,]
blueLoss=d[d$blueWins==0,]

windiff=blueWin$blueKills-blueWin$redKills
lossdiff=blueLoss$blueKills-blueLoss$redKills

wdeathdiff=blueWin$blueDeaths-blueWin$redDeaths
ldeathdiff=blueLoss$blueDeaths-blueLoss$redDeaths

winlvldiff=blueWin$blueAvgLevel-blueWin$redAvgLevel
losslvldiff=blueLoss$blueAvgLevel-blueLoss$redAvgLevel

#Average Level Difference(IN PROJECT)
avglvl=ggplot()+geom_histogram(data=blueWin,aes(x=winlvldiff),binwidth=.2,color="darkblue",fill="blue",alpha=0.75)+
  geom_histogram(data=blueLoss,aes(x=losslvldiff),binwidth=.2,color="darkred",fill="red",alpha=0.75)+
  xlab("Average Level Difference")+ylab("")+
  theme(plot.background = element_rect(fill="transparent",colour=NA),
        panel.background=element_rect(fill="transparent",colour=NA),
        panel.grid.major.y =element_blank(),
        panel.grid.major.x=element_blank(),
        legend.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_text(colour = "white",size=22),
        axis.title.y = element_text(colour = "white",size=22),
        legend.title = element_blank(),
        axis.text.x = element_text(colour = "white",size=12),
        axis.text.y = element_blank())
avglvl
ggsave("avglvl.png",avglvl,bg="transparent")

#Death Difference
ggplot()+geom_histogram(data=blueWin,aes(x=wdeathdiff),binwidth=1,color="darkblue",fill="blue",alpha=0.75)+
  geom_histogram(data=blueLoss,aes(x=ldeathdiff),binwidth=1,color="darkred",fill="red",alpha=0.75)+
  xlab("Death Difference")+ylab("")+
  geom_vline(xintercept = min(ldeathdiff),colour="darkgreen",linetype="dashed")+
  geom_vline(xintercept = max(wdeathdiff),colour="darkorange",linetype="dashed")+
  theme(panel.background = element_rect(fill = "transparent"))

#KDA Difference
ggplot()+geom_histogram(data=blueWin,aes(x=kdadiff),binwidth=2,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=kdadiff),binwidth=2,color="darkred",fill="red",alpha=0.7)+
  xlab("KDA Difference")+ylab("")

#GOLD DIFFERENCE (IN PROJECT)
golddiffp=ggplot()+geom_histogram(data=blueWin,aes(x=blueGoldDiff),binwidth=500,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=blueGoldDiff),binwidth=500,color="darkred",fill="red",alpha=0.7)+
  xlab("Gold Difference")+ylab("")+
  theme(plot.background = element_rect(fill="transparent",colour=NA),
        panel.background=element_rect(fill="transparent",colour=NA),
        panel.grid.major.y =element_blank(),
        panel.grid.major.x=element_blank(),
        legend.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_text(colour = "white",size=22),
        axis.title.y = element_text(colour = "white",size=22),
        legend.title = element_blank(),
        axis.text.x = element_text(colour = "white",size=12),
        axis.text.y = element_blank())
golddiffp
ggsave("golddiff.png",golddiffp,bg="transparent")


#CS vs EXP (IN PROJECT)
cspp=(d$blueCSPerMin*10)+d$blueTotalJungleMinionsKilled
cspp=cspp/5
d$cs_pp=cspp

expcsp=ggplot(data=d)+geom_point(aes(x=d$cs_pp,y=d$blueExperienceDiff,color=as.factor(d$blueWins)),show.legend = FALSE,size=0.8)+
  scale_color_manual(values=c("darkred","darkblue"))+
  geom_density_2d(aes(x=d$cs_pp,y=d$blueExperienceDiff),color="lightblue")+
  xlab("Minions per Player")+ylab("Experience Difference")+
  theme(plot.background = element_rect(fill="transparent",colour=NA),
        panel.background=element_rect(fill="beige"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_text(colour = "white",size=22),
        axis.title.y = element_text(colour = "white",size=22),
        legend.title = element_blank(),
        axis.text.x = element_text(colour = "white",size=10),
        axis.text.y = element_text(colour = "white",size=10))
expcsp
ggsave("expcsp.png",expcsp,bg="transparent")


#CS vs GOLD (IN PROJECT)


goldcsp=ggplot(data=d)+geom_point(aes(x=d$cs_pp,y=d$blueGoldDiff,color=as.factor(d$blueWins)),show.legend = FALSE,size=0.8)+
  scale_color_manual(values=c("darkred","darkblue"))+
  geom_density_2d(aes(x=d$cs_pp,y=d$blueGoldDiff),color="gold")+
  xlab("Minions per Player")+ylab("Gold Difference")+
  theme(plot.background = element_rect(fill="transparent",colour=NA),
        panel.background=element_rect(fill="beige"),
        legend.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_text(colour = "white",size=22),
        axis.title.y = element_text(colour = "white",size=22),
        legend.title = element_blank(),
        axis.text.x = element_text(colour = "white",size=10),
        axis.text.y = element_text(colour = "white",size=10))
goldcsp
ggsave("goldcs.png",goldcsp,bg="transparent")

#Deaths
ggplot()+geom_histogram(data=blueWin,aes(x=blueDeaths),binwidth=1,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=blueDeaths),binwidth=1,color="darkred",fill="red",alpha=0.7)+
  xlab("Deaths")+ylab("")
mean(blueLoss$blueDeaths)

#Average Level With Minimum
ggplot()+geom_histogram(data=blueWin,aes(x=blueAvgLevel),binwidth=0.2,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=blueAvgLevel),binwidth=0.2,color="darkred",fill="red",alpha=0.7)+
  geom_vline(xintercept = min(blueWin$blueAvgLevel),colour="darkblue",linetype="dashed")+
  xlab("Average Level")+ylab("")+theme(panel.background = element_rect(fill = "transparent"))

#Kills
ggplot()+geom_histogram(data=blueWin,aes(x=winlvldiff),binwidth=0.2,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=losslvldiff),binwidth=0.2,color="darkred",fill="red",alpha=0.7)+
  xlab("Level")+ylab("")

nbkills=d$blueKills+rnorm(9879, 0, 0.1)
nbdeaths=d$blueDeaths+rnorm(9879, 0, 0.1)


#New K v Death Scatter Plot
Kills=c()
Deaths=c()
ratio=c()
for (i in 0:15) {
  for (j in 0:15){
    k=d[((d$blueKills==i)&(d$blueDeaths==j)),]
    r=sum(k$blueWins)/length(k$blueWins)
    Kills=c(Kills,i)
    Deaths=c(Deaths,j)
    ratio=c(ratio,r)
  }
}
dk=data.frame(Kills)
dk$Deaths=Deaths
dk$Win_Ratio=ratio
p=ggplot(data=dk)+geom_point(aes(x=Deaths,y=Kills,color=Win_Ratio),size=3)+
  scale_color_gradient(low="red",high="blue")+
  theme(plot.background = element_rect(fill="transparent",colour=NA),
        panel.background=element_rect(fill="beige"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_text(colour = "white",size=18),
        axis.title.y = element_text(colour = "white",size=18),
        legend.title = element_text(colour = "white",size=14),
        legend.text = element_text(colour = "white",size=10),
        axis.text.x = element_text(colour = "white",size=10),
        axis.text.y = element_text(colour = "white",size=10),
        legend.key.size = unit(0.75, 'cm'))
p
ggsave("kdratio.png",p,bg="transparent")


#Old Kills Vs. Deaths Scatter
ggplot()+geom_point(data=d,aes(x=nbkills,y=nbdeaths,
                              colour=as.factor(d$blueWins)),show.legend=F,alpha=0.15,size=0.3)+
  geom_density_2d(data=blueLoss,aes(x=blueLoss$blueKills,y=blueLoss$blueDeaths),size=0.25,colour="darkred")+
  geom_density_2d(data=blueWin,aes(x=blueWin$blueKills,y=blueWin$blueDeaths),size=0.25,colour="darkblue")+
  scale_color_manual(values = c("red","blue"))+
  xlab("Kills")+ylab("Deaths")+theme(panel.background = element_rect(fill = "transparent"))


winkills=mean(blueWin$blueKills)
losskills=mean(blueLoss$blueKills)

windeaths=mean(blueWin$blueDeaths)
lossdeaths=mean(blueLoss$blueDeaths)
min(blueWin$blueAvgLevel)
min(blueWin$redAvgLevel)
min(blueLoss$blueAvgLevel)
min(blueLoss$redAvgLevel)

#CS Vs Jungle Minions
ggplot(data=d)+geom_point(aes(x=blueTotalMinionsKilled,y=blueTotalJungleMinionsKilled,
                              colour=as.factor(d$blueWins)),show.legend=F,alpha=0.25,size=0.5)+
  scale_color_manual(values = c("red","blue"))+
  xlab("CS")+ylab("Jungle Minions")+theme(panel.background = element_rect(fill = "transparent"))

#Lane Minions
ggplot()+geom_histogram(data=blueWin,aes(x=blueTotalMinionsKilled),bins=35,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=blueTotalMinionsKilled),bins=35,color="darkred",fill="red",alpha=0.7)+
  xlab("Lane Minions")+ylab("")+theme(panel.background = element_rect(fill = "transparent"))

#Jungle Minions
ggplot()+geom_histogram(data=blueWin,aes(x=blueTotalJungleMinionsKilled),bins=30,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=blueTotalJungleMinionsKilled),bins=30,color="darkred",fill="red",alpha=0.7)+
  xlab("Jungle Minions")+ylab("")+theme(panel.background = element_rect(fill = "transparent"))

#Gold
gold=ggplot()+geom_histogram(data=blueWin,aes(x=blueTotalGold),bins=30,color="darkblue",fill="blue",alpha=0.7)+
  geom_histogram(data=blueLoss,aes(x=blueTotalGold),bins=30,color="darkred",fill="red",alpha=0.7)+
  xlab("Gold")+ylab("")+theme(panel.background = element_rect(fill = "transparent"))+theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    axis.title.x = element_text(colour = "white")
  )
gold
ggsave("gold.png",gold,bg="transparent")
