require(readxl)
library(googlesheets)
library(tidyverse)
options(warn=-1)

gs_auth()
sname <- gs_title('Fitzroy 2019')
temp <- gs_read(sname,ws='Stats')
temp <- subset(temp,!Stat%in%c('Turnover Goals','Stoppage Goals','Free Kick Goals'))

fixs <- gs_read(sname,ws='FI',range='A2:J23')
fixs <- subset(fixs,!(Round%in%c('F1','F2','F3')))

G <- as.numeric(sapply(fixs$Score,function(x) strsplit(x,'[.]')[[1]][1]))
B <- as.numeric(sapply(fixs$Score,function(x) strsplit(x,'[.]')[[1]][2]))
P <- as.numeric(sapply(fixs$Score,function(x) strsplit(x,'[.]')[[1]][3]))
OG <- as.numeric(sapply(fixs$Against,function(x) strsplit(x,'[.]')[[1]][1]))
OB <- as.numeric(sapply(fixs$Against,function(x) strsplit(x,'[.]')[[1]][2]))
OP <- as.numeric(sapply(fixs$Against,function(x) strsplit(x,'[.]')[[1]][3]))

SH <- G+B
OSH <- OG+OB
      
for(i in unique(temp$Stat)){
      temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat=i,For=NA,Against=NA,Diff=NA))
}
temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat='Scoring Shots',For=SH,Against=OSH,Diff=SH-OSH))
temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat='Points',For=P,Against=OP,Diff=P-OP))
temp <- subset(temp,!(is.na(For) & paste(Round,Stat)%in%with(subset(temp,!is.na(For)),paste(Round,Stat))))
temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat='Scores per IN50 %',
                              For=100*sapply(fixs$Round,function(x) temp$For[temp$Stat=='Scoring Shots' & temp$Round==x]/temp$For[temp$Stat=='Inside 50s' & temp$Round==x]),
                              Against=100*sapply(fixs$Round,function(x) temp$Against[temp$Stat=='Scoring Shots' & temp$Round==x]/temp$Against[temp$Stat=='Inside 50s' & temp$Round==x]),
                              Diff=sapply(fixs$Round,function(x) temp$For[temp$Stat=='Scoring Shots' & temp$Round==x]/temp$For[temp$Stat=='Inside 50s' & temp$Round==x])-sapply(fixs$Round,function(x) temp$Against[temp$Stat=='Scoring Shots' & temp$Round==x]/temp$Against[temp$Stat=='Inside 50s' & temp$Round==x]))
)
temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat='F50 Marks per IN50',
                              For=100*sapply(fixs$Round,function(x) temp$For[temp$Stat=='F50 Marks' & temp$Round==x]/temp$For[temp$Stat=='Inside 50s' & temp$Round==x]),
                              Against=100*sapply(fixs$Round,function(x) temp$Against[temp$Stat=='F50 Marks' & temp$Round==x]/temp$Against[temp$Stat=='Inside 50s' & temp$Round==x]),
                              Diff=100*sapply(fixs$Round,function(x) temp$For[temp$Stat=='F50 Marks' & temp$Round==x]/temp$For[temp$Stat=='Inside 50s' & temp$Round==x])-100*sapply(fixs$Round,function(x) temp$Against[temp$Stat=='F50 Marks' & temp$Round==x]/temp$Against[temp$Stat=='Inside 50s' & temp$Round==x]))
)
temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat='Int Marks per Oppo IN50',
                              For=100*sapply(fixs$Round,function(x) temp$For[temp$Stat=='Intercepts' & temp$Round==x]/temp$Against[temp$Stat=='Inside 50s' & temp$Round==x]),
                              Against=100*sapply(fixs$Round,function(x) temp$Against[temp$Stat=='Intercepts' & temp$Round==x]/temp$For[temp$Stat=='Inside 50s' & temp$Round==x]),
                              Diff=100*sapply(fixs$Round,function(x) temp$For[temp$Stat=='Intercepts' & temp$Round==x]/temp$Against[temp$Stat=='Inside 50s' & temp$Round==x])-100*sapply(fixs$Round,function(x) temp$Against[temp$Stat=='Intercepts' & temp$Round==x]/temp$For[temp$Stat=='Inside 50s' & temp$Round==x]))
)
temp <- rbind(temp,data.frame(Round=fixs$Round,Oppo=fixs$Opponent,Stat='All Clearances',
                              For=sapply(fixs$Round,function(x) temp$For[temp$Stat=='Centre Clearances' & temp$Round==x]+temp$For[temp$Stat=='BU+TI Clearances' & temp$Round==x]),
                              Against=sapply(fixs$Round,function(x) temp$Against[temp$Stat=='Centre Clearances' & temp$Round==x]+temp$Against[temp$Stat=='BU+TI Clearances' & temp$Round==x]),
                              Diff=sapply(fixs$Round,function(x) temp$For[temp$Stat=='Centre Clearances' & temp$Round==x]+temp$For[temp$Stat=='BU+TI Clearances' & temp$Round==x])-sapply(fixs$Round,function(x) temp$Against[temp$Stat=='Centre Clearances' & temp$Round==x]+temp$Against[temp$Stat=='BU+TI Clearances' & temp$Round==x]))
)


temp$Key <- factor(with(temp,paste(Oppo,Round)),unique(with(fixs,paste(Opponent,Round))))
temp <- subset(temp,!Stat%in%c('Forward Pressure Acts'))
temp$Stat <- factor(temp$Stat,c('Points','Scoring Shots','Inside 50s','Scores per IN50 %','F50 Marks','F50 Marks per IN50','Intercepts','Int Marks per Oppo IN50','Square Ups','Centre Clearances','BU+TI Clearances','All Clearances','Repeat Stoppages','Tackles','Blocks','Handball Receives'))


For <- with(temp,tapply(For,Stat,function(x) mean(x,na.rm=TRUE)))
Against <- with(temp,tapply(Against,Stat,function(x) mean(x,na.rm=TRUE)))
Stat <- levels(temp$Stat)
avgs <- data.frame(Stat,For,Against)

p <- ggplot(temp,aes(x=Key))
p <- p + facet_wrap(~Stat,nc=4,scales='free')
p <- p + geom_line(mapping=aes(y=For,group=1),colour='blue')
p <- p + geom_line(mapping=aes(y=Against,group=1),colour='red')
p <- p + geom_point(mapping=aes(y=For),colour='blue',size=3)
p <- p + geom_point(mapping=aes(y=Against),colour='red',size=3)
p <- p + scale_y_continuous(limits=c(0,NA))
p <- p + geom_hline(data = avgs,mapping=aes(yintercept=For),lty=2,col='blue')
p <- p + geom_hline(data = avgs,mapping=aes(yintercept=Against),lty=2,col='red')
p <- p + theme(
            axis.text.x = element_text(angle = 90, hjust=1, vjust=0.2, size=10),
            strip.text = element_text(size=12)
      )
p <- p+xlab('')
p <- p+ylab('')
ggsave(p,filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/2019 Stats.png',height=18,width=14)

temp <- gs_read(sname,ws='Stats')
goal <- subset(temp,Stat%in%c('Turnover Goals','Stoppage Goals','Free Kick Goals'))
goal <- rbind(goal,tibble(
      Round=rep(fixs$Round[!fixs$Round%in%goal$Round],each=3),
      Oppo=rep(fixs$Opponent[!fixs$Round%in%goal$Round],each=3),
      Stat=rep(c('Turnover Goals','Stoppage Goals','Free Kick Goals'),times=sum(!fixs$Round%in%goal$Round)),
      For=NA,
      Against=NA,
      Diff=NA
))

Fitzroy <- with(goal,tapply(For,Stat,function(x) mean(x,na.rm=TRUE)))
Oppo <- with(goal,tapply(Against,Stat,function(x) mean(x,na.rm=TRUE)))
Stat <- names(Fitzroy)
avgs <- data.frame(Stat,Fitzroy,Oppo) %>% gather(Team,Value,2:3)


names(goal) <- c('Round','Opposition','Stat','Fitzroy','Oppo','Diff')
goal$Key <- factor(with(goal,paste(Opposition,Round)),rev(unique(with(fixs,paste(Opponent,Round)))))
goal <- gather(goal,Team,Value,4:5) %>% select(Round,Opposition,Stat,Team,Value,Key)
goal$Team <- factor(goal$Team)

p <- ggplot(goal,aes(x=Key,fill=Team,y=Value))
p <- p + facet_wrap(~Stat,nc=3)
p <- p + geom_bar(position='dodge',stat='identity')
p <- p + scale_fill_manual(values=c('blue','red'))
p <- p + scale_colour_manual(values=c('blue','red'))
p <- p + scale_y_continuous(
      breaks = (0:10)*2
)
p <- p + xlab('')
p <- p + ylab('')
p <- p + theme(
      axis.text = element_text(size=10),
      strip.text=element_text(size=12)
)
p <- p + geom_hline(data = avgs,mapping=aes(yintercept=Value,colour=Team),lty=2)
p <- p + coord_flip()
ggsave(p,filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/2019 Goals.png',height=11,width=14)

sname <- gs_title('Fitzroy 2019')
test <- gs_read(sname,ws='PlyStats')

plyavg <- test %>% 
      group_by(Player) %>% 
      summarise(Mt=n(),Tackle=mean(Tackle+ChaseTackle),Block=mean(Block),Spoil=mean(Spoil),Smother=mean(Smother),Knockon=mean(Knockon),Total=mean(Total))
plyavg <- subset(plyavg,Mt>=max(Mt)/2)

source('C:/Users/karl.jackson/Dropbox/fitzroyfc.github.io/VAFA.R')


temp <- gs_read(sname,ws='PlyStats')
temp <- temp %>% arrange(desc(Round))
avg = temp %>% 
      group_by(Player) %>% 
      summarise(MT =n(),
                Tackle=mean(Tackle),
                InTackle=mean(InTackle),
                ChaseTackle=mean(ChaseTackle),
                Block=mean(Block),
                Spoil=mean(Spoil),
                Smother=mean(Smother),
                Knockon=mean(Knockon),
                Stepped=mean(Stepped),
                FA=mean(FA),
                Fifty=mean(Fifty),
                Total=mean(Total))

tot = temp %>% 
      group_by(Player) %>% 
      summarise(MT =n(),
                Tackle=sum(Tackle),
                InTackle=sum(InTackle),
                ChaseTackle=sum(ChaseTackle),
                Block=sum(Block),
                Spoil=sum(Spoil),
                Smother=sum(Smother),
                Knockon=sum(Knockon),
                Stepped=sum(Stepped),
                FA=sum(FA),
                Fifty=sum(Fifty),
                Total=sum(Total))

qsource('pngTable.R')
Leader_Tackle <- temp %>% top_n(5,Tackle) %>% arrange(desc(Tackle)) %>% select(Player,Round,Opposition,Tackle)
Leader_InTackle <- temp %>% top_n(5,InTackle) %>% arrange(desc(InTackle)) %>% select(Player,Round,Opposition,'Ineffective Tackle'=InTackle)
Leader_ChaseTackle <- temp %>% top_n(5,ChaseTackle) %>% arrange(desc(ChaseTackle)) %>% select(Player,Round,Opposition,'Chase\nTackle'=ChaseTackle)
Leader_Block <- temp %>% top_n(5,Block) %>% arrange(desc(Block)) %>% select(Player,Round,Opposition,Block)
Leader_Spoil<- temp %>% top_n(5,Spoil) %>% arrange(desc(Spoil)) %>% select(Player,Round,Opposition,Spoil)
Leader_Smother <- temp %>% top_n(5,Smother) %>% arrange(desc(Smother)) %>% select(Player,Round,Opposition,Smother)
Leader_Knockon <- temp %>% top_n(5,Knockon) %>% arrange(desc(Knockon)) %>% select(Player,Round,Opposition,Knockon)
Leader_Stepped <- temp %>% top_n(5,Stepped) %>% arrange(desc(Stepped)) %>% select(Player,Round,Opposition,Stepped)
Leader_FA <- temp %>% top_n(5,FA) %>% arrange(desc(FA)) %>% select(Player,Round,Opposition,'Free Against'=FA)
Leader_Fifty <- temp %>% top_n(5,Fifty) %>% arrange(desc(Fifty)) %>% select(Player,Round,Opposition,Fifty)
Leader_Total <- temp %>% top_n(20,Total) %>% arrange(desc(Total)) %>% select(Player,Round,Opposition,Total)
Leader_Distance <- temp %>% top_n(10,Distance) %>% arrange(desc(Distance)) %>% select(Player,Round,Opposition,Distance) %>% mutate(Distance = format(round(Distance,1),nsmall=1)) %>% rename('Distance\n(km)'=Distance)
Leader_Speed <- temp %>% top_n(10,Speed) %>% arrange(desc(Speed)) %>% select(Player,Round,Opposition,Speed) %>% mutate(Speed = format(round(Speed,1),nsmall=1)) %>% rename('Max Speed\n(km/h)'=Speed)
Leader_Hard <- temp %>% top_n(10,Hard) %>% arrange(desc(Hard)) %>% select(Player,Round,Opposition,Hard) %>% rename('Hard Running\n(m)'=Hard)

pngTable(head(Leader_Tackle,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Tackles.png',hc=1,ht=200,wt=500)
pngTable(head(Leader_ChaseTackle,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - ChaseTackle.png',hc=1,ht=200,wt=500)
pngTable(head(Leader_Block,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Block.png',hc=1,ht=200,wt=500)
pngTable(head(Leader_Spoil,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Spoil.png',hc=1,ht=200,wt=500)
pngTable(head(Leader_Smother,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Smother.png',hc=1,ht=200,wt=500)
pngTable(head(Leader_Knockon,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Knockon.png',hc=1,ht=200,wt=500)
pngTable(head(Leader_Total,20),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Total.png',hc=1,ht=800,wt=500)
pngTable(head(Leader_Distance,10),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Distance.png',hc=1,ht=400,wt=500)
pngTable(head(Leader_Speed,10),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Top Speed.png',hc=1,ht=400,wt=500)
pngTable(head(Leader_Hard,10),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Game Records - Hard Running.png',hc=1,ht=400,wt=500)

Total_Tackle <- tot %>% top_n(5,Tackle) %>% arrange(desc(Tackle)) %>% select(Player,Matches=MT,Tackle)
Total_ChaseTackle <- tot %>% top_n(5,ChaseTackle) %>% arrange(desc(ChaseTackle)) %>% select(Player,Matches=MT,'Chase\nTackle'=ChaseTackle)
Total_Block <- tot %>% top_n(5,Block) %>% arrange(desc(Block)) %>% select(Player,Matches=MT,Block)
Total_Spoil<- tot %>% top_n(5,Spoil) %>% arrange(desc(Spoil)) %>% select(Player,Matches=MT,Spoil)
Total_Smother <- tot %>% top_n(5,Smother) %>% arrange(desc(Smother)) %>% select(Player,Matches=MT,Smother)
Total_Knockon <- tot %>% top_n(5,Knockon) %>% arrange(desc(Knockon)) %>% select(Player,Matches=MT,Knockon)
Total_Total <- tot %>% top_n(10,Total) %>% arrange(desc(Total)) %>% select(Player,Matches=MT,Total)
pngTable(head(Total_Tackle,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - Tackles.png',hc=1,ht=200,wt=500)
pngTable(head(Total_ChaseTackle,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - ChaseTackle.png',hc=1,ht=200,wt=500)
pngTable(head(Total_Block,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - Block.png',hc=1,ht=200,wt=500)
pngTable(head(Total_Spoil,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - Spoil.png',hc=1,ht=200,wt=500)
pngTable(head(Total_Smother,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - Smother.png',hc=1,ht=200,wt=500)
pngTable(head(Total_Knockon,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - Knockon.png',hc=1,ht=200,wt=500)
pngTable(head(Total_Total,10),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Total Leaders - Total.png',hc=1,ht=400,wt=500)

avg$Tackle <- format(round(avg$Tackle,1),nsmall=1)
avg$ChaseTackle <- format(round(avg$ChaseTackle,1),nsmall=1)
avg$Block <- format(round(avg$Block,1),nsmall=1)
avg$Spoil <- format(round(avg$Spoil,1),nsmall=1)
avg$Smother <- format(round(avg$Smother,1),nsmall=1)
avg$Knockon <- format(round(avg$Knockon,1),nsmall=1)
avg$Total <- format(round(avg$Total,1),nsmall=1)

Avg_Tackle <- avg %>% top_n(5,Tackle) %>% arrange(desc(Tackle)) %>% select(Player,Matches=MT,Tackle)
Avg_ChaseTackle <- avg %>% top_n(5,ChaseTackle) %>% arrange(desc(ChaseTackle)) %>% select(Player,Matches=MT,'Chase\nTackle'=ChaseTackle)
Avg_Block <- avg %>% top_n(5,Block) %>% arrange(desc(Block)) %>% select(Player,Matches=MT,Block)
Avg_Spoil<- avg %>% top_n(5,Spoil) %>% arrange(desc(Spoil)) %>% select(Player,Matches=MT,Spoil)
Avg_Smother <- avg %>% top_n(5,Smother) %>% arrange(desc(Smother)) %>% select(Player,Matches=MT,Smother)
Avg_Knockon <- avg %>% top_n(5,Knockon) %>% arrange(desc(Knockon)) %>% select(Player,Matches=MT,Knockon)
Avg_Total <- avg %>% top_n(10,Total) %>% arrange(desc(Total)) %>% select(Player,Matches=MT,Total)
pngTable(head(Avg_Tackle,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - Tackles.png',hc=1,ht=200,wt=500)
pngTable(head(Avg_ChaseTackle,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - ChaseTackle.png',hc=1,ht=200,wt=500)
pngTable(head(Avg_Block,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - Block.png',hc=1,ht=200,wt=500)
pngTable(head(Avg_Spoil,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - Spoil.png',hc=1,ht=200,wt=500)
pngTable(head(Avg_Smother,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - Smother.png',hc=1,ht=200,wt=500)
pngTable(head(Avg_Knockon,5),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - Knockon.png',hc=1,ht=200,wt=500)
pngTable(head(Avg_Total,15),filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Average Leaders - Total.png',hc=1,ht=400,wt=500)

temp$Key <- factor(with(temp,paste(Opposition,Round)),unique(with(fixs,paste(Opponent,Round))))
temp <- temp %>% gather(Stat,Value,4:14)

p <- ggplot(temp,aes(x=Key,y=Value,group=Stat,colour=Stat))
p <- p + facet_wrap(~Player,nc=4)
p <- p + geom_line()
p <- p + geom_point(size=3)
p <- p + scale_y_continuous(limits=c(0,NA))
p <- p + theme(
      axis.text.x = element_text(angle = 90, hjust=1, vjust=0.2, size=10),
      strip.text = element_text(size=12)
)
p <- p+xlab('')
p <- p+ylab('')
ggsave(p,filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/2019 Player Stats by Round.png',height=30,width=18)
p

temp <- gs_read(sname,ws='PlyStats')
temp <- temp %>% arrange(desc(Round))
avg = temp %>% 
      group_by(Player) %>% 
      summarise(MT =n(),
                Tackle=mean(Tackle),
                InTackle=mean(InTackle),
                ChaseTackle=mean(ChaseTackle),
                Block=mean(Block),
                Spoil=mean(Spoil),
                Smother=mean(Smother),
                Knockon=mean(Knockon),
                Stepped=mean(Stepped),
                FA=mean(FA),
                Fifty=mean(Fifty),
                Total=mean(Total))
avg <- avg %>% gather(Stat,Value,3:13)
avg <- avg %>%
      ungroup() %>%
      arrange(Stat,desc(Value)) %>%
      mutate(.r = row_number())
avg$Stat <- factor(avg$Stat,c('Total','Tackle','ChaseTackle','Block','Spoil','Smother','Knockon','InTackle','Stepped','FA','Fifty'))
levels(avg$Stat) <- c('Total Defensive Score','Tackle','Chase Tackle','Block','Spoil','Smother','Knock-on','Ineffective Tackle','Stepped','Free Against','50m Penalty Against')


scale_x_reordered <- function(..., sep = "___") {
      reg <- paste0(sep, ".+$")
      ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
      new_x <- paste(x, within, sep = sep)
      stats::reorder(new_x, by, FUN = fun)
}

p <- ggplot(avg,aes(x=reorder_within(Player,-Value,Stat),y=Value,fill=MT))
p <- p + facet_wrap(~Stat,nc=3,scales='free')
p <- p + geom_col()
p <- p + scale_y_continuous(limits=c(0,NA))
p <- p + theme(
      axis.text.x = element_text(angle = 90, hjust=1, vjust=0.2, size=10),
      strip.text = element_text(size=12)
)
p <- p+xlab('')
p <- p+ylab('')
p <- p + scale_x_reordered()
#p <- p + scale_x_continuous(
#      breaks = avg$.r,
#      labels = avg$Player
#)
ggsave(p,filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/2019 Player Stats by Stat.png',height=20,width=16)
p
