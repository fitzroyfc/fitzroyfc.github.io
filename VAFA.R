require(readxl)
library(googlesheets)

sname <- gs_title('Fitzroy 2019')
temp <- gs_read(sname,ws='Fixture')
temp <- with(temp,data.frame(Round,Home_1,Away_1,HS,AS))
temp <- subset(temp,!is.na(temp$HS))
names(temp) <- c('Round','Home','Away','HS','AS')
temp$Home <- as.character(temp$Home)
temp$Away <- as.character(temp$Away)

vteams <- unique(c(unique(temp$Home),unique(temp$Away)))

team_ratings <- team_rank <- NULL
for(rnd in 1:max(temp$Round)){
	results <- matrix(0,nr=length(vteams),nc=length(vteams))
	row.names(results) <- vteams
	for(i in 1:dim(temp[temp$Round<=rnd,])[1]){
		results[vteams==temp$Home[i],vteams==temp$Away[i]] <- (max(c(min(temp$AS[i],10),temp$HS[i]))/max(c(min(temp$HS[i],10),temp$AS[i])))
		results[vteams==temp$Away[i],vteams==temp$Home[i]] <- (max(c(min(temp$HS[i],10),temp$AS[i]))/max(c(min(temp$AS[i],10),temp$HS[i])))
	}
	
	ratings <- rep(1,dim(results)[1])
	for(i in 1:(2^(rnd-1))){
		ratings <- (results%*%ratings)
		ratings <- ratings / mean(ratings)
	}
	round(cbind(ratings[order(ratings,decreasing=TRUE),]),2)
	ratings[ratings>2] <- 2
	team_ratings <- cbind(team_ratings,ratings)
	team_rank <- cbind(team_rank,rank(-ratings,ties.method='random'))
}

test <- data.frame(Rating=as.numeric(team_ratings)*100,Rank=as.numeric(team_rank))
test$Club <- rep(vteams,times=dim(team_ratings)[2])
test$Round <- rep(1:max(temp$Round),each=dim(team_ratings)[1])
test$Club <- factor(test$Club,levels=vteams[order(team_ratings[,dim(team_ratings)[2]],decreasing=TRUE)])

p <- ggplot(test)
p <- p + geom_line(aes(x=Round,y=Rating,colour=Club),lwd=1.2)
p <- p + geom_point(aes(x=Round,y=Rating,fill=Club),pch=21,size=6,colour='black')
p <- p + geom_text(aes(x=Round,y=Rating,label=Rank),size=3)
p <- p + geom_text(aes(x=Round,y=Rating,label=ifelse(Club%in%c('Kew','West Brunswick','Richmond','Gold Coast Suns','Collingwood','Geelong Cats','North Melbourne','Sydney Swans'),NA,Rank)),colour='white',size=3)
p <- p + geom_hline(yintercept=100,lty=2)
#p <- p + scale_colour_manual(values=lPal[order(team_ratings[,dim(team_ratings)[2]],decreasing=TRUE)])
#p <- p + scale_fill_manual(values=cPal[order(team_ratings[,dim(team_ratings)[2]],decreasing=TRUE)])
p <- p + scale_y_continuous(limits=c(min(test$Rating),max(test$Rating)),breaks=(0:10)*50)
p <- p + scale_x_continuous(breaks=(1:20))
p <- p + xlab('\nRound Number')
p <- p + ylab('Team Rating\n')
p <- p + labs(title=paste('VAFA Premier B Seniors'),subtitle='2019 Team Ratings')
p <- p + theme(	axis.text = element_text(size=12),
			axis.title= element_text(size=14),
			plot.title=element_text(size=18),
			plot.subtitle=element_text(size=12),
			legend.key.width=unit(3,'line'),
			legend.key.height=unit(2,'line')
		)
p
ggsave(p,filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Team Ratings.png',height=10,width=(max(temp$Round)+1)*2)
