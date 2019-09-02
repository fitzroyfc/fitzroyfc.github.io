require(readxl)
library(googlesheets)
library(elo)

sname <- gs_title('Fitzroy 2019')
temp <- gs_read(sname,ws='Fixture')
temp <- with(temp,data.frame(Round,Home_1,Away_1,HS,AS))
temp <- subset(temp,!is.na(temp$HS))
names(temp) <- c('Round','Home','Away','HS','AS')
temp$Home <- as.character(temp$Home)
temp$Away <- as.character(temp$Away)

#ELO Parameters
HGA <- 20
carryOver <- 1
B <- 0.03
k_val <- 50
temp <- temp %>% mutate(MA = HS-AS)
exp_mar <- function(x) 1 / (1 + exp(-B*x))

elo.data <- elo.run(exp_mar(MA) ~ adjust(Home,HGA) + Away + group(Round),k = k_val,data=temp)
elo.two <- elo.run(exp_mar(MA) ~ adjust(Home,HGA) + Away + group(Round),k = k_val,data=temp,initial.elos=final.elos(elo.data))

club_rating <- sort(final.elos(elo.data),decreasing=TRUE)

output <- as.tibble(elo.data) %>% mutate(Round=temp$Round)
output <- output %>% 
      gather('Home','Club',1:2) %>% 
      gather('Old','ELO',4:5) %>% 
      filter((Home=='team.A' & Old=='elo.A')|(Home=='team.B' & Old=='elo.B')) %>% 
      select(Round,Club,ELO) %>% 
      arrange(Round,desc(ELO)) %>%
      mutate(Rating = ELO - 1400) %>%
      union(as.tibble(data.frame(Round=rep(0,10),Club=names(club_rating),ELO=rep(1500,10),Rating=rep(100,10)))) %>%
      group_by(Round) %>%
      mutate(Rank=rank(-Rating,ties.method='random')) %>%
      mutate(Club=factor(Club,names(club_rating)))

p <- ggplot(output)
p <- p + geom_line(aes(x=Round,y=Rating,colour=Club),lwd=1.2)
p <- p + geom_point(aes(x=Round,y=Rating,fill=Club),pch=21,size=6,colour='black')
p <- p + geom_text(aes(x=Round,y=Rating,label=ifelse(Round==0,NA,Rank)),size=4)
p <- p + geom_hline(yintercept=100,lty=2)
#p <- p + scale_colour_manual(values=lPal[order(team_ratings[,dim(team_ratings)[2]],decreasing=TRUE)])
#p <- p + scale_fill_manual(values=cPal[order(team_ratings[,dim(team_ratings)[2]],decreasing=TRUE)])
p <- p + scale_y_continuous(limits=c(min(output$Rating),max(output$Rating)),breaks=(0:10)*50)
p <- p + scale_x_continuous(breaks=0:20,minor_breaks=NULL)
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
ggsave(p,filename='C:/Users/karl.jackson/dropbox/fitzroyfc.github.io/Team Ratings.png',height=10,width=30)
