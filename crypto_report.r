
###BITCOIN REPORT###
####################

###Packages###
packs = c("Quandl","dplyr")
lapply(packs,require,character.only=TRUE)


###Data###

coin = "Bitcoin"

until = as.Date("2017-11-30")
startd = as.Date("2014-11-01")

btc = Quandl("BCHARTS/BITSTAMPUSD") %>% filter(Date >= startd &
		`Weighted Price` != 0 & Date <= until) %>%
		select(Date,`Weighted Price`) %>% rename(price = `Weighted Price`) %>% arrange(Date) %>%
		mutate(per_ind = price / first(price), ym = strftime(Date,"%Y-%m"))


		
###Functions###

log_price <- function(dat) {
	
	
	y = dat %>% mutate(ind2 = 1:n()) %>% group_by(ym) %>% slice(which.max(Date)) %>% data.frame()

	plot(dat$price,log="y",type="l",las=2,main="Performance Index (log-Scale), Price in USD",adj=0,font.main=1,
		ylab="",xaxt="n",xlab="",col="dodgerblue",lwd=3)
	axis(1,y$ind2,y$Date,las=2,cex.axis=0.85)
	}
	

roll_plot <- function(periods) {
	
	dat = roll_return(periods)
	y = btc %>% group_by(ym) %>% slice(which.max(Date)) %>% data.frame()
	
	max_p = max(dat)
	min_p = min(dat)
	ylims = pretty(c(min_p,max_p))
		
	cols = c("dodgerblue","darkred","black","orange","darkgreen")
		
	plot(dat[,1],type="l",ylim=c(min(ylims),max(ylims)),
		col=cols[1],lwd=2.5,
		xaxt="n",xlab="",
		yaxt="n",ylab="",
		main=paste0("Rolling return of ",toString(periods)," monthly holding Periods"),adj=0,font.main=1)
	legend("topleft",lty=1,col=cols[1:ncol(dat)],paste0(periods," months"),bty="n",lwd=2.5)
	axis(1,1:nrow(y),y$ym,las=2,cex.axis=0.85)
	axis(2,ylims,paste0(ylims*100,"%"),las=2)
	if (ncol(dat) > 1) {
		for (i in 2:ncol(dat)) {
			lines(dat[,i],lwd=2.5,
				col=cols[i])
		}
	}
}
	
			
drawback_ath <- function(dat) {
			
	dat = btc %>% group_by(ym) %>% slice(which.max(Date)) %>% data.frame()

	n = nrow(dat)
	z = c(1,rep(0,n-1))
	db = rep(0,n)

	for (i in 2:n) {
		z[i] = ifelse(all(dat[i,"price"] > dat[1:(i-1),"price"]),1,0)
		last_alth = last(which(z==1))
		db[i] = (dat[i,"price"] / dat[last_alth,"price"]) - 1
		}

	res = dat %>% mutate(alh = z,drawb = db)
	x = 1:nrow(res)
	y = res$drawb
	ylabs = pretty(c(-0.7,0))
	xlims = seq(1,n,2)
	
	plot(res$drawb,type="l",ylim=c(-0.5,0),col="white",
		main = "Percent decline from All-Time-High", font.main=1,adj=0,
		ylab="",yaxt="n",
		xlab="", xaxt="n",bty="n")
	polygon(x=c(0,x,rev(x),0),c(0,0*y,rev(y),0),col="dodgerblue")
	axis(2,ylabs,paste0(ylabs*100,"%"),las=2)
	axis(1,xlims,res$ym[xlims],las=2,cex.axis=0.8)
}	

month_perform <- function(dat) {
		x = dat %>% group_by(ym) %>% slice(which.max(Date)) %>% data.frame() %>%
				mutate(month_change = (price/lag(price)-1)*100 ) %>%
				filter(!is.na(month_change))
				
		n = nrow(x)
		datc = x$month_change		
		ylims = pretty(datc)
		xlims = seq(1,n,2)
		
		a=barplot(datc,col=ifelse(datc>0,"dodgerblue","darkred"),ylim=c(min(ylims),max(ylims)),
			xlab="",xaxt="n",
			ylab="",yaxt="n",
			main="Monthly Performance in Percent",adj=0,font.main=1)
		axis(2,ylims,paste0(ylims,"%"),las=2)
		axis(1,a,x$ym,las=2,cex.axis=0.8)
}	

roll_return = function(periods) {
			sapply(periods, function(x) {
				y = btc %>% group_by(ym) %>% slice(which.max(Date)) %>% data.frame()
				ind = nrow(y) - x
				ret = rep(0,ind)
				ret2 = rep(0,nrow(y) - ind)
				
					for (i in 1:ind) {
					ret[i] = (y[x+(i-1),"price"] / y[i,"price"])-1
					}
				return(c(ret2,ret))
			})
		}
		
perc_prof <- function(periods) {	
	pers = 	periods
	prof_periods = apply(roll_return(periods),2,function(z) c(length(which(z>0)),length(which(z<0))))
	pp = t(t(prof_periods)/ colSums(prof_periods))	
	
	ylims = seq(0,100,10)
	a=barplot(pp,beside=TRUE,col=c("dodgerblue","darkred"),ylim=c(0,1.2),
		main = "Percent Profitable / Unprofitable Periods",adj=0,font.main=1,ylab="",yaxt="n")
	axis(2,ylims/100,paste0(ylims,"%"),las=2)
	axis(1,colSums(a)/2,paste0("Rolling Months: ",pers),las=1,cex.axis=1)
	text(a+0.05,pp,paste0(round(pp*100,1),"%"),pos=3,cex=0.8,font=2)
	legend("topleft",fill=c("dodgerblue","darkred"),c("Positive Return","Negative Return"),bty="n",cex=0.9)
	
	}
	
	
	
####REPORT###


pdf("C:/Projects/R_Projects/CryptoReport/report.pdf",10,10)

plot(c(0,1),c(1,0),col="white",xaxt="n",xlab="",yaxt="n",ylab="",bty="n")
text(0.33,0.8,paste0(coin, " Performance"),cex=2)
text(0.38,0.65,paste0("Report as of ",until),cex=2)


log_price(btc)

month_perform(btc)
	
roll_plot(c(3,12,24))

perc_prof(c(3,6,12,24))

drawback_ath(btc)

dev.off()