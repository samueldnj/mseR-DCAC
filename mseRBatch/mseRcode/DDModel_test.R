# |---------------------------------------------------------------------------|
# | Delay Difference Model
# |---------------------------------------------------------------------------|
graphics.off()

# |---------------------------------------------------------------------------|
# | Model parameters
# |---------------------------------------------------------------------------|
agek    <- 2
bo      <- 40.7846 #39739.2
steepness <- 0.75
reck    <- (4*steepness)/(1 - steepness)
m       <- 0.4
s       <- exp(-m)
alpha   <- 1.40538
rho     <- 0.83756
wk <- 0.82775
 


# |----------------------------------------------------------------------------|
# | Equilibrium unfished conditions.
# |----------------------------------------------------------------------------|
be        <- bo 
se <- s
we        <- -(se*alpha - wk*(se) + wk)/(-1 + (se)*rho)
re        <- -be*(se*alpha+se*rho*we-we)/(we*wk)
ne        <- be / we
ro        <- sum(re)

#BH pars
so        <- reck * (ro/bo)
beta      <- (reck-1.0)/bo
aj        <- reck * (re/be)
bj        <- (reck-1.0)/be

#cat("Printing bo be se we ro re ne ro so beta aj bj \n")
#print(bo)
#print(be)
#print(se)
#print(we)
#print(ro)
#print(re)
#print(ne)
#print(so)
#print(beta)
#print(aj)
#print(bj)

# |----------------------------------------------------------------------------|
# | Equilibrium fished conditions.
# |----------------------------------------------------------------------------|
# | Given a vector of fishing mortality rates calculate corresponding 
# | equilibrium conditions, including yields (ye)

equil <- function(fe)
{	
	se<-exp(-fe-m)
	we <- -(se*alpha - wk*(se) + wk)/(-1 + (se)*rho)
	be <- -(-we+se*alpha+se*rho*we+wk*aj*we)/(bj*(-we+se*alpha+se*rho*we))
	#re <- aj*be/(1.0+bj*be)
	# print(re)
	ne <- be / we
	re=ne*(1.-se)
	ue <- (1-exp(-fe))
	#ye <- ue * be
	ye<- (fe/(fe+m))*(1.-exp(-(fe+m)))*be
	
	return(ye)
}

# |----------------------------------------------------------------------------|
# | Dynamic calculations													
# |----------------------------------------------------------------------------|
nT  <- 200
ftest<-seq(0,0.5,by=0.001)
nf<-length(ftest)
bt = nt = rt = wt = ct = matrix(NA,nrow=T+1,ncol=nf)
	for(ff in 1:nf){	#:nf
		Ft=ftest[ff]
		se=exp(-m)
		surv = exp(-m-Ft)
		we= -(se*alpha - wk*(se) + wk)/(-1 + (se)*rho)
		no=bo/we;
		ro=no*(1.-se)
                
                bt[1,ff] = bo
		nt[1,ff] = no
		rt[1,ff] = ro #pe*ro 
		wt[1,ff] = we
		
		for(i in 1:nT)
		{

			if(i <= agek)
			{
				rt[i,] = ro #pe*ro 
			}
			else
			{
				st     <- bt[i-agek,ff] #spawners
				rt[i,ff] = so*st/(1.+beta*st)
			}

			bt[i+1,ff] <- (surv)*(alpha*nt[i,ff]+rho*bt[i,ff]) + wk*rt[i,ff]
			nt[i+1,ff] <- (surv)*nt[i,ff] + rt[i,ff]
			wt[i+1,ff] <- bt[i+1,ff]/nt[i+1,ff]
			ct[i,ff] <- (Ft/(Ft+m))*(1.-exp(-(Ft+m)))*bt[i,ff]
		}
		
   }
msy<-max(ct[nT,])
fmsy<-ftest[which(t(ct[nT,])==msy)]
cat("Reference points fmsy and msy - model run out\n")
print(fmsy)
print(msy)
plot(ftest,ct[nT,],type="o",col="red", las=1, xlab="F", ylab="Terminal year catch")
abline(v=fmsy, lty=2,col=1,lwd=1)
windows()

fe <- seq(0,.5,by=0.001)
ye <- t(sapply(fe,equil))
plot(fe,ye,type="o",col="blue")

msy<-max(ye)
fmsy<-fe[which(ye==max(ye))]

abline(v=fmsy, lty=2,col=1,lwd=1)
cat("Reference points  fmsy and msy\n")
print(fmsy)
print(msy)


