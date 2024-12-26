#n: Represents the amount that if the person's capital reaches that amount, the game is all
#k: Indicates the amount of the person's initial capita
#p: Indicates the chance to win or loser open gambling at any round of the game
#mean(simlist):Represents the possibility of a bankruptcy of gambling open
gamble<-function(k,n,p){
  state=k
  while(0<state & state<n) {
    bet=sample(c(1,-1),1,prob=c(p,1-p))
    state=state+bet
  }
  if(state==0) return(1) else return(0)
}
k<-6
n<-10
p<-1/2
trials<-100
simlist<-replicate(trials,gamble(k,n,p))
mean(simlist) 
par(mfrow=c(3,3))
replicate(9,plot(simlist<-replicate(trials,gamble(k,n,p)),type = "l"))