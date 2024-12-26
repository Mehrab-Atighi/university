#n neshan dahandeye meghdari hast ke agar sarmaye be on berese ghomarbaz barande mishavad
#k neshan dahandeye sarmaye avalie ghomarbaz mibashad
#p neshan dahandeye ehtemal barande shodan ghomar baz dar har dast az bazi mibashad
#mean(simlist) neshan dahadeye ehtemal bazande shodan ghomar baz mibashad
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