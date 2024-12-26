# min = 0 , max = 1 hour
# min = 0 , max = 60 minute
#40 min = 40/60 hour

number_of_true_event = 0 
number_of_repeat = 500000
for(i in 1: number_of_repeat){

times = sort(runif(3, min = 0 , max = 60 ))

if(times[2] - times[1] >= 40 ){
  number_of_true_event = number_of_true_event + 1
}
}
(prob = number_of_true_event / number_of_repeat)









