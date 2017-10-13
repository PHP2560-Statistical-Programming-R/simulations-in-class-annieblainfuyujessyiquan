---
  title: "Simulations In-Class Project"
  date: "Due October 13, 2017 at 11:59pm"
  output: html_document
---
  
  <style type="text/css">
  .table {
    
    width: 80%;
    margin-left:10%; 
    margin-right:10%;
  }
</style>
  ```{r,setup, echo=FALSE, cache=TRUE}
## numbers >= 10^5 will be denoted in scientific notation,
## and rounded to 2 digits
options(scipen = 3, digits = 3)
```

#Project Goals:


With this project we will simulate a famous probability problem. This will not require knowledge of probability or statistics but only the logic to follow the steps in order to simulate this problem. This is one way to solve problems by using the computer. 

1. **Gambler's Ruin**: Suppose you have a bankroll of $1000 and make bets of $100 on a fair game. By simulating the outcome directly for at most 5000 iterations of the game (or hands), estimate:
```{r}
sim_bets <- function(num_bets, bank_start = 1000){
bank<-rep(NA,num_bets)
bank[1]<-bank_start
for (i in 2:num_bets){
bank[i] <- bank[i-1] + rbinom(1,1,0.5)*(200) - 100
if (bank[i]<=0){
break()
}  
}
return(list(bets=i,bankroll=bank[i]))
}

bankruptcy <- function(num_bets, max_iterations = 5000){
n <- 0
for (i in 1:max_iterations){
if (sim_bets(num_bets)$bets < num_bets) { 
n <- n + 1 
}
}
n/max_iterations
}
```    
a. the probability that you have "busted" (lost all your money) by the time you have placed your one hundredth bet. 
```{r}
bankruptcy(100)
```    
b. the probability that you have busted by the time you have placed your five hundredth bet by simulating the outcome directly. 
```{r}
bankruptcy(500)
```
c. the mean time you go bust, given that you go bust within the first 5000 hands.
```{r}
df <- replicate(1000, sim_bets(5000)$bets)
df <- df[df!=5000]
mean(df)
```
d. the mean and variance of your bankroll after 100 hands (including busts).
```{r}
mean(replicate(1000, sim_bets(100)$bankroll))
var(replicate(1000, sim_bets(100)$bankroll))
```
e. the mean and variance of your bankroll after 500 hands (including busts).
```{r}
mean(replicate(1000, sim_bets(500)$bankroll))
var(replicate(1000, sim_bets(500)$bankroll))
```

2. Repeat the previous problem with betting on black in American roulette, where the probability of winning on any spin is 18/38 for an even payout.
```{r}
roulette <- function(num_bets, bank_start = 1000){
bank<-rep(NA,num_bets)
bank[1]<-bank_start
for (i in 2:num_bets){
bank[i] <- bank[i-1] + rbinom(1,1,(18/38))*(200) - 100
if (bank[i]<=0){
break()
}  
}
return(list(bets=i,bankroll=bank[i]))
}

``` 

3. **Markov Chains**. Suppose you have a game where the probability of winning on your first hand is 48%; each time you win, that probability goes up by one percentage point for the next game (to a maximum of 100%, where it must stay), and each time you lose, it goes back down to 48%. Assume you cannot go bust and that the size of your wager is a constant $100.
```{r}
unfair_game <- function(num_bets, p1 = .48, increase = .01){
bank <- rep(NA, num_bets+1)
bank[1] = 100
chance = p1

for(i in 2:num_bets) {
sample1 <-rbinom(1,1,chance)
bank[i] <- bank[i-1] + sample1*(200) - 100

if(chance < 1 & sample1 == 1) {
chance <- chance + increase
} else if (chance < 1 & sample1 == 0) {
chance <- p1
}
}
return(bank[num_bets])
}


```
a. Is this a fair game? Simulate one hundred thousand sequential hands to determine the size of your return. Then repeat this simulation 99 more times to get a range of values to calculate the expectation.
```{r}
unfair_game(100000)

mean(replicate(99, unfair_game(100000)))

```
b. Repeat this process but change the starting probability to a new value within 2% either way. Get the expected return after 100 repetitions. Keep exploring until you have a return value that is as fair as you can make it. Can you do this automatically?
```{r, eval = FALSE}

############### METHOD ONE ####################################
unfair_game(num_bets = 100000, p1 = .485)
mean(replicate(100, unfair_game(num_bets = 100000, p1 = .485)))

unfair_game(num_bets = 100000, p1 = .49)
mean(replicate(100, unfair_game(num_bets = 100000, p1 = .49)))

mean(replicate(100, unfair_game(num_bets = 100000, p1 = .491)))

unfair_game(num_bets = 100000, p1 = .50)
mean(replicate(100, unfair_game(num_bets = 100000, p1 = .50)))

############### METHOD TWO ####################################
probs <- seq(.48,.50, .00025)
fair_mean<-rep(NA, length(probs))
for(i in 1:length(probs)) {
fair_mean[i] <- mean(replicate(100, unfair_game(num_bets = 5000, p1 = probs[i])),na.rm=TRUE)
}
#find the place where there is a sign change
fair_mean

############### METHOD THREE ####################################

#minimum increase for each hand for winning is .001 and maximum is .02
#using midpoint displacement to find the fairest increment value for this game 
min <- .01
max <- .02
fair_mean <- NA
#deciding that a fair game will result is, on average winning or loosing between $0 and $1000
while(!(fair_mean %in% -900:1100)){
middle <- (min + max) / 2
fair_mean <- mean(replicate(100, unfair_game(num_bets = 100000, increase = middle)))
#if the mean is too small - change the bounds to the midpoint and max, then take midpoint of those points
if(fair_mean <= -900) {
min <- middle
#if the mean is too big - change the bounds to the min and midpoint, then take midpoint of those points
} else if(fair_mean >= 1100) {
max <- middle
} else {
fair_increase <- middle
break
}
}
fair_increase
```
c. Repeat again, keeping the initial probability at 48%, but this time change the probability increment to a value different from 1%. Get the expected return after 100 repetitions. Keep changing this value until you have a return value that is as fair as you can make it. 
```{r, eval = FALSE}

```

4. Creating a Bootstrap function. There is a particular concept called [bootstrapping]
(https://en.wikipedia.org/wiki/Bootstrapping_(statistics)) where we can easily create 95% confidence intervals, even for complex estimators.

The steps of this process are:

a. Draw a sample, with replacement, from your data which is the same length of your data.
b. Calculate the statistic of interest on this boostrap sample (ie mean, variance, regression,...)
c. Peform steps 1:2 at least 1000 times over until you have a vector of your statistics. 
d. The lower bound of a 95% CI will be the 0.025 percentile
e. The upper bound of a 95% CI will be the 0.975 percentile

Make a function called `boot_ci` which calculates the 95% confidence interval in this manner. 
```{r}
boot_ci <- function(data, fun = mean, rep = 1000) {
resample <- function(data) {
sample(data, length(data), replace = T)
}
statistic <- replicate(rep, fun(resample(data)))
quantile(statistic, probs = c(.025,.975))
}
```

5. For problems 3b and 3c, you calculated a mean value. Because you saved these final results in a vector, use the bootstrap to estimate the variance of the return in each case for your final answer. Once you have these results, which game has the smaller variance in returns?

```{r}

```

