library(Surrogate)
library(markovchain)
library(magrittr)
library(igraph)

#Answer rMath question
#Added markov visualizations 

#r/Math question
r_math <- function(x,reps=1e6){
  
  
  outcomes <- c(1:x)
  
  mean(rle(
    sample(x = outcomes, 
           size = reps, 
           replace = T, 
           prob = rep(1/length(outcomes),length(outcomes))
    )
  )$lengths)
  
}

#RPS Extension

outcomes <- c(1:3) #There are 3 moves in Rock, Paper, Scissors

reps <- 1000 #100 throws

a <- sample(x = outcomes, 
            size = reps,
            replace = T, 
            prob = rep(1/length(outcomes),length(outcomes))
) #Assume each piece is chosen with equal probability 

par(mfrow=c(1,2))

plot(a, xlab='Throw #', ylab='Move Selection') #what does this random sequence look like?

markovchainFit(a)$estimate %>% plot(layout=layout_in_circle,edge.curved=0.2)


#What if there the probability of each piece dropping is different each game?

#This is where RandVec function comes in!

outcomes <- c(1:3)

a <- sample(x = outcomes, 
            size = reps, 
            replace = T, 
            prob = RandVec(a=0, b=1, s=1, n=max(outcomes), m=1)$RandVecOutput
)

par(mfrow=c(1,2))

plot(a, xlab='Throw #', ylab='Move Selection') #what does this random sequence look like?

markovchainFit(a)$estimate %>% plot(layout=layout_in_circle,edge.curved=0.2)


#Tetris Extension

outcomes <- c(1:7) #There are 7 pieces in Tetris

reps <- 1000 #100 pieces drop

a <- sample(x = outcomes, 
            size = reps,
            replace = T, 
            prob = rep(1/length(outcomes),length(outcomes))
) #Assume each piece is chosen with equal probability 

par(mfrow=c(1,2))

plot(a) #what does this random sequence look like?

markovchainFit(a)$estimate %>% plot(layout=layout_in_circle, edge.curved=0.2)


#What if there the probability of each piece dropping is different each game?

#This is where RandVec function comes in!

outcomes <- c(1:7)

a <- sample(x = outcomes, 
            size = reps, 
            replace = T, 
            prob = RandVec(a=0, b=1, s=1, n=max(outcomes), m=1)$RandVecOutput
)

par(mfrow=c(1,2))

plot(a)

markovchainFit(a)$estimate %>% plot(layout=layout_in_circle, edge.curved=0.2)
