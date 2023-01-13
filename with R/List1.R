Anderson <- function(alpha, SAMPLE_NUM, SAMPLE_EV, POP_EV){

  level <- 1 - (alpha / 2) / 100
  z <- qnorm(level, 0, 1)
  T_Anderson <- abs(sqrt((SAMPLE_NUM - 1) / 2) * (SAMPLE_EV - POP_EV) / POP_EV)
  
 return(if(T_Anderson <= z) {1} else {0})

}

ConfiPlot <- function(alpha, Tri, Max, df){

  POP_NUM <- Max
  SAMPLE_NUM <- seq(5, POP_NUM, by = 5)
  Percents <- numeric(length(SAMPLE_NUM)) 

  POP <- as.matrix(df)
  POP_EV <- (prcomp(POP)$sdev[1]) ^ 2
  
  decide <- 0

  for(i in 1:length(SAMPLE_NUM)){

    Percent <- 0

    for(j in 1:(100*Tri)){
    SAMPLE <- POP[sample(1:nrow(df), SAMPLE_NUM[i]), ]
    SAMPLE_EV <- (prcomp(SAMPLE)$sdev[1]) ^ 2
    Percent <- Percent + Anderson(alpha, SAMPLE_NUM[i], SAMPLE_EV, POP_EV)
    }

    Percents[i] <- (Percent / Tri)
    if((Percents[i] >= 100 - alpha) && (decide == 0))
      decide <- SAMPLE_NUM[i]
  }

  plot(SAMPLE_NUM, Percents, xlim = c(0, POP_NUM), ylim = c(0, 100)
       , xaxt = "n", yaxt = "n", xlab = "抽出した標本の数(SAMPLE_NUM)"
       , ylab = "構成した信頼区間に母固有値が入った回数(Percents)", pch = 1)
  abline(h = 100 - alpha, col = 'red')

  axis(side = 2, at = c(0, 50, 100), labels = c(0, 50, 100), cex.axis=0.6)
  axis(side = 2, at = 100 - alpha, labels = 100 - alpha, col.ticks ='red', col.axis = "red")
  axis(side = 1, at = c(0, POP_NUM / 2, POP_NUM), labels = c(0, POP_NUM / 2, POP_NUM), cex.axis = 0.6)
 
  if(decide != 0){
     abline(v = decide, col = 'black')
     axis(side = 1, at = decide,　labels = decide, col.ticks ='black', col.axis = "black")
  }
}

library(kernlab)
data(spam)

ALLSPAM <- spam[, 1:57]

ConfiPlot(5, 5, nrow(ALLSPAM), ALLSPAM)