#信頼区間(8)の上限を返す関数
Anderson <- function(A, df, M){ #仮説検定の有意水準A/100のA%，
                                #標本として受け取るデータセットdf，
                                #第M主成分（呼び出し時はM=m+1）の番号を受け取る．

  SL.mp <- sum( (prcomp(df)$sdev[M:ncol(df)])^2 ) #第m+1主成分を考えていることがわかりやすいように，
  SL.sp <- sum( (prcomp(df)$sdev[1:ncol(df)])^2 ) #また引数がM=m+1となるように，関数を定義している．
  SL.sm <- sum( (prcomp(df)$sdev[1:(M-1)])^2 )

  SL.mp2 <- sum( (prcomp(df)$sdev[M:ncol(df)])^4 )
  SL.sm2 <- sum( (prcomp(df)$sdev[1:(M-1)])^4 )

  z <- qnorm(1 - A / 100 , 0, 1) # 100(A/100)%点z（両側検定であれば100(A/2/100)%を作る）

  s.error.n <- z * sqrt( 2*((SL.mp)^2)*(SL.sm2) + 2*((SL.sm)^2)*(SL.mp2) ) #第二項目の分子
  s.error.d <- sqrt(nrow(df)-1) * ((SL.sp)^2)                              #第二項目の分母
  s.error <- s.error.n / s.error.d                                         #第二項目

  ci <- SL.mp/SL.sp + s.error #信頼区間(8)の上限
 return(ci)                   #信頼区間(8)の上限を返す

}

#データセット全てを標本とし，それぞれの標本固有値でAnderson()関数を用いて
#信頼区間の上限を導出する．
#構成した信頼区間が帰無仮説Hと重なるかグラフで確認するため，
#各主成分に対応する標本固有値における信頼区間と帰無仮説の位置関係をプロットする関数．
sdev.Onesided <- function(A, df, Delta){
                          #仮説検定の有意水準A/100のA%，標本として受け取るデータセットdf，
                          #帰無仮説Hのdeltaを受け取る．

  plot(NULL, xlab="m", ylab="Confidence Interval", xlim=c(1, ncol(df)-1), 
       ylim=c(0,1.1))              #各主成分に対応するように信頼区間を表示する．

  for(m in 1:(ncol(df)-1)){ #Anderson()関数にあるsumがm+1より始まるから，
                            #p+1を考えないようにするため，-1をしている．

   ci.upper <- Anderson(A, df, m+1) #第m+1主成分に対応する標本固有値でAnderson()関数を呼び出し，
                                    #信頼区間の上限をci.upperに代入する．

   if(ci.upper >= Delta){           
    points(x=c(m, m), y=c(0, ci.upper), pch=15, cex=0.5, col=2)       #信頼区間に帰無仮説Hが重なれば，
    segments(x0=m, y0=0, x1=m, y1=ci.upper, col=2)                    #その信頼区間を赤色で描画する．
   }                                  
   else{                              
    points(x=c(m, m), y=c(0, ci.upper), pch=15, cex=0.5, col=1)  #信頼区間に帰無仮説Hが重ならなければ，  
    segments(x0=m, y0=0, x1=m, y1=ci.upper, col=1)               #その信頼区間を黒色で描画する．

   }                                  
  }        
                         
  abline(h=Delta, lty=2)  
}
library(kernlab)
data(spam)
library(scar)
data(decathlon)

df1 <- spam[,1:57]           #spamデータ全てのデータセット
df2 <- decathlon             #decathlonデータのデータセット

sdev.Onesided(5, df1, 0.6)  #仮説検定の有意水準A/100を5/100=0.05として，
                            #標本として受け取るデータセットdfをdf1，
                            #ここではspamデータ全てのデータセットとして，
                            #帰無仮説Hのdeltaをdelta=0.6としてsdev.Onesided()関数を呼び出す．
