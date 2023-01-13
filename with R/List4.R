#lambda_iの信頼区間(5)'の下限を返す関数
Anderson <- function(A, N, L){ #仮説検定の有意水準A/100のA%，
                               #標本数N，標本固有値Lを受け取る．

  z <- qnorm(1 - A / 100, 0, 1) # 100(A/100)%点z（両側検定であれば100(A/2/100)%を作る）
  
  ci <- L / (1 + sqrt(2 / (N - 1)) * z) #lambda_iの信頼区間の下限，(5)の左辺
  
 return(ci) #lambda_iの信頼区間の下限を返す

}

#データセット全てを標本とし，それより第一主成分から順に対応する標本固有値を導出する．
#それぞれの標本固有値でAnderson()関数を用いて信頼区間の下限を導出する．
#構成した信頼区間が帰無仮説H：lambda_i<=lambda_i^0と重なるかグラフで確認するため，
#各主成分に対応する標本固有値における信頼区間と帰無仮説の位置関係をプロットする関数．
AndersonConInt <- function(A, df, Lam0, y.lower, y.upper){
                          #仮説検定の有意水準A/100のA%，標本として受け取るデータセットdf，
                          #帰無仮説H：lambda_i<=Lam0，グラフ自体のy軸の上限(y.upper)と下限(y.lower)を受け取る．
  M <- ncol(df) #データセットの次元数
  plot(NULL, xlab="主成分", ylab="Confidence Interval", xlim=c(1, M), 
       ylim=c(y.lower, y.upper)) #各主成分に対応するように信頼区間を表示する．
                 
  for(i in 1:M){
   ci.lower <- Anderson(A, nrow(df), (prcomp(df)$sdev[i]) ^ 2) 
                                     #第i主成分に対応する標本固有値でAnderson()関数を呼び出し，
                                     #信頼区間の下限をci.lowerに代入する．
   if( ci.lower > Lam0){
    points(x=i, y=ci.lower, pch=15, cex=0.5, col=1)       #信頼区間に帰無仮説H：lambda_i<=Lam0が重ならなければ，
    segments(x0=i, y0=ci.lower, x1=i, y1=2*y.upper, col=1)#その信頼区間を黒色で描画する．
   }                                  
   else{                              
    points(x=i, y=ci.lower, pch=15, cex=0.5, col=2)       #信頼区間に帰無仮説H：lambda_i<=Lam0が重なれば，
    segments(x0=i, y0=ci.lower, x1=i, y1=2*y.upper, col=2)#その信頼区間を赤色で描画する．
   }                                  
  }                                 
  abline(h=Lam0, lty=2)   #帰無仮説H：lambda_i<=Lam0の位置に点線を引く．              

}

library(kernlab)
data(spam)
library(scar)
data(decathlon)

df1 <- spam[,1:57]           #spamデータ全てのデータセット
df2 <- spam[1:1813, 1:57]    #spamデータのラベル"spam"のみのデータセット
df3 <- spam[1814:4601, 1:57] #spamデータのラベル"nonspam"のみのデータセット
df4 <- decathlon             #decathlonデータのデータセット
df5 <- iris[, 1:4]           #irisデータのデータセット

Mu <- prcomp(df2)$center     #df2のデータセットの期待値ベクトル
Sigma <- cov(df2)            #df2のデータセットの共分散行列
a <- 2000                    #標本のデータセットとして作りたい標本の数
library(MASS)
df6 <- mvrnorm(a, Mu, Sigma) #標本として考えたいデータセットを生成する．

AndersonConInt(1, df1, 1.0, 0, 5)  #仮説検定の有意水準A/100を1/100=0.01として，
                                   #標本として受け取るデータセットdfをdf1，
                                   #ここではspamデータ全てのデータセットとして，
                                   #帰無仮説H：lambda_i<=Lam0をLam0=1.0，
                                   #グラフ自体のy軸の上限を5，下限を0としてAndersonConInt()関数を呼び出す．