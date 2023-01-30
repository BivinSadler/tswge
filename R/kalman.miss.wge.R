kalman.miss.wge=function(y,start,gam0,F,gamV,Gtmiss,gamW){
  n=length(y)
  sqgamV=sqrt(gamV)
  sqgamW=sqrt(gamW)
  chgamW=chol(gamW)
  yy=y
  
  #Fix from David Stoffer 1-29-2023
  result=astsa::Ksmooth(y=yy, A=Gtmiss, mu0=start, Sigma0=gam0, Phi=F, sQ=sqgamV, sR=chgamW)
  # end fix
  
  pfs=cbind(yy,result$xp,result$Pp,result$xf,result$Pf,result$xs,result$Ps)
  colnames(pfs)=c("Data","Prediction","Var_Predict","Filter","Var_Filter","Smooth","Var_Smooth")
  return(pfs)
}