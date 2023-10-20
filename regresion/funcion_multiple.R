library(nortest)
library(lmtest)
setwd("/home/hectormontes/Escritorio/Maestria/pronosticos/regresion")
df_precios <- read.csv("precios_inm_multiple.csv", sep=",")
predictoras <- c("p_est_1","p_est_2","p_est_3",	"p_est_4","p_est_5",
                 "p_est_6","ICI","tc")

df_modelos <- data.frame(p_est_1=numeric(),
                       p_est_2=numeric(),
                       p_est_3=numeric(),
                       p_est_4=numeric(),
                       p_est_5=numeric(),
                       p_est_6=numeric(),
                       ICI=numeric(),
                       tc=numeric()
                       )

for (i in c(0,1)){
  for (j in c(0,1)) {
    for (k in c(0,1)){
      for (l in c(0,1)){
        for (m in c(0,1)){
          for (n in c(0,1)){
            for(o in c(0,1)){
              for (p in c(0,1)){
                df_inc = data.frame(
                  p_est_1 = i,
                  p_est_2 = j,
                  p_est_3= k,
                  p_est_4= l,
                  p_est_5=m,
                  p_est_6=n,
                  ICI=o,
                  tc=p
                )
                df_modelos<-rbind(df_modelos, df_inc)
              }
            }
          }
        }
      }
    }
  }
}

obtener_p_values<-function(binarias){
  cols <- colnames(binarias)[binarias==1]
  formula_mod = paste("price~")
  w<-0
  for (col in cols){
    if (w==0){
      formula_mod <- paste0(formula_mod,col)
    }else{
      formula_mod <- paste0(formula_mod, "+", col)  
    }
    w<- w+1
  }
  
  model <- lm(formula=formula_mod, data=df_precios)
  p_value_lillie<-lillie.test(model$residuals)$p.value
  p_value_BP<-bptest(model)$p.value
  summary_mod <- summary(model)
  rownames(summary_mod$coefficients)
  df_final<-df_modelos[df_modelos$p_est_1==10,]
  df_final$`(Intercept)`<-numeric()
  for (z in colnames(df_final)){
    if(z%in%rownames(summary_mod$coefficients)){
      p_value<-summary_mod$coefficients[z,"Pr(>|t|)"]
      df_final[1,z]<-p_value
    }else{
      p_value<-NaN
      df_final[1,z]<-p_value
    }
  }
  df_final$p.value_lillie<-p_value_lillie
  df_final$p.value_BP<-p_value_BP
  return(df_final)
}

df_p_values<-obtener_p_values(df_modelos[2,])
for (i in 3:nrow(df_modelos)){
  fila<-obtener_p_values(df_modelos[i,])
  df_p_values<-rbind(df_p_values, fila)
}
df_p_values