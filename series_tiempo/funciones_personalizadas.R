## Función para crear una tabla con una impresión más estética
ftable <- function(x) {
  x %>% 
    flextable() %>% 
    theme_vanilla() %>%
    color(part = "footer", color = "#666666") %>%
    color( part = "header", color = "#FFFFFF") %>%
    bg( part = "header", bg = "#2c7fb8") %>%
    fontsize(size = 11) %>%
    font(fontname = 'Calibri') %>%
    # Ajustes de ancho y tipo de alineación de las columnas
    set_table_properties(layout = "autofit") %>% 
    # width(j=1, width = 3) %>%
    align(i = NULL, j = c(2:ncol(x)), align = "right", part = "all")
}

# Función para crear un gráfico para la prueba de Lilliefors basada en un
# dataframe de variables nombradas x y y.

graph_lilliefors <- function(df, for_residuals, x_label, y_label) {
  subtitle_text <- if (for_residuals) paste0(y_label," vs ", x_label) else ""
  if (for_residuals){
    modelo <- lm(formula = y~x, data=df)
    n <- length(modelo$residuals)
    df_res<-data.frame(residuals = modelo$residuals)
    lillie_test <- lillie.test(modelo$residuals)
  }else{
    df_res <-data.frame(residuals=df$residuals)
    lillie_test <- lillie.test(df$residuals)
  }
  df_res <- df_res %>% 
    arrange(residuals)
  mean_res <- mean(df_res$residuals)
  sd_res <- sd(df_res$residuals)
  cdf_teorica <- pnorm(df_res$residuals,
                       mean = mean_res,
                       sd = sd_res)
  cdf_empirica <- ecdf(df_res$residuals)
  diffs_abs <- abs(cdf_empirica(df_res$residuals) - cdf_teorica)
  posicion <- which.max(diffs_abs)
  estadistico_l <- diffs_abs[posicion]
  print(paste0("Este es el valor del estadístico: ", estadistico_l,
               " y esta es la posición en ",
               "la que ocurre la máxima diferencia ",
               posicion))
  x_posicion <- df_res$residuals[posicion]
  y_inferior <- cdf_teorica[posicion]
  y_superior <- cdf_empirica(x_posicion)
  
  cat("x_posicion: ", format(x_posicion, digits=5), 
      " y_inferior: ", format(y_inferior, digits=5),
      " y_superior: ", format(y_superior, digits=5),
      sep='')
  
  ggplot(df_res, aes(x = residuals)) +
    stat_function(fun = pnorm, args = list(mean = mean_res,
                                           sd = sd_res),
                  color = "red", size = 1) +
    stat_ecdf(color = "green", size = 1) +
    geom_segment(aes(x = x_posicion, y = y_inferior, xend = x_posicion,
                     yend = y_superior),
                 color = "purple", size = 1, linetype = "solid") +
    geom_point(aes(x = x_posicion, y = y_inferior),
               color = "purple",size = 3) +
    geom_point(aes(x = x_posicion, y = y_superior),
               color = "purple", size = 3) +
    annotate("text", x = x_posicion , y = (y_inferior + y_superior)/2, 
             label = paste0("Máxima Separación=",
                            format(estadistico_l,
                                   digits=5)," 🤔💭", sep=''),
             color = "purple", size = 3, hjust = -0.05, vjust = 0) +
    coord_cartesian(xlim = c(min(df_res$residuals),
                             max(df_res$residuals))) +
    xlab("Residuales del modelo") +
    ylab("Probabilidad acumulada") +
    labs(title = "Esquema de funcionamiento de la prueba de Lilliefors",
         subtitle = paste0(subtitle_text,"\np_value=",
                           lillie_test$p.value))+
    theme_minimal()
}