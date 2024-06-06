validar_supuestos <- function(model, plot = F) {
  s <- summary(model)
  
  # Validación
  ## Prueba de significancia global
  psg <- ifelse(
    s$fstatistic[[1]] > 1,
    "Significativo",
    "No significativo"
  )
  ## Prueba de significancia marginal
  psm <- ifelse(
    any(s$coefficients[, 4] < 0.05),
    "Significativas",
    "No significativo"
  )
  ## Ajuste del modelo (R2 ajustado)
  radj <- ifelse(
    s$adj.r.squared > 0.75,
    "Significativo",
    "No significativo"
  )
  
  # Verificación de supuestos del error
  st_r <- rstudent(model)
  ## Normalidad
  nor <- ifelse(
    shapiro.test(st_r)$p.value >= 0.05,
    "Cumple",
    "No cumple"
  )
  ## Independencia
  ind <- ifelse(
    durbinWatsonTest(fit)$p >= 0.05,
    "Cumple",
    "No cumple"
  )
  ## Homocedasticidad
  hom <- ifelse(
    ncvTest(fit)$p >= 0.05,
    "Cumple",
    "No cumple"
  )
  
  # Multicolinealidad
  mul <- ifelse(
    any(mctest(model)$odiags[, 2] == 0),
    "Cumple",
    "No cumple"
  )
  
  # Outliers/Influenciales
  ## Outliers 
  res <- which(st_r > 3 | st_r < -3)
  nout <- ifelse(
    length(res) == 0, 
    "No hay outliers", 
    length(res)
  )
  ## Influenciales
  cooksd <- cooks.distance(model)
  obs_inf <- which(cooksd > 0.05)
  inf <- ifelse(
    length(obs_inf) > 0,
    paste(obs_inf, collapse = ", "),
    "No hay influenciales"
  )
  
  
  message("Validación")
  out_v <- c(
    "Significancia Global" = psg,
    "Significancia Marginal" = psm,
    "R2 ajustado" = radj
  )
  print(out_v)
  
  out_r <- c(
    "Normalidad" = nor,
    "Independencia" = ind,
    "Homocedasticidad" = hom
  )
  
  message("Verificación de residuales")
  print(out_r)
  
  message("")
  print(c("Multicolinealidad" = mul))
  if(!(mul == "Cumple")) {
    print(c(vif(model)))
  }
  
  message("Outliers/Influenciales")
  out_oi <- c(
    "Outliers" = nout,
    "Influenciales" = inf
  )
  print(out_oi)
  if(plot == T) {
    plot(model, which = 4, las = 1)
  }
}
