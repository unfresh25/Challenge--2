
cat('Esta función fue creada por Jorge Borja', '\n')
cat('<https://jorgeborja-portfolio.vercel.app/> para la validación', '\n')
cat('de la construcción de un modelo de regresión lineal múltiple.', '\n', '\n')

cat('En caso de presentar algún inconveniente, por favor', '\n')
cat('repórtelo a <jborja@uninorte.edu.co>', '\n', '\n')
cat('Última modificación: Junio 05, 2024', '\n', '\n')

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
  
  
  message("1. Validación")
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
  
  message("2. Verificación de residuales")
  print(out_r)
  
  message("3. Multicolinealidad")
  print(c("Multicolinealidad" = mul))
  if(!(mul == "Cumple")) {
    print(c(vif(model)))
  }
  
  message("4. Outliers/Influenciales")
  out_oi <- c(
    "Outliers" = nout,
    "Influenciales" = inf
  )
  print(out_oi)
  if(plot) {
    plot(model, which = 4, las = 1)
  }
}
