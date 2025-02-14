
# deskr_metric: Funktion für deskriptive Statistiken für metrische Variablen
# Input: - x: numerischer Vektor
# output: - Liste vershiedener Deskriptiver Werte
deskr_metric <- function(x) {
  if (!is.numeric(x)) {
    stop("Die Variable muss numerisch sein.")
  }
  
  # Berechnungen
  stats <- list(
    Mittelwert = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Standardabweichung = sd(x, na.rm = TRUE),
    Varianz = var(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    Quantile_25 = quantile(x, 0.25, na.rm = TRUE),
    Quantile_50 = quantile(x, 0.50, na.rm = TRUE),  # entspricht dem Median
    Quantile_75 = quantile(x, 0.75, na.rm = TRUE)
  )
  
  return(statistik)
}

# deskr_factor: Funktion für deskriptive Statistiken für kategoriale Variablen
# input: - x: kategorieller Vektor
# output: - Liste asu Kennwerten fuer Kategorielle Daten
deskr_factor <- function(x) {
  if (!is.factor(x) && !is.character(x)) {
    stop("Die Variable muss kategorial (Faktor oder Charakter) sein.")
  }
  
  # Häufigkeitstabelle
  freq_table <- table(x)
  
  # Prozentsätze
  percentages <- prop.table(freq_table) * 100
  
  # Gleichverteilungstest
  exp <- expected_freq(x)
  
  chi2 <- chisq.test(freq_table, p = exp, rescale.p = TRUE)
  
  # Streuung
  gini_index <- gini(x)
  
  # Ausgabe als Liste
  statistic <- list(
    Häufigkeit = freq_table,
    Prozentsätze = percentages,
    Chi2 = unname(chi2$statistic),
    Chi2.p = chi2$p.value,
    Gini = gini_index
  )
  
  return(statistic)
}

#weiß nicht ob die nötig ist, gibt ja schon prop.table, also falls wer ne bessere hat gerne einfügen
# Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt
# tabelle mit Anteilen zutückgeben
bivariat_kategorial <- function(var1, var2) {
  tabelle <- table(var1, var2)
  
  prop_tabelle <- prop.table(tabelle, margin = 1)
  return(prop_tabelle)
}


# Eine Funktion, die geeignete deskriptive bivariate Statistiken für den Zusammengang zwischen einer metrischen und einer dichotomen Variablen berechnet und ausgibt





#Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt

visualize_categorical <- function(data, var1, var2, var3, var4 = NULL) {
  # Überprüfen, ob die vierte Variable angegeben wurde
  if (is.null(var4)) {
    # Kontingenztabelle für drei Variablen
    contingency_table <- table(data[[var1]], data[[var2]], data[[var3]])
    
    # Mosaikplot für drei Variablen
    mosaicplot(contingency_table, 
               main = paste("Visualisierung von", var1, "nach", var2, "und", var3),
               xlab = var1,  
               ylab = paste(var2, "und", var3),
               color = c("red", "blue", "green"),
               las = 1,  # Horizontale Achsenbeschriftung für bessere Lesbarkeit
               cex.axis = 0.8,  # Kleinere Schrift für Achsenbeschriftungen
               border = "gray")  # Dezentere Linien für bessere Optik
  } else {
    # Kontingenztabelle für vier Variablen
    contingency_table <- table(data[[var1]], data[[var2]], data[[var3]], data[[var4]])
    
    # Mosaikplot für vier Variablen
    mosaicplot(contingency_table, 
               main = paste("Visualisierung von", var1, "nach", var2, var3, "und", var4),
               xlab = var1,  
               ylab = paste(var2, var3, "und", var4),
               color = c("red", "blue", "green", "orange"),
               las = 1,  
               cex.axis = 0.8,
               border = "gray")
  }
}





# Freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen
