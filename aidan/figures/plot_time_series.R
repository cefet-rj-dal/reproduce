pdf('figures/plot_time_series.pdf', width = 7, height = 5)

datasets <- c('bioenergy', 'climate', 'gdp', 'emissions', 'pesticides', 'fertilizers')

titulo_custom <- function(ds, suf) {
  ds <- tolower(ds)
  suf <- tolower(suf)
  if (ds == 'bioenergy') {
    if (suf == 'biocons') return(expression(bold('Bioenergy (Cons.)')))
    if (suf == 'bioprod') return(expression(bold('Bioenergy (Prod.)')))
  }
  if (ds == 'climate') return(expression(bold('Climate Change')))
  if (ds == 'gdp') return(expression(bold('GDP')))
  if (ds == 'emissions') {
    if (suf == 'ch4')  return(expression(bold('Emissions (CH'[4]*')')))
    if (suf == 'co2')  return(expression(bold('Emissions (CO'[2]*')')))
    if (suf == 'n2o')  return(expression(bold('Emissions (N'[2]*'O)')))
  }
  if (ds == 'pesticides') return(expression(bold('Pesticides')))
  if (ds == 'fertilizers') {
    if (suf == 'k2o')  return(expression(bold('Fertilizers (K'[2]*'O)')))
    if (suf == 'n')    return(expression(bold('Fertilizers (N)')))
    if (suf == 'p2o5') return(expression(bold('Fertilizers (P'[2]*'O'[5]*')')))
  }
  return(expression(bold(paste(ds, suf))))
}

par(mfrow = c(3, 4))
par(mar = c(2, 2, 2, 1))
plot_count <- 0

for (ds in datasets) {
  df <- getExportedValue('tspredbench', ds)
  ts_sel <- names(df)[grepl('usa|china|germany', names(df), ignore.case = TRUE)]
  sufixos <- gsub('^(usa_|china_|germany_)', '', ts_sel, ignore.case = TRUE)
  sufixos_unicos <- unique(sufixos)
  
  for (suf in sufixos_unicos) {
    usa_name   <- ts_sel[grepl(paste0('^usa_', suf, '$'), ts_sel, ignore.case = TRUE)]
    china_name <- ts_sel[grepl(paste0('^china_', suf, '$'), ts_sel, ignore.case = TRUE)]
    germany_name <- ts_sel[grepl(paste0('^germany_', suf, '$'), ts_sel, ignore.case = TRUE)]
    
    if (length(usa_name) == 0 && length(china_name) == 0) next
    
    plot_count <- plot_count + 1
    
    all_series <- c()
    if (length(usa_name) > 0) all_series <- c(all_series, df[[usa_name]])
    if (length(china_name) > 0) all_series <- c(all_series, df[[china_name]])
    if (length(germany_name) > 0) all_series <- c(all_series, df[[germany_name]])
    
    max_len <- max(length(df[[usa_name[1]]]), length(df[[china_name[1]]]), length(df[[germany_name[1]]]))
    
    plot(0, type = 'n',
         xlim = c(1, max_len),
         ylim = range(all_series, na.rm = TRUE),
         xlab = '', ylab = '',
         main = titulo_custom(ds, suf))
    
    if (length(usa_name) > 0) lines(as.numeric(df[[usa_name]]), col = 'blue', lwd = 2)
    if (length(china_name) > 0) lines(as.numeric(df[[china_name]]), col = 'red', lwd = 2)
    if (length(germany_name) > 0) lines(as.numeric(df[[germany_name]]), col = 'forestgreen', lwd = 2)
  }
}

plot(1, type = 'n', xlab = '', ylab = '', axes = FALSE)
text(0.85, 1.25, labels = expression(bold("Legend:")), cex = 1.2, xpd = TRUE)
legend('center',
       legend = c('USA', 'China','Germany'),
       col = c('blue', 'red','forestgreen'),
       lwd = 3,
       bty = 'n',
       cex = 1.2)

dev.off()
