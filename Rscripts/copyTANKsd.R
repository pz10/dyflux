setkey(TANKsd, TANKcode)
TANKsd <- TANKsd[, lapply(.SD, sd ,na.rm = TRUE) , by = TANKcode, .SDcols = c(ids, "start", "end")]
setnames(TANKsd, c("TANKcode", ids, "start", "end"), c("TANKcode", TANK_ids_sd, "start", "end"))
TANKsd[, start:= TANK$start]
TANKsd[, end:= TANK$end]

# format columns
no.format <- c("TANKcode", "start", "end")
no.format <- copy(names(TANKsd)) %in% no.format
to.format <- copy(names(TANKsd))[!no.format]

TANKsd[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format]

# TANKcode ordering
setkey(TANKsd, TANKcode)

write.table(TANKsd, file= tankfilesd, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
print(  paste0( "TANK sd values ",Sys.time(), " TANK sd file was written to:  ")  )
print(tankfilesd)
