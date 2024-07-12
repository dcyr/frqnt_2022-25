
### Snags (compute TimeSinceDeath based on YearOfDeath and T0)
snags_init_fnc <- function(snags, file) {
    # formatting and writing to file
    
    sink(file)
    
    cat('LandisData "ForC Succession"')
    cat("\n")
    cat("\n")
    cat("SnagData")
    cat("\n")
    cat("\n")
    cat(c(">>species\tAgeAtDeath\tTimeSinceDeath\tCause"))
    cat("\n")
    cat("\n")
    sink()
    
    write.table(snags, file,
                append = T,
                row.names = F,
                col.names = F,
                sep = "\t",
                quote = F,
                #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                eol = "\n") #default line endings on windows system.)
    
}


