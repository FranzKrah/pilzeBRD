#' Abfrage Pilzartenliste Deutschlands nach MTB 
#' 
#' @param suche ist das MTB (MessTischBlatt) für das eine Artenliste abgefragt werden soll
#' @details hier kommen noch Details
#' @return Gibt einen Vektor mit Arten zurueck
#' @references \url{http://brd.pilzkartierung.de}
#' @examples 
#' \dontrun{
#' pilze_BRD(suche = "Regensburg")
#' }
#' @export

pilze_BRD <- function(suche){
  require("stringr")
  require("XML")
  
  if(!is.character(suche)) stop("'Suche' ist nicht aus der class character")
  
  url <- "http://brd.pilzkartierung.de/f2sqlmtb.php"
  kart <- htmlTreeParse(url, useInternalNodes = T)
  opts <- sapply(getNodeSet(kart, "//option"), xmlValue)
  opts <- gsub("\\s+$", "", opts)
  opts <- unique(opts)
  
  su <- grep(suche, opts)
  if(length(su)==0) {stop("Suchbegriff nicht gefunden")}
  
  opt <- data.frame(options = opts[su])
  
  if(nrow(opt)>1) {
    print(cbind.data.frame(nr = 1:nrow(opt), option = as.character(opt[,1])))
    x <- readline("Welches Gebiet möchtest Du waehlen? (Nummer eingeben und ENTER)   ")
    x <- as.numeric(x)
  }
  if(nrow(opt)==1){ x <- 1}
  
  url2 <- paste("http://brd.pilzkartierung.de/f2topkstart.php?lfundanzeige=true&ctopknr=", 
    gsub(" ", "\\+", opt[x,]),"+%A0%A0%A0%A0", sep="")
  
  spec_table <- readHTMLTable(url2)
  main_tab <- spec_table[[1]]
  main_tab[] <- unlist(lapply(main_tab, as.character))
  main_tab <- rbind.data.frame(names(main_tab), main_tab)
  names(main_tab) <- c("Art_Autor", "MTB", "Quadrant", "Teilquadrant", 
    "Teilquadrant2",  "FinderKürzel", "Finder", "Ort", "Habitat", 
    "Datum", "Sonstiges", "","Wirt", "","Bundesland", "Date", "Nr")
  
  rec <- gsub("^\\s+", "", main_tab[,1])
  recs <- lapply(rec, word, start = 1, end = 2)
  main_tab <- cbind.data.frame(Art = unlist(recs), main_tab)
  main_tab$Nr <- gsub(" bearbeiten", "", main_tab$Nr)
  gsub("+$", "", main_tab$Nr)
  main_tab$Nr <- main_tab[main_tab==""] <- NA
  main_tab <- main_tab[,!(colSums(is.na(main_tab)) == nrow(main_tab))]
  return(main_tab)
}
