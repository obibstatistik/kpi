source("global.R")

# konverter biblioteksnummer til kommunens navn

isil2name <- function(x){
  isil <- case_when(
    as.character(x) == "746100" ~ "Odense",
    as.character(x) == "785100" ~ "Aalborg",
    as.character(x) == "765700" ~ "Herning",
    as.character(x) == "763000" ~ "Vejle",
    as.character(x) == "775100" ~ "Aarhus",
    as.character(x) == "710100" ~ "København",
    as.character(x) == "726500" ~ "Roskilde",
    as.character(x) == "715700" ~ "Gentofte"
  ) 
  return(isil)
}

eresstattypedansk <- function(x){
 stattype <- case_when(
   as.character(x) == "stattype" ~ "Statistiktype",
   as.character(x) == "visits" ~ "Antal unikke besøgende",
   as.character(x) == "actions" ~ "Samlede antal handlinger",
   as.character(x) == "visit_length" ~ "Gennemsnitlig besøgstid i minutter"
 ) 
 return(stattype)
}
       
       
# konverter integer til danske måneder, forkortet
kortemåneder <- function(x){
  måned <- case_when(
    as.character(x) == "1" ~ "jan",
    as.character(x) == "2" ~ "feb",
    as.character(x) == "3" ~ "mar",
    as.character(x) == "4" ~ "apr",
    as.character(x) == "5" ~ "maj",
    as.character(x) == "6" ~ "jun",
    as.character(x) == "7" ~ "jul",
    as.character(x) == "8" ~ "aug",
    as.character(x) == "9" ~ "sep",
    as.character(x) == "10" ~ "okt",
    as.character(x) == "11" ~ "nov",
    as.character(x) == "12" ~ "dec"
  )
  return(måned)
}

# konverter integer til danske måneder
danskemåneder <- function(x){
  måned <- case_when(
    as.character(x) == "1" ~ "Januar",
    as.character(x) == "2" ~ "Februar",
    as.character(x) == "3" ~ "Marts",
    as.character(x) == "4" ~ "April",
    as.character(x) == "5" ~ "Maj",
    as.character(x) == "6" ~ "Juni",
    as.character(x) == "7" ~ "Juli",
    as.character(x) == "8" ~ "August",
    as.character(x) == "9" ~ "September",
    as.character(x) == "10" ~ "Oktober",
    as.character(x) == "11" ~ "November",
    as.character(x) == "12" ~ "December"
  ) 
  return(måned)
}

# konverter integer til danske dage
danskedage <- function(x){
  måned <- case_when(
    as.character(x) == "1" ~ "Mandag",
    as.character(x) == "2" ~ "Tirsdag",
    as.character(x) == "3" ~ "Onsdag",
    as.character(x) == "4" ~ "Torsdag",
    as.character(x) == "5" ~ "Fredag",
    as.character(x) == "6" ~ "Lørdag",
    as.character(x) == "7" ~ "Søndag"
  ) 
  return(måned)
}

# fjerner na værdier fra tabel
fjernna = function(var, decimal.places) {
  var = sprintf(paste0("%1.",decimal.places,"f"), var)
  var[var=="NA"] = ""
  var
}

# procent funktion, som ser ud til at virke overalt

procenten <- function(x){
  paste(round(100*(x), 0), "%", sep="")
}

# find hverdage

Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday", "lørdag", "søndag" )))

