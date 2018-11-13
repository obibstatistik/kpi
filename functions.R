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

# procent funktion, som ser ud til at virke overalt

procenten <- function(x){
  paste(round(100*(x), 0), "%", sep="")
}

# find hverdage

Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday", "lørdag", "søndag" )))