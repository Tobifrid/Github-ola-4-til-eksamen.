# =====================================================================
# OLA 4 – Opgave 3.5: Automatisk overvågning af Nordsøen
# =====================================================================
# Dette script kører helt af sig selv, fx hver time med cron:
# 1) henter friske fly og nyt token, 2) finder mistænkelige fly med min algoritme,
# 3) gemmer listen i en CSV med dato og klokkeslæt, 4) sender den på mail.
# Nederst står, hvordan man sætter det til at køre hver time.

setwd("~/Documents/OLA 4")                                       # SKRIV HER: stien til dit R-projekt (cron starter ikke i projektmappen)

library(dplyr)
library(emayili)
source("Funktioner til OLA 4.R")                                 # overvaag_omraade() og de andre funktioner
readRenviron("~/.Renviron")                                      # login til OpenSky og mail

# ---- Området, der overvåges (samme bbox som i opgave 3.1) ----
lamin <- 52.055129                                               # syd
lomin <- -6.065140                                               # vest
lamax <- 56.196869                                               # nord
lomax <- 4.305954                                                # øst

# ---- Find mistænkelige fly lige nu ----
mistaenkelige_ruter <- overvaag_omraade(lamin, lomin, lamax, lomax)   # nye kald til OpenSky

if (nrow(mistaenkelige_ruter) > 0) {                             # kun gem og send, hvis der er fundet noget
  mistaenkelige <- mistaenkelige_ruter |> distinct(icao)         # én række pr. fly
  
  filnavn <- paste0("mistaenkelige_fly_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv")   # dato og klokkeslæt i filnavnet
  write.csv(mistaenkelige, filnavn, row.names = FALSE)           # gem listen, tidligere kørsler overskrives ikke
  
  mail <- envelope() |>
    from(Sys.getenv("MAIL_ADRESSE")) |>
    to(Sys.getenv("MAIL_ADRESSE")) |>                            # TEST: kun til mig selv. I drift: sigint-20@fe.dk
    subject(paste("Mistænkelige fly over Nordsøen", format(Sys.time(), "%d-%m-%Y kl. %H:%M"))) |>
    text(paste("Mistænkelige fly:", paste(mistaenkelige$icao, collapse = ", "))) |>
    attachment(filnavn)                                          # vedhæft CSV-filen
  
  smtp <- server(host     = "smtp.office365.com",
                 port     = 587,
                 username = Sys.getenv("MAIL_ADRESSE"),
                 password = Sys.getenv("MAIL_KODE"))
  
  # smtp(mail)                                                   # virker ikke fra skolens Microsoft 365 (fejl 535), se opgave 3.5
}


# =====================================================================
# Sådan sættes scriptet til at køre hver time (kør linjerne ÉN gang i konsollen)
# =====================================================================
# install.packages("cronR")
# library(cronR)
# kommando <- cron_rscript("~/Documents/OLA 4/overvaag.R")       # SKRIV HER: stien til denne fil
# cron_add(kommando, frequency = "hourly",
#          id = "fly_overvaagning", description = "Mistænkelige fly over Nordsøen")   # kør hver time
# cron_ls()                                                      # tjek, at jobbet er oprettet
# cron_rm(id = "fly_overvaagning")                               # stop overvågningen igen
#
# Bemærk: cron kører kun, når Mac'en er tændt, og hver kørsel bruger kald fra OpenSkys daglige grænse.