# =====================================================================
# Loops til OLA 4 opgave 1.R: alle loops til opgave 1 samlet ét sted
# Hentes ind i opgavescriptet med source("Loops til OLA 4 opgave 1.R")
#
# Loopene er pakket ind i funktioner. Derfor sker der INGENTING, når
# filen sources. Funktionerne bliver kun kørt, når man kalder dem.
# =====================================================================
#
# OVERSIGT OVER FUNKTIONERNE
#
# hent_bilbasen_sider(basis_url, antal_sider)
#   Henter søgesiderne fra Bilbasen med chromote og giver en liste,
#   hvor hver plads er én side som HTML-tekst. Tager ca. 4 minutter.
#
# udtraek_bilbasen_biler(alle_sider)
#   Går igennem alle søgesiderne og alle bilkort på hver side og laver
#   én tabel med carid, model, region, pris, km, registreringsdato og link.
#
# hent_forhandlere(alle_biler)
#   Besøger hver bils egen side og henter forhandlerens navn, adresse og CVR.
#   Bruger tryCatch, så loopet ikke stopper, hvis browseren går ned.
#   Tager ca. 1½ time.
#
# find_kommer_snart(alle_sider)
#   Finder id'erne på de biler, hvor der står "kommer snart" i bilkortet.
#
# hent_autoscout_sider(basis_url, antal_sider)
#   Henter søgesiderne fra autoscout24.de med chromote. Samme idé som Bilbasen.
#
# udtraek_tyske_biler(alle_tyske_sider)
#   Laver én tabel med de tyske biler. Data ligger som attributter på
#   bilkortene, så der skal kun ét loop til (ét pr. side, ikke ét pr. bil).
#
# hent_power_side_med_scroll(basis_url, antal_scroll)
#   Henter Powers kampagneside med chromote og scroller ned, så alle produkter
#   når at loade (lazy loading). Giver hele siden som HTML-tekst.
# =====================================================================

library(chromote)
library(rvest)
library(stringr)
library(dplyr)


# ---- 1. Hent Bilbasens søgesider ----
hent_bilbasen_sider <- function(basis_url, antal_sider) {
  browser    <- ChromoteSession$new()                                         # én usynlig Chrome til alle sider
  alle_sider <- list()                                                        # tom liste før loopet
  
  for (i in 1:antal_sider) {                                                  # én side ad gangen
    browser$Page$navigate(str_c(basis_url, i))                                # fx ...&page=1, ...&page=2
    Sys.sleep(5)                                                              # vent på loading og vær høflig over for Bilbasen
    alle_sider[[i]] <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value   # gem siden som tekst
    print(str_c("Side ", i, " hentet"))                                       # så man kan følge med
  }
  
  browser$close()                                                             # luk browseren
  return(alle_sider)
}


# ---- 2. Pil bilerne ud af Bilbasens søgesider ----
udtraek_bilbasen_biler <- function(alle_sider) {
  alle_biler <- data.frame()                                                  # tom tabel før loopet
  
  for (i in 1:length(alle_sider)) {                                           # yderste loop: én side ad gangen
    side      <- read_html(alle_sider[[i]])                                   # tekst → HTML
    bil_liste <- side |> html_elements("article")                             # article = ét bilkort (fundet med Inspicér)
    
    for (bil in bil_liste) {                                                  # inderste loop: én bil ad gangen
      bil_tekst          <- bil |> html_text2()                               # bilkortet som tekst
      pris               <- str_extract(bil_tekst, "[\\d.]+ kr")              # cifre eller punktum + " kr", fx "419.900 kr"
      km                 <- str_extract(bil_tekst, "[\\d.]+ km")              # fx "15.500 km"
      registrerings_dato <- str_extract(bil_tekst, "\\d{1,2}/\\d{4}")         # måned/år, fx "6/2024"
      bil_link           <- bil |> html_element("a") |> html_attr("href")     # links står altid i <a href="...">
      carid              <- str_extract(bil_link, "\\d+$")                    # cifrene helt til sidst i linket = bilens id
      bilens_attributter <- str_split(bil_tekst, "\n")[[1]]                   # teksten delt op i linjer
      model              <- str_subset(bilens_attributter, "^BMW")[1]         # første linje, der starter med BMW (springer "Ny annonce" over)
      region             <- bilens_attributter[length(bilens_attributter)]    # sidste linje = by og landsdel
      
      en_bil <- data.frame(                                                   # én række for denne bil
        carid              = carid,
        model              = model,
        region             = region,
        pris               = pris,
        km                 = km,
        registrerings_dato = registrerings_dato,
        link               = bil_link
      )
      alle_biler <- bind_rows(alle_biler, en_bil)                             # læg bilen nederst i tabellen
    }
  }
  return(alle_biler)
}


# ---- 3. Hent forhandlerinfo fra hver bils egen side ----
hent_forhandlere <- function(alle_biler) {
  browser          <- ChromoteSession$new()                                   # én browser til alle bilsider
  alle_forhandlere <- data.frame()                                            # tom tabel før loopet
  
  for (i in 1:nrow(alle_biler)) {                                             # én bil ad gangen
    bilside_tekst <- tryCatch({                                               # prøv at hente siden ...
      browser$Page$navigate(alle_biler$link[i])                               # gå til bil nr. i
      Sys.sleep(4)                                                            # vent på loading og vær høflig
      browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    }, error = function(fejl) {                                               # ... og hvis det fejler:
      print(str_c("Fejl ved bil ", i, ", starter ny browser"))                # skriv fejlen
      browser <<- ChromoteSession$new()                                       # ny browser, så loopet kan fortsætte
      NULL                                                                    # giv NULL tilbage
    })
    if (is.null(bilside_tekst)) next                                          # spring bilen over, hvis den fejlede
    
    forhandler_boks  <- read_html(bilside_tekst) |>
      html_element('[data-e2e="vip-section-dealer-info"]')                    # forhandlerboksen (data-e2e er Bilbasens egen mærkning)
    forhandler_tekst <- forhandler_boks |> html_text2()                       # al teksten i boksen
    
    en_forhandler <- data.frame(
      carid              = alle_biler$carid[i],                               # så vi kan koble til bilen bagefter
      forhandler_navn    = forhandler_boks |> html_element("h2") |> html_text2(),        # navnet står i <h2>
      forhandler_adresse = str_extract(forhandler_tekst, "[^\n]+, \\d{4} [^\n]+"),        # linjen med ", 4 cifre" = vej, postnr og by
      forhandler_cvr     = str_extract(forhandler_tekst, "\\d{8}")                       # 8 cifre i træk = CVR-nummer
    )
    alle_forhandlere <- bind_rows(alle_forhandlere, en_forhandler)            # læg nederst i tabellen
    
    if (i %% 100 == 0) {                                                      # for hver 100. bil (modulo)
      saveRDS(alle_forhandlere, "alle_forhandlere.rds")                       # gem undervejs, så intet går tabt
      print(str_c(i, " bilsider hentet"))
    }
  }
  
  browser$close()
  return(alle_forhandlere)
}


# ---- 4. Find biler, der "kommer snart" ----
find_kommer_snart <- function(alle_sider) {
  kommer_snart <- c()                                                         # tom vektor før loopet
  
  for (i in 1:length(alle_sider)) {                                           # én side ad gangen
    bil_liste <- read_html(alle_sider[[i]]) |> html_elements("article")
    for (bil in bil_liste) {                                                  # én bil ad gangen
      bil_tekst <- bil |> html_text2()
      if (str_detect(str_to_lower(bil_tekst), "kommer snart")) {              # str_to_lower: store/små bogstaver er ligegyldige
        bil_link     <- bil |> html_element("a") |> html_attr("href")
        kommer_snart <- c(kommer_snart, str_extract(bil_link, "\\d+$"))       # gem bilens id
      }
    }
  }
  return(kommer_snart)
}


# ---- 5. Hent autoscout24's søgesider ----
hent_autoscout_sider <- function(basis_url, antal_sider) {
  browser          <- ChromoteSession$new()                                   # én usynlig Chrome til alle sider
  alle_tyske_sider <- list()                                                  # tom liste før loopet
  
  for (i in 1:antal_sider) {                                                  # én side ad gangen
    browser$Page$navigate(str_c(basis_url, i))                                # ...&page=1, ...&page=2
    Sys.sleep(5)                                                              # vent på loading og vær høflig
    alle_tyske_sider[[i]] <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    print(str_c("Tysk side ", i, " hentet"))
  }
  
  browser$close()
  return(alle_tyske_sider)
}


# ---- 6. Pil de tyske biler ud af attributterne ----
udtraek_tyske_biler <- function(alle_tyske_sider) {
  alle_tyske_biler <- data.frame()                                            # tom tabel før loopet
  
  for (i in 1:length(alle_tyske_sider)) {                                     # kun ét loop: én side ad gangen
    tyske_bilkort    <- read_html(alle_tyske_sider[[i]]) |> html_elements("article")   # alle 20 biler på siden
    bilkort_tekst    <- tyske_bilkort |> html_text2()                         # teksten fra alle 20 bilkort
    
    en_tysk_side <- data.frame(                                               # html_attr på alle 20 kort giver 20 værdier på én gang
      tysk_bil_id         = tyske_bilkort |> html_attr("data-guid"),          # bilens unikke id
      model               = tyske_bilkort |> html_attr("data-model"),         # fx "530"
      pris_eur            = tyske_bilkort |> html_attr("data-price"),         # pris i euro
      km                  = tyske_bilkort |> html_attr("data-mileage"),       # kilometer
      forste_registrering = tyske_bilkort |> html_attr("data-first-registration"),   # fx "10-2020"
      braendstof          = tyske_bilkort |> html_attr("data-fuel-type"),     # 2 = hybrid, E = el
      saelger_type        = tyske_bilkort |> html_attr("data-seller-type"),   # d = forhandler
      forhandler_id       = tyske_bilkort |> html_attr("data-customer-id"),   # forhandlerens id på autoscout24
      postnummer          = tyske_bilkort |> html_attr("data-listing-zip-code"),     # forhandlerens postnummer
      forhandler_navn     = str_extract(bilkort_tekst, "[^\n]+(?=DE-\\d{5})"),     # alt på linjen FØR "DE-postnr" (navn og DE står klistret sammen)
      forhandler_by       = str_extract(bilkort_tekst, "(?<=DE-\\d{5} )[^\n]+")      # resten af linjen EFTER "DE-postnr " (lookbehind)
      # link mangler: <a>-tags på autoscout24 har intet href (findes næste gang)
    )
    alle_tyske_biler <- bind_rows(alle_tyske_biler, en_tysk_side)             # læg siden nederst
  }
  return(alle_tyske_biler)
}

# ---- Hent Powers kampagneside og vis hvert produkt på skærmen, så det bliver bygget (lazy loading) ----
hent_power_side_med_scroll <- function(basis_url) {
  browser <- ChromoteSession$new()                                                  # én usynlig Chrome
  browser$Page$navigate(basis_url)
  Sys.sleep(5)                                                                      # lad siden starte
  
  antal_kort <- browser$Runtime$evaluate(
    "document.querySelectorAll('pwr-product-item-wrapper').length")$result$value     # hvor mange produktkort er der?
  
  for (k in 0:(antal_kort - 1)) {                                                   # JavaScript tæller fra 0
    browser$Runtime$evaluate(paste0(
      "document.querySelectorAll('pwr-product-item-wrapper')[", k, "].scrollIntoView({block: 'center', inline: 'center'})"))   # vis kort nr. k
    Sys.sleep(1)                                                                    # vent, så Power når at bygge kortet
  }
  
  power_html <- browser$Runtime$evaluate("document.documentElement.outerHTML")$result$value   # gem hele siden som tekst
  browser$close()
  return(power_html)
}