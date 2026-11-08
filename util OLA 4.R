library(httr)
library(jsonlite)

clientId     <- "tobiashfrid@gmail.com-api-client"
clientSecret <- "NLR3dsiF8zUDHeJPEbncgcJHdBEYnuaz"                    

# Hent et adgangstoken
getToken <- function() {
  token <- NULL
  response <- POST(
    url  = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(grant_type = "client_credentials", client_id = clientId, client_secret = clientSecret),
    encode = "form"
  )
  if (response$status_code == 200) {                              # 200 = login lykkedes
    token <- content(response, as = "parsed")$access_token         # tag tokenet ud af svaret
  }
  return(token)
}

# Hent data fra en URL, og prøv igen med nyt token, hvis det er udløbet
getRawFromeurl <- function(url, token) {
  rawres <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")),
                timeout(10))                     # vent højst 10 sekunder
  if (rawres$status_code == 401) {                                                     # tokenet er udløbet
    token  <- getToken()
    rawres <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " "))) # prøv igen
  }
  return(rawres)
}