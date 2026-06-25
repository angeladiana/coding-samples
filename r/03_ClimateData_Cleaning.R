library(readxl)
library(writexl)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Getting the coordinates
coordinates <- read_xlsx("coordinates.xlsx") #Load coordinates file

# Function to download the 
get_climate_data <- function(lat, lon, start_year = 2000, end_year = 2023) {
  all_data <- list()
  
  for (year in start_year:end_year) {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-12-31")
    
    # Construct API URL for the year
    url <- paste0("https://archive-api.open-meteo.com/v1/archive?latitude=",
                  lat, "&longitude=", lon, 
                  "&start_date=",start_date,
                  "&end_date=",end_date,
                  "&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,rain_sum&timezone=Asia%2FSingapore")
    response <- GET(url,user_agent("R"))
    
    # Check if the response status is 200 (OK)
    if(status_code(response) != 200) {
      warning(paste("Failed to retrieve data for", year, "at coordinates:", lat, lon))
      next # Skip this year if data is not retrieved
    }
    
    # Parse the response content
    response_content <- content(response, as = "text", encoding = "UTF-8")
    climate_data <- fromJSON(response_content)
    
    # Check for daily data
    if (!is.null(climate_data$daily)) {
      yearly_data <- data.frame(
        date = as.Date(climate_data$daily$time),
        temperature_max = climate_data$daily$temperature_2m_max,
        temperature_min = climate_data$daily$temperature_2m_min,
        temperature_mean = climate_data$daily$temperature_2m_mean,
        rain = climate_data$daily$rain_sum,
        latitude = lat,
        longitude = lon
      )
      
      # Add yearly data to the list
      all_data[[as.character(year)]] <- yearly_data
    } else {
      warning(paste("No daily data available for", year, "at coordinates:", lat, lon))
    }
    
    Sys.sleep(3)
  }
  
  # Combine all yearly data into a single data frame
  full_data <- do.call(rbind, all_data)
  return(full_data)
}

# Downloading the data for all stations one by one and saving them into separate data frames
  {
# NAIA -------------------------------------------
  lat <- coordinates$latitude[1]
  lon <- coordinates$longitude[1]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_NAIA <- get_climate_data(lat, lon)
  climate_NAIA <- climate_NAIA %>%
    mutate(station = "NAIA") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Port Area -------------------------------------------
  lat <- coordinates$latitude[2]
  lon <- coordinates$longitude[2]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Port_Area <- get_climate_data(lat, lon)
  climate_Port_Area <- climate_Port_Area %>%
    mutate(station = "Port_Area") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Science Garden -------------------------------------------
  lat <- coordinates$latitude[3]
  lon <- coordinates$longitude[3]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Science_Garden <- get_climate_data(lat, lon)
  climate_Science_Garden <- climate_Science_Garden %>%
    mutate(station = "Science_Garden") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Baguio -------------------------------------------
  lat <- coordinates$latitude[4]
  lon <- coordinates$longitude[4]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Baguio <- get_climate_data(lat, lon)
  climate_Baguio <- climate_Baguio %>%
    mutate(station = "Baguio") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Dagupan -------------------------------------------
  lat <- coordinates$latitude[5]
  lon <- coordinates$longitude[5]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Dagupan <- get_climate_data(lat, lon)
  climate_Dagupan <- climate_Dagupan %>%
    mutate(station = "Dagupan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Laoag -------------------------------------------
  lat <- coordinates$latitude[6]
  lon <- coordinates$longitude[6]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Laoag <- get_climate_data(lat, lon)
  climate_Laoag <- climate_Laoag %>%
    mutate(station = "Laoag") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Sinait_1 -------------------------------------------
  lat <- coordinates$latitude[7]
  lon <- coordinates$longitude[7]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Sinait_1 <- get_climate_data(lat, lon)
  climate_Sinait_1 <- climate_Sinait_1 %>%
    mutate(station = "Sinait_1") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Sinait_2 -------------------------------------------
  lat <- coordinates$latitude[8]
  lon <- coordinates$longitude[8]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Sinait_2 <- get_climate_data(lat, lon)
  climate_Sinait_2 <- climate_Sinait_2 %>%
    mutate(station = "Sinait_2") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Aparri -------------------------------------------
  lat <- coordinates$latitude[9]
  lon <- coordinates$longitude[9]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Aparri <- get_climate_data(lat, lon)
  climate_Aparri <- climate_Aparri %>%
    mutate(station = "Aparri") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Basco -------------------------------------------
  lat <- coordinates$latitude[10]
  lon <- coordinates$longitude[10]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Basco <- get_climate_data(lat, lon)
  climate_Basco <- climate_Basco %>%
    mutate(station = "Basco") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Calayan -------------------------------------------
  lat <- coordinates$latitude[11]
  lon <- coordinates$longitude[11]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Calayan <- get_climate_data(lat, lon)
  climate_Calayan <- climate_Calayan %>%
    mutate(station = "Calayan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Itbayat -------------------------------------------
  lat <- coordinates$latitude[12]
  lon <- coordinates$longitude[12]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Itbayat <- get_climate_data(lat, lon)
  climate_Itbayat <- climate_Itbayat %>%
    mutate(station = "Itbayat") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Tuguegarao -------------------------------------------
  lat <- coordinates$latitude[13]
  lon <- coordinates$longitude[13]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Tuguegarao <- get_climate_data(lat, lon)
  climate_Tuguegarao <- climate_Tuguegarao %>%
    mutate(station = "Tuguegarao") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Baler -------------------------------------------
  lat <- coordinates$latitude[14]
  lon <- coordinates$longitude[14]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Baler <- get_climate_data(lat, lon)
  climate_Baler <- climate_Baler %>%
    mutate(station = "Baler") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Cabanatuan -------------------------------------------
  lat <- coordinates$latitude[15]
  lon <- coordinates$longitude[15]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Cabanatuan <- get_climate_data(lat, lon)
  climate_Cabanatuan <- climate_Cabanatuan %>%
    mutate(station = "Cabanatuan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  
  # Casiguran -------------------------------------------
  lat <- coordinates$latitude[16]
  lon <- coordinates$longitude[16]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Casiguran  <- get_climate_data(lat, lon)
  climate_Casiguran <- climate_Casiguran %>%
    mutate(station = "Casiguran") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Clark -------------------------------------------
  lat <- coordinates$latitude[17]
  lon <- coordinates$longitude[17]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Clark  <- get_climate_data(lat, lon)
  climate_Clark <- climate_Clark %>%
    mutate(station = "Clark") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Subic -------------------------------------------
  lat <- coordinates$latitude[18]
  lon <- coordinates$longitude[18]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Subic  <- get_climate_data(lat, lon)
  climate_Subic <- climate_Subic %>%
    mutate(station = "Subic") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Iba -------------------------------------------
  lat <- coordinates$latitude[19]
  lon <- coordinates$longitude[19]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Iba  <- get_climate_data(lat, lon)
  climate_Iba <- climate_Iba %>%
    mutate(station = "Iba") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Alabat -------------------------------------------
  lat <- coordinates$latitude[20]
  lon <- coordinates$longitude[20]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Alabat  <- get_climate_data(lat, lon)
  climate_Alabat <- climate_Alabat %>%
    mutate(station = "Alabat") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Ambulong -------------------------------------------
  lat <- coordinates$latitude[21]
  lon <- coordinates$longitude[21]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Ambulong  <- get_climate_data(lat, lon)
  climate_Ambulong <- climate_Ambulong %>%
    mutate(station = "Ambulong") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Infanta -------------------------------------------
  lat <- coordinates$latitude[22]
  lon <- coordinates$longitude[22]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Infanta  <- get_climate_data(lat, lon)
  climate_Infanta <- climate_Infanta %>%
    mutate(station = "Infanta") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Sangley_Point -------------------------------------------
  lat <- coordinates$latitude[23]
  lon <- coordinates$longitude[23]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Sangley_Point  <- get_climate_data(lat, lon)
  climate_Sangley_Point <- climate_Sangley_Point %>%
    mutate(station = "Sangley_Point") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Tanay -------------------------------------------
  lat <- coordinates$latitude[24]
  lon <- coordinates$longitude[24]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Tanay  <- get_climate_data(lat, lon)
  climate_Tanay <- climate_Tanay %>%
    mutate(station = "Tanay") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Tayabas -------------------------------------------
  lat <- coordinates$latitude[25]
  lon <- coordinates$longitude[25]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Tayabas  <- get_climate_data(lat, lon)
  climate_Tayabas <- climate_Tayabas %>%
    mutate(station = "Tayabas") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Calapan -------------------------------------------
  lat <- coordinates$latitude[26]
  lon <- coordinates$longitude[26]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Calapan  <- get_climate_data(lat, lon)
  climate_Calapan <- climate_Calapan %>%
    mutate(station = "Calapan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Coron -------------------------------------------
  lat <- coordinates$latitude[27]
  lon <- coordinates$longitude[27]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Coron  <- get_climate_data(lat, lon)
  climate_Coron <- climate_Coron %>%
    mutate(station = "Coron") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Cuyo -------------------------------------------
  lat <- coordinates$latitude[28]
  lon <- coordinates$longitude[28]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Cuyo  <- get_climate_data(lat, lon)
  climate_Cuyo <- climate_Cuyo %>%
    mutate(station = "Cuyo") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Puerto Princesa -------------------------------------------
  lat <- coordinates$latitude[29]
  lon <- coordinates$longitude[29]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Puerto_Princesa  <- get_climate_data(lat, lon)
  climate_Puerto_Princesa <- climate_Puerto_Princesa %>%
    mutate(station = "Puerto_Princesa") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Romblon -------------------------------------------
  lat <- coordinates$latitude[30]
  lon <- coordinates$longitude[30]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Romblon  <- get_climate_data(lat, lon)
  climate_Romblon <- climate_Romblon %>%
    mutate(station = "Romblon") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # San Jose -------------------------------------------
  lat <- coordinates$latitude[31]
  lon <- coordinates$longitude[31]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_San_Jose  <- get_climate_data(lat, lon)
  climate_San_Jose <- climate_San_Jose %>%
    mutate(station = "San_Jose") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Daet -------------------------------------------
  lat <- coordinates$latitude[32]
  lon <- coordinates$longitude[32]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Daet  <- get_climate_data(lat, lon)
  climate_Daet <- climate_Daet %>%
    mutate(station = "Daet") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Juban -------------------------------------------
  lat <- coordinates$latitude[33]
  lon <- coordinates$longitude[33]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Juban  <- get_climate_data(lat, lon)
  climate_Juban <- climate_Juban %>%
    mutate(station = "Juban") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Legaspi -------------------------------------------
  lat <- coordinates$latitude[34]
  lon <- coordinates$longitude[34]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Legaspi  <- get_climate_data(lat, lon)
  climate_Legaspi <- climate_Legaspi %>%
    mutate(station = "Legaspi") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Masbate -------------------------------------------
  lat <- coordinates$latitude[35]
  lon <- coordinates$longitude[35]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Masbate  <- get_climate_data(lat, lon)
  climate_Masbate <- climate_Masbate %>%
    mutate(station = "Masbate") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Virac -------------------------------------------
  lat <- coordinates$latitude[36]
  lon <- coordinates$longitude[36]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Virac  <- get_climate_data(lat, lon)
  climate_Virac <- climate_Virac %>%
    mutate(station = "Virac") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Roxas -------------------------------------------
  lat <- coordinates$latitude[37]
  lon <- coordinates$longitude[37]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Roxas  <- get_climate_data(lat, lon)
  climate_Roxas <- climate_Roxas %>%
    mutate(station = "Roxas") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Dauis -------------------------------------------
  lat <- coordinates$latitude[38]
  lon <- coordinates$longitude[38]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Dauis  <- get_climate_data(lat, lon)
  climate_Dauis <- climate_Dauis %>%
    mutate(station = "Dauis") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Dumaguete -------------------------------------------
  lat <- coordinates$latitude[39]
  lon <- coordinates$longitude[39]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Dumaguete  <- get_climate_data(lat, lon)
  climate_Dumaguete <- climate_Dumaguete %>%
    mutate(station = "Dumaguete") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Mactan -------------------------------------------
  lat <- coordinates$latitude[40]
  lon <- coordinates$longitude[40]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Mactan  <- get_climate_data(lat, lon)
  climate_Mactan <- climate_Mactan %>%
    mutate(station = "Mactan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Borongan_1 -------------------------------------------
  lat <- coordinates$latitude[41]
  lon <- coordinates$longitude[41]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Borongan_1  <- get_climate_data(lat, lon)
  climate_Borongan_1 <- climate_Borongan_1 %>%
    mutate(station = "Borongan_1") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Borongan_2 -------------------------------------------
  lat <- coordinates$latitude[42]
  lon <- coordinates$longitude[42]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Borongan_2  <- get_climate_data(lat, lon)
  climate_Borongan_2 <- climate_Borongan_2 %>%
    mutate(station = "Borongan_2") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Catarman -------------------------------------------
  lat <- coordinates$latitude[43]
  lon <- coordinates$longitude[43]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Catarman  <- get_climate_data(lat, lon)
  climate_Catarman <- climate_Catarman %>%
    mutate(station = "Catarman") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Catbalogan -------------------------------------------
  lat <- coordinates$latitude[44]
  lon <- coordinates$longitude[44]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Catbalogan  <- get_climate_data(lat, lon)
  climate_Catbalogan <- climate_Catbalogan %>%
    mutate(station = "Catbalogan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Guiuan -------------------------------------------
  lat <- coordinates$latitude[45]
  lon <- coordinates$longitude[45]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Guiuan  <- get_climate_data(lat, lon)
  climate_Guiuan <- climate_Guiuan %>%
    mutate(station = "Guiuan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Maasin -------------------------------------------
  lat <- coordinates$latitude[46]
  lon <- coordinates$longitude[46]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Maasin  <- get_climate_data(lat, lon)
  climate_Maasin <- climate_Maasin %>%
    mutate(station = "Maasin") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Tacloban_1 -------------------------------------------
  lat <- coordinates$latitude[47]
  lon <- coordinates$longitude[47]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Tacloban_1  <- get_climate_data(lat, lon)
  climate_Tacloban_1 <- climate_Tacloban_1 %>%
    mutate(station = "Tacloban_1") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Tacloban_2 -------------------------------------------
  lat <- coordinates$latitude[48]
  lon <- coordinates$longitude[48]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Tacloban_2  <- get_climate_data(lat, lon)
  climate_Tacloban_2 <- climate_Tacloban_2 %>%
    mutate(station = "Tacloban_2") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Dipolog -------------------------------------------
  lat <- coordinates$latitude[49]
  lon <- coordinates$longitude[49]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Dipolog  <- get_climate_data(lat, lon)
  climate_Dipolog <- climate_Dipolog %>%
    mutate(station = "Dipolog") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Zamboanga -------------------------------------------
  lat <- coordinates$latitude[50]
  lon <- coordinates$longitude[50]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Zamboanga  <- get_climate_data(lat, lon)
  climate_Zamboanga <- climate_Zamboanga %>%
    mutate(station = "Zamboanga") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # El Salvador -------------------------------------------
  lat <- coordinates$latitude[51]
  lon <- coordinates$longitude[51]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_El_Salvador  <- get_climate_data(lat, lon)
  climate_El_Salvador <- climate_El_Salvador %>%
    mutate(station = "El_Salvador") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Malaybalay -------------------------------------------
  lat <- coordinates$latitude[52]
  lon <- coordinates$longitude[52]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Malaybalay  <- get_climate_data(lat, lon)
  climate_Malaybalay <- climate_Malaybalay %>%
    mutate(station = "Malaybalay") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Davao -------------------------------------------
  lat <- coordinates$latitude[53]
  lon <- coordinates$longitude[53]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Davao  <- get_climate_data(lat, lon)
  climate_Davao <- climate_Davao %>%
    mutate(station = "Davao") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # General Santos -------------------------------------------
  lat <- coordinates$latitude[54]
  lon <- coordinates$longitude[54]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_General_Santos  <- get_climate_data(lat, lon)
  climate_General_Santos <- climate_General_Santos %>%
    mutate(station = "General_Santos") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Butuan -------------------------------------------
  lat <- coordinates$latitude[55]
  lon <- coordinates$longitude[55]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Butuan  <- get_climate_data(lat, lon)
  climate_Butuan <- climate_Butuan %>%
    mutate(station = "Butuan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Hinatuan -------------------------------------------
  lat <- coordinates$latitude[56]
  lon <- coordinates$longitude[56]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Hinatuan  <- get_climate_data(lat, lon)
  climate_Hinatuan <- climate_Hinatuan %>%
    mutate(station = "Hinatuan") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Surigao -------------------------------------------
  lat <- coordinates$latitude[57]
  lon <- coordinates$longitude[57]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Surigao  <- get_climate_data(lat, lon)
  climate_Surigao <- climate_Surigao %>%
    mutate(station = "Surigao") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())
  
  # Cotabato -------------------------------------------
  lat <- coordinates$latitude[58]
  lon <- coordinates$longitude[58]
  
  # Download data for selected coordinates from 2000 to 2023
  climate_Cotabato  <- get_climate_data(lat, lon)
  climate_Cotabato <- climate_Cotabato %>%
    mutate(station = "Cotabato") %>% 
    select(date, station, latitude = 6, longitude = 7, everything())

  }
# Combining all stations into one data frame ------------------
df_list <- list(climate_NAIA,
                     climate_Port_Area,
                     climate_Science_Garden,
                     climate_Baguio,
                     climate_Dagupan,
                     climate_Laoag,
                     climate_Sinait_1,
                     climate_Sinait_2,
                     climate_Aparri,
                     climate_Basco,
                     climate_Calayan,
                     climate_Itbayat,
                     climate_Tuguegarao,
                     climate_Baler,
                     climate_Cabanatuan,
                     climate_Casiguran,
                     climate_Clark,
                     climate_Subic,
                     climate_Iba,
                     climate_Alabat,
                     climate_Ambulong,
                     climate_Infanta,
                     climate_Sangley_Point,
                     climate_Tanay,
                     climate_Tayabas,
                     climate_Calapan,
                     climate_Coron,
                     climate_Cuyo,
                     climate_Puerto_Princesa,
                     climate_Romblon,
                     climate_San_Jose,
                     climate_Daet,
                     climate_Juban,
                     climate_Legaspi,
                     climate_Masbate,
                     climate_Virac,
                     climate_Roxas,
                     climate_Dauis,
                     climate_Dumaguete,
                     climate_Mactan,
                     climate_Borongan_1,
                     climate_Borongan_2,
                     climate_Catarman,
                     climate_Catbalogan,
                     climate_Guiuan,
                     climate_Maasin,
                     climate_Tacloban_1,
                     climate_Tacloban_2,
                     climate_Dipolog,
                     climate_Zamboanga,
                     climate_El_Salvador,
                     climate_Malaybalay,
                     climate_Davao,
                     climate_General_Santos,
                     climate_Butuan,
                     climate_Hinatuan,
                     climate_Surigao,
                     climate_Cotabato
                    )
  climate_all_daily <- do.call(rbind, df_list)
  
  # Save dataset as an excel file
  write_xlsx(climate_all_daily, "climate_all_daily.xlsx")
  


# Getting the monthly average for climate variables --------------------------
climate_all_monthly <- climate_all_daily %>% 
  mutate(yearmon = floor_date(date, "month")) %>% 
  group_by(yearmon, station, latitude, longitude) %>% 
  summarise(
    temp_max = mean(temperature_max, na.rm = TRUE), # average temperature max for the month
    temp_min = mean(temperature_min, na.rm = TRUE), # average temperature min for the month
    temp_mean = mean(temperature_mean, na.rm = TRUE), # average temperature mean for the month
    rain = mean(rain, na.rm = TRUE) # average rain for the month
    ) %>%
    ungroup() %>%
    arrange(station, yearmon)

  # Save dataset as an excel file
  write_xlsx(climate_all_monthly, "climate_all_monthly.xlsx")  



# Load PAGASA data ---------------
file_path <- "C:\\Users\\Angela\\OneDrive - National Economic and Development Authority\\Technical Projects\\2024_NEDA_Agricultural Policy Tools\\August 2024\\Datasets\\Compiled_Monthly Rainfall and Temp Data.xlsx"
sheet_names <- excel_sheets(file_path)
sheet_data <- lapply(sheet_names, function(sheet) read_excel(file_path, sheet = sheet))
names(sheet_data) <- sheet_names

# Cleaning PAGASA data -----------
station_data_list <- list() # List which contains the data frames for all stations

for (x in sheet_names) {
  station_data <- sheet_data[[x]]
  processed_data <- station_data %>%
  mutate(station = x, yearmon = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  select(yearmon, station, TMAX, TMIN, TMEAN, RAINFALL, -YEAR, -MONTH) %>%
  rename(temp_max = TMAX, temp_min = TMIN, temp_mean = TMEAN, rain = RAINFALL)
  station_data_list[[x]] <- processed_data
}

pagasa_all_monthly <- do.call(rbind, station_data_list) # Combining data frames

pagasa_all_monthly <- pagasa_all_monthly %>%
  mutate(across(c(temp_max, temp_min, temp_mean, rain), ~ifelse(. == -999, NA, .)))

# Save dataset as an excel file
write_xlsx(pagasa_all_monthly, "pagasa_all_monthly.xlsx")



