#' Flower function
#' @param xx parameter
#' @export
flowerfun  <- function(xx){

  return((0.00558*(xx^0.63))-(0.000969*xx)-0.00383)


}

#' check file exist and load
#' @param filename, file name
#' @export
check_file_exist <- function(filename){

  #print(filename)
  if(!file.exists(filename)){
    print(paste('Error, file:', filename,  'does not exist', sep = ' '))
    break
  } else {

    DataArray <- read.table(filename, header=TRUE, sep=',')
  }

  return(DataArray)

}


#' check file exist and load
#' @param filename, file name
#' @export
check_xml_exist <- function(filename){

  #print(filename)
  if(!file.exists(filename)){
    print(paste('Error, file:', filename,  'does not exist', sep = ' '))
    break
  } else {

    DataArray <- xmlToList(xmlParse(filename))
  }

  return(DataArray)

}


#' check file exist and load
#' @param filename, file name
#' @export
check_xml_table_exist <- function(filename){


  if(!file.exists(filename)){
    print(paste('Error, file:', filename,  'does not exist', sep = ' '))
    break
  } else {

    DataArray <- xmlToDataFrame(filename)
  }

  return(DataArray)
}




#'convert data.frame to list
#'@param sdat data.frame
#'@export
Convert_to_List <- function(sdat){

  cont_list <- list()

  for(des_name in levels(sdat$description)){

    i = which(sdat$description == des_name)
    cont_list[[des_name]] <- as.character(sdat$value[i])

  }
  return(cont_list)

}


#'Number of days from sdat to origin
#'@param sdat date in format "\%Y-\%m-\%d" as default
#'@param o origin
#'@param f format
#'@export
as_datenum <- function(sdat, o = '0000-01-01', f = "%Y-%m-%d"){
  

  n = as.numeric(sdat - as.Date(o, f))
  return(n)

}

#' Number of days from sdat to origin
#' @param x date where x should be in the format: x['Year'], x['Month'], x['Day']
#' @export
as_datenum_string <- function(x){
  
  
  y = as.Date(paste(x['year'], x['month'], x['day'], sep='-'), format="%Y-%m-%d")
  n = as.numeric(y - as.Date('0000-01-01',"%Y-%m-%d"))
  return(n)
  
}



#' Number of days from sdat to origin
#' @param sdat date in format "\%Y-\%m-\%d"
#' @export
as_date <- function(sdat){

  n = as.Date(sdat, origin = '0000-01-01')
  return(n)

}



#' It's datavec in Matlab. Converts data in numeric format to date in vector
#' format i.e. yyyy, m, d
#' @export
as_date_list <- function(sdat, origin_value = '0000-01-01'){

  d <- strsplit(as.character(as.Date(sdat, origin = origin_value)), '-')

  return(d)

}

#' export dataframe as xml
#' @param sdata dataset
#' @param filename name of file
#' @export
export_as_xml <- function(sdata, filename){


  write.xml(sdata, file=filename)

}

#' Convert to numeric
#' @param sdata dataset
#' @export
conver2num <- function(sdata){


  return(as.numeric(as.character(sdata)))

}

#' convert list values to numeric
#' @param slist list
#' @param lnames field names to transform
#' FIXME: do it for subsets
#'@export
convert_list_2numeric <- function(slist, lnames = NULL){

  if(is.null(lnames)){

    lnames <- names(slist)

  }

  for(lname in lnames){

    slist[lname] <- as.numeric(slist[lname])
  }

  return(slist)

}


#' convert list values to numeric
#' @param slist list
#' @param lnames field names to transform
#' FIXME: do it for subsets
#'@export
convert_table_list_2numeric <- function(slist, lnames = NULL){
  
  if(is.null(lnames)){
    
    lnames <- names(slist)
    
  }
  
  for(lname in lnames){
    
    slist[[lname]] <- as.numeric(as.character(slist[[lname]]))
  }
  
  return(slist)
  
}


#' calculate Water vapour pressure (Kpa) according to FAO
#' @param x list with three parameters x[[1]] = RH, x[[2]] = TMAX, x[[3]] = TMIN
#' @param AVP actual vapour pressure 
#' @export
#' @examples
#' RH = 57.8
#' TMAX = 22.96
#' TMIN = 9.23
#' calcAVP(RH, TMAX, TMIN)
calcAVP = function(x)
{
  
  TAVG <- mean(c(x[[2]], x[[3]]))
  SVAP    =  0.611 * exp(17.27 * TAVG/ (TAVG + 237.3))
  AVP <- x[[1]] * SVAP/100
  return(AVP)
  
}

#' change date format
#' @param x date, where year = x[[1]], DOY = x[[2]]
#' @return d
#' @export
#' @examples
#' year = '2011'
#' DOY = 1
#' ChangeDateFormat(year, DOY)
#' 

ChangeDateFormat <- function(x){
  
  if((x[['YEAR']] %% 2) == 1) {
    d = as.character(as.Date(x[['DOY']], 
                             origin = paste(x[['YEAR']], '-01-01', sep=''))-1)
  }
  else{
    d = as.character(as.Date(x[['DOY']], 
                             origin = paste(x[['YEAR']], '-01-01', sep='')))
  }
  
 
  return(d)
}

#' get parameter year from date string
#' @param x date, where year = x[[1]], DOY = x[[2]] 
#' @return year 
#' @export
get_year <- function(x){
  
  return(strsplit(ChangeDateFormat(x), '-')[[1]][1])
  
}

#' get parameter month from date string
#' @param x date, where year = x[[1]], DOY = x[[2]] 
#' @return month of the year
#' @export
get_month <- function(x){
  
  return(strsplit(ChangeDateFormat(x), '-')[[1]][2])
  
}

#' get parameter day from date string
#' @param x date, where year = x[[1]], DOY = x[[2]] 
#' @return day of the month
#' @export
get_day <- function(x){0
  
  return(strsplit(ChangeDateFormat(x), '-')[[1]][3])
  
}

#' calculate sowingdate
#' @param weather_data weather data
#' @param start_date start window 'dd/mm'
#' @param day label in dataset
#' @param month label in dataset
#' @param year label in dataset
#' @param P label irrigation
#' @param year_list year to be analysed
#' @param Crop crop name
#' @param ccl crop calendar length
#' @export


calculate_sowingdate <- function(weather_data, start_date, end_date, thr = 5,
                                 day, month, year, P, year_list, Crop,
                                 ccl = 100){
  
  
  weather_data <- setDT(weather_data)
  days = as.numeric(strsplit(start_date, '/')[[1]][1])
  months = as.numeric(strsplit(start_date, '/')[[1]][2])
  dayf = as.numeric(strsplit(end_date, '/')[[1]][1])
  monthf = as.numeric(strsplit(end_date, '/')[[1]][2])
  
  sowing_dates = c()
  
  for(cname in year_list){
  
      s  <- intersect(intersect(which(weather_data[[days]] == days),
                  which(weather_data[[month]] == months)),
                  which(weather_data[[year]] == cname))
      
      f  <- intersect(intersect(which(weather_data[[day]] == dayf),
                                which(weather_data[[month]] == monthf)),
                      which(weather_data[[year]] == cname))
    
      iv <- s
      raincum <- 0
      while(raincum < 35 & s != f){ # until end date
        
        if(weather_data[[P]][s] == 0 & raincum != 0 ){
          
           raincum <- 0
          
        } else {
          
          raincum <- weather_data[[P]][s] + raincum
        }
        s = s + 1
        
        print(paste(raincum, weather_data[[P]][s], sep = ':'))
      }
      
      if(s == f){
        s = iv
      } 
       
      PlantDate <- paste(weather_data[[day]][s], weather_data[[month]][s], 
                  weather_data[[year]][s], sep = '/')
      HarvestDate <- paste(weather_data[[day]][s + ccl], weather_data[[month]][s + ccl], 
                  weather_data[[year]][s + ccl], sep = '/')
      sowing_dates <- rbind(sowing_dates, cbind(PlantDate, HarvestDate, Crop))
    
   
  }
  
  return(sowing_dates)
}

#' convert to DOY
#' @param fdate date
#' @return DOY
#' @export
#' @examples
#' convertDOY('01-01-2000')
convertDOY = function(fdate)
{
 
  return(strftime(fdate, format = "%j"))
  
}


#' get stefan Boltzmann temp
#' @param t temperature
#' @return temperature
#' @export
#' @examples
#' get_stefan_Boltzmann(14.88)
#' 
get_stefan_Boltzmann <- function(t){
  
  x <- seq(1, 48.5, by=0.5)
  y <- c(27.7, 27.9,  28.11,  28.31,
         28.52,  28.72,  28.93, 29.14,  29.35,  29.56,  29.78,  29.99,  30.21,  30.42,  
         30.64,  30.86,  31.08, 31.3,  31.52,  31.74,  31.97,  32.19,  32.42,  32.65,  
         32.88,  33.11,  33.34, 33.57,  33.81,  34.04,  34.28,  34.52,  34.75,  34.99,  
         35.24,  35.48,  35.72, 35.97,  36.21,  36.46,  36.71,  36.96,  37.21,  37.47,
         37.72,  37.98,  38.23, 38.49,  38.75,  39.01,  39.27,  39.53,  39.8,  40.06, 
         40.33,  40.6,  40.87, 41.14,  41.41,  41.69,  41.96, 42.24,  42.52,  42.8,  
         43.08,  43.36,  43.64,  43.93,  44.21,  44.5,  44.79, 45.08,  45.37,  45.67,  
         45.96,  46.26,  46.56, 46.85,  47.15,  47.46,  47.76,  48.06,  48.37,  48.68,  
         48.99,  49.3, 49.61, 49.92,  50.24, 50.56, 50.87, 51.19, 51.51, 51.84, 52.16,52.49)
  
  return(approx(x, y, t)$y)
  
}

#' Calculate actual vapor pressure (ea) derived from dewpoint temperature
#' @param Tdew Dewpoint temperature (oC)
#' @return ea
#' @export
#' @examples
#' get_ea_dp(14.65)

get_ea_dp <- function(Tdew){
  
  Vpa <- 0.6108
  Vpt <- 17.27
  Vp3 <- 237.3
 
  #Actual vapour pressure derived from dewpoint temperature
  ea <- Vpa * exp((Vpt * Tdew) / (Tdew + Vp3))
  return(ea)
  
}

#' Calculate actual vapor pressure (ea) derived from mean relative humidity
#' @param rh relative humidity (\%)
#' @param eoTmax saturation vapour Tmax
#' @param eoTmin saturation vapour Tmin
#' @return ea 
#' @export
#' @examples
#' get_ea_rh(57.44)

get_ea_rh <- function(rh, eoTmax, eoTmin){
  
  #Actual vapour pressure derived from dewpoint temperature
  ea <- rh/100 * (((eoTmax + eoTmin))/2)
  return(ea)
  
}

#' Calculate atmospheric pressure (P)
#' @param z  elevation above sea level (m)
#' @return P atmospheric pressure (kPa)
#' @export
#' @examples 
#' get_atmospheric_pressure(1800)

get_atmospheric_pressure <- function(z){
  
  P	<- 101.3*((293 - (0.0065)*z)/293)^5.26
  return(P)
}


#' Slope of saturation vapour pressure curve
#' @param Tmean mean temperature (oC)
#' @return delta  Slope of saturation vapour pressure curve T(kPa oC-1)
#' @export
#' @examples 
#' get_slope_saturation_vp(1800)
get_slope_saturation_vp <- function(Tmean){
  
  Vpa <- 0.6108
  Vp3 <- 237.3
  
  delta <- (4098 * (Vpa *exp((17.2 * Tmean) / (Tmean + Vp3)))) /
    (Tmean + Vp3)^2
  return(delta)
  
}


#' Calculate net radiation mm/day
#' @param Tmax max temperature
#' @param Tmin min temperature
#' @param RA solar radiation, all Sky Insolation Incident on a Horizontal 
#' Surface (MJ/m^2/day) 
#' @param altitude (m)
#' @param easqrt sqrt(ea)
#' @return rn_mm_day mm/day
#' @export
#' @examples
#' get_net_radiation(29.5, 18.88, 27.47, 83.64)

get_net_radiation <- function(Tmax, Tmin, RA, altitude, easqrt){
  
  # constants
  alpha <- 0.23 # canopy reflection coefficient
  lambda	<- 0.48 # latent heat heat of vaporization
  G <- 0
  
  rs <- 0.16 * sqrt(Tmax - Tmin) * RA
  rso <- (0.75+2 * altitude/100000) * RA
  rs_rso <- rs/rso
  cloudiness <- 1.35*rs_rso - 0.35 
  rns <- rs * (1 - alpha)
  Tmink_Tmaxk <- (get_stefan_Boltzmann(Tmax) + get_stefan_Boltzmann(Tmin))/2
  
  rnl <- Tmink_Tmaxk * easqrt * cloudiness
  rn <- rns - rnl
  # Net radiation in MJ m-2 day-1
  rn <- rn - G
  # Net radiation mm/day
  rn_mm_day <- lambda * (rn)
  
  return(rn_mm_day)
  
  
  
}

#' Calculate ETo using Penman_Monteith
#' @param x weather parameters Tmax = x[[1]],  Tmax <- x[[1]], Tmin <- x[[2]],
#' RA <- x[[3]], Wind <- x[[4]], Tdew <- x[[5]], altitude <- x[[6]] =  max temperature
#' @export
#' @examples 
#' get_Eto(x)

get_Eto <- function(x){

  
  Tmax <- x[[1]]
  Tmin <- x[[2]]
  RA <- x[[3]]
  Wind <- x[[4]]
  Tdew <- x[[5]]
  altitude <- x[[6]]
  
  # Constants 

  Vpa <- 0.6108
  Vpt <- 17.27
  Vp3 <- 237.3
  vp4 <- 273
  
  Tmean <- (Tmax + Tmin)/2
  # Atmospheric pressure
  P	<- get_atmospheric_pressure(altitude)
  # gamma - Psychrometric constant 
  gamma = 0.665*10^-3 * P
  # Slope of saturation vapour pressure curve
  delta <- get_slope_saturation_vp(Tmean)
  
  # saturation vapour
  eoTmax <-  Vpa * exp((Vpt * Tmax)/ (Tmax + Vp3))
  eoTmin <-  Vpa * exp((Vpt * Tmin)/ (Tmin + Vp3))
  
  # Mean saturation vapor pressure
  es <- (eoTmax + eoTmin)/ 2
  
  #Actual vapour pressure derived from dewpoint temperature
  ea <- get_ea_dp(Tdew)
  
  easqrt <- 0.34 - 0.14 * sqrt(ea)
  # Vapour pressure deficit
  vpd <- es - ea
  
  L900T <- 900/(Tmean + vp4)*Wind
  deltadelta <- delta  / (delta + (gamma*(1 + (0.34 * Wind))))
  gammadelta <- gamma  / (delta + (gamma*(1 + (0.34 * Wind))))
  
  ## Net Radiation
  
  rn_mm_day <- get_net_radiation(Tmax, Tmin, RA, altitude, easqrt)
  
  ## Eto
  
  s1 <- rn_mm_day * deltadelta
  s2 <- L900T * vpd *  gammadelta
  Eto <- s1 + s2
  
  return(Eto)
  
}
