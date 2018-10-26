#' Flower function
#' @param xx parameter
flowerfun  <- function(xx){

  return((0.00558*(xx^0.63))-(0.000969*xx)-0.00383)


}

#' check file exist and load
#' @param filename, file name
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
#'
as_datenum <- function(sdat, o = '0000-01-01', f = "%Y-%m-%d"){
  

  n = as.numeric(sdat - as.Date(o, f))
  return(n)

}

#' Number of days from sdat to origin
#' @param x date where x should be in the format: x['Year'], x['Month'], x['Day']
as_datenum_string <- function(x){
  
  
  y = as.Date(paste(x['year'], x['month'], x['day'], sep='-'), format="%Y-%m-%d")
  n = as.numeric(y - as.Date('0000-01-01',"%Y-%m-%d") + 1)
  return(n)
  
}



#' Number of days from sdat to origin
#' @param sdat date in format "\%Y-\%m-\%d"
as_date <- function(sdat){

  n = as.Date(sdat, origin = '0000-01-01')
  return(n)

}



#' It's datavec in Matlab. Converts data in numeric format to date in vector
#' format i.e. yyyy, m, d
as_date_list <- function(sdat, origin_value = '0000-01-01'){

  d <- strsplit(as.character(as.Date(sdat, origin = origin_value)), '-')

  return(d)

}

#' export dataframe as xml
#' @param sdata dataset
#' @param filename name of file
export_as_xml <- function(sdata, filename){


  write.xml(sdata, file=filename)

}

#' Convert to numeric
#' @param sdata dataset
conver2num <- function(sdata){


  return(as.numeric(as.character(sdata)))

}

#' convert list values to numeric
#' @param slist list
#' @param lnames field names to transform
#' FIXME: do it for subsets
#'
convert_list_2numeric <- function(slist, lnames = NULL){

  if(is.null(lnames)){

    lnames <- names(slist)

  }

  for(lname in lnames){

    slist[lname] <- as.numeric(slist[lname])
  }

  return(slist)

}



#' calculate Water vapour pressure (Kpa) according to FAO
#' @param x list with three parameters x[[1]] = RH, x[[2]] = TMAX, x[[3]] = TMIN
#' @param AVP actual vapour pressure 
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
get_year <- function(x){
  
  return(strsplit(ChangeDateFormat(x), '-')[[1]][1])
  
}

#' get parameter month from date string
#' @param x date, where year = x[[1]], DOY = x[[2]] 
#' @return month of the year
get_month <- function(x){
  
  return(strsplit(ChangeDateFormat(x), '-')[[1]][2])
  
}

#' get parameter day from date string
#' @param x date, where year = x[[1]], DOY = x[[2]] 
#' @return day of the month
get_day <- function(x){0
  
  return(strsplit(ChangeDateFormat(x), '-')[[1]][3])
  
}

#' calculate_sowingdate

#' @param weather_data weather data
#' @param start_date start window 'dd/mm'
#' @param day label in dataset
#' @param month label in dataset
#' @param year label in dataset
#' @param P label irrigation
#' @param year_list year to be analysed
#' @param Crop crop name
#' @param ccl crop calendar length
calculate_sowingdate <- function(weather_data, start_date, thr = 5,
                                 day, month, year, P, year_list, Crop,
                                 ccl = 100 ){
  
  
  weather_data <- setDT(weather_data)
  dayn = strsplit(start_date, '/')[[1]][1]
  monthn = strsplit(start_date, '/')[[1]][2]
  sowing_dates = c()
 
  for(cname in year_list){

    i = intersect(intersect(which(weather_data[[day]] == as.numeric(dayn)),
                  which(weather_data[[month]] == as.numeric(monthn))),
                  which(weather_data[[year]] == cname))
    iv <- i
    s <- 0
    while(s < 30 & i <= iv+60){
      
      s <- sum(weather_data[[P]][i:(i+4)])
  
      i = i + 1
    }
    
    if(is.na(s)){
     
      i = iv
    } 
       
      PlantDate <- paste(weather_data[[day]][i], weather_data[[month]][i], 
                  weather_data[[year]][i], sep = '/')
      HarvestDate <- paste(weather_data[[day]][i + ccl], weather_data[[month]][i + ccl], 
                  weather_data[[year]][i+ ccl], sep = '/')
      sowing_dates <- rbind(sowing_dates, cbind(PlantDate, HarvestDate, Crop))
    
   
  }
  write.table(sowing_dates, file='CropRotationCalendar.csv', sep=',',
        row.names = FALSE, quote = FALSE)
  return(sowing_dates)
}

# convert to DOY
#' @param fdate date
#' @return DOY
#' @example 
#' convertDOY('01-01-2000')

convertDOY = function(fdate)
{
 
  return(strftime(fdate, format = "%j"))
  
}
