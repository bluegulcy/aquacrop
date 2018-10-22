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
