library( ggplot2 )
library( reshape2 )
library( tidyr )
library( dplyr )

# --------------------------------------------------------------------------------------
## printlog: time-stamped output ##
  # params: msg (message", " can be many items); ts (ad timestamp), cr (print CR)
  printlog <- function( msg, ..., ts=TRUE, cr=TRUE ) {
    if( ts ) cat( date(), " " )
    cat( msg, ..., sep=" " )
    if( cr ) cat( "\n" )
  }

# --------------------------------------------------------------------------------------
## Function "inputData" to read in data and output number of rows and columns and variable names
  ## folderPath = location of folder that contains input file
  ## inputFile = name of .csv file to be read in

inputData <- function( d, inputFile, skipNumber )
{
  printlog( paste( "Reading data...", inputFile, sep = " " ) )
  d <- read.csv( inputFile, skip = skipNumber, stringsAsFactors = FALSE, header = TRUE )
  printlog( nrow( d ), "rows" )
  printlog( ncol( d ), "columns" )
  printlog( colnames( d ) )
  return( d )
}

# --------------------------------------------------------------------------------------
## Shorter command to replace column names
colnameReplace <- function ( d, x, y )
{
  colnames( d )[ colnames( d ) == x ] <- y
  return( d )
}

# --------------------------------------------------------------------------------------
## Create year variable in melted dataframe ##
  yr <- function( d )
  {
    d$variable <- as.numeric( substr( d$variable, 2, 5 ) )
    d <- colnameReplace( d, "variable", "year" )
    return( d )
  }

# --------------------------------------------------------------------------------------
## Function to count from the end of the string
substrRight <- function( x, n )
  {
  substr( x, nchar( x ) - n + 1, nchar( x ) )
  }

# -----------------------------------------------------------------------------
## Define basic theme for figures
theme_basic <- theme_bw() +
  theme( legend.text = element_text( size = 16, family = "Gill Sans MT", vjust = .5 ) ) +
  theme( legend.title = element_text( size = 16, family = "Gill Sans MT", vjust = 2 ) ) +
  theme( axis.text = element_text( size = 16, family = "Gill Sans MT" ) ) +
  theme( axis.title = element_text( size = 20, family = "Gill Sans MT", face = "bold" ) ) +
  theme( plot.title = element_text( size = 24, family = "Gill Sans MT", face = "bold", vjust = 1 ) ) +
  theme( strip.text = element_text( size = 14, family = "Gill Sans MT" ) )

# -----------------------------------------------------------------------------
## List of GCAM regions
gcam_regions <- c( "Africa_Eastern", "Africa_Northern", "Africa_Southern", "Africa_Western", "Argentina", "Australia_NZ", "Brazil", "Canada",
              "Central America and Caribbean", "Central Asia", "China", "Colombia", "EU-12", "EU-15", "Europe_Eastern", "Europe_Non_EU",
              "European Free Trade Association", "India", "Indonesia", "Japan", "Mexico", "Midle East", "Pakistan", "Russia", "South Africa",
              "South America_Northern", "South America_Southern", "South Asia", "South Korea", "Southeast Asia", "Taiwan", "USA" )

# -----------------------------------------------------------------------------

## ISO replacements for countries with non-standard names (can ad to as they come up)
## d = dataframe name, iso_id = name of column with iso variable (usually "iso"), country_id = name of column with country variable (usually "country_name")
isoReplace <- function( d )
{
  iso <- function( d, x, y )
  {
    d$iso <- ifelse( d$country_name == x, y, d$iso )
    return( d )
  }
  d <- iso( d, "Brunei", "brn" )
  d <- iso( d, "Burma (Myanmar)", "mmr" )
  d <- iso( d, "China, mainland", "chn")
  d <- iso( d, "China, Hong Kong SAR", "hkg" )
  d <- iso( d, "China, Taiwan Province of", "twn" )
  d <- iso( d, "Congo (Brazzaville)", "cog" )
  d <- iso( d, "Congo (Kinshasa)", "cod" )
  d <- iso( d, "Cote dIvoire (IvoryCoast)", "civ" )
  d <- iso( d, "Cote Divoire", "civ" )
  d <- iso( d, "Cote d'Ivoire", "civ" )
  d <- iso( d, "C\x99te d'Ivoire", "civ" )
  d <- iso( d, "C\x92\x82te d'Ivoire", "civ" )
  d <- iso( d, "C\xed\xc7te d'Ivoire", "civ" )
  d <- iso( d, "C\xc8te d'Ivoire", "civ" )
  d <- iso( d, "C̫te d'Ivoire", "civ" )
  d <- iso( d, "Ethiopia PDR", "eth" )
  d <- iso( d, "Gambia, The", "gmb" )
  d <- iso( d, "Iran", "irn" )
  d <- iso( d, "Korea, North", "prk" )
  d <- iso( d, "Democratic People's Republic of Korea", "prk" )
  d <- iso( d, "Democratic Peoples Republic of Korea", "prk" )
  d <- iso( d, "North Korea", "prk" )
  d <- iso( d, "Korea, South", "kor" )
  d <- iso( d, "Korea", "kor" )
  d <- iso( d, "Republic of Korea", "kor" )
  d <- iso( d, "South Korea", "kor" )
  d <- iso( d, "Laos", "lao" )
  d <- iso( d, "Libya", "lby" )
  d <- iso( d, "Palestinian Territories", "pse" )
  d <- iso( d, "Russia", "rus" )
  d <- iso( d, "Sudan and South Sudan", "sdn" )
  d <- iso( d, "Syria", "syr" )
  d <- iso( d, "Tanzania", "tza" )
  d <- iso( d, "Vietnam", "vnm" )
  d <- iso( d, "Bel-lux", "bel" )
  d <- iso( d, "Belgium-Luxembourg", "bel" )
  d <- iso( d, "Bosnia Herzg", "bih" )
  d <- iso( d, "Brunei Darsm", "brn" )
  d <- iso( d, "Cent Afr Rep", "caf" )
  d <- iso( d, "Czech Rep", "cze" )
  d <- iso( d, "Czech Rep.", "cze" )
  d <- iso( d, "Former Czechoslovakia", "cze" )
  d <- iso( d, "Czechoslovakia", "cze" )
  d <- iso( d, "Dominican Rp", "dom" )
  d <- iso( d, "Eq Guinea", "gnq" )
  d <- iso( d, "Fr Guiana", "guf" )
  d <- iso( d, "Guineabissau", "gnb" )
  d <- iso( d, "Iran", "irn" )
  d <- iso( d, "Laos", "lao" )
  d <- iso( d, "Libya", "lby" )
  d <- iso( d, "Macedonia", "mkd" )
  d <- iso( d, "Moldova Rep", "mda" )
  d <- iso( d, "Papua N Guin", "png" )
  d <- iso( d, "Russian Fed", "rus" )
  d <- iso( d, "Syria", "syr" )
  d <- iso( d, "Tanzania", "tza" )
  d <- iso( d, "Trinidad Tob", "tto" )
  d <- iso( d, "Uk", "gbr" )
  d <- iso( d, "Great Britain", "gbr" )
  d <- iso( d, "Untd Arab Em", "are" )
  d <- iso( d, "United States", "usa" )
  d <- iso( d, "Usa", "usa" )
  d <- iso( d, "USA", "usa" )
  d <- iso( d, "Yugoslav Fr", "yug" )
  d <- iso( d, "Zaire", "cod" )
  d <- iso( d, "Brunei", "brn" )
  d <- iso( d, "Central African Rep.", "caf" )
  d <- iso( d, "Congo DRC", "cod" )
  d <- iso( d, "Moldova", "mda" )
  d <- iso( d, "Russia", "rus" )
  d <- iso( d, "Vietnam", "vnm" )
  d <- iso( d, "Bosnia & Herzegovina", "bih" )
  d <- iso( d, "Cayman Is.", "cym" )
  d <- iso( d, "Cook Is.", "cok" )
  d <- iso( d, "Falkland Is.", "flk" )
  d <- iso( d, "Faroe Is.", "fro" )
  d <- iso( d, "Marshall Is.", "mhl" )
  d <- iso( d, "Micronesia", "fsm" )
  d <- iso( d, "Occupied Palestinian Territory", "pse" )
  d <- iso( d, "Sao Tome & Principe", "stp" )
  d <- iso( d, "Solomon Is.", "slb" )
  d <- iso( d, "St. Kitts & Nevis", "kna" )
  d <- iso( d, "St. Kitts and Nevis", "kna" )
  d <- iso( d, "St. Lucia", "lca" )
  d <- iso( d, "St. Vincent & the Grenadines", "vct" )
  d <- iso( d, "St.Vincent and Grenadines", "vct" )
  d <- iso( d, "Saint Vincent/Grenadines", "vct" )
  d <- iso( d, "Svalbard", "sjm" )
  d <- iso( d, "The Bahamas", "bhs" )
  d <- iso( d, "The Gambia", "gmb" )
  d <- iso( d, "Timor-Leste", "tls" )
  d <- iso( d, "Trinidad & Tobago", "tto" )
  d <- iso( d, "Turks & Caicos Is.", "tca" )
  d <- iso( d, "Virgin Is.", "vir" )
  d <- iso( d, "Bahamas, The", "bhs" )
  d <- iso( d, "Falkland Islands (Islas Malvinas)", "flk" )
#   d <- iso( d, "Former Serbia and Montenegro", "" )
#   d <- iso( d, "Former U.S.S.R.", "" )
  d <- iso( d, "Former Yugoslavia", "yug" )
  d <- iso( d, "Yugoslav SFR", "yug" )
#   d <- iso( d, "Germany, East", "" )
#   d <- iso( d, "Germany, West", "" )
#   d <- iso( d, "Hawaiian Trade Zone", "" )
  d <- iso( d, "Timor-Leste (East Timor)", "tls" )
  d <- iso( d, "Turks and Caicos Islands", "tca" )
#   d <- iso( d, "U.S. Pacific Islands", "" )
  d <- iso( d, "Virgin Islands,  U.S.", "vir" )
  d <- iso( d, "Antigua and Barbuda", "atg" )
  d <- iso( d, "Bolivia (Plurinational State of)", "bol" )
  d <- iso( d, "British Virgin Islands", "vgb" )
  d <- iso( d, "Cabo Verde", "cpv" )
  d <- iso( d, "Democratic Republic of the Congo", "cod" )
  d <- iso( d, "Iran (Islamic Republic of)", "irn" )
  d <- iso( d, "Lao People's Democratic Republic", "lao" )
  d <- iso( d, "Libya", "lby" )
  d <- iso( d, "Micronesia (Federated States of)", "fsm" )
  d <- iso( d, "R\x8eunion", "reu" )
  d <- iso( d, "R̩union", "reu" )
  d <- iso( d, "R\x92\xa9union", "reu" )
  d <- iso( d, "R\xed\xa9union", "reu" )
  d <- iso( d, "Republic of Moldova", "mda" )
  d <- iso( d, "Saint Helena, Ascension and Tristan da Cunha", "shn" )
  d <- iso( d, "South Sudan", "ssd" )
  d <- iso( d, "Sudan (former)", "sdn" )
  d <- iso( d, "The former Yugoslav Republic of Macedonia", "mkd" )
  d <- iso( d, "Turks and Caicos Islands", "tca" )
  d <- iso( d, "United Republic of Tanzania", "tza" )
  d <- iso( d, "United States Virgin Islands", "vir" )
  d <- iso( d, "USSR", "svu" )
  d <- iso( d, "Venezuela (Bolivarian Republic of)", "ven" )
  d <- iso( d, "Wallis and Futuna Islands", "wlf" )
  d <- iso( d, "West Bank and Gaza Strip", "pse" )
#   d <- iso( d, "Wake Insland", "" )
  ## Country code for Romania is sometimes "rom" and sometimes "rou"
  d <- iso( d, "Romania", "rou" )
  return ( d )
}

# --------------------------------------------------------------------------------------
