## Historical data w/ estimated consumer price
## Ryna, Mar 2017

## Develop the empirical relationship, by food item

# -----------------------------------------------------------------------------
## Notes
# -----------------------------------------------------------------------------
## Input data sources:
  ## GDP_cap (PPP) - Penn World
  ## consumption - FAO
  ## Consumer prices - estimated based on empirical relationship w/ domestic producer prices, intl producer prices, w/ or w/o consumption, and GDP
  #### historical CP data from the ILO LABORSAT October Inquiry: national/sub-national annual retail food prices, 93 items, 1985-2008  
  #### downloaded @ http://laborsta.ilo.org/data_topic_E.html

## Definitions:
  ## Staple foods: corn, other grains, rice, wheat, roots & tubers
  ## Nonstaple foods: everything else, except alcohol
  ## Consumer prices: domestic consumer prices, aggregated to GCAM commodities, weighted by consumption
  ## Calorie: dietary calorie (= kcal of energy calories)
## Output variables and units:
  ## GDP/cap: 1,000 (2005USD/cap)
  ## Price, consumer (2005USD/1000 calories)
  ## Consumption, GCAM commodities: 1000 calories/person/year
  ## Population (thousands)

# -----------------------------------------------------------------------------
# Define functions and load libraries
# -----------------------------------------------------------------------------
path <- dirname(normalizePath(sys.frame(1)$ofile))
source( file.path(path,"functions.R") ) # Load my standard set of functions
library(Rcpp)
library(magrittr)

# -----------------------------------------------------------------------------
# Input raw data
# -----------------------------------------------------------------------------
setwd( file.path(path,"../../data/raw-data" ))
d.iso <- inputData( d, "iso_GCAM_regID_name.csv", 0 )
d.cons <- inputData( d, "FAOstat_food_supply.csv.gz", 0 )
d.commod.map_gcam <- inputData( d, "FAOStat_FBS_GCAM_mapping.csv", 0 )
d.pop <- inputData( d, "FAOstat_population.csv", 0 )
# FAO data have missing GDP for China, WB has missing Argentina data and only cover 1990+
# Use PWT
d.gdp <- inputData( d, "pwt81.csv", 0 )
# right now read in the estimated CP by region and GCAM_commodity directly, later R scripts from raw data
d.est.cp <- inputData( d, "est_cp_glm_wo_cons.csv", 0) # 2005usd_tonne

# -----------------------------------------------------------------------------
# Edit standard GCAM region definitions to separate Hong Kong and Macau (per discussion with Robert)
# Because the SARs are quite different than mainland China, it gives additional data for the estimation
# -----------------------------------------------------------------------------
d.iso$GCAM_region_ID <- ifelse( d.iso$iso == "hkg" | d.iso$iso == "mac", 33, d.iso$GCAM_region_ID )
d.iso$GCAM_region_name <- ifelse( d.iso$iso == "hkg" | d.iso$iso == "mac", "HongKong_Macau", d.iso$GCAM_region_name )

# -----------------------------------------------------------------------------
# Function to clean FAO datasets for processing
# -----------------------------------------------------------------------------
faoClean <- function( d )
{
  ## Function to replace characters
  charReplace <- function ( d, x )
  {
    d[[x]] <- tolower( d[[x]] )
    d[[x]] <- gsub( "- ", "", d[[x]] )
    d[[x]] <- gsub( " & ", "_", d[[x]] )
    d[[x]] <- gsub( " ", "_", d[[x]] )
    d[[x]] <- gsub( "\\/", "_", d[[x]] )
    d[[x]] <- gsub( "\\(", "", d[[x]] )
    d[[x]] <- gsub( "\\)", "", d[[x]] )
    d[[x]] <- gsub( "\\.", "", d[[x]] )
    d[[x]] <- gsub( "\\$", "d", d[[x]] )
    d[[x]] <- gsub( ",", "", d[[x]] )
    return( d )
  }
  d <- d %>%
    dplyr::rename( var = Domain, country_name = AreaName, unit = ElementName, item = ItemName, year = Year, value = Value ) %>%
    dplyr::select( country_name, year, item, var, unit, value ) %>%
    full_join( d.iso, by = "country_name" ) %>%
    # Remove country names that cause duplicate iso values
    filter( !( country_name ==  "Belgium-Luxembourg" | country_name == "Ethiopia PDR" | country_name ==  "Czechoslovakia" | country_name == "Micronesia" ) ) %>%
    # Remove total and miscellaneous categories
    filter( !( item == "Miscellaneous" | item == "Grand Total" ) ) %>%
    filter( year > 1969 ) %>%
    isoReplace( ) %>%
    filter( !is.na( iso ) ) %>% # Bulk download includes aggregated regions
    dplyr::select( country_name, iso, var, item, unit, year, value ) %>%
    filter( !( iso == "sdn" & year > 2009 ) ) %>%
    distinct( iso, unit, item, year, .keep_all=TRUE ) %>%
    ## Replace problem characters in values
    charReplace( "var" ) %>%
    charReplace( "unit" )
  return( d )
}

# -----------------------------------------------------------------------------
# Clean and prepare data for analysis
# -----------------------------------------------------------------------------
## Clean population
d.pop <- d.pop %>%
  faoClean( ) %>%
  dplyr::rename( pop_thous = value ) %>%
  dplyr::select( iso, year, pop_thous )

## Clean consumption data
d.cons <- faoClean( d.cons )
## Prepare for merging with price data (below)
d.cons <- d.cons %>%
  filter( unit != "food" ) %>%
  filter( unit != "" ) %>%
  dplyr::select( -var ) %>%
  spread( unit, value ) %>%
  dplyr::rename( FBS_commod = item ) %>%
  full_join( d.commod.map_gcam, by = "FBS_commod" ) %>%
  dplyr::select( -country_name, -code ) %>%
  filter( !is.na( GCAM_commodity ) )
## Aggregate to GCAM commodity, to match the estimated CP
d.cons <- aggregate( d.cons[ , 4:5], 
                     by = as.list( d.cons[ c( "iso", "GCAM_commodity", "year" ) ] ), sum, na.rm = T )

## Clean GDP data, aggregate to GCAM regions
# Units are GDP: mil2005$, population: millions
d.gdp <- d.gdp %>%
  dplyr::rename( iso = countrycode ) %>%
  mutate( iso = tolower( iso ) ) %>%
  filter( currency_unit != "Zimbabwe Dollar" ) %>%
  mutate( pop_thous_pwt = pop * 1000 ) %>%
  dplyr::select( iso, year, rgdpna, pop_thous_pwt ) %>%
  colnameReplace( "rgdpna", "gdp_mil_2005usd" ) %>%
  filter( !is.na( gdp_mil_2005usd ) )

## Join price, consumption, pop, and gdp data
## Multiply country per capita values by population, aggregate to region, divide by population
d.cons <- d.cons %>%
  inner_join( d.pop, by = c( "iso", "year" ) ) %>%
  # Compute total country demand
  mutate( food_supply_thous_cal_day = food_supply_kcal_capita_day * pop_thous ) %>%
  mutate( food_supply_thous_kg_day = food_supply_quantity_kg_capita_yr * pop_thous / 365 ) %>%
  dplyr::select( -food_supply_kcal_capita_day, -food_supply_quantity_kg_capita_yr ) %>%
  inner_join( d.gdp, by = c( "iso", "year" ) ) %>%
  # Aggregate individual commodities to GCAM regions
  inner_join( d.iso, by = "iso" ) %>%
  dplyr::select( -GCAM_region_ID, -country_name ) 

## Now that we've matched consumption, gdp, and pop data (countries are only included if they have all three in any year), we can separate GDP/cap estimation 
d.gdp.reg <- d.cons %>%
  dplyr::select( GCAM_region_name, iso, year, gdp_mil_2005usd, pop_thous_pwt ) #%>%
d.gdp.reg <- d.gdp.reg[!duplicated( d.gdp.reg[1:5] ), ]
d.gdp.reg <- d.gdp.reg %>%
  gather( var, value, 4:5 ) %>%
  spread( iso, value ) %>%
  mutate( value = rowSums( .[4:158], na.rm = TRUE ) ) %>%
  dplyr::select( GCAM_region_name, year, var, value ) %>%
  spread( var, value ) %>%
  mutate( gdp_pcap_thous2005usd = gdp_mil_2005usd / pop_thous_pwt ) %>%
  dplyr::select( GCAM_region_name, year, gdp_pcap_thous2005usd )

## Same for population by region
d.pop.reg <- d.cons %>%
  dplyr::select( GCAM_region_name, iso, year, pop_thous ) #%>%
d.pop.reg <- d.pop.reg[!duplicated( d.pop.reg[1:4] ), ]
d.pop.reg <- d.pop.reg %>%
  spread( iso, pop_thous ) %>%
  mutate( pop_thous = rowSums( .[3:ncol(.)], na.rm = TRUE ) ) %>%
  dplyr::select( GCAM_region_name, year, pop_thous )

## Now aggregate consumption commodities to regions
d.cons.reg <- d.cons %>%
  dplyr::select( -gdp_mil_2005usd, -pop_thous_pwt, -pop_thous ) %>%
  ## Multiply quantity by prices to get expenditures by commodity and iso
  inner_join( d.est.cp, by = c( "GCAM_region_name", "GCAM_commodity", "year" ) ) %>%
  mutate( commod_exp_2005usd_day = ( ( est.cp_2005usd_tonne ) * food_supply_thous_kg_day ) ) %>%
  dplyr::select( -food_supply_thous_kg_day, -est.cp_2005usd_tonne ) %>%
  ## Aggregate caloric consumption and expenditures by commodity to region
  gather( var, value, c( food_supply_thous_cal_day, commod_exp_2005usd_day ) ) %>%
  spread( iso, value ) %>%
  mutate( value = rowSums( .[6:ncol(.) ], na.rm = TRUE ) ) %>%
  dplyr::select( 1:5,ncol(.) ) %>%
  ## Aggregate commodity to staples and non-staples
  filter( GCAM_commodity != "alcohol" ) %>%
  mutate( s_ns = ( ifelse( ( GCAM_commodity == "Corn" | GCAM_commodity == "OtherGrain" | GCAM_commodity == "Rice" |
                               GCAM_commodity == "Root_Tuber" | GCAM_commodity == "Wheat" ), "s", "ns" ) ) ) %>%
  #select( -GCAM_commodity ) %>%
  spread( GCAM_commodity, value ) %>%
  mutate( value = rowSums( .[6:ncol(.)], na.rm = TRUE ) ) %>%
  dplyr::select( 1:5,ncol(.) ) %>%
  mutate( var = paste( s_ns, var, sep = "_" ) ) %>%
  dplyr::select( -s_ns ) %>%
  spread( var, value ) %>%
  ## Divide expenditures by cons/day to get price
  mutate( ns_usd_p1000cal = ns_commod_exp_2005usd_day / ns_food_supply_thous_cal_day ) %>%
  mutate( s_usd_p1000cal = s_commod_exp_2005usd_day / s_food_supply_thous_cal_day ) %>%
  dplyr::select( -ns_commod_exp_2005usd_day, -s_commod_exp_2005usd_day ) %>%
  ## Divide cons/day by pop to get cons/cap/day
  full_join( d.pop.reg, by = c( "GCAM_region_name", "year" ) ) %>%
  mutate( ns_cal_pcap_day_thous = ns_food_supply_thous_cal_day / ( pop_thous * 1000 ) ) %>%
  mutate( s_cal_pcap_day_thous = s_food_supply_thous_cal_day / ( pop_thous * 1000 ) ) %>%
  ## Consumption shares
  mutate( s_share = s_cal_pcap_day_thous / ( s_cal_pcap_day_thous + ns_cal_pcap_day_thous ) ) %>%
  mutate( ns_share = ns_cal_pcap_day_thous / ( s_cal_pcap_day_thous + ns_cal_pcap_day_thous ) ) %>%
  ## Bind with GDP data
  full_join( d.gdp.reg, by = c( "GCAM_region_name", "year" ) ) %>%
  dplyr::select( GCAM_region_name, year, pop_thous, gdp_pcap_thous2005usd, ns_cal_pcap_day_thous, ns_usd_p1000cal, 
          s_cal_pcap_day_thous, s_usd_p1000cal, s_share, ns_share )

## create final data set
source(file.path(path,'../R/util.R'))
allrgn.data <- filter(d.cons.reg, GCAM_region_name != 'Europe_Non_EU', !(GCAM_region_name=='Central Asia' & year <= 1990),
                      GCAM_region_name != 'Taiwan', year > 1970 ) %>% # data goes back to 1971, no data for Taiwan
  assign.sigma.Q()  %>%
  calc.pop.weight()
write.csv( allrgn.data, "../food-dmnd-price-allrgn.csv", row.names = FALSE )
create.xval.data(allrgn.data, '..')

rm( d.commod.map, d.cons, d.conv, d.deflator, d.gdp, d.gdp.reg, d.iso, d.pop, d.pop.reg, d.pp, d.pp.archive )

#------------------------------------------------------
## Figures.  These will be generated and stored in the lists
##           plots1 and plots2, but they will not be displayed
##           unless specifically asked for.
#------------------------------------------------------
d <- d.cons.reg
## Assign colors to regions
region_color <- c( "Africa_Eastern" = "olivedrab2",
                   "Africa_Northern" = "darkgreen",
                   "Africa_Southern" = "green4",
                   "Africa_Western" = "olivedrab3",
                   "Argentina" = "mediumpurple",
                   "Australia_NZ" = "cornflowerblue",
                   "Brazil" = "mediumpurple4",
                   "Canada" = "chocolate1",
                   "Central America and Caribbean" = "darkorchid4",
                   "Central Asia" = "firebrick1",
                   "China" = "firebrick4",
                   "Colombia" = "magenta4",
                   "EU-12" = "dodgerblue4",
                   "EU-15" = "dodgerblue3",
                   "Europe_Eastern" = "deepskyblue3",
                   "Europe_Non_EU" = "cadetblue2",
                   "European Free Trade Association" = "cadetblue",
                   "HongKong_Macau" = "red2",
                   "India" = "indianred",
                   "Indonesia" = "firebrick2",
                   "Japan" = "deeppink4",
                   "Mexico" = "orchid4",
                   "Middle East" = "darkolivegreen3",
                   "Pakistan" = "indianred4",
                   "Russia" = "red4",
                   "South Africa" = "olivedrab4",
                   "South America_Northern" = "purple4",
                   "South America_Southern" = "purple2",
                   "South Asia" = "indianred2",
                   "South Korea" = "deeppink3",
                   "Southeast Asia" = "red3",
                   "Taiwan" = "deeppink3",
                   "USA" = "chocolate3" )
colScaleRegion <- scale_colour_manual( name = "gcam_region", values = region_color )
colFillRegion <- scale_fill_manual( name = "gcam_region", values = region_color )

## Plots of time vs. variables:
d.fig <- melt( d, id.vars = 1:2 )
vars <- c( "pop_thous", "gdp_pcap_thous2005usd", "ns_cal_pcap_day_thous", "ns_usd_p1000cal", "s_cal_pcap_day_thous",
           "s_usd_p1000cal", "s_share", "ns_share" )
makePlot1 <- function ( v )
{
  d.f <- subset( d.fig, variable == v )
  p <- ggplot( d.f, aes( year, value, color = GCAM_region_name ) ) + geom_line( size = 1 )
  p <- p + theme_basic + colScaleRegion + guides( col = guide_legend( ncol = 1 ) )
  p <- p + xlab( "year" ) + ylab( v )
  p <- p + theme( axis.text.x = element_text( angle = 50, vjust = 0.5 ) )
  ggsave( plot = p, paste( "time_v_", v, ".pdf", sep = "" ), width = 400, height = 300, units = "mm"  )  
  return( p )
}
plots1 <- lapply( vars, makePlot1 )

## Plots of GDP/cap vs. variables:
vars <- c( "ns_cal_pcap_day_thous", "ns_usd_p1000cal", "s_cal_pcap_day_thous",
           "s_usd_p1000cal", "s_share", "ns_share" )
makePlot2 <- function( v )
{
  p <- ggplot( d, aes_string( "gdp_pcap_thous2005usd", v, color = "GCAM_region_name" ) ) + geom_point( size = 1 )
  p <- p + theme_basic + colScaleRegion + guides( col = guide_legend( ncol = 1 ) )
  p <- p + xlab( "GDP/cap PPP" ) + ylab( v )
  p <- p + theme( axis.text.x = element_text( angle = 50, vjust = 0.5 ) )
  ggsave( plot = p, paste( "gdppcap_v_", v, ".pdf", sep = "" ), width = 400, height = 300, units = "mm"  )  
  return( p )
}
plots2 <- lapply( vars, makePlot2 )



