## Historical data processing for GCAM food demand
## Stephanie, August 2016

## Create variables for GCAM food demand, by region, staple and non-staple

# -----------------------------------------------------------------------------
## Notes
# -----------------------------------------------------------------------------
### For now, let price of fish == price of sheep; change when more info becomes available

## Input data sources:
  ## GDP_cap (PPP) - WB WDI
  ## consumption - FAO
  ## Prices - FAO

## Definitions:
  ## Staple foods: corn, other grains, rice, wheat, roots & tubers
  ## Nonstaple foods: everything else, except alcohol
  ## Prices: domestic producer prices, aggregated to staple/nonstaple commodities, weighted by consumption
  ## Calorie: dietary calorie (= kcal of energy calories)
## Output variables and units:
  ## GDP/cap: 1,000 (2005USD/cap)
  ## Price, staples & nonstaples (2005USD/1000 calories)
  ## Consumption, staples & nonstaples: 1000 calories/person/year
  ## Share calories from staples & nonstaples
  ## Population (thousands)

# -----------------------------------------------------------------------------
# Define functions and load libraries
# -----------------------------------------------------------------------------
path <- dirname(sys.frame(1)$ofile)
source( file.path(path,"functions.R") ) # Load my standard set of functions

# -----------------------------------------------------------------------------
# Input raw data
# -----------------------------------------------------------------------------
setwd( file.path(path,"../../data/raw-data" ))
d.iso <- inputData( d, "iso_GCAM_regID_name.csv", 0 )
d.cons <- inputData( d, "FAOstat_food_supply.csv.gz", 0)
d.pp <- inputData( d, "FAOstat_producer_prices.csv", 0)
d.commod.map <- inputData( d, "FAOStat_GCAM_mapping.csv", 2 )
d.deflator <- inputData( d, "FAOstat_USD_deflator.csv", 0 )
d.pop <- inputData( d, "FAOstat_population.csv", 0 )
# FAO data have missing GDP for China, use WB instead
# Argentina is missing for WDI, omit at this point, but check later if we can find it anywhere else
d.gdp <- inputData( d, "WDI_gdp_pcap_ppp.csv", 0 )


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
    rename( var = Domain, country_name = AreaName, unit = ElementName, item = ItemName, year = Year, value = Value ) %>%
    select( country_name, year, item, var, unit, value ) %>%
    filter( year > 1990 ) %>%
    full_join( d.iso, by = "country_name" ) %>%
    # Remove country names that cause duplicate iso values
    filter( !( country_name ==  "Belgium-Luxembourg" | country_name == "Ethiopia PDR" | country_name ==  "Czechoslovakia" | country_name == "Micronesia" ) ) %>%
    # Remove total and miscellaneous categories
    filter( !( item == "Miscellaneous" | item == "Grand Total" ) ) %>%
    isoReplace( ) %>%
    filter( !is.na( iso ) ) %>% # Bulk download includes aggregated regions
    select( country_name, iso, var, item, unit, year, value ) %>%
    filter( !( iso == "sdn" & year > 2009 ) ) %>%
    distinct( iso, unit, item, year ) %>%
  ## Replace problem characters in values
    charReplace( "var" ) %>%
    charReplace( "unit" )
  return( d )
}

# -----------------------------------------------------------------------------
# Clean and prepare data for analysis
# -----------------------------------------------------------------------------
## Clean USD GDP deflator
d.deflator <- d.deflator %>%
  faoClean( ) %>%
  rename( deflator_2005usd = value ) %>%
  select( year, deflator_2005usd )

## Clean population
d.pop <- d.pop %>%
  faoClean( ) %>%
  rename( pop_thous = value ) %>%
  select( iso, year, pop_thous )

## Clean consumption data
d.cons <- faoClean( d.cons )
  ## Prepare for merging with price data (below)
d.cons <- d.cons %>%
  select( -var ) %>%
  spread( unit, value ) %>%
  rename( cons_commod = item ) %>%
  full_join( d.commod.map, by = "cons_commod" ) %>%
  select( -country_name )

## Clean producer price data
d.pp <- d.pp %>%
  faoClean( ) %>%
  select( -country_name, -iso ) %>%
  spread( item, value )
  d.pp[["average(Sugar cane, Sugar beet)"]] <- ( d.pp[["Sugar cane"]] + d.pp[["Sugar beet"]] ) / 2
  d.pp[["average(Rapeseed, Mustard seed)"]] <- ( d.pp[["Rapeseed"]] + d.pp[["Mustard seed"]] ) / 2
  d.pp[["average(Beans, dry; Peas, dry; Chick peas; Lentils)"]] <- ( d.pp[["Beans, dry"]] + d.pp[["Peas, dry"]]
                                                                     + d.pp[["Chick peas"]] + d.pp[["Lentils"]] ) / 4
  d.pp[["average(Almonds, with shell; Walnuts, with shell)"]] <- ( d.pp[["Almonds, with shell"]] + d.pp[["Walnuts, with shell"]] ) / 2
  ### For now, let price of fish == price of sheep; change when more info becomes available ###
  d.pp$fish <- d.pp[["Meat live weight, sheep"]]
d.pp <- d.pp %>%
  gather( pp_commod, pp_usd_tonne, 4:ncol( d.pp ) ) %>%
  ## Merge price and deflator data
  full_join( d.deflator, by = "year") %>%
  mutate( pp_2005usd_tonne = deflator_2005usd * pp_usd_tonne ) %>%
  ## Merge with GCAM mapping variables
  full_join( d.commod.map, by = "pp_commod" ) %>%
  select( cons_commod, pp_commod, year, pp_2005usd_tonne )

## Clean GDP data, aggregate to GCAM regions
deflator_2011 <- filter( d.deflator, year == 2011 )
d.gdp <- d.gdp %>%
  gather( variable, gdp_pcap_ppp2011d, X1960:X2015 ) %>%
  yr( ) %>%
  rename( iso = Country.Code ) %>%
  mutate( gdp_pcap_thous2005usd = ( gdp_pcap_ppp2011d / deflator_2011$deflator_2005usd ) / 1000 ) %>%
  select( iso, year, gdp_pcap_thous2005usd ) %>%
  mutate( iso = tolower( iso ) ) %>%
  inner_join( d.pop, by = c( "iso", "year" ) ) %>%
  mutate( gdp_mil_2005usd = gdp_pcap_thous2005usd  * pop_thous ) %>%
  select( -gdp_pcap_thous2005usd ) %>%
  na.omit( ) %>%
  inner_join( d.iso, by = "iso" ) %>%
  select( -country_name, -GCAM_region_ID ) %>%
  gather( var, value, c( pop_thous, gdp_mil_2005usd ) ) %>%
  spread( iso, value ) %>%
  mutate( value = rowSums( .[4:187], na.rm = TRUE ) ) %>%
  select( GCAM_region_name, year, var, value ) %>%
  spread( var, value )

## Join price, consumption, pop, and gdp data
## Multiply country per capita values by population, aggregate to region, divide by population
d.cons <- d.cons %>%
  inner_join( d.pop, by = c( "iso", "year" ) ) %>%
    # Compute total country demand
  mutate( food_supply_thous_cal_day = food_supply_kcal_capita_day * pop_thous ) %>%
  mutate( food_supply_thous_kg_day = food_supply_quantity_kg_capita_yr * pop_thous / 365 ) %>%
  select( -food_supply_kcal_capita_day, -food_supply_quantity_kg_capita_yr, -food, -pop_thous ) %>%
    # Aggregate individual commodities to GCAM regions
  full_join( d.iso, by = c( "iso" ) ) %>%
  select( -GCAM_region_ID, -country_name ) %>%
  gather( var, value, c( food_supply_thous_cal_day, food_supply_thous_kg_day ) ) %>%
  spread( iso, value ) %>%
  mutate( value = rowSums( .[7:246], na.rm = TRUE ) ) %>%
  mutate( s_ns = ( ifelse( ( GCAM_commodity == "Corn" | GCAM_commodity == "OtherGrain" | GCAM_commodity == "Rice" |
                    GCAM_commodity == "Root_Tuber" | GCAM_commodity == "Wheat" ), "s", "ns" ) ) ) %>%
  filter( !is.na( GCAM_commodity ) ) %>%
  filter( GCAM_commodity != "alcohol" ) %>%
  select( GCAM_region_name, s_ns, cons_commod, year, var, value )

## Create weight variables for prices
d.cons.weight <- d.cons %>%
  filter( var == "food_supply_thous_cal_day" ) %>%
  spread( cons_commod, value ) %>%
  mutate( value = rowSums( .[5:91], na.rm = TRUE ) )
for( x in 5:91 ) { d.cons.weight[[x]] = d.cons.weight[[x]] / d.cons.weight$value }
d.cons.weight <- d.cons.weight %>%
  select( -value, -var, -s_ns ) %>%
  gather( cons_commod, weight, c( 3:89 ) ) %>%
  filter( !is.na( weight ) )

## Estimate price per calorie
d <- d.cons %>%
  spread( var, value ) %>%
  # Compute cal/kg by region ## NOTE: for regions with very low caloric consumption of commodities, the cal/kg look wrong
  # (e.g. bananas in Eastern Africa have cal/kg ~ 2-4 times higher than they should), but consumption is small, so don't worry about it
  mutate( thous_cal_p_kg = ifelse ( food_supply_thous_kg_day != 0,
                                  ( ( food_supply_thous_cal_day ) / ( food_supply_thous_kg_day * 1000 ) ), 0 ) ) %>%
  # Merge with prices per tonne
  full_join( d.pp, by = c( "cons_commod", "year" ) ) %>%
  filter( !( pp_commod == "non_food" | pp_commod == "cons_not_reported" | pp_commod == "not_available" |
               pp_commod == "aggregate" ) ) %>%
  # Estimate cost per calorie in region by commodity
  mutate( usd_p1000cal = ifelse( thous_cal_p_kg != 0,( ( pp_2005usd_tonne / 1000 ) / thous_cal_p_kg ), NA ) ) %>%
  na.omit( )

## Aggregate to staple and non-staples prices, weighted by consumption
d.price.weight<- d %>%
  select( GCAM_region_name, s_ns, cons_commod, year, usd_p1000cal ) %>%
  inner_join( d.cons.weight, by = c( "GCAM_region_name", "cons_commod", "year" ) ) %>%
  mutate( price_weight = usd_p1000cal * weight ) %>%
  select( -usd_p1000cal, - weight ) %>%
  spread( cons_commod, price_weight ) %>%
  mutate( usd_p1000cal = rowSums( .[4:81], na.rm = TRUE ) ) %>%
  select( GCAM_region_name, s_ns, year, usd_p1000cal )

## Calculate staple and non-staple per capita consumption by region
d <- d %>%
  select( GCAM_region_name, s_ns, cons_commod, year, food_supply_thous_cal_day ) %>%
  spread( cons_commod, food_supply_thous_cal_day ) %>%
  mutate( cal_thous_day = rowSums( .[ 4:81], na.rm = TRUE ) ) %>%
  select( GCAM_region_name, s_ns, year, cal_thous_day ) %>%
  # Merge with price, gdp, and population data
  inner_join( d.gdp, by = c( "GCAM_region_name", "year" ) ) %>%
  full_join( d.price.weight, by = c( "GCAM_region_name", "year", "s_ns" ) ) %>%
  # Estimate per capita values by region
  mutate( cal_pcap_day_thous = cal_thous_day / ( pop_thous * 1000 ) ) %>%
  mutate( gdp_pcap_thous2005usd = gdp_mil_2005usd / pop_thous ) %>%
  select( -cal_thous_day, -gdp_mil_2005usd ) %>%
  filter( year < 2012 )

## Create columns for staple, non-staple cons, share, and prices
d.s.ns <- d %>%
  select( -gdp_pcap_thous2005usd, -pop_thous ) %>%
  gather( var, value, c( cal_pcap_day_thous, usd_p1000cal ) ) %>%
  mutate( var = paste( s_ns, var, sep = "_" ) ) %>%
  select( -s_ns ) %>%
  spread( var, value ) %>%
  mutate( s_share = s_cal_pcap_day_thous / ( s_cal_pcap_day_thous + ns_cal_pcap_day_thous ) ) %>%
  mutate( ns_share = ns_cal_pcap_day_thous / ( s_cal_pcap_day_thous + ns_cal_pcap_day_thous ) )

# Bind with population to calculate consumption per capita
d <- d %>%
  filter( s_ns != "ns" ) %>%
  select( GCAM_region_name, year, pop_thous, gdp_pcap_thous2005usd ) %>%
  full_join( d.s.ns, by = c( "GCAM_region_name", "year" ) ) %>%
  na.omit( )

## create final data set
source(file.path(path,'../R/util.R'))
allrgn.data <- assign.sigma.Q(d)
write.csv( allrgn.data, "../food-dmnd-price-allrgn.csv", row.names = FALSE )


rm( d.s.ns, d.cons.weight, d.price.weight, d.commod.map, d.iso, d.deflator_2011, d.pop, d.gdp, d.cons.ag, d.cons.an, d.map,
    d.cons, d.pp, d.deflator, deflator_2011 )

#------------------------------------------------------
## Figures.  These will be generated and stored in the lists
##           plots1 and plots2, but they will not be displayed
##           unless specifically asked for.
#------------------------------------------------------
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
  return( p )
}
plots2 <- lapply( vars, makePlot2 )
