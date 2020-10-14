# Historical data processing for GCAM food demand
# Stephanie Waldhoff, Kanishka Narayan, January 2020

# Create variables for GCAM food demand, by region, staple and non-staple

# -----
# Notes
# -----
# Final data set should have columns:
# iso, year, pop_thous, gdp_pcapthous2010usd, ns_cal_pcap_day_thous, ns_usd_pthous_cal, s_cal_pcap_day_thous, s_usd_pthous_cal, s_share, ns_share
# Where iso = country code, s = staples (grains, roots&tubers), ns = non-staples (everything else)
# cal_pcap_day_thous = 1,000 non-staple dietary calories per capita per day
# usd_pthous_cal = 2010USD  per 1,000 dietary calories, weighted by caloric consumption
# share = share of total consumption

# Input data sources:
# GDP_cap - FAOStat
# consumption, imports, exports, production - FAOStat old methodology (1961-2013) and FAOStat new methodology (2014-2017)
# Prices - FAOStat3 (1991-2017, in 2010 USD) and FAOStat Archive (1966-1990, in nominal local currency units),

# Definitions:
# Staple foods: corn, other grains, rice, wheat, roots & tubers
# Nonstaple foods: everything else, except alcohol
# Prices: weighted average of domestic producer price and world price ()
  # P_world = export weighted average of domestic producer prices
  # Prices aggregated to staple/nonstaple commodities, weighted by consumption
# Calorie: dietary calorie (= kcal of energy calories)
# Output variables and units:
# GDP/cap: 1,000 (2010USD/cap)
# Price, staples & nonstaples (2005USD/1000 calories)
# Consumption, staples & nonstaples: 1000 calories/person/year
# Share calories from staples & nonstaples
# Population (thousands)

## For now, let price of fish == price of sheep; change when more info becomes available

# -----
# Load libraries
# -----
# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)

srcdir <- "C:/Users/kanis/Downloads/"
datadir <- paste0(srcdir, "data_raw/")
outdir <- paste0(srcdir)
`%notin%` <- Negate(`%in%`)

#Set definition of staple crops.
Staples<- c("Corn","OtherGrain","Rice","RootTuber","Wheat")

#Set this constant to FALSE if no diagnostics are required
diagnostics <- FALSE
diagnostics_region <- "dnk"


# -----
# Read in raw data
# -----
# Mappings
d.cal.ag <- read.csv(paste0(datadir, "FAO_ag_items_cal_SUA_2020.csv"), header = T, stringsAsFactors = F, skip = 7)
d.cal.an <- read.csv(paste0(datadir, "FAO_an_items_cal_SUA_2020.csv"), header = T, stringsAsFactors = F, skip = 7)
d.iso <- read.csv(paste0(datadir, "iso_GCAM_regID_name.csv"), header = T, stringsAsFactors = F)
d.iso.addl <- read.csv(paste0(datadir, "iso_codes.csv"), header = T, stringsAsFactors = F)
d.commod.map <- read.csv(paste0(datadir, "FAOSTAT_GCAM_mapping_2020.csv"), header = T, stringsAsFactors = F)
# Consumption data
d.cons.new <- read.csv(paste0(datadir, "FoodBalanceSheets_E_All_Data.csv"), header = T, stringsAsFactors = F)
#kbn take out columns with flags in the dataset
d.cons.new<-d.cons.new[,-grep("F",colnames(d.cons.new))]
d.cons.archive <- read.csv(paste0(datadir, "FoodBalanceSheetsHistoric_E_All_Data.csv"), header = T, stringsAsFactors = F)
#kbn take out columns with flags in the dataset
d.cons.archive<-d.cons.archive[,-grep("F",colnames(d.cons.archive))]
# Producer price data
d.pp.new <- read.csv(paste0(datadir, "Prices_E_All_Data.csv"), header = T, stringsAsFactors = F)
d.pp.new <- d.pp.new[,-grep("F",colnames(d.pp.new))]
d.pp.archive <- read.csv(paste0(datadir, "PricesArchive_E_All_Data.csv"), header = T, stringsAsFactors = F)
d.pp.archive <- d.pp.archive[,-grep("F",colnames(d.pp.archive))]
# GDP and deflators
d.deflator <- read.csv(paste0(datadir, "FAOSTAT_GDP_deflator.csv"), header = T, stringsAsFactors = F)
d.gdp <- read.csv(paste0(datadir, "FAOSTAT_GDP.csv"), header = T, stringsAsFactors = F)
# Trade data
# d.bitrade <-

# -----
# Extract relevant variables and clean data
# -----
# iso codes
d.iso <- select(d.iso, country_name, iso) %>%
  rename(ctry_name = country_name) %>%
  bind_rows(d.iso.addl)
rm(d.iso.addl)

# Bind calroic values for animal and agricultural commodities
d.cal <- bind_rows(d.cal.ag, d.cal.an) %>%
  select(item, Mcal_t)
#rm(d.cal.ag, d.cal.an)

# Local currency conversions to 2010 USD
d.deflator <- select(d.deflator, Area, Year, Value) %>%
  rename(ctry_name = Area, year = Year, cur_conv = Value) %>%
  mutate(cur_conv = (cur_conv / 100)) %>%
  full_join(d.iso, by = "ctry_name") %>%
  select(iso, year, cur_conv) %>%
  filter(!is.na(cur_conv))

# Clean population
# Typo in new food balance sheets and "Grand total" consumption is called population, use element code 511 for population
d.pop <- bind_rows(filter(d.cons.new, Element.Code == 511), filter(d.cons.archive, Element.Code == 511)) %>%
  select(-Area.Code, -Item, -Item.Code, -Element.Code, -Element) %>%
  gather(year, pop_thous, -Area, -Unit) %>%
  rename(ctry_name = Area, unit = Unit) %>%
  filter(!is.na(pop_thous), !grepl("F", year)) %>%
  mutate(year = as.numeric(substr(year, 2,5)),
         pop_thous = as.numeric(pop_thous)) %>%
  left_join(d.iso, by = "ctry_name") %>%
  filter(!is.na(iso)) %>%
  select(iso, year, pop_thous) %>%
  arrange(iso, year)

if (diagnostics== TRUE){
  VizData <- d.pop %>% filter(iso=="chn")
  g<- ggplot(data=VizData, mapping= aes(x= year, y=pop_thous) )+
      geom_line()+
      ggtitle("Population across countries")+
      facet_wrap(~iso)
  g

}


# GDP per capita (2010 USD/cap)
d.gdp.pcap <- select(d.gdp, Area, Year, Value) %>%
  rename(ctry_name = Area, year = Year, gdp_mil2010usd = Value) %>%
  full_join(d.iso, by = "ctry_name") %>%
  full_join(d.pop, by = c("iso", "year")) %>%
  mutate(gdp_pcap_2010usd = (gdp_mil2010usd * 1000 / pop_thous)) %>%
  filter(!is.na(gdp_pcap_2010usd)) %>%
  select(iso, year, gdp_pcap_2010usd)

#Diagnostics for GDP
if (diagnostics== TRUE){
  VizData <- d.gdp.pcap %>% filter(iso=="chn")
  g<- ggplot(data=VizData, mapping= aes(x= year, y=gdp_pcap_2010usd) )+
    geom_line()+
    ggtitle("GDP for debug region")+
    facet_wrap(~iso)
  g

}

sugar_crops<-c("Sugar cane","Sugar beet","Sugar crops nes")
# Clean producer prices
#Note using SLC here since these are standardized prices from FAO. Note that SLC are not available in archive data.
d.pp <- select(d.pp.new, -Area.Code, -Item.Code, -Element.Code, -Element) %>%
  filter(Unit == "SLC") %>%
  select(-Unit) %>%
  gather(Year, Value, -Area, -Item) %>%
  rename(Country = Area) %>%
  filter(!is.na(Value), !grepl("F", Year)) %>%
  mutate(Year = as.numeric(substr(Year, 2,5)),
         Value = as.numeric(Value)) %>%
  bind_rows(select(d.pp.archive, Country, Item, Year, Value)) %>%
  rename(ctry_name =  Country, item = Item, year = Year, pp_lcu_t = Value) %>%
  full_join(d.iso, by = "ctry_name") %>%
  select(-ctry_name) %>%
  full_join(d.deflator, by = c("iso", "year")) %>%
  filter(!is.na(iso),  year %in% c(1970:2017), !is.na(pp_lcu_t), !is.na(cur_conv)) %>%
  mutate(pp_2010usd_t = (pp_lcu_t / cur_conv)) %>%
  filter(!is.na(pp_2010usd_t)) %>%
  mutate(item2=item) %>%
  mutate(item=if_else(item %in% sugar_crops,"Sugar (Raw Equivalent)",item)) %>%
  group_by(iso, item, year) %>%
  mutate(pp_2010usd_t=mean(pp_2010usd_t)) %>%
  ungroup() %>%
  select(iso, item, year, pp_2010usd_t) %>%
  rename(pp_commod = item)

#Diagnostics for producer prices
if (diagnostics== TRUE){
  VizData <- d.pp %>% filter(iso=="dnk") %>% filter(pp_commod=="Animal fats") %>% filter(year<1990)
  g<- ggplot(data=VizData, mapping= aes(x= year, y=pp_2010usd_t) )+
    geom_line()+
    ggtitle("Producer prices by item")+
    facet_wrap(~pp_commod)
  g

}


# Clean consumption
d.cons <- bind_rows(filter(d.cons.new, Element == "Food"), filter(d.cons.archive, Element == "Food")) %>%
  select(-Area.Code, -Item.Code, -Element.Code, -Element) %>%
  gather(year, cons_thous_t, -Area, -Item, -Unit) %>%
  rename(ctry_name = Area, item = Item, unit = Unit) %>%
  filter(!is.na(cons_thous_t), !grepl("F", year)) %>%
  mutate(year = as.numeric(substr(year, 2,5)),
         cons_thous_t = as.numeric(cons_thous_t)) %>%
  left_join(d.iso, by = "ctry_name") %>%
  filter(!is.na(iso)) %>%
  select(iso, item, year, cons_thous_t) %>%
  arrange(iso, item, year) %>%
  full_join(d.cal, by = "item") %>%
  filter(!is.na(Mcal_t), Mcal_t != 0) %>%
  # Join with population
  left_join(d.pop, by = c("iso", "year")) %>%
  # Calculate dietary calories per capita  per day
  mutate(cal_cap_day = (((cons_thous_t * Mcal_t * 10^3) / pop_thous) / 365)) %>%
  select(-Mcal_t, -pop_thous) %>%
  filter(year %in% c(1970:2017), cons_thous_t >= 0, !is.na(cal_cap_day)) %>%
  rename(cons_commod = item)

#Diagnostics for Consumption
if (diagnostics== TRUE){
  VizData <- d.cons %>% filter(iso=="usa")
  g<- ggplot(data=VizData, mapping= aes(x= year, y=cal_cap_day) )+
    geom_line()+
    ggtitle("Consumption in calories per capita per day")+
    facet_wrap(~cons_commod)
  g

}

# Shares of exports, imports to calculate world prices (export-weighted average of domestic producer prices) and "consumer" prices (consumption-weighted average of domestic  and  world prices)
# Calculate shares of global exports (Exp_iso / Sum(Exp_iso)) to calculate world price (weighted avg of domestic producer prices)
# Calculate shares of domestic and imported consumption (import_iso / consumption_iso) to calculate price in each country
d.trade = bind_rows(filter(d.cons.archive, Element %in% c("Production", "Export Quantity", "Import Quantity", "Domestic supply quantity")),
                    filter(d.cons.new, Element %in% c("Production", "Export Quantity", "Import Quantity", "Domestic supply quantity"))) %>%
  #Some simple data cleaning.
  select(-Area.Code, -Item.Code, -Element.Code) %>%
  gather(Year, thous_t, -Area, -Item, -Unit, -Element) %>%
  rename(ctry_name = Area, item = Item, unit = Unit, variable = Element) %>%
  distinct() %>%
  filter(!is.na(thous_t)) %>%
  mutate(Year = gsub("Y","",Year)) %>%
  mutate(year = as.numeric(Year),
         thous_t = as.numeric(thous_t)) %>%
  left_join(d.iso, by = "ctry_name") %>%
  filter(!is.na(iso)) %>%
  select(iso, variable, item, year, thous_t) %>%
  #Prices are unreliable for these items. Drop them.
  filter(item %notin% c("Olives (including preserved)","Olive Oil")) %>%
  # Keep only the same commodities as used for calculating consumption
  left_join(d.cal, by = "item") %>%
  filter(!is.na(Mcal_t), Mcal_t != 0) %>%
  select(-Mcal_t) %>%
  unique() %>%
  mutate(variable = ifelse(variable == "Production", "prod", variable),
         variable = ifelse(variable == "Export Quantity", "exp", variable),
         variable = ifelse(variable == "Import Quantity", "imp", variable),
         variable = ifelse(variable == "Consumption", "cons", variable),
         variable = ifelse(variable == "Domestic supply quantity", "cons", variable)) %>%
  arrange(iso, year, variable, item) %>%
  spread(variable, thous_t) %>%
  #We need to make sure all holes in the balance sheets are filled. Hence filling NA's with 0.
  mutate(cons= ifelse(is.na(cons),0,cons)) %>%
  mutate(exp= ifelse(is.na(exp),0,exp)) %>%
  mutate(imp= ifelse(is.na(imp),0,imp)) %>%
  mutate(prod= ifelse(is.na(prod),0,prod)) %>%
  #Ensure that there are no negative values anywhere
  mutate(cons= ifelse(cons < 0,0,cons)) %>%
  mutate(exp= ifelse(exp < 0,0,exp)) %>%
  mutate(imp= ifelse(imp < 0,0,imp)) %>%
  mutate(prod= ifelse(prod < 0 ,0,prod)) %>%
  mutate(imp_share = (imp /cons)) %>%
  #Make sure that imports shares are capped at 100%
  mutate(imp_share= if_else(imp_share>1, 1,imp_share) ) %>%
  mutate(dom_share= (prod-exp)/cons) %>%
  #Make sure that domestic shares don't go negative and don't go higher than 1
  mutate(dom_share= ifelse(dom_share< 0, 0, dom_share)) %>%
  mutate(dom_share= ifelse(dom_share> 1, 1, dom_share)) %>%
  #left_join_items here to bring in commodity names to match commodity names in producer prices.
  left_join(d.commod.map %>% rename(item=cons_commod) %>% select(-GCAM_commodity),by=c("item")) %>%
  group_by(item,year) %>%
  mutate(sum_exp= sum(exp)) %>%
  ungroup() %>%
  #Calculate export share for each commodity in each year
  mutate(exp_share = exp/sum_exp) %>%
  group_by(iso, item, year) %>%
  #Compute total consumption within country for each commodity
  mutate(tot_cons=sum(cons)) %>%
  ungroup()


#Check the number of commoditties here. Breaking code into separate chunk for debugging
print (paste0("The number of crops in trade dataset are- ", length(c(unique(d.trade$pp_commod)))))

d.trade %>%
  #Join in producer prices
  left_join(d.pp,by=c("pp_commod","iso","year")) %>%
  #If we don't have producer prices, use a 0
  mutate(pp_2010usd_t =if_else(is.na(pp_2010usd_t),0,pp_2010usd_t)) %>%
  #Make sure we don't have NAs in dom_share, imp_share
  mutate(dom_share =if_else(is.na(dom_share),0,dom_share)) %>%
  mutate(imp_share =if_else(is.na(imp_share),0,imp_share)) %>%
  na.omit() ->d.trade_temp

#This will compute median prices for commoditties
d.trade_temp %>%
  filter(pp_2010usd_t !=0) %>%
  #Get consumption codes here now.
  rename(cons_commod= item) %>%
  group_by(cons_commod,year) %>%
  #adding code to get median global producer price by commod, year
  mutate(pp_world_median = median(pp_2010usd_t)) %>%
  ungroup() %>%
  select(cons_commod,year,pp_world_median) %>%
  distinct()->pp_world_median


d.trade_temp %>%
  #Get consumption codes here now.
  rename(cons_commod= item) %>%
  #Join median prices
  left_join(pp_world_median, by= c("cons_commod","year")) %>%
  #Compute share of world price for each commoditty, year, country
  mutate(p_world = pp_2010usd_t*exp_share) %>%
  na.omit() %>%
  #If this share is higher than 1.5 times the global median, use the global median to compute the share of the global price.
  #note that p_world here is the share of a country's price in the global price. Later, this becomes the global price when we aggregate.
  mutate(p_world= if_else((p_world/pp_world_median)>1.5, pp_world_median*exp_share,p_world)) %>%
  group_by(cons_commod,year) %>%
  #Compute total world price for each commodity in each year
  mutate(p_world=sum(p_world)) %>%
  #If global price is higher than twice the global_median, setting to global median for 2 commoditties, namely Coffee and Oranges
  mutate(p_world=if_else(pp_commod %in% c("Coffee, green","Oranges") & p_world> 2*pp_world_median,pp_world_median,p_world)) %>%
  ungroup() %>%
  #Compute consumption price using world price for imports and domestic price for domestic consumption.
  mutate(cons_price= (((dom_share*cons)/tot_cons)*pp_2010usd_t)+(((imp_share*cons)/tot_cons)*p_world)) %>%
  #filter for consumption prices greater than 0
  filter(cons_price>0)->d.trade2

#Check global prices here
if (diagnostics== TRUE){
  VizData2<-d.trade2 %>% select(pp_commod,year,p_world)  %>%  distinct()
  ggplot(data=VizData2 ,mapping=aes(x=year,y=p_world))+
    geom_line()+
    ylab("Computed Global Price")+
    ggtitle("Checking global prices in USD/tonne for non staples")+
    facet_wrap(~pp_commod,scales="free_y")->g2
  g2

}

#Check computed consumption prices here
if (diagnostics== TRUE){
  VizData2<-d.trade2 %>% select(iso,cons_commod,year,p_world) %>% distinct() %>% filter(iso=="usa")
  ggplot(data=VizData2 ,mapping=aes(x=year,y=p_world))+
    geom_line()+
    ylab("Absolute consumption in tons")+
    ggtitle("Comparing consumption in the USA on staples and non-staples")+
    facet_wrap(~cons_commod+iso)->g2
  g2

}




#Part 3: Now aggregate all variables to staples and non-staples


#Aggregate it all into 1 dataset
d.agg_data_s_ns <- d.trade2 %>%
  #Get GCAM_commoditty names
  left_join(d.commod.map %>% select(cons_commod,GCAM_commodity) %>% distinct(),by=c("cons_commod")) %>%
  na.omit() %>%
  #Make sure only unique values exist
  distinct() %>%
  #Classify into staples and non-staples
  mutate(cat= if_else(GCAM_commodity %in% Staples, "Staples","Non-Staples" )) %>%
  group_by(iso,year) %>%
  mutate(total_cons_by_iso=sum(cons)) %>%
  ungroup() %>%
  #Bring in population
  inner_join(d.pop, by=c("iso","year")) %>%
  #Bring in GDP
  inner_join(d.gdp.pcap, by=c("iso","year")) %>%
  #Bring in calories per capita
  inner_join(d.cons, by=c("iso","year","cons_commod")) %>%
  #Check for unique values after join
  distinct() %>%
  group_by(iso,year) %>%
  #Compute total calories by iso year
  mutate(tot_cal_pcap_day=sum(cal_cap_day)) %>%
  #Compute total consumption by iso year
  mutate(tot_cons_thous_t= sum(cons_thous_t)) %>%
  ungroup() %>%
  group_by(iso, cat, year ) %>%
  #Compute consumption price per tonne for staples and non-staples weighted by caloric consumption
  mutate(cons_price_by_cal= sum(cal_cap_day*cons_price)) %>%
  mutate(cons_price_s_ns=cons_price_by_cal/tot_cal_pcap_day) %>%
  #Compute share of consumption of staples, non-staples
  mutate(cons_thous_t_s_ns =sum(cons_thous_t)/tot_cons_thous_t) %>%
  #Compute total expenditure on staples and non-staples
  mutate(cons_exp_s_ns= sum(cons_thous_t*cons_price)) %>%
  #Compute expenditure per 1000 calorie by staples and non-staples
  mutate(usd_1000cal_s_ns=(cons_exp_s_ns/sum(cal_cap_day*pop_thous*365))*1000) %>%
  #Compute 1000 calories per capita per day
  mutate(cal_cap_day_s_ns_1000= sum(cal_cap_day)/1000) %>%
  #Compute shares
  mutate(share_s_ns = sum(cal_cap_day)/tot_cal_pcap_day) %>%
  ungroup()



#Finally just keep relevant columns.
d.agg_data_s_ns %>% select(iso,year,pop_thous,gdp_pcap_2010usd,cat,cons_price_s_ns,usd_1000cal_s_ns,share_s_ns,cons_exp_s_ns,cal_cap_day_s_ns_1000,cons_thous_t_s_ns) %>% distinct()->d.aggregated

#Check price per tonne for staples, non-staples by iso
if (diagnostics==TRUE){

  VizData<-d.aggregated %>% filter(year>1970) %>% filter(iso=="dnk")
  g<-ggplot(data=VizData,mapping=aes(x=year,y=usd_1000cal_s_ns))+
    geom_line(aes(linetype=cat))+
    ylab("Price per tonne")+
    ggtitle("Price per tonne of staples and non staples")+
    facet_wrap(~iso)

  g
}

d.aggregated <-split(d.aggregated,d.aggregated$cat)

staples_data <- as.data.frame(d.aggregated$Staples) %>%
  select(-cat,-pop_thous,-gdp_pcap_2010usd) %>%
  filter(cons_price_s_ns != 0, usd_1000cal_s_ns != 0 , cons_exp_s_ns !=0 ) %>%
  rename(cons_price_s = cons_price_s_ns, s_usd_p1000cal = usd_1000cal_s_ns,
         s_cal_pcap_day_thous=cal_cap_day_s_ns_1000,cons_exp_s=cons_exp_s_ns,s_cons_thous_t= cons_thous_t_s_ns)

non_staples_data <- as.data.frame(d.aggregated$`Non-Staples`) %>%
  select(-cat) %>%
  filter(cons_price_s_ns != 0, usd_1000cal_s_ns != 0 , cons_exp_s_ns !=0 ) %>%
  rename(cons_price_ns = cons_price_s_ns, ns_usd_p1000cal = usd_1000cal_s_ns,
         ns_cal_pcap_day_thous=cal_cap_day_s_ns_1000,cons_exp_ns=cons_exp_s_ns ,ns_cons_thous_t= cons_thous_t_s_ns)
#join staples data
non_staples_data %>%
  inner_join(staples_data, by=c("iso","year")) %>%
  mutate(gdp_pcap = gdp_pcap_2010usd/1000)->Training_Dataset


write.csv(Training_Dataset,paste0("paper1/Latest_Iteration/Training_Data.csv"))






