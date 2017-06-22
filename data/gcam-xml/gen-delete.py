#!/usr/bin/env python

from string import format

regions = ["USA", "Africa_Eastern", "Africa_Northern",
           "Africa_Southern", "Africa_Western", "Australia_NZ",
           "Brazil", "Canada", "Central America and Caribbean",
           "Central Asia", "China", "EU-12", "EU-15",
           "Europe_Eastern", "Europe_Non_EU", "European Free Trade
           Association", "India", "Indonesia", "Japan", "Mexico",
           "Middle East", "Pakistan", "Russia", "South Africa", "South
           America_Northern", "South America_Southern", "South Asia",
           "South Korea", "Southeast Asia", "Argentina", "Colombia"]



outfile = open('delete-old-food-demand.xml', 'w')
outfile.write('<?xml version="1.0" encoding="UTF-8"?>\n<scenario>\n\t<world>\n')

for rgn in regions:
    openrgn = '\t\t<region name="{}">\n'.format(rgn)
    outfile.write(openrgn)
    for name in ['FoodDemand_Crops', 'FoodDemand_Meat']:
        for sector in ['supplysector', 'energy-final-demand']:
            delsector = '\t\t\t<{0} name="{1}" delete="1"/>\n'.format(sector, name)
            outfile.write(delsector)
    closergn = '\t\t</region>\n'
    outfile.write(closergn)

outfile.write('\t</world>\n</scenario>\n')
outfile.close()

