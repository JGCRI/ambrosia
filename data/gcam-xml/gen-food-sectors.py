#!/usr/bin/env python

### Generate the XML inputs for the new food demand sectors.  This script parses
### the original demand_input.xml file, makes changes as required, and writes a
### new file called food_demand_input.xml.  This new file, along with
### delete-old-food-demand.xml, is intended to be used as an add-on to the base
### GCAM configuration (which includes demand_input.xml).  Therefore, this file
### mentions only the sectors that will be changed from their base
### configurations.  Unchanged sectors are not mentioned.  Sectors that are
### deleted entirely are deleted in delete-old-food-demand.xml.

import xml.etree.ElementTree as ET
from sys import stdout

### some constants
constants = {'supply-component-tag': 'supply-component',
             'demand-sys-tag':       'demand-system',
             'food-demand-sys-tag':  'food-demand-system',
             'param-tag':            'param',
             'rgn-bias-staple-param': 'rgn-bias-staple',
             'rgn-bias-nonstaple-param': 'rgn-bias-nonstaple'}

params_ordered = ['As', 'An', 'g-ss', 'g-nn', 'g-cross', 'nu', 'lambda', 'kappa', 'pm']
param_vals = {
    'As' : 1.2818,
    'An' : 1.13681,
    'g-ss' : -0.186635,
    'g-nn' : -0.334902,
    'g-cross' : 0.210314,
    'nu' : 0.979294,
    'lambda' : 0.0955194,
    'kappa' : 2.765275,
    'pm' : 5.05801 }

region_bias_staple = {
    'Africa_Eastern' : 0.759,
    'Africa_Northern' : 1.523,
    'Africa_Southern' : 0.968,
    'Africa_Western' : 1.094,
    'Argentina' : 0.932,
    'Australia_NZ' : 0.754,
    'Brazil' : 0.875,
    'Canada' : 0.870,
    'Central America and Caribbean' : 0.814,
    'Central Asia' : 1.025,
    'China' : 1.240,
    'Colombia' : 0.862,
    'EU-12' : 1.106,
    'EU-15' : 0.916,
    'Europe_Eastern' : 1.200,
    'European Free Trade Association' : 0.870,
    'HongKong_Macau' : 0.959,
    'India' : 0.984,
    'Indonesia' : 1.309,
    'Japan' : 1.099,
    'Mexico' : 1.151,
    'Middle East' : 1.337,
    'Pakistan' : 0.869,
    'Russia' : 1.212,
    'South Africa' : 1.309,
    'South America_Northern' : 0.836,
    'South America_Southern' : 0.938,
    'South Asia' : 1.030,
    'South Korea' : 1.325,
    'Southeast Asia' : 1.022,
    'Taiwan' : 0.852,
    'USA' : 0.901
}

region_bias_nonstaple = {
    'Africa_Eastern' : 1.427,
    'Africa_Northern' : 1.035,
    'Africa_Southern' : 0.735,
    'Africa_Western' : 1.124,
    'Argentina' : 1.222,
    'Australia_NZ' : 1.023,
    'Brazil' : 1.195,
    'Canada' : 1.014,
    'Central America and Caribbean' : 1.113,
    'Central Asia' : 1.026,
    'China' : 0.980,
    'Colombia' : 1.001,
    'EU-12' : 1.086,
    'EU-15' : 1.066,
    'Europe_Eastern' : 1.157,
    'European Free Trade Association' : 0.987,
    'HongKong_Macau' : 0.918,
    'India' : 1.041,
    'Indonesia' : 0.708,
    'Japan' : 0.714,
    'Mexico' : 0.964,
    'Middle East' : 0.733,
    'Pakistan' : 1.269,
    'Russia' : 0.908,
    'South Africa' : 0.724,
    'South America_Northern' : 0.873,
    'South America_Southern' : 0.938,
    'South Asia' : 0.705,
    'South Korea' : 0.696,
    'Southeast Asia' : 0.813,
    'Taiwan' : 0.948,
    'USA' : 1.030
}

def buildxml(filename):
    demand_tree = ET.parse(filename)
    scenario = demand_tree.getroot()
    world = scenario[0]             # world is the only tag under scenario

    ## We want to generate a new xml input starting in which we:
    ##   * Drop global technology database (no need for changes here)
    ##   * Retain all FoodDemand_Crops supply sectors
    ##     ** Change subsector logit exponents to -1
    ##   * Retain all FoodDemand_Meat supply sectors
    ##     ** Change subsector logit exponents to -1
    ##   * Remove all other supply sectors
    ##   * Add to each region a food demand sector.  We will need some values from the
    ##     old final demand sectors for this step
    ##   * Remove any remaining sub-elements of the regions (i.e. anything that is not
    ##     one of the new supply sectors or food final demand that we have created)

    ## These will function as the staple and nonstaple food supply
    ## sectors, respectively.
    keepsectors = ['FoodDemand_Crops', 'FoodDemand_Meat']
    component_names = keepsectors

    ## Drop the global technology database, as it won't have any changes.
    world.remove(world.find('global-technology-database'))

    ## Rename the supply sectors we need to keep; drop the rest
    for sector in world.findall('.//supplysector'):
        name = sector.get('name')
        if name in keepsectors:
            ## This sector needs to have its sectoral logit exponent set to -1
            ## There should be only one match here, so use find.  
            logit = sector.find('./*/logit-exponent')
            logit.text = '-1'
        else:
            ## Not one of the ones we want to keep
            parent = get_parent(world, sector)
            stdout.write('Removing {} {} from {} {}\n'.format(sector.tag, sector.get('name'), parent.tag, parent.get('name')))
            parent.remove(sector)

    ## Now we need to create the new food demand sector in each region.  As we do so
    ## we'll grab the base-service values from the old final demand sectors 
    for rgn in world.findall('region'):
        stdout.write('Creating food demand in {} {}\n'.format(rgn.tag, rgn.get('name')))
        fnldmnd = ET.Element('consumer-final-demand')
        fnldmnd.text = '\n\t\t'
        fnldmnd.tail = '\n\t'
        
        ## add names of staple and nonstaple supply sectors before any other elements.
        for sectorname in component_names:
            supply = ET.Element(constants['supply-component-tag'])
            supply.text = sectorname
            supply.tail = '\n\t\t'
            fnldmnd.append(supply)

        ## now construct the rest of the elements and add them.
        for sectorname in component_names:
            ## Grab the base service values from the old food demand
            ## sectors and add them to the new sector we are creating.
            bsvcs = rgn.findall('./energy-final-demand[@name="{}"]/base-service'.format(sectorname))
            for bsvc in bsvcs:
                stdout.write('\tsetting component = {} for base service year {} \n'.format(sectorname, bsvc.get('year')))
                bsvc.set('component', sectorname) # sector these values pertain to.
                
            fnldmnd.extend(bsvcs)

            # ## We're done with the old final demand sector, so remove
            # ## it from the region
            # fdsec = rgn.find('./energy-final-demand[@name="{}"]'.format(sectorname))
            # stdout.write('Removing {} {} from {} {}\n'.format(fdsec.tag, fdsec.get('name'), rgn.tag, rgn.get('name')))
            # rgn.remove(fdsec)

        ## add demand system
        rname = rgn.get('name')
        fnldmnd.append(mkdmdsys(rname))

        ## add the new final demand sector into the region.
        rgn.append(fnldmnd)

        ## Remove all the energy-final-demand sectors.  We couldn't do this before
        ## now because we needed the base service values from some of them.
        for fdsec in rgn.findall('./energy-final-demand'):
            stdout.write('Removing {} {} from {} {}\n'.format(fdsec.tag, fdsec.get('name'), rgn.tag, rgn.get('name')))
            rgn.remove(fdsec)

    ## return the modified demand tree.
    return demand_tree


def mkdmdsys(rgnname):
    """Create a food demand system node for the consumer final demand sector.

       rgnname:  name of the region 
    """
    
    dmndsys = ET.Element(constants['demand-sys-tag'])
    dmndsys.set('type', constants['food-demand-sys-tag'])
    dmndsys.text = '\n\t\t\t'

    for param in params_ordered: 
        ## note that these parameters have to be given in the canonical order.
        p = ET.Element(constants['param-tag'])
        p.set('name', param)
        p.text = str(param_vals[param])
        p.tail = '\n\t\t\t'
        dmndsys.append(p)

    ## Now create the region specific parameters
    ps = ET.Element(constants['param-tag'])
    ps.set('name', constants['rgn-bias-staple-param'])
    ps.text = str(region_bias_staple.get(rgnname, 1.0)) # default regional bias is 1
    ps.tail = '\n\t\t\t'
    dmndsys.append(ps)
    
    pn = ET.Element(constants['param-tag'])
    pn.set('name', constants['rgn-bias-nonstaple-param'])
    pn.text = str(region_bias_nonstaple.get(rgnname, 1.0))
    ps.tail = '\n\t\t'
    dmndsys.append(pn)

    return dmndsys


def get_parent(root, node):
    """Find the parent of 'node' in a tree starting at 'root'."""
    ## Generate a unique name for an attribute that we will use to
    ## identify this node.  Since str(node) contains the memory
    ## address of the node, we can use that. (But we have to start the
    ## name with a letter.)
    uid = 'x{}'.format(id(node))
    node.set(uid,'')
    parent = root.find('.//*[@{}]/..'.format(uid))
    ## remove the unneeded id
    del node.attrib[uid]
    return parent

if __name__ == "__main__":
    xmlrslt = buildxml('demand_input.xml')
    xmlrslt.write('food-demand-input.xml', 'UTF-8', True)
