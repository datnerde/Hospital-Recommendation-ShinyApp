# -*- coding: utf-8 -*-

import requests
import csv

temps_file = open("Hospital_General_Information.csv", 'r')

for i in range(1):
    temps_file.readline()

data = []
for line in temps_file: 
    line = line.strip()
    fields = line.split(',')
    street = fields[2]
    city = fields[3]
    address = ', '.join([street, city])
    data_row = [address]
    data.append(data_row)
 
GOOGLE_MAPS_API_URL = 'http://maps.googleapis.com/maps/api/geocode/json'

latlng = [['Latitude', 'Longitude']]
for add in data: # need to split the data due to google api limit
    params = {
    'address': add,
    'sensor': 'false'
    }
    # Do the request and get the response data
    req = requests.get(GOOGLE_MAPS_API_URL, params=params)
    res = req.json()

    # Use the first result
    result = res['results']
    if result:
        lat = result[0]['geometry']['location']['lat']
        lng = result[0]['geometry']['location']['lng']
        address = result[0]['formatted_address']
        ll = [lat,lng]
        latlng.append(ll)
    else:
        n = 1
        # if NA, then query at most 3 times
        while n <3:
            req = requests.get(GOOGLE_MAPS_API_URL, params=params)
            res = req.json()
            result = res['results']
            if result:
                lat = result[0]['geometry']['location']['lat']
                lng = result[0]['geometry']['location']['lng']
                address = result[0]['formatted_address']
                ll = [lat,lng]
                latlng.append(ll)
                break
            else:
                n = n + 1
        if not result:
            latlng.append(["NA"])          

with open('latlng.csv', "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    writer.writerows(latlng)