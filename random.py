import json

import csv

v='{"city": "Delhi", "addresses": {"address": {"addressLine":["Bhikaiji Cama Place", "Ring Road", "New Delhi 110607", "Ring Road", "New Delhi", "110607", "IN"]}}, "geoCodes": {"geoCode": {"latitude": "28.569035", "longitude": "77.185234"}}, "name": "Hyatt Regency Delhi"}'

employee_parsed = json.loads(v)

emp_data = employee_parsed['addresses']

# open a file for writing

employ_data = open('D:/triposo/Data.csv', 'w')

# create the csv writer object

csvwriter = csv.writer(employ_data)

count = 0

for emp in emp_data:

      if count == 0:

             header = emp.keys()

             csvwriter.writerow(header)

             count += 1

      csvwriter.writerow(emp.values())

employ_data.close()
