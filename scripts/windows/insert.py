import csv
import requests
import sys

route=sys.argv[1]
filename=sys.argv[2]

with open(filename, 'rb') as csvfile:
    reader = csv.reader(csvfile, quotechar='"')
    for row in reader:
        query=','.join(row)
        print query
        response = requests.post(route, params={'entry' : query}) 
        print response
