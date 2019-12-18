import csv
import requests

filename='data/windows-security-log-sample.csv'
route='http://0.0.0.0:3000/insert/windows'

with open(filename, 'rb') as csvfile:
    reader = csv.reader(csvfile, quotechar='"')
    for row in reader:
        query=','.join(row)
        print query
        response = requests.post(route, params={'entry' : query}) 
        print response
