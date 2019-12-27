import requests
import sys

route=sys.argv[1]
filename=sys.argv[2]

with open(filename, 'rb') as logfile:
    for log in logfile:
        # query = requests.utils.quote(log)
        # url = route + '?entry=' + query
        response = requests.post(route, params={'entry' : log}) 
        print log
        print response
