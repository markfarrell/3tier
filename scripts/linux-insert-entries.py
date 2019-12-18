import requests

filename='data/linux-audit-log-sample.log'
route='http://0.0.0.0:3000/insert/linux'

with open(filename, 'rb') as logfile:
    for log in logfile:
        # query = requests.utils.quote(log)
        # url = route + '?entry=' + query
        response = requests.post(route, params={'entry' : log}) 
        print log
        print response
