


import urllib
import urllib2
username = "bhaskar|crayondata.com"
password = "hkUrJ2oo"
url = 'http://multicodes.giatamedia.com/webservice/rest/1.0/account=bhaskar|crayondata.com&token=hkUrJ2oo/properties/22905'
values = { 'bhaskar|crayondata.com': username,'hkUrJ2oo': password }
data = urllib.urlencode(values)
req = urllib2.Request(url, data)
response = urllib2.urlopen(req)
result = response.read()
print result



import requests
url = 'https://updates.opendns.com/nic/update?hostname='
username = 'username'
password = 'password'
print(requests.get(url, auth=(username, password)).content)
