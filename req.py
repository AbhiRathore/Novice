import requests
import re
from requests import *


f = open( "D:\enitity\ids.txt", "r" )
a = []
for line in f:
    a.append(line)

fwrite = open('output3', 'w')
#c = 0
for i in a:
    url= "http://multicodes.giatamedia.com/webservice/rest/1.0/properties/" + re.sub('[^0-9]',"",i)
    #url = 'http://multicodes.giatamedia.com/webservice/rest/1.0/properties/p'
    username = 'bhaskar|crayondata.com'
    password = 'hkUrJ2oo'
    l=requests.get(url, auth=(username, password))
    #print(l.content)
    fwrite.write(l.content + "\n" + "------------------" + "\n")
    """

    c += 1
    if c == 3:
        break
        """

fwrite.close()
    
    
