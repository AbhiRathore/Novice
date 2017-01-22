import re
f = open( "D:\enitity\ids.txt", "r" )
a = []
for line in f:
    a.append(line)
print(a[1])


for i in a:
    url= "http://multicodes.giatamedia.com/webservice/rest/1.0/properties/" + re.sub('[^0-9]',"",i)
    
