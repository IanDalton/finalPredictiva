#Get the HTML of the following link https://www.airbnb.com/rooms/175005

import requests
from bs4 import BeautifulSoup

url = 'https://www.airbnb.com/rooms/175005'
r = requests.get(url)
html_doc = r.text
soup = BeautifulSoup(html_doc)
#save html
with open('airbnb.html', 'wb') as f:
    f.write(html_doc.encode('utf-8'))