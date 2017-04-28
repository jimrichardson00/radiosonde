# extract_info.py

def extract_info( region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename):

  # print YEAR
  # print MONTH
  # print DAY

  # YEAR = "2010"
  # MONTH = "11"
  # DAY = "31"

  #import the library used to query a website
  import urllib2
  #specify the url
  url = "http://weather.uwyo.edu/cgi-bin/sounding?region=" + region + "&TYPE=TEXT%3ALIST&YEAR=" + YEAR + "&MONTH=" + MONTH + "&FROM=" + FROM + "&TO=" + TO + "&STNM=" + STNM
  # print url
  #Query the website and return the html to the variable 'page'
  page = urllib2.urlopen(url)
  #import the Beautiful soup functions to parse the data returned from the website
  from bs4 import BeautifulSoup
  #Parse the html in the 'page' variable = variable, and store it in Beautiful Soup format
  soup = BeautifulSoup(page)#import the library used to query a website

  h2 = soup.find_all("h2")
  # print h2

  pre = soup.find_all("pre")
  # print pre[1]

  # print h2 != []

  if h2 != []:
    file = open("raw" + str(filename) + "_info.txt", "w")
    file.write(pre[1].find(text = True)) 
