# extract_data.py

def extract_data( region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename):

  # region = "pac"
  # YEAR = "2016"
  # MONTH = "01"
  # DAY = "01"
  # HOUR = "00"
  # FROM = DAY + HOUR
  # TO = DAY + HOUR
  # STNM = "94578"
  # filename = STNM + "_" + YEAR + "_" + MONTH + "_" + DAY + "_" + HOUR

  #import the library used to query a website
  import urllib2
  #specify the url
  url = "http://weather.uwyo.edu/cgi-bin/sounding?region=" + region + "&TYPE=TEXT%3ALIST&YEAR=" + YEAR + "&MONTH=" + MONTH + "&FROM=" + FROM + "&TO=" + TO + "&STNM=" + STNM
  #Query the website and return the html to the variable 'page'
  page = urllib2.urlopen(url)
  #import the Beautiful soup functions to parse the data returned from the website
  from bs4 import BeautifulSoup
  #Parse the html in the 'page' variable = variable, and store it in Beautiful Soup format
  soup = BeautifulSoup(page)#import the library used to query a website

  h2 = soup.find_all("h2")

  pre = soup.find_all("pre")

  if h2 == []:
    print 'Empty url!'
    print url

  if h2 != []:
    file = open("data/" + STATION + "/raw" + str(filename) + "_data.txt", "w")
    file.write(pre[0].find(text = True)) 
