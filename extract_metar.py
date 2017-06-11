# extract_data.py

def extract_metar( region, YEAR, MONTH, DAY, HOUR, FROM, TO, STNM, STATION, filename):

  # region = "pac"
  # YEAR = "2016"
  # MONTH = "01"
  # DAY = "19"
  # HOUR = "00"
  # FROM = DAY + HOUR
  # TO = DAY + HOUR
  # STNM = "94578"
  # STATION = "YBBN"
  # filename = STNM + "_" + YEAR + "_" + MONTH + "_" + DAY + "_" + HOUR

  #import the library used to query a website
  import urllib2
  #specify the url
  url = "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=" + STATION + "&data=all&year1=" + str(int(YEAR)) + "&month1=" + str(int(MONTH)) + "&day1=" + str(int(DAY)) + "&year2=" + str(int(YEAR)) + "&month2=" + str(int(MONTH)) + "&day2=" + str(int(DAY)) + "&tz=Etc%2FUTC&format=comma&latlon=yes&direct=no&report_type=1&report_type=2"
  #Query the website and return the html to the variable 'page'
  page = urllib2.urlopen(url)
  #import the Beautiful soup functions to parse the data returned from the website
  from bs4 import BeautifulSoup
  #Parse the html in the 'page' variable = variable, and store it in Beautiful Soup format
  soup = BeautifulSoup(page)#import the library used to query a website

  body = soup.find_all("body")
  # print body

  file = open("data/" + STATION + "/raw" + str(filename) + "_metar.txt", "w")
  file.write(body[0].find(text = True)) 
