#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
from __future__ import print_function
import httplib
import json
import os
import sys
import socket
import urllib


RATINGS = {
  '': '-',
  '1': u'\u2606 (bad. soo bad)',
  '2': u'\u2606\u2606 _(meh)_',
  '3': u'\u2605\u2605\u2605 *(good. i could recommend it)*',
  '+3' : u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**',
  '3+' : u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**',
  '3++': u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**',
  '4'  : u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**'
}

#RATINGS = {
#  '': '-',
#  '1': '1 (bad. soo bad)',
#  '2': '_2 (meh)_',
#  '3': '*3 (good. i could recommend it)*',
#  '+3': '**3++ (excellent. i could always see it again)**',
#  '3+': '**3++ (excellent. i could always see it again)**',
#  '3++': '**3++ (excellent. i could always see it again)**',
#  '4': '**3++ (excellent. i could always see it again)**'
#}

def prepare_title(title):
    words = title.lower().split(' ')
    swords = []
    for w in words:
        if w in ('a', 'an', 'i', 'or', 'as', 'at', 'by', 'and', 'but', 'for', 'not', 'the', 'yet', 'who', 'with'):
            continue
        swords.append(w)
    return ' '.join(swords)    

# http://stackoverflow.com/questions/1966503/does-imdb-provide-an-api
# http://www.imdb.com/xml/find?json=1&nr=1&tt=on&q=lost
def imdbapi_data(title, year=None):
  # first try http://imdbapi.org
  short_title = prepare_title(title)
  params = {
    'type': 'json',
    'q': short_title,
    'plot': 'full',
    'limit': 5
  }
  if year:
    params['year'] = year
  qs = urllib.urlencode(params)
  imdb_data = {}
  conn = None
  try:
    conn = httplib.HTTPConnection("imdbapi.org")
    conn.request("GET", "/?" + qs)
    response = conn.getresponse()
    if response.status == 200:
      data = response.read()
      imdb_data = json.loads(data)
  except ValueError:
    pass
  except socket.error:
    pass
  finally:
    if conn:
      conn.close()
  if imdb_data:
    if isinstance(imdb_data, dict):
      #print("imdbapi: 1 result")
      return imdb_data, 1
    else:
      # print("imdbapi: %s results" % len(imdb_data))
      # print("imdbapi: %s" % imdb_data[0])
      res = None
      max_match = 0
      for d in imdb_data:
        match = match_len(title, d.get('title', ''))
        if year and year == d.get('year', 0):
          match += 1
        # print("imdbapi: %s, match: %s" % (d.get('title', '').encode('utf8'), match))
        if match > max_match:
          res = d
          max_match = match
      if max_match > 0:
        #print("imdb result:", res.get('title').encode('utf8'), "match:", max_match)
        return res, 1
  
  return {}, 0

def match_len(in_title, movie_title):
  intitle_set = set()
  movtitle_set = set()
  for w in in_title.lower().split():
    intitle_set.add(w)
  for w in movie_title.lower().split():
    movtitle_set.add(w)
  return len(movtitle_set.intersection(intitle_set)) 

def omdbapi_data(title, year=None):
  # now let's try www.omdbapi.com
  short_title = prepare_title(title)
  imdb_data = {}
  params = {
    't': short_title,
    'plot': 'full',
    'r': 'json'
  }
  if year:
    params['y'] = year
  qs = urllib.urlencode(params)
  conn = None
  try:
    conn = httplib.HTTPConnection("www.omdbapi.com")
    conn.request("GET", "/?" + qs)
    response = conn.getresponse()
    if response.status == 200:
      data = response.read()
      imdb_data = json.loads(data)
      if imdb_data and imdb_data.has_key('imdbID'):
        imdb_data['imdb_url'] = "http://www.imdb.com/title/%s" % imdb_data['imdbID']
  finally:
    if conn:
      conn.close()

  if match_len(title, imdb_data.get('Title', '')) == 0:
    #os.system("open -a Safari \"http://www.omdbapi.com/?t=%s\"" % urllib.quote_plus(title))
    return {}, 0
  # print("omdb_data: %s" % imdb_data)
  return imdb_data, 1

def rotten_data(title, year=None):
  short_title = prepare_title(title)
  params = {
    'q': short_title, 
    'page_limit': 25, 
    'page': 1, 
    'apikey': '3r2avcbv2fhn6gk2nhtxeke9'  
  }
  qs = urllib.urlencode(params)
  rotten_dt = {}
  conn = None
  try:
    conn = httplib.HTTPConnection("api.rottentomatoes.com")
    conn.request("GET", "/api/public/v1.0/movies.json?" + qs)
    response = conn.getresponse()
    if response.status == 200:
      data = response.read()
      rotten_dt = json.loads(data)
  finally:
    if conn:
      conn.close()

  if rotten_dt:
    # print("rotten_data: %s" % rotten_dt['movies'][0])
    movie = None
    max_match = 0
    for mov in rotten_dt['movies']:
      match = match_len(title, mov.get('title', ''))
      if year and year == mov.get('year', 0):
        match += 1
      if match > max_match:
        movie = mov
        max_match = match

    if max_match > 0:
      return movie, 1

  return {}, 0

def find_actors(d1, d2, d3):
  actors = []
  uniques = {}
  if d1.has_key('actors'):
    for a in d1['actors']:
      if not uniques.has_key(a):
        actors.append(a)
        uniques[a] = True
  if d2.has_key('Actors'):
    for a in d2['Actors'].split(', '):
      if not uniques.has_key(a):
        actors.append(a)
        uniques[a] = True
  if d3.has_key('abridged_cast'):
    for a in [a['name'] for a in d3['abridged_cast']]:
      if not uniques.has_key(a):
        actors.append(a)
        uniques[a] = True
  return actors

def main(title, year=None, my_rating=""):
  imdbapid, r1 = imdbapi_data(title, year)
  omdbapid, r2 = omdbapi_data(title, year)
  rottend, r3 = rotten_data(title, year)
  data = {
    'title': imdbapid.get('title', '') or rottend.get('title', '') or omdbapid.get('Title', '') ,
    'year': year or imdbapid.get('year', '') or rottend.get('year', '') or omdbapid.get('Year'),
    'genre': imdbapid.get('genres', []) or omdbapid.get('Genre', '').split(', '),
    'imdb_url': imdbapid.get('imdb_url', '') or rottend.get('imdb_url', '') or omdbapid.get('imdb_url', ''),
    'rotten_url': rottend.get('links', {}).get('alternate', ''),
    'my_rating': my_rating,
    'imdb_rating': imdbapid.get('rating', 'n/a'),
    'critics_rating': rottend.get('ratings', {}).get('critics_rating', 'n/a'),
    'critics_score': rottend.get('ratings', {}).get('critics_score', 'n/a'),
    'audience_rating': rottend.get('ratings', {}).get('audience_rating', 'n/a'),
    'audience_score': rottend.get('ratings', {}).get('audience_score', 'n/a'),
    'critics_consensus': rottend.get('critics_consensus', 'n/a'),
    'directors': imdbapid.get('directors', []) or omdbapid.get('Director', '').split(', '),
    'actors': find_actors(imdbapid, omdbapid, rottend),
    'plot': imdbapid.get('plot', '') or omdbapid.get('Plot', ''),
    'synopsis': rottend.get('synopsis', ''),
    'poster_imdb': imdbapid.get('poster'),
    'poster_rotten': rottend.get('posters', {}).get('original')
  }

  print_output(data)
#  if r1 == 0:
#    os.system("open -a Safari \"http://www.imdb.com/find?q=%s\"" % urllib.quote_plus(title))
#  if r3 == 0:
#    os.system("open -a Safari \"http://www.rottentomatoes.com/search/?search=%s\"" % urllib.quote_plus(title))

def print_output(data):
  print(u"# Movie: %s (%s) " % (data['title'], data['year']))
  print("\n")
  print("Year :", data['year'], "   ")
  print("Genre:", ', '.join(data['genre']), "   ")
  print("")
  print("## Ratings ")
  print("")
  print("My rating        : %s   " % RATINGS[data.get('my_rating', '')].encode('utf8'))
  print("IMDB rating      : %s/10   " %  data['imdb_rating'])
  print("Tomato rating    : %s   " % data['audience_rating'])
  print("Tomato score     : %s   " % data['audience_score'])
  print("Critics rating   : %s   " % data['critics_rating'])
  print("Critics score    : %s   " % data['critics_score'])
  print("")
  if data['imdb_url'] or data['rotten_url']:
    print("Links:\n")
    if data['imdb_url']:
      print("*   <%s>" % data['imdb_url'])
    if data['rotten_url']:
      print("*   <%s>" % data['rotten_url'])
  print("")
  print(u"Critics consensus:\n\n> %s   " % data['critics_consensus'].encode('utf8'))
  print("")
  print("* * * * * * * * * * *")
  print("")
  print("Directors: ", ', '.join(data['directors']).encode('utf8'))
  print("")
  print("Actors :")
  print("")
  for d in data['actors']:
    print("*   " + d.encode('utf8'))
  print("")
  print("* * * * * * * * * *")
  print("")
  print("### Plot ")
  print("")
  print(data['plot'].encode('utf8'))
  print("")
  print(data['synopsis'].encode('utf8'))
  if data['poster_imdb']:
    print("")
    print("![Poster imdb %s](%s)" % (data['title'], data['poster_imdb']))
  if data['poster_rotten']:
    print("")
    print("![Poster %s](%s)" % (data['title'], data['poster_rotten']))
  print("")
  tags = "#movie:%s" % data['year']
  for g in data['genre']:
    tags += " #movie:%s" % g.lower().replace(' ', '-')
  tags += " #movie"
  print(tags)

if __name__ == '__main__':
  # print("Args: ", sys.argv[1:])
  year = None
  title_words = []
  my_rating = ""
  idx = 1
  while idx < len(sys.argv):
    if sys.argv[idx].startswith('--year='):
      try:
        year = int(sys.argv[idx][7:])
        # idx += 1
      except ValueError:
        pass
    elif sys.argv[idx].startswith('--rating='):
      my_rating = sys.argv[idx][9:]
    else:
      title_words.append(sys.argv[idx])
    idx += 1
  title = ' '.join(title_words)
  main(title, year, my_rating)



# Sample imdbapi.org result:
#
# [
#   {"runtime": ["108 min"], "rating": 6.5, "genres": ["Adventure", "Family", "Fantasy"], "rated": "PG", "language": ["English"], "title": "Alice in Wonderland", "filming_locations": "Antony House, Torpoint, Cornwall, England, UK", "imdb_url": "http://www.imdb.com/title/tt1014759/", "writers": ["Linda Woolverton", "Lewis Carroll"], "imdb_id": "tt1014759", "directors": ["Tim Burton"], "rating_count": 166594, "actors": ["Johnny Depp", "Mia Wasikowska", "Helena Bonham Carter", "Anne Hathaway", "Crispin Glover", "Matt Lucas", "Michael Sheen", "Stephen Fry", "Alan Rickman", "Barbara Windsor", "Paul Whitehouse", "Timothy Spall", "Marton Csokas", "Tim Pigott-Smith", "John Surman"], "plot_simple": "19-year-old Alice returns to the magical world from her childhood adventure, where she reunites with her old friends and learns of her true destiny: to end the Red Queen's reign of terror.", "year": 2010, "country": ["USA"], "type": "M", "release_date": 20100326, "also_known_as": ["Alice in Wonderland: An IMAX 3D Experience"]}, 
#   {"runtime": ["75 min"], "rating": 7.4, "genres": ["Animation", "Adventure", "Family", "Fantasy", "Musical"], "rated": "G", "language": ["English"], "title": "Alice in Wonderland", "filming_locations": "Walt Disney Studios, 500 South Buena Vista Street, Burbank, California, USA", "imdb_url": "http://www.imdb.com/title/tt0043274/", "writers": ["Lewis Carroll", "Winston Hibler"], "imdb_id": "tt0043274", "directors": ["Clyde Geronimi", "Wilfred Jackson", "and 1 more credit"], "rating_count": 49240, "actors": ["Kathryn Beaumont", "Ed Wynn", "Richard Haydn", "Sterling Holloway", "Jerry Colonna", "Verna Felton", "J. Pat O'Malley", "Bill Thompson", "Heather Angel", "Joseph Kearns", "Larry Grey", "Queenie Leonard", "Dink Trout", "Doris Lloyd", "James MacDonald"], "plot_simple": "Alice stumbles into the world of Wonderland. Will she get home? Not if the Queen of Hearts has her way.", "year": 1951, "country": ["USA"], "type": "M", "release_date": 19510728, "also_known_as": ["Alicia en el pa\u00eds de las maravillas"]}, 
#   {"runtime": ["USA: 150 min", "65 min (2 episodes)", "USA: 129 min (VHS version)", "Finland: 131 min (TV) (2 parts)"], "rating": 6.0, "genres": ["Adventure", "Comedy", "Family", "Fantasy"], "rated": "PG", "language": ["English"], "title": "Alice in Wonderland", "filming_locations": "Burnham Beeches, Buckinghamshire, England, UK", "imdb_url": "http://www.imdb.com/title/tt0164993/", "writers": ["Lewis Carroll", "Peter Barnes"], "imdb_id": "tt0164993", "directors": ["Nick Willing"], "rating_count": 4406, "actors": ["Robbie Coltrane", "Whoopi Goldberg", "Ben Kingsley", "Christopher Lloyd", "Pete Postlethwaite", "Miranda Richardson", "Martin Short", "Peter Ustinov", "George Wendt", "Gene Wilder", "Tina Majorino", "Ken Dodd", "Jason Flemyng", "Sheila Hancock", "Simon Russell Beale"], "plot_simple": "The wizards behind \"Merlin\" and \"The Odyssey\" combine Lewis Carroll's \"Alice in Wonderland\" and \"Through the Looking Glass\" into a three-hour special that just gets curiouser and curiouser.", "year": 1999, "country": ["UK", "USA", "Germany"], "type": "TV", "release_date": 19990228, "also_known_as": ["Alice im Wunderland"]}
# ]
#
# Sample omdbapi.com:
#
# {"Title":"Lost Girl","Year":"2010","Rated":"18","Released":"12 Sep 2010","Runtime":"1 h","Genre":"Crime, Fantasy, Horror","Director":"N/A","Writer":"M.A. Lovretta","Actors":"Anna Silk, Kris Holden-Ried, Ksenia Solo, Richard Howland","Plot":"Lost Girl focuses on the gorgeous and charismatic Bo, a supernatural being called a succubus who feeds on the energy of humans...","Poster":"http://ia.media-imdb.com/images/M/MV5BMTY4NzA1MDAyMF5BMl5BanBnXkFtZTcwMzQ4MTkxNA@@._V1_SX300.jpg","imdbRating":"7.7","imdbVotes":"7,203","imdbID":"tt1429449","Response":"True"}
#
