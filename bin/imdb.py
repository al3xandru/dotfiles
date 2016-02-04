#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# vim: ts=2 shiftwidth=2:
from __future__ import print_function

import argparse
import httplib
import json
import os
import sys
import socket
import subprocess
import tempfile
import urllib
import urllib2
import urlparse


RATINGS = {
  '': '-',
  '1': u'\u2606 (bad. soo bad)',
  '2': u'\u2606\u2606 _(meh)_',
  '3': u'\u2605\u2605\u2605 *(good. i could recommend it)*',
  '+3': u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**',
  '3+': u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**',
  '3++': u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**',
  '4': u'\u2605\u2605\u2605\u2605 **(excellent. i could always see it again)**'
}

_API = ('http://api.trakt.tv/movie/seen/8861c688930852cfbff18e51f195acb0',
        'alpo',
        '808a49948b398609e61e62917bd235a8c8139866')

# this is required for v2
DAYONE_JOURNAL = '~/Library/Group Containers/5U8NS4GX82.dayoneapp2/Data/Auto Import/Default Journal.dayone'


def prepare_title(title):
  words = title.lower().split(' ')
  swords = []
  for w in words:
    if w in ('a', 'an', 'i', 'or', 'as', 'at', 'by', 'and', 'but', 'for', 'not', 'the', 'yet', 'with'):
      continue
    swords.append(w)
  return ' '.join(swords)


# http://stackoverflow.com/questions/1966503/does-imdb-provide-an-api
# http://www.imdb.com/xml/find?json=1&nr=1&tt=on&q=lost
def imdbapi_data(title, year=None):
  """ http://imdbapi.org

  This website has been down for a while.
  """
  return {}, 0
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
      if isinstance(imdb_data, dict):
        return imdb_data, 1
      else:
        res = None
        max_match = 0
        for d in imdb_data:
          match = match_len(title, d.get('title', ''))
          if year and year == d.get('year', 0):
            match += 1
          if match > max_match:
            res = d
            max_match = match
        if max_match > 0:
          return res, 1
  except ValueError:
    pass
  except socket.error:
    pass
  finally:
    if conn:
      conn.close()

  return {}, 0


def themoviedb_data(title, year=None):
  short_title = prepare_title(title)
  params = {
    'api_key': '99026a194a4dbafd98c3070108bc93db',
    'query': short_title
  }
  if year:
    params['year'] = year

  qs = urllib.urlencode(params)
  imdb_data = {}

  conn = None
  try:
    conn = httplib.HTTPConnection('api.themoviedb.org')
    conn.request('GET', '/3/search/movie?' + qs)
    response = conn.getresponse()
    if response.status != 200:
      return {}, 0

    data = response.read()
    jdata = json.loads(data)
    if ('results' in jdata) and len(jdata['results']) == 1:
      movie_id = jdata['results'][0]['id']
      imdb_data['title'] = jdata['results'][0]['title']
    else:
      movie_id = None
      max_match = 0
      for r in jdata['results']:
        match = match_len(title, r['title'])
        if year and r['release_date'][:4] == str(year):
          match += 1
        if match > max_match:
          movie_id = r['id']
          max_match = match

    if not movie_id:
      return {}, 0

    conn = httplib.HTTPConnection('api.themoviedb.org')
    conn.request('GET', "/3/movie/%s?api_key=%s" % (movie_id, params['api_key']))
    response = conn.getresponse()
    if response.status != 200:
      return {}, 0
    data = json.loads(response.read())
    imdb_data['year'] = data['release_date'][:4]
    imdb_data['imdb_url'] = "http://www.imdb.com/title/%s" % data['imdb_id']
    imdb_data['genres'] = [t['name'] for t in data['genres']]
    imdb_data['plot'] = data['overview']

    conn = httplib.HTTPConnection('api.themoviedb.org')
    conn.request('GET', "/3/movie/%s/credits?api_key=%s" % (movie_id, params['api_key']))
    response = conn.getresponse()
    if response.status != 200:
      return imdb_data, 1

    data = json.loads(response.read())
    imdb_data['actors'] = [t['name'] for t in data['cast']]
    imdb_data['directors'] = [t['name'] for t in data['crew'] if t['job'].lower() == 'director']

    return imdb_data, 1

  except ValueError:
    pass
  except socket.error:
    pass
  finally:
    if conn:
      conn.close()

  return {}, 0


def omdbapi_data(title, year=None):
  """ Another service to try is www.omdbapi.com """
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
      if imdb_data and 'imdbID' in imdb_data:
        imdb_data['imdb_url'] = "http://www.imdb.com/title/%s" % imdb_data['imdbID']
  finally:
    if conn:
      conn.close()

  if match_len(title, imdb_data.get('Title', '')) == 0:
    return {}, 0

  return imdb_data, 1


def rotten_data(title, year=None):
  """ Try RottenTomatoes service """
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
  if 'actors' in d1:
    for a in d1['actors']:
      if not a in uniques:
        actors.append(a)
        uniques[a] = True
  if 'Actors' in d2:
    for a in d2['Actors'].split(', '):
      if not a in uniques:
        actors.append(a)
        uniques[a] = True
  if 'abridged_cast' in d3:
    for a in [a['name'] for a in d3['abridged_cast']]:
      if not a in uniques:
        actors.append(a)
        uniques[a] = True
  return actors


def match_len(in_title, movie_title):
  intitle_set = set()
  movtitle_set = set()
  for w in in_title.lower().split():
    intitle_set.add(w)
  for w in movie_title.lower().split():
    movtitle_set.add(w)
  return len(movtitle_set.intersection(intitle_set))


def main(title, opts):
  #print("Trying: imdbapi.org")
  #imdbapid, r1 = imdbapi_data(title, opts.year)
  print("Trying: themoviedb.org")
  imdbapid, r1 = themoviedb_data(title, opts.year)
  print("Trying: www.omdbapi.com")
  omdbapid, r2 = omdbapi_data(title, opts.year)
  print("Trying: rottentomatoes.com")
  rottend, r3 = rotten_data(title, opts.year)

  data = {
    'title': imdbapid.get('title', '') or rottend.get('title', '') or omdbapid.get('Title', '') ,
    'year': opts.year or imdbapid.get('year', '') or rottend.get('year', '') or omdbapid.get('Year'),
    'genre': imdbapid.get('genres', []) or omdbapid.get('Genre', '').split(', '),
    'imdb_url': imdbapid.get('imdb_url', '') or rottend.get('imdb_url', '') or omdbapid.get('imdb_url', ''),
    'rotten_url': rottend.get('links', {}).get('alternate', ''),
    'my_rating': opts.rating,
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

  generate_output(data, opts.dayone)

  # track(data, opts)

  # generate search links if needed
  print("")
  print("http://www.imdb.com/find?q=%s" % urllib.quote_plus(title))
  print("http://www.rottentomatoes.com/search/?search=%s" % urllib.quote_plus(title))
  print("http://trakt.tv/search?query=%s" % urllib.quote_plus(title))


def track(data, opts):
  """TODO: Docstring for track.

  :param data:
  :param opts:
  :returns:

  """
  if not opts.track:
    return

  imdb_id = get_imdb_id(data, opts.imdb)

  if not imdb_id:
    print('No IMDB id; cannot post to trakt.tv')
    return

  json_data = {'username': _API[1], 'password': _API[2],
               'movies': [{'imdb_id': imdb_id, 'title': data['title'], 'year': int(data['year'])}]}
  req = urllib2.Request(_API[0])
  req.add_header('Content-Type', 'application/json')
  res = urllib2.urlopen(req, json.dumps(json_data))
  if res.code == 200:
    print('SUCCESS:', res.read())
  else:
    print("NOT SAVED (%s): %s" % (res.code, res.read()))


def get_imdb_id(data, id):
  if not id:
    id = data['imdb_url']
  if not id:
    return None

  if id.startswith('http'):
    _, _, path, _, _, _ = urlparse.urlparse(id)
    id = [t for t in path.split('/') if t][-1]

  return id


def generate_output(data, to_dayone=False):
  print_to(sys.stdout, data)
  if to_dayone:
    tmpf = tempfile.NamedTemporaryFile()

    try:
      print_to(tmpf, data)
      # required for v2 and -j flag doesn't really work
      journal_path_flag = "--journal-file=%s" % os.path.expanduser(DAYONE_JOURNAL)
      cat_cmd = subprocess.Popen(['cat', tmpf.name], stdout=subprocess.PIPE)
      subprocess.check_call(['/usr/local/bin/dayone', journal_path_flag  , 'new', '-'], stdin=cat_cmd.stdout)
      cat_cmd.wait()
    finally:
      tmpf.close()


def print_to(stream, data):
  """ Write data to the stream. """
  stream.write(u"# Movie: %s (%s) " % (data['title'], data['year']))
  stream.write("\n\n")
  stream.write("Year : %s   \n" % data['year'])
  stream.write("Genre: %s   \n" % ', '.join(data['genre']))
  stream.write("Tagline: \n")
  stream.write("\n")

  # Ratings
  stream.write("## Ratings \n\n")
  stream.write("My rating        : %s   \n" % RATINGS[data.get('my_rating', '')].encode('utf8'))
  stream.write("IMDB rating      : %s/10   \n" %  data['imdb_rating'])
  stream.write("Tomato rating    : %s   \n" % data['audience_rating'])
  stream.write("Tomato score     : %s   \n" % data['audience_score'])
  stream.write("Critics rating   : %s   \n" % data['critics_rating'])
  stream.write("Critics score    : %s   \n" % data['critics_score'])
  stream.write("\n")

  # Links
  if data['imdb_url'] or data['rotten_url']:
    stream.write("Links:\n\n")
    if data['imdb_url']:
      stream.write("*   <%s>\n" % data['imdb_url'])
    if data['rotten_url']:
      stream.write("*   <%s>\n" % data['rotten_url'])
  stream.write("\n")

  # Critics
  if data['critics_consensus']:
    stream.write(u"Critics consensus:\n\n> %s   " % data['critics_consensus'].encode('utf8'))
    stream.write("\n")
  stream.write("* * * * * * * * * * *")
  stream.write("\n\n")

  # Director(s) & Actors
  stream.write("Directors: %s\n\n" % ', '.join(data['directors']).encode('utf8'))
  stream.write("\n")
  stream.write("Actors :")
  stream.write("\n\n")
  for d in data['actors']:
    stream.write("*   " + d.encode('utf8') + "\n")
  stream.write("\n")
  stream.write("* * * * * * * * * *")
  stream.write("\n\n")

  # Plot
  stream.write("### Plot")
  stream.write("\n\n")
  stream.write(data['plot'].encode('utf8'))
  stream.write("\n\n")
  stream.write(data['synopsis'].encode('utf8'))
  stream.write("\n\n")

  # Posters
  if data['poster_imdb']:
    stream.write("![Poster imdb %s](%s)\n" % (data['title'], data['poster_imdb']))
  if data['poster_rotten']:
    stream.write("![Poster %s](%s)\n" % (data['title'], data['poster_rotten']))
  stream.write("\n")

  # Tags
  tags = "#movie:%s" % data['year']
  for g in data['genre']:
    tags += " #movie:%s" % g.lower().replace(' ', '-')
  tags += " #movie"
  stream.write(tags)
  stream.write("\n")

  # Flush stream
  stream.flush()


if __name__ == '__main__':
  # print("Args: ", sys.argv[1:])
  parser = argparse.ArgumentParser(description='Movie details')
  parser.add_argument('--dayone', action='store_true', help='Save entry in DayOne')
  parser.add_argument('--track', action='store_true', help='Save entry in trakt.tv')
  parser.add_argument('--imdb', action='store', help='IMDB movie id or url')
  parser.add_argument('-r', '--rating', action='store', choices=['1', '2', '3', '+3', '3+'])
  parser.add_argument('-y', '--year', action='store', type=int)
  parser.add_argument('title', nargs='+')

  opts = parser.parse_args()
  opts.dayone = True  # enable DayOne by default
  title = u' '.join(opts.title)

  main(title, opts)


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

