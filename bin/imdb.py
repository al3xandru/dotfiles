#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# vim: ts=2 shiftwidth=2:
from __future__ import print_function

import argparse
import httplib
import json
import os
import re
import sys
import socket
import subprocess
import StringIO
import tempfile
import time
import urllib
import urllib2
import urlparse
import warnings


DEBUG_HTTP_STATUS = True
DEBUG_HTTP_BODY = False

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

WORDS = ('a', 'an',  'and', 'as', 'at',  'but', 'by', 'for', 'i', 'in', 'not', 'of', 'on','or', 'the', 'yet', 'with')

# this is required for v2
DAYONE_JOURNAL = '~/Library/Group Containers/5U8NS4GX82.dayoneapp2/Data/Auto Import/Default Journal.dayone'


def prepare_title(title):
  words = title.lower().split(' ')
  swords = []
  for w in words:
    if w in WORDS:
      continue
    swords.append(w)
  return ' '.join(swords)


# The following SO entry seem to contain some IMDB direct links
# http://stackoverflow.com/questions/1966503/does-imdb-provide-an-api
# http://www.imdb.com/xml/find?json=1&nr=1&tt=on&q=lost

def imdbapi_data(title, year=None):
  """ http://imdbapi.org

  This website has been down for a while.
  """
  warnings.warn("imdbapi.org has been down for too long", FutureWarning)
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
  themoviedb_api_key = '99026a194a4dbafd98c3070108bc93db'
  short_title = prepare_title(title)
  jdata = httpGet('api.themoviedb.org',
                  httpQuery('/3/search/movie',
                            api_key=themoviedb_api_key,
                            query=short_title,
                            year=year))
  if not jdata:
    return {}, 0

  imdb_data = {}

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
        imdb_data['title'] = r['title']

  if not movie_id:
    return {}, 0

  jdata = httpGet('api.themoviedb.org', "/3/movie/%s?api_key=%s" % (movie_id, themoviedb_api_key))
  if not jdata:
    return {}, 0

  imdb_data['year'] = jdata['release_date'][:4]
  imdb_data['imdb_url'] = "http://www.imdb.com/title/%s" % jdata['imdb_id']
  imdb_data['genres'] = [t['name'] for t in jdata['genres']]
  imdb_data['plot'] = jdata['overview']

  jdata = httpGet('api.themoviedb.org', "/3/movie/%s/credits?api_key=%s" % (movie_id, themoviedb_api_key))
  if jdata:
    imdb_data['actors'] = [t['name'] for t in jdata['cast']]
    imdb_data['directors'] = [t['name'] for t in jdata['crew'] if t['job'].lower() == 'director']

  return imdb_data, 1


def theimdbapi_data(title, year=None):
  """http://www.theimdbapi.org/api"""
  short_title = prepare_title(title)

  jdata = httpGet('www.theimdbapi.org',
                  httpQuery('/api/find/movie', title=short_title, year=year))

  if not jdata:
    return {}, 0

  imdb_data = {}

  if len(jdata) == 1:
    jdata = jdata[0]
  else:
    max_match = 0
    result = None
    for r in jdata:
      match = match_len(title, r['title'])
      if year and r['release_date'][:4] == str(year):
        match += 1
      if match > max_match:
        max_match = match
        result = r
    jdata = result

  if not jdata:
    return {}, 0

  # imdb_data['year'] = jdata['release_date'][:4]
  imdb_data['title'] = jdata['title']
  imdb_data['year'] = jdata['year']
  imdb_data['imdb_url'] = jdata['url']['url']
  imdb_data['genres'] = jdata['genre']
  imdb_data['plot'] = jdata['description']
  imdb_data['actors'] = [t['name'] for t in jdata['cast']]
  imdb_data['directors'] = [jdata['director']]
  imdb_data['rating'] = jdata['rating']

  return imdb_data, 1


def omdbapi_data(title, year=None):
  """ Another service to try is www.omdbapi.com """
  short_title = prepare_title(title)

  imdb_data = httpGet('www.omdbapi.com',
                      httpQuery('/', apikey='b8324934', t=short_title, plot='full', r='json', y=year)) or {}

  if imdb_data is None:
    return {}, 0
  if match_len(title, imdb_data.get('Title', '')) == 0:
    return {}, 0
  if imdb_data and 'imdbID' in imdb_data:
    imdb_data['imdb_url'] = "http://www.imdb.com/title/%s" % imdb_data['imdbID']

  return imdb_data, 1


def rotten_data(title, year=None):
  """ Try RottenTomatoes service """
  short_title = prepare_title(title)

  rotten_dt = httpGet('api.rottentomatoes.com',
                      httpQuery("/api/public/v1.0/movies.json",
                                q=short_title,
                                page_limit=25,
                                page=1,
                                apikey='3r2avcbv2fhn6gk2nhtxeke9'))

  if rotten_dt:
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


def httpGet(server, uri):
  """
  GET a remote URL
  """
  print("GET %s%s" % (server, uri))
  c = None
  try:
    c = httplib.HTTPConnection(server)
    c.request('GET', uri)
    response = c.getresponse()
    if DEBUG_HTTP_STATUS:
      print("    response: ", response.status)
    body = response.read()
    if response.status != 200:
      return None
    else:
      if DEBUG_HTTP_BODY:
        print("    body: ", body)
      return json.loads(body)
  except socket.error:
    return None
  finally:
    if c:
      c.close()


def httpQuery(uri, **kwargs):
  params = kwargs or {}
  return uri + '?' + urllib.urlencode(params)


def get(attr, default=None, *args):
  # first try perfect match
  for d in args:
    if d and attr in d:
      return d[attr]
  # next try case insensitive match
  attr = attr.lower()
  for d in args:
    if not d:
      continue
    for k in d.keys():
      if k.lower() == attr:
        return d[k]

  return default


def find_actors(*args):
  actors = []
  uniques = {}
  for d in args:
    if 'actors' in d:
      for a in d['actors']:
        if not a in uniques:
          actors.append(a)
          uniques[a] = True
      continue
    if 'Actors' in d:
      for a in d['Actors'].split(', '):
        if not a in uniques:
          actors.append(a)
          uniques[a] = True
      continue
    if 'abridged_cast' in d:
      for a in [a['name'] for a in d['abridged_cast']]:
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
  # print("Trying: themoviedb.org")
  imdbapid, r1 = themoviedb_data(title, opts.year)
  # tmdbapid, r3 = theimdbapi_data(title, opts.year)
  tmdbapid = {}
  # print("Trying: www.omdbapi.com")
  omdbapid, r2 = omdbapi_data(title, opts.year)
  # omdbapid = {}
  # Rotten Tomatoes killed the free API
  # print("Trying: rottentomatoes.com")
  # rottend, r3 = rotten_data(title, opts.year)
  rottend = {}

  data = {
    'title': get('title', '', imdbapid, tmdbapid, rottend, omdbapid),
    'year': opts.year or get('year', '', imdbapid, tmdbapid, omdbapid),
    'genre': get('genres', [], imdbapid, tmdbapid) or omdbapid.get('Genre', '').split(', '),
    'imdb_url': get('imdb_url', '', imdbapid, tmdbapid, omdbapid),
    'my_rating': opts.rating,
    'imdb_rating': get('rating', '', imdbapid, tmdbapid),
    'directors': get('directors', [], imdbapid, tmdbapid) or omdbapid.get('Director', '').split(', '),
    'actors': find_actors(imdbapid, tmdbapid, omdbapid, rottend),
    'plot': get('plot', '', imdbapid, tmdbapid, omdbapid),
    'poster_imdb': imdbapid.get('poster'),
    'audience_rating': 'Upright Spilled',
    'audience_score' : '0',
    'critics_rating' : 'Fresh Rotten',
    'critics_score'  : '0',
    'critics_consensus': '',
    'synopsis': '',
  }
  if rottend:
    data['rotten_url'] = rottend.get('links', {}).get('alternate', '')
    data['critics_rating'] = rottend.get('ratings', {}).get('critics_rating', 'n/a')
    data['critics_score'] = rottend.get('ratings', {}).get('critics_score', 'n/a')
    data['audience_rating'] = rottend.get('ratings', {}).get('audience_rating', 'n/a')
    data['audience_score'] = rottend.get('ratings', {}).get('audience_score', 'n/a')
    data['critics_consensus'] = rottend.get('critics_consensus', '')
    data['poster_rotten'] = rottend.get('posters', {}).get('original')
    data['synopsis'] = rottend.get('synopsis', '')
  else:
    rotten_slug = data['title'].lower()
    rotten_slug = re.sub('\s+', '_', rotten_slug)
    rotten_slug = re.sub('\W+', '', rotten_slug)
    data['rotten_url'] = "http://www.rottentomatoes.com/m/%s" % rotten_slug

  generate_output(data, opts)

  # track(data, opts)

  # generate search links if needed
  print("")
  print(data['imdb_url'])
  print(data['rotten_url'])
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


def title_to_filename(title):
  title = title.replace("'", '')
  title = title.replace('&', '')
  title = title.replace(':', '')
  title = title.replace('/', '_')
  words = title.lower().split(' ')
  swords = []
  for w in words:
    if not w or w in WORDS:
      continue
    swords.append(w)
  return '_'.join(swords)


def generate_output(data, opts):
  print_to(sys.stdout, data)
  if opts.output == 'j':
    filename = "m-%s-%s-%s.md" % (time.strftime("%Y%m%d"), data['year'], title_to_filename(data['title']))
    filepath = os.path.join(os.path.expanduser("~"), "Dropbox",  "Dox", "mydox", "myjrnl", "m", filename)
    with open(filepath, "w+") as fout:
      print_to(fout, data)
    print("print to file (path in clipboard):", filepath)
    echo_cmd = subprocess.Popen(['echo', filepath], stdout=subprocess.PIPE)
    subprocess.check_call(['pbcopy'], stdin=echo_cmd.stdout)
    echo_cmd.wait()

  if opts.output == 'd':
    print("print to DayOne")
    tmpf = tempfile.NamedTemporaryFile()

    try:
      print_to(tmpf, data)
      # required for v2 and -j flag doesn't really work
      cat_cmd = subprocess.Popen(['cat', tmpf.name], stdout=subprocess.PIPE)
      if os.path.isfile('/usr/local/bin/dayone2'):
        subprocess.check_call(['/usr/local/bin/dayone2', '-j', 'Movies 🎥'  , 'new'], stdin=cat_cmd.stdout)
      else:
        journal_path_flag = "--journal-file=%s" % os.path.expanduser(DAYONE_JOURNAL)
        subprocess.check_call(['/usr/local/bin/dayone', journal_path_flag  , 'new', '-'], stdin=cat_cmd.stdout)
      cat_cmd.wait()
    finally:
      tmpf.close()
  if opts.output == 'b':
    print("Print to Bear")
    output = StringIO.StringIO()
    try:
      print_to(output, data, encode=False)
      bear_uri = httpQuery('bear://x-callback-url/create',
                          text=output.getvalue().encode('utf8'))
      print(bear_uri)
    finally:
      output.close()


def print_to(stream, data, encode=True):
  """ Write data to the stream. """
  if data['imdb_url']:
    header =  u"# 🎥 Movie: [%s (%s)](%s) " % (data['title'], data['year'], data['imdb_url'])
  else:
    header = u"# 🎥 Movie: %s (%s) " % (data['title'], data['year'])
  if encode:
    stream.write(header.encode('utf8'))
  else:
    stream.write(header)
  stream.write("\n\n")
  if encode:
    stream.write("My rating: %s   \n" % RATINGS[data.get('my_rating', '')].encode('utf8'))
  else:
    stream.write("My rating: %s   \n" % RATINGS[data.get('my_rating', '')])
  stream.write("IMDB     : %s(10)   \n" %  data['imdb_rating'])
  stream.write("Metascore:    \n")
  stream.write("Tomato   : %s %s   \n" % (data['audience_score'], data['audience_rating']))
  stream.write("Critics  : %s %s   \n" % (data['critics_score'], data['critics_rating']))

  stream.write("\n")
  if data['imdb_url']:
    stream.write("IMDb link  : <%s>   \n" % data['imdb_url'])
  if data['rotten_url']:
    if data['rotten_url'].startswith('http'):
      stream.write("Tomato link: <%s>   \n" % data['rotten_url'])
    else:
      stream.write("Tomato link: <http:%s>   \n" % data['rotten_url'])
  stream.write("Year     : %s   \n" % data['year'])
  stream.write("Genre    : %s   \n" % ', '.join(data['genre']))
  stream.write("Tagline  : \n")
  stream.write("\n")


  # Critics
  if data['critics_consensus']:
    if encode:
      stream.write(u"Critics consensus:\n\n> %s   " % data['critics_consensus'].encode('utf8'))
    else:
      stream.write(u"Critics consensus:\n\n> %s   " % data['critics_consensus'])
    stream.write("\n")
  stream.write("* * * * * * * * * * *")
  stream.write("\n\n")

  # Plot
  stream.write("## Plot")
  stream.write("\n\n")
  if encode:
    stream.write(data['plot'].encode('utf8'))
  else:
    stream.write(data['plot'])
  stream.write("\n\n")
  if encode:
    stream.write(data['synopsis'].encode('utf8'))
  else:
    stream.write(data['synopsis'])
  stream.write("\n\n")

  # Director(s) & Actors
  stream.write("## Cast\n\n")
  if encode:
    stream.write("Directors: %s\n\n" % ', '.join(data['directors']).encode('utf8'))
  else:
    stream.write("Directors: %s\n\n" % ', '.join(data['directors']))
  stream.write("\n")
  # stream.write("Actors :")
  # stream.write("\n\n")
  for d in data['actors']:
    if encode:
      stream.write("*   " + d.encode('utf8') + "\n")
    else:
      stream.write("*   " + d + "\n")
  stream.write("\n")
  stream.write("* * * * * * * * * *")
  stream.write("\n\n")


  # Posters
  # if data['poster_imdb']:
  #   stream.write("![Poster imdb %s](%s)\n" % (data['title'], data['poster_imdb']))
  # if data['poster_rotten']:
  #   stream.write("![Poster %s](%s)\n" % (data['title'], data['poster_rotten']))
  # stream.write("\n")


  # Tags
  tags = "t#movie:%s" % data['year']
  for g in data['genre']:
    tags += " t#movie:%s" % g.lower().replace(' ', '-')
  tags += " t#movie"
  stream.write(tags)
  stream.write("\n")

  # Flush stream
  stream.flush()


if __name__ == '__main__':
  # print("Args: ", sys.argv[1:])
  parser = argparse.ArgumentParser(description='Movie details')
  # parser.add_argument('--dayone', action='store_true', help='Save entry in DayOne')
  # parser.add_argument('--bear', action='store_true', help='Save entry in Bear')
  # parser.add_argument('--track', action='store_true', help='Save entry in trakt.tv')
  # parser.add_argument('--imdb', action='store', help='IMDB movie id or url')
  parser.add_argument('-o', '--output', action='store', choices=['j', 'd'], default='j')
  parser.add_argument('-r', '--rating', action='store', choices=['1', '2', '3', '+3', '3+'])
  parser.add_argument('-y', '--year', action='store', type=int)
  parser.add_argument('title', nargs='+')

  opts = parser.parse_args()
  # opts.dayone = True    # enable DayOne by default
  # opts.bear = False      # enable Bear by default
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

