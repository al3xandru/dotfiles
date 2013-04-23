try:
  import readline
except ImportError:
  print "Module readline unavailable"
else:
  import rlcompleter
  readline.parse_and_bind("tab: complete")
