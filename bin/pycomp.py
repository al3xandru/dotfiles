codecomp = False
try:
    import readline
except ImportError:
    print "No readline"
else:
    import rlcompleter
    readline.parse_and_bind("tab: complete")
    codecomp = True
