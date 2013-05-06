AP_AUTOCOMPLETE=False
import sys
try:
  import readline
except ImportError:
  print "Module readline unavailable"
else:
  import rlcompleter
  class MyCompleter(rlcompleter.Completer):
    def complete(self, text, state):
      if text.lstrip() == '':
        if state == 0:
          return text + '\t'
        else:
          return None
      else:
        return rlcompleter.Completer.complete(self, text, state)

  readline.parse_and_bind("tab: complete")
  if sys.platform == 'darwin':
    readline.parse_and_bind("bind ^I rl_complete")
  readline.set_completer(MyCompleter().complete)
  AP_AUTOCOMPLETE=True

# vim:set tabstop=2 shiftwidth=2 softtabstop=2 :
