import json
def valid_input(ruleset, bot_response):
   if bot_response in ruleset["good"]:
       print "Good Response!"
   else:
       print "What was that response? Get out of here"
ruleset = {"good":{'r', 'p', 's'}, "bad":{'3', '4', '0'}}
