#this is a rock paper scissors class

import sys
class RPS(object):
	'''
	This is the rock paper scissors class that will control the game

	data consists of user options, game history, and score

    in history
    k = king win
    c = challenger win
    t = tie
	'''

	def __init__(self, player1, player2):
		'''
		This is the function instance method
		'''
		self.player1 = player1
		self.player2 = player2

		self.data = {
			'options':[1,2,3],
			'history':"",
			'score':0
		}
		#player1_response = get_response()
		#player2_response = get_response()

	def start_match(self, games):
		'''
		This function is the meat and potatoes of the entire game
		-get player choice
		-validate player choice
		-if 3 bad choices: lose
		-play game, record score

		'''
		result = 0
		for i in xrange(games):
			p1_response = self.valid_input(self.player1, 3)
			if p1_response == "Bad input":#automatic loss if bad input
				self.data['score'] -= 1
				continue
			p2_response = self.valid_input(self.player2, 3)
			if p2_response == "Bad input":#automatic loss if bad input
				self.data['score'] += 1
				continue

			result = RPS.play_rps(p1_response, p2_response)
			self.data['score'] += result
			self.log_history(result)

	  	if self.data['score'] > 0:
	  		return self.player1
	  	elif self.data['score'] < 0:
	  		return self.player2
	  	else:
	  		return self.start_match(games)

	@staticmethod
	def play_rps(player1_choice, player2_choice):
		'''
		This is a function that plays rock paper scissors
		-1 = loss
		0 = tie
		1 = win

		diff = player1_choice - player2_choice

		if diff == 0:return 0
		elif diff == 2 or diff == -1:return -1
		else:return 1
		'''
		c = [player1_choice, player2_choice]
		if c == [1,1] or c == [2,2] or c == [3,3]:
		    return 0
		elif c == [1,2] or c == [2,3] or c == [3,1]:
		    return -1
		else:
		    return 1



	def clear_data(self, a=0):
		'''
		Clears history from object
		'''
		try:
		    self.data['history'] = ""
		    self.data['score'] = 0
		except NameError:
		    return False

	def new_players(self, p1, p2):
		'''
		sets new players for new game
		'''
		try:
			self.player1 = p1
			self.player2 = p2
			return True
		except NameError:
			return False

	def get_player_response(self, player):
		'''
		gets the response from a player

		import player
		return player.response(self.data)
		'''
		print "hello"
		a =  sys.stdin.readlines()
		print "whatever"
                # import importlib
                # f = importlib.import_module("AutoArena.player_programs."+player[:-3])

                # return f.play_turn(self.data)

	def is_valid_choice(self, choice):
	    '''
	    return true if good choice
	    returns false if bad
	    '''
	    return choice in self.data['options']

	def valid_input(self, player, chances):
	    '''
	    given the player and the number of chancs they get to make a move
	    functino will return a valid choice or the string "I give bad input"
	    to show that that program is bad and should feel bad
	    '''
	    for chance in range(chances):
	        player_choice = self.get_player_response(player)
	        if self.is_valid_choice(player_choice):
	        	return player_choice
	    return "Bad input"

	def log_history(self, result):
	    '''
	    logs history to 'data', descriptors shows in class docs
	    '''
	    if result == 1:self.data['history'] += 'k'
	    elif result == 0:self.data['history'] += 't'
	    elif result == -1:self.data['history'] += 'c'
	    else:
	        return False
	    return True


if __name__ == "__main__":
	g = RPS('a','b')

	print g.start_match(3)

	print g.data

