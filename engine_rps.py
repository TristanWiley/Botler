#This is the rock paper scissors engine for python stuffs


class engine_rps(Object):
	'''
	This class will contain all the logic for a rock 
	paper scissors game
	'''
	valid_moves = ['r','p','s']
	def __init__(self, game):
		self.game = game
		self.p1_move = -1
		self.p2_move = -1
		self.history = []
	def new_game():
		self.history = []
	def decide_winner(p1_move, p2_move):
		if p1_move not in self.valid_moves:
			self.history.append(2)
			return False, True
		if p2_move not in self.valid_moves:
			self.history.append(1)
			return True, False

		if p1_move == 'r' and p2_move == 's' or
		   p1_move == 's' and p2_move == 'p' or
		   p1_move == 'p' and p2_move == 'r':
		   self.history.append(1)
		   return True, False
		elif p1_move == p2_move:
			
		self.history.append(2)
		return False, True