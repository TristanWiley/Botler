def main(data):
	response = data['valid_input'][len(data['history'])%3]
	if response == 0:
		return "r"
	elif response == 1:
		return "p"
	elif response == 2:
		return "s"
	else:
		return "p"