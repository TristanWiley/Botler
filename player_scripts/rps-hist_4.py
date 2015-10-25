def main(data):
	if data['history'][-1] == "0":
		return "r"
	elif data['history'][-1] == "1":
		return "p"
	elif data['history'][-1] == "2":
		return "s"