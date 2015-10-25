from multiprocessing import Pool
import os
from os import path, listdir
import itertools
from engine_pipe import Engine
import re
import json
import sys

# todo: get rid of exec, do this with sockets

SCRIPT_PATH = 'player_scripts'
DATA_PATH = 'player_data'

# this is the horrible runtime code version

class Player:

    def __init__(self, script, filepath, player_id):
        try:
            self.executable = compile(script, filepath, 'exec')
        except:
            self.chances = 3
            self.id = player_id
            self.wins = 0
            self.losses = 1
            self.ties = 0
            self.update_stats()
        else:
            self.chances = 3
            self.id = player_id
            self.wins = 0
            self.losses = 0
            self.ties = 0
            self.update_stats()

    def take_turn(self, world_state):
        exec (self.executable, globals())  # oh absolute horror
        # I hope context works the way I'd expect here
        return main(world_state)

    def update_stats(self):
        stats_file_path = os.path.join(DATA_PATH, str(self.id) + '.json')

        with open(stats_file_path, 'a+') as f:
            f.seek(0)
            try:
                text = f.read()
                current_stats = json.loads(text)
            except ValueError:
                print "no stats file for user #", self.id
                print "creating one..."
                current_stats = {}

            # should do something else/more here
            current_stats['wins'] = self.wins
            current_stats['losses'] = self.losses
            current_stats['ties'] = self.ties

            print current_stats
            new_stats = json.dumps(current_stats, sort_keys=True,
                                   indent=4, separators=(',', ': '))
            f.truncate(0)
            f.write(new_stats)

    def win(self):
        self.wins = self.wins + 1
        self.update_stats()

    def lose(self):
        self.losses = self.losses + 1
        self.update_stats()

    def tie(self):
        self.ties = self.ties + 1
        self.update_stats()


def create_sandbox(path, filename):
    # horrible brittle regexy piece of shit.
    filepath = os.path.join(path, filename)
    with open(filepath, "r") as f:
        return Player(f.read(),filepath, filename)


def run_sim(player_programs):

    #game_type = re.search('([a-z]*)', player_programs[0]).group(0)  # ugh
    p1_game_type = player_programs[0].split('-')[0] #get the game by getting the prefix variable
    p2_game_type = player_programs[1].split('-')[0] #get the game by getting the prefix variable
    
    if p1_game_type != p2_game_type:
        return None

    n = 10 #number of games

    player1 = create_sandbox(SCRIPT_PATH, player_programs[0])
    player2 = create_sandbox(SCRIPT_PATH, player_programs[1])
    #creates player object for each challenger

    cycleit = itertools.cycle([player1, player2])
    #cycle object to iterate between p1 and p2

    engine = Engine(p1_game_type)
    #game engine object with game type entered

    # stats_obj = get_player_stats(player1)
    for player in [next(cycleit) for i in range(n)]:
        world_state = engine.getState()
        try:
            ret = player.take_turn(world_state)
            print ret
            engine.update(ret)
        except:
            break
    player1.update_stats()
    player2.update_stats()


if __name__ == '__main__':
    num_processes = 4 #4 total processes after each call
    
    p = Pool(num_processes)

    files = []
    for f in listdir(SCRIPT_PATH):
        if (path.isfile(path.join(SCRIPT_PATH, f)) and f[0] != '.'):
            files.append(f)
    #iterate through the scripts, if the files exists, add it to a general list

    matches = [m for m in itertools.combinations(files, r=2)]
    #generate tuple pairs of files

    print(map(run_sim, matches))  # todo: switch to p.map after debug
    #stdout each match with run_sim function
