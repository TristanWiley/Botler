from multiprocessing import Pool
import os
from os import path, listdir
import itertools
import engine_pipe as engine
import re
import json
import sys

SCRIPT_PATH = 'player_scripts'
DATA_PATH = 'player_data'


class Player:

    def __init__(self, script, filepath, player_id):
        self.executable = compile(script, filepath, 'exec')
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
            current_stats['wins'] = current_stats.get('wins', 0) + 1
            print current_stats
            new_stats = json.dumps(current_stats, sort_keys=True,
                                   indent=4, separators=(',', ': '))
            f.truncate(0)
            f.write(new_stats)


def create_sandbox(path, filename):
    # horrible brittle regexy piece of shit.
    player_id = re.search('(\d*)\.', filename).group(0)[:-1]
    filepath = os.path.join(path, filename)
    with open(filepath, "r") as f:
        return Player(f.read(),filepath, player_id)


def run_sim(player_programs):
    n = 10
    player1 = create_sandbox(SCRIPT_PATH, player_programs[0])
    player2 = create_sandbox(SCRIPT_PATH, player_programs[1])
    cycleit = itertools.cycle([player1, player2])
    # stats_obj = get_player_stats(player1)
    for player in [next(cycleit) for i in range(n)]:
        world_state = engine.get_world_state()
        ret = player.take_turn(world_state)
        print ret
        engine.update(ret)
    player1.update_stats()
    player2.update_stats()


if __name__ == '__main__':
    num_processes = 4
    p = Pool(num_processes)
    files = [f for f in listdir(SCRIPT_PATH) if (
        path.isfile(path.join(SCRIPT_PATH, f)) and f[0] != '.')]
    matches = [m for m in itertools.combinations(files, r=2)]
    print(map(run_sim, matches))  # todo: switch to p.map after debug
