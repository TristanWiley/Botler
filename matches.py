from multiprocessing import Pool
import os
import itertools
import engine_pipe as engine
import pickle

SCRIPT_PATH = 'player_scripts'
DATA_PATH = 'player_data'


class Player:

    def __init__(self, script, id):
        self.executable = compile(script)
        self.id = id
        self.wins = 0
        self.losses = 0
        self.ties = 0
        update_stats()

    def take_turn(self, world_state):
        exec self.executable  # oh absolute horror
        # I hope context works the way I'd expect here
        return main(world_state)

    def update_stats(self):
        with open(os.path.join(DATA_PATH, str(id) + '.json')) as f:
            current_stats = json.loads()
            json.dumps({'4': 5, '6': 7}, sort_keys=True,
                       indent=4, separators=(',', ': '))


def create_sandbox(filename):
    with open(filename, "r") as f:
        return Player(f.read())


def run_sim(player_programs):
    n = 10
    player1 = create_sandbox(player_programs[0])
    player2 = create_sandbox(player_programs[1])
    cycleit = itertools.cycle([player1, player2])
    stats_obj = get_player_stats(player1)
    for player in [next(cycleit) for i in range(n)]:
        world_state = engine.get_world_state()
        ret = player.eval(world_state)
        engine.update(ret)
    update_player_stats()


if __name__ == '__main__':
    num_processes = 4
    p = Pool(num_processes)
    files = [f for f in os.listdir(SCRIPT_PATH) if os.isfile(join(SCRIPT_PATH, f))]
    matches = [m for m in itertools.permutations(files, r=2)]
    print(p.map(f, m))
