# links the python stuff to the haskell stuff
import subprocess
import sys
from subprocess import PIPE, Popen
from threading  import Thread
import json

class Engine:
    def __init__(self, program):
        self.program = program

        print 'One line at a time:'
        self.proc = subprocess.Popen('game-engines/build_and_run.sh', 
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE,
)

    def get_world_state(self):
        try:
            output = self.proc.stdout.readline()
        except ValueError:
            print sys.exc_info()
        return json.decode(output.rstrip())


    def update(self, data_string): # see spec?
        # pipe.send(data_string) 
        self.proc.stdin.write('%s\n' % data_string)

