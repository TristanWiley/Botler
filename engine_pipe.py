# links the python stuff to the haskell stuff
import subprocess
import sys
from subprocess import PIPE, Popen
from threading import Thread
import json


class Engine:

    def __init__(self, gametype):    
        self.proc = subprocess.Popen(['game-engines/build_and_run.sh', gametype], #yeah this is secure
                                     stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE)

    def __del__(self):
        print "destroy!"
        self.proc.terminate()

    def read(self):
        try:
            output = self.proc.stdout.readline()
        except ValueError:
            print sys.exc_info()
        return json.loads(output.rstrip())

    def write(self, data_string):  # see spec?
        self.proc.stdin.write('%s\n' % data_string)

    def getState(self):
        self.write('["getState"]')
        return self.read()

    def getStatus(self):
        self.write('["checkStatus"]')
        return self.read()

    def makeMove(self, move):
        self.write('["makeMove", %s]' % (json.dumps(move),))
        
    def getValidMoves(self):
        self.write('["validMoves"]')
        return self.read()

    def reset(self):
        self.write('["reset"]')

