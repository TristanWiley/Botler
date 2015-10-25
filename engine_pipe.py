# links the python stuff to the haskell stuff
import subprocess
import sys
from subprocess import PIPE, Popen
from threading import Thread
import json


class Engine:

    def __init__(self, program):
        self.program = program

        self.proc = subprocess.Popen('game-engines/build_and_run.sh',
                                     stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE)

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

    def checkStatus(self):
        self.write('["checkStatus"]')
        return self.read()

    def makeMove(self, move):
        self.write('["makeMove", %s]' % json.dumps(move))
        
    def validMoves(self):
        self.write('["validMoves"]')
        return self.read()
