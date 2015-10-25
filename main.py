from flask import Flask, request, session, render_template, jsonify, redirect, url_for, send_from_directory, Response
from werkzeug import secure_filename
#from flask.ext.socketio import SocketIO, send, emit
from os import path, listdir
import os
import json
import re
import random
import subprocess

app = Flask(__name__, static_url_path='/static')
#socketio = SocketIO(app)
app.config['SECRET_KEY'] = 'secret!'
app.debug = True
app.config['UPLOAD_FOLDER'] = 'player_data'
app.config['SCRIPTS_FOLDER'] = 'player_scripts'

DATA_PATH = 'player_data'


@app.route('/')
def render_index():
    return render_template('index.html')


@app.route('/docs')
def render_docs():
    return render_template('docs.html')


@app.route('/upload')
def render_upload():
    return render_template('ide.html')


@app.route('/stats')
def render_stats():
    return render_template('stats.html')


@app.route('/api/stats/<name>')
def get_stats(name):
    for file_name in listdir(DATA_PATH):#get a list of strings of all files
        if name in file_name:#if name searched in 
            with open(DATA_PATH+'/'+file_name) as f:
                return Response(response=f.read(), status=200, mimetype="application/json")
    return Response(response="{}", status=200, mimetype="application/json")


@app.route('/api/stats/bots')
def get_num_bots():
    num_bots = len(listdir(DATA_PATH))
    return Response(response=num_bots, status=200, mimetype="application/json")



@app.route('/api/stats/fake')
def fake_stats():
    return Response(response=json.dumps(
        {'wins': random.randint(0, 10), 
        'losses': random.randint(0, 10), 
        'ties': random.randint(0, 10)}
        ), status=200, mimetype="application/json")


@app.route('/api/gamestats/<name>')
def get_game_stats(name):
    # total plays
    # total players who have played
    # best player
    total_plays = 0
    num_players = 0
    best_player = {'id': -1, 'wins': 0}
    files = [f for f in listdir(DATA_PATH) if (
        path.isfile(path.join(DATA_PATH, f)) and f[0] != '.')]
    for fname in files:
        with open(path.join(DATA_PATH, fname)) as f:
            player_id = re.search('(\d*)\.', fname).group(0)[:-1]
            stats = json.loads(f.read())
            total_plays = total_plays + stats['wins']
            total_plays = total_plays + stats['losses']
            total_plays = total_plays + stats['ties']
            num_players = num_players + 1
            if(stats['wins'] > best_player['wins']):
                best_player['id'] = player_id
                best_player['wins'] = stats['wins']
    gamestats = json.dumps({'total_plays': total_plays, 'num_players': num_players, 'best_player': best_player}, sort_keys=True,
                           indent=4, separators=(',', ': '))

    return Response(response=gamestats, status=200, mimetype="application/json")

#@socketio.on('connect')
# def handle_connect(message):
    # pipe data from the engine to the browser
    # x should be an svg or something
    # facilitator.on_data(lambda x: emit('frame', {'frame': x}))
#    pass
# ffrdc


@app.route('/script_upload', methods=['GET', 'POST'])
def script_upload():
    if request.method == 'POST':
        code = request.form['code']
        game = request.form['game']
        script_name = request.form['script_name']
        if len(script_name) == 0:
            return redirect('/')
        filename = '-'.join(
            [game, secure_filename(script_name).replace('-', '_').replace(' ', '_')]) + '.py'

        with open(os.path.join(app.config['SCRIPTS_FOLDER'], filename), 'a+') as f:
            f.seek(0)
            f.truncate(0)
            f.write(code)

        with open(os.path.join(app.config['UPLOAD_FOLDER'], filename[:-3]+".json"), 'a+') as g:
            g.write('{"wins":0,"losses":0,"ties":0}')

        return redirect('/')


if __name__ == '__main__':
    # socketio.run(app)
    app.run()
