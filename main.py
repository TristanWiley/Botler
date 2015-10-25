from flask import Flask, request, session, render_template, jsonify, redirect, url_for, send_from_directory, Response
from werkzeug import secure_filename
from flask.ext.socketio import SocketIO, send, emit
from os import path, listdir
import os
import json
import re
import random
import subprocess
from matches import match_against

app = Flask(__name__, static_url_path='/static')
socketio = SocketIO(app)
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


@app.route('/api/gamestats/bots')
def get_num_bots():
    num_bots = len(listdir(DATA_PATH))
    return Response(response=str(num_bots), status=200)



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
    best_script = {'name': "none", 'wins': 0}
    files = [f for f in listdir(DATA_PATH) if (
        path.isfile(path.join(DATA_PATH, f)) and f[0] != '.')]
    for fname in files:
        with open(path.join(DATA_PATH, fname)) as f:
            script_name = fname.split('-')[1]
            game_name = fname.split('-')[0]

            stats = json.loads(f.read())
            total_plays = total_plays + stats['wins']
            total_plays = total_plays + stats['losses']
            total_plays = total_plays + stats['ties']
            num_players = num_players + 1
            if(stats['wins'] > best_script['wins']):
                best_script['name'] = script_name
                best_script['wins'] = stats['wins']
    gamestats = json.dumps({'total_plays': total_plays, 'num_players': num_players, 'best_script': best_script}, sort_keys=True,
                           indent=4, separators=(',', ': '))

    return Response(response=gamestats, status=200, mimetype="application/json")

@socketio.on('simstart')
def handle_simstart(message):
    filename = '-'.join(
        [message['game_name'], secure_filename(message['script_name']).replace('-', '_').replace(' ', '_')]) + '.py'

    py_filename = filename
    print "running?"
    def printstuff(x):
        print "got callback"
        print x

    match_against(py_filename, lambda x: emit('frame', {'data':x}))
    emit('seqend',{})
    # pipe data from the engine to the browser
    # x should be an svg or something
    # facilitator.on_data(lambda x: emit('frame', {'frame': x}))
    pass
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
        py_filename = os.path.join(app.config['SCRIPTS_FOLDER'], filename)
        with open(py_filename, 'a+') as f:
            f.seek(0)
            f.truncate(0)
            f.write(code)

        json_filename = os.path.join(app.config['UPLOAD_FOLDER'], filename[:-3]+".json")
        with open(json_filename, 'a+') as g:
            g.write('{"wins":0,"losses":0,"ties":0,"history":[]}')

        return redirect('/upload')


if __name__ == '__main__':
    # socketio.run(app)
    # app.run()
    #app.run(host='0.0.0.0', port=5000)
    socketio.run(app, host='0.0.0.0', port=5000)
