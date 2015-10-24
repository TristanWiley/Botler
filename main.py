from flask import Flask, request, session, render_template, jsonify, redirect, url_for, send_from_directory, Response
from werkzeug import secure_filename
#from flask.ext.socketio import SocketIO, send, emit
from os import path

app = Flask(__name__, static_url_path='/static')
#socketio = SocketIO(app)
app.config['SECRET_KEY'] = 'secret!'
app.debug = True
app.config['UPLOAD_FOLDER'] = 'player_data'
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

@app.route('/api/stats/<name>')
def get_stats(name):
    with open(path.join(DATA_PATH, name+".json")) as f:
        return Response(response=f.read(), status=200, mimetype="application/json")


#@socketio.on('connect')
#def handle_connect(message):
    # pipe data from the engine to the browser
    # x should be an svg or something
    # facilitator.on_data(lambda x: emit('frame', {'frame': x}))
#    pass


@app.route('/script_upload', methods=['GET', 'POST'])
def script_upload():
    if request.method == 'POST':
        file = request.files['file']
        if file and allowed_file(file.filename):
            filename = secure_filename(file.filename)
            file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
            return redirect('/')

if __name__ == '__main__':
    #socketio.run(app)
    app.run()