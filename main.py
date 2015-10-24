from flask import Flask, request, session, render_template, jsonify, redirect, url_for, send_from_directory
from werkzeug import secure_filename
app = Flask(__name__, static_url_path='/static')

app.config['UPLOAD_FOLDER'] = 'player_data'


@app.route('/')
def index():
    pass


@app.route('/script_upload', methods=['GET', 'POST'])
def script_upload():
    if request.method == 'POST':
        file = request.files['file']
        if file and allowed_file(file.filename):
            filename = secure_filename(file.filename)
            file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
            return redirect('/')
