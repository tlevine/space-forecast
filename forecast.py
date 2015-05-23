#!/usr/bin/env python3
import os

from bottle import Bottle, request, response, \
                   abort, redirect, \
                   view, TEMPLATE_PATH, \
                   static_file

TEMPLATE_PATH.append(os.path.join(os.path.dirname(__name__), 'views'))

app = Bottle()

@app.route('/')
@view('plot')
def home():
    return {}

app.run(server = 'cherrypy', reloader = True, port = 8081, host = '127.0.0.1')
