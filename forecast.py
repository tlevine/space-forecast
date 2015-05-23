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
