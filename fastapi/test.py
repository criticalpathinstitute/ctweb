#!/usr/bin/env python3

import os
import psycopg2
import psycopg2.extras
from configparser import ConfigParser
from pydantic import BaseModel

class Summary(BaseModel):
    num_studies: int

config_file = './config.ini'
assert os.path.isfile(config_file)
config = ConfigParser()
config.read(config_file)

print(list(config['DEFAULT']))

dsn_tmpl = 'dbname=ct user={} password={} host={}'
dsn = dsn_tmpl.format(config['DEFAULT']['dbuser'], config['DEFAULT']['dbpass'],
                      config['DEFAULT']['dbhost'])
dbh = psycopg2.connect(dsn)

def get_cur():
    """ Get db cursor """

    return dbh.cursor(cursor_factory=psycopg2.extras.DictCursor)


def summary():
    """ DB summary stats """

    cur = get_cur()
    cur.execute('select count(study_id) as num_studies from study')
    res = cur.fetchone()
    cur.close()

    return Summary(num_studies=res['num_studies'])

print(summary())
