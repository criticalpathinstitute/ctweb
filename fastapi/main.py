"""
FastAPI server for Clinical Trials
"""

import csv
import ct
import io
import os
import psycopg2
import psycopg2.extras
import re
from fastapi.responses import StreamingResponse
from configparser import ConfigParser
from itertools import chain
from fastapi import FastAPI
from pymongo import MongoClient
from starlette.middleware.cors import CORSMiddleware
from typing import List, Optional
from pydantic import BaseModel

#
# Read configuration for global settings
#
config_file = './config.ini'
assert os.path.isfile(config_file)
config = ConfigParser()
config.read(config_file)

app = FastAPI(root_path=config['DEFAULT']['api_prefix'])
#client = MongoClient(config['DEFAULT']['mongo_url'])
#mongo_db = client['ct']

origins = [
    "http://localhost:*",
    "*",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


class ConditionDropDown(BaseModel):
    condition_id: int
    condition_name: str
    num_studies: int


class Phase(BaseModel):
    phase_id: int
    phase_name: str
    num_studies: int


class StudyCart(BaseModel):
    study_id: int
    nct_id: str
    title: str


class StudyDownload(BaseModel):
    study_id: int
    nct_id: str
    title: str
    detailed_description: str


class StudySearchResult(BaseModel):
    study_id: int
    nct_id: str
    title: str


class StudyDoc(BaseModel):
    study_doc_id: int
    doc_id: str
    doc_type: str
    doc_url: str
    doc_comment: str


class StudyOutcome(BaseModel):
    study_outcome_id: int
    outcome_type: str
    measure: str
    time_frame: str
    description: str


class StudySponsor(BaseModel):
    sponsor_id: int
    sponsor_name: str


class StudyCondition(BaseModel):
    condition_id: int
    condition_name: str


class StudyIntervention(BaseModel):
    intervention_id: int
    intervention_name: str


class StudyDetail(BaseModel):
    study_id: int
    study_type_id: int
    study_type: str
    phase_id: int
    phase: str
    overall_status_id: int
    overall_status: str
    last_known_status_id: int
    last_known_status: str
    nct_id: str
    official_title: str
    brief_title: str
    detailed_description: str
    org_study_id: str
    acronym: str
    source: str
    rank: str
    brief_summary: str
    why_stopped: Optional[str]
    study_type: Optional[str]
    has_expanded_access: Optional[str]
    target_duration: Optional[str]
    biospec_retention: Optional[str]
    biospec_description: Optional[str]
    keywords: Optional[str]
    start_date: Optional[str]
    completion_date: Optional[str]
    enrollment: Optional[int]
    sponsors: List[StudySponsor]
    conditions: List[StudyCondition]
    interventions: List[StudyIntervention]
    study_outcomes: List[StudyOutcome]
    study_docs: List[StudyDoc]


class StudyType(BaseModel):
    study_type_id: int
    study_type_name: str
    num_studies: int


class Sponsor(BaseModel):
    sponsor_id: int
    sponsor_name: str
    num_studies: int


class Summary(BaseModel):
    num_studies: int


dsn_tmpl = 'dbname=ct user={} password={} host={}'
dsn = dsn_tmpl.format(config['DEFAULT']['dbuser'], config['DEFAULT']['dbpass'],
                      config['DEFAULT']['dbhost'])
dbh = psycopg2.connect(dsn)


# --------------------------------------------------
def get_cur():
    """ Get db cursor """

    return dbh.cursor(cursor_factory=psycopg2.extras.DictCursor)


# --------------------------------------------------
@app.get('/view_cart', response_model=List[StudyCart])
def view_cart(study_ids: str) -> List[StudyCart]:
    """ View studies in cart """

    sql = """
        select s.study_id, s.nct_id, s.official_title
        from   study s
        where  s.study_id in ({})
    """.format(study_ids)
    # print(sql)

    res = []
    try:
        cur = get_cur()
        cur.execute(sql)
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()

    if not res:
        return []

    def f(rec):
        return StudyCart(study_id=rec['study_id'],
                         nct_id=rec['nct_id'],
                         title=rec['official_title'])

    return list(map(f, res))


# --------------------------------------------------
@app.get('/download', response_model=List[StudyDownload])
def download(study_ids: str, fields: Optional[str] = '') -> StreamingResponse:
    """ Download """

    ids = list(filter(str.isdigit, re.split('\s*,\s*', study_ids)))
    if not ids:
        return []

    default_fields = [
        'nct_id', 'official_title', 'brief_title', 'brief_summary',
        'detailed_description', 'keywords', 'enrollment', 'start_date',
        'completion_date', 'last_known_status', 'overall_status'
    ]

    sql = """
        select s.nct_id, s.official_title, s.brief_title, s.brief_summary,
               s.detailed_description, s.keywords, s.enrollment, s.start_date,
               s.completion_date, st1.status_name as last_known_status,
               st2.status_name as overall_status
        from   study s, status as st1, status as st2
        where  s.study_id in ({})
        and    s.last_known_status_id=st1.status_id
        and    s.overall_status_id=st2.status_id
    """.format(', '.join(ids))
    # print(sql)

    res = []
    try:
        cur = get_cur()
        cur.execute(sql)
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()

    def clean(s):
        if isinstance(s, str):
            return re.sub('\s+', ' ', s)

    stream = io.StringIO()
    if res:
        flds = fields.split(',') if fields else default_fields
        writer = csv.DictWriter(stream, fieldnames=flds, delimiter=',')
        writer.writeheader()
        for row in map(dict, res):
            writer.writerow({f: clean(row[f]) for f in flds})

    response = StreamingResponse(iter([stream.getvalue()]),
                                 media_type="text/csv")
    response.headers[
        "Content-Disposition"] = "attachment; filename=download.csv"
    return response


# --------------------------------------------------
@app.get('/search', response_model=List[StudySearchResult])
def search(text: Optional[str] = '',
           enrollment: Optional[str] = '',
           overall_status_id: Optional[int] = 0,
           last_known_status_id: Optional[int] = 0,
           condition_ids: Optional[str] = '',
           sponsor_ids: Optional[str] = '',
           condition_names: Optional[str] = '',
           sponsor_names: Optional[str] = '',
           study_type_ids: Optional[str] = '',
           phase_ids: Optional[str] = '') -> List[StudySearchResult]:
    """ Search """

    flds = ['study_id', 'nct_id', 'official_title']
    where = []

    if text:
        where.append({
            'tables': [],
            'where': ['s.all_text @@ to_tsquery({})'.format(make_bool(text))]
        })

    if phase_ids:
        ids = list(filter(str.isdigit, phase_ids.split(',')))
        where.append({
            'tables': [],
            'where': ['s.phase_id in ({})'.format(','.join(ids))]
        })

    if study_type_ids:
        ids = list(filter(str.isdigit, study_type_ids.split(',')))
        where.append({
            'tables': [],
            'where': ['s.study_type_id in ({})'.format(','.join(ids))]
        })

    if match := re.match(r'(=|==|<|<=|>|>=)?\s*(\d+)', enrollment):
        op = match.group(1) or '>='
        num = match.group(2)
        where.append({'tables': [], 'where': [f's.enrollment {op} {num}']})

    if overall_status_id > 0:
        where.append({
            'tables': [],
            'where': ['s.overall_status_id = {}'.format(overall_status_id)]
        })

    if last_known_status_id > 0:
        where.append({
            'tables': [],
            'where':
            ['s.last_known_status_id = {}'.format(last_known_status_id)]
        })

    if condition_names:
        names = 'c.condition_name @@ to_tsquery({})'.format(
            make_bool(condition_names))

        where.append({
            'tables': ['study_to_condition s2c', 'condition c'],
            'where': [
                's.study_id=s2c.study_id', 's2c.condition_id=c.condition_id',
                names
            ]
        })

    if sponsor_names:
        names = 'sp.sponsor_name @@ to_tsquery({})'.format(
            make_bool(sponsor_names))
        where.append({
            'tables': ['study_to_sponsor s2p', 'sponsor sp'],
            'where':
            ['s.study_id=s2p.study_id', 's2p.sponsor_id=sp.sponsor_id', names]
        })

    if condition_ids:
        where.append({
            'tables': ['study_to_condition s2c'],
            'where': [
                's.study_id=s2c.study_id',
                's2c.condition_id in ({})'.format(condition_ids)
            ]
        })

    if sponsor_ids:
        where.append({
            'tables': ['study_to_sponsor s2p'],
            'where': [
                's.study_id=s2p.study_id',
                's2p.sponsor_id in ({})'.format(sponsor_ids)
            ]
        })

    if not where:
        return []

    table_names = ['study s'] + list(
        chain.from_iterable(map(lambda x: x['tables'], where)))
    where = '\nand '.join(chain.from_iterable(map(lambda x: x['where'],
                                                  where)))
    sql = """
        select s.study_id, s.nct_id, s.official_title
        from   {}
        where  {}
    """.format(', '.join(table_names), where)

    # print(sql)

    res = []
    try:
        cur = get_cur()
        cur.execute(sql)
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()

    def f(rec):
        return StudySearchResult(study_id=rec['study_id'],
                                 nct_id=rec['nct_id'],
                                 title=rec['official_title'] or 'NA')

    return list(map(f, res))


# --------------------------------------------------
def make_bool(s: str):
    """ Turn and or to & | """

    s = re.sub('[*]', '', s)
    s = re.sub('\s+and\s+', ' & ', s, re.I)
    s = re.sub('\s+or\s+', ' | ', s, re.I)
    s = re.sub('\s+not\s+', ' ! ', s, re.I)
    return f"'{s}'"


# --------------------------------------------------
@app.get('/summary', response_model=Summary)
def summary():
    """ DB summary stats """

    cur = get_cur()
    cur.execute('select count(study_id) as num_studies from study')
    res = cur.fetchone()
    cur.close()

    return Summary(num_studies=res['num_studies'])


# --------------------------------------------------
@app.get('/study/{nct_id}', response_model=Optional[StudyDetail])
def study(nct_id: str) -> StudyDetail:
    """ Study details """

    if studies := ct.Study.select().where(ct.Study.nct_id == nct_id):
        study = studies[0]

        sponsors = [
            StudySponsor(sponsor_id=s.sponsor_id,
                         sponsor_name=s.sponsor.sponsor_name)
            for s in ct.StudyToSponsor.select().where(
                ct.StudyToSponsor.study_id == study.study_id)
        ]

        conditions = [
            StudyCondition(condition_id=c.condition_id,
                           condition_name=c.condition.condition_name)
            for c in ct.StudyToCondition.select().where(
                ct.StudyToCondition.study_id == study.study_id)
        ]

        interventions = [
            StudyIntervention(
                intervention_id=c.intervention_id,
                intervention_name=c.intervention.intervention_name)
            for c in ct.StudyToIntervention.select().where(
                ct.StudyToIntervention.study_id == study.study_id)
        ]

        study_docs = [
            StudyDoc(study_doc_id=doc.study_doc_id,
                     doc_id=doc.doc_id,
                     doc_type=doc.doc_type,
                     doc_url=doc.doc_url,
                     doc_comment=doc.doc_comment)
            for doc in ct.StudyDoc.select().where(
                ct.StudyDoc.study_id == study.study_id)
        ]

        study_outcomes = [
            StudyOutcome(study_outcome_id=o.study_outcome_id,
                         outcome_type=o.outcome_type,
                         measure=o.measure,
                         time_frame=o.time_frame,
                         description=o.description)
            for o in ct.StudyOutcome.select().where(
                ct.StudyOutcome.study_id == study.study_id)
        ]

        return StudyDetail(
            study_id=study.study_id,
            study_type_id=study.study_type_id,
            study_type=study.study_type.study_type_name,
            phase_id=study.phase_id,
            phase=study.phase.phase_name,
            overall_status_id=study.overall_status_id,
            overall_status=study.overall_status.status_name,
            last_known_status_id=study.last_known_status_id,
            last_known_status=study.last_known_status.status_name,
            nct_id=study.nct_id,
            official_title=study.official_title or '',
            brief_title=study.brief_title or '',
            detailed_description=study.detailed_description or '',
            org_study_id=study.org_study_id or '',
            acronym=study.acronym or '',
            source=study.source or '',
            rank=study.rank or '',
            brief_summary=study.brief_summary or '',
            why_stopped=study.why_stopped or '',
            has_expanded_access=study.has_expanded_access or '',
            target_duration=study.target_duration or '',
            biospec_retention=study.biospec_retention or '',
            biospec_description=study.biospec_description or '',
            keywords=study.keywords or '',
            start_date=str(study.start_date) or '',
            completion_date=str(study.completion_date) or '',
            enrollment=study.enrollment,
            sponsors=sponsors,
            conditions=conditions,
            interventions=interventions,
            study_outcomes=study_outcomes,
            study_docs=study_docs)


# --------------------------------------------------
@app.get('/study_types', response_model=List[StudyType])
def study_types(study_type: Optional[str] = '') -> List[StudyType]:
    """ Study Types """

    clause = f"and t.study_type_name like '%{study_type}%'" \
        if study_type else ''
    sql = f"""
        select   t.study_type_id, t.study_type_name,
                 count(s.study_id) as num_studies
        from     study s, study_type t
        where    s.study_type_id=t.study_type_id
        {clause}
        group by 1, 2
        order by 2
    """

    cur = get_cur()
    cur.execute(sql)
    res = cur.fetchall()
    types = list(map(lambda r: StudyType(**dict(r)), res))
    cur.close()

    return types


# --------------------------------------------------
@app.get('/conditions', response_model=List[ConditionDropDown])
def conditions(name: Optional[str] = '') -> List[ConditionDropDown]:
    """ Conditions/Num Studies """

    clause = 'and c.condition_name @@ to_tsquery({})'.format(
        make_bool(name)) if name else ''

    sql = f"""
        select   c.condition_id, c.condition_name,
                 count(s.study_id) as num_studies
        from     condition c, study_to_condition s2c, study s
        where    c.condition_id=s2c.condition_id
        and      s2c.study_id=s.study_id
        {clause}
        group by 1, 2
        order by 2
    """

    cur = get_cur()
    cur.execute(sql)
    res = cur.fetchall()
    conditions = list(map(lambda r: ConditionDropDown(**dict(r)), res))
    cur.close()

    return conditions


# --------------------------------------------------
@app.get('/sponsors', response_model=List[Sponsor])
def sponsors() -> List[Sponsor]:
    """ Sponsors/Num Studies """

    sql = """
        select   p.sponsor_id, p.sponsor_name, count(s.study_id) as num_studies
        from     sponsor p, study_to_sponsor s2p, study s
        where    p.sponsor_id=s2p.sponsor_id
        and      s2p.study_id=s.study_id
        group by 1, 2
        order by 2
    """

    cur = get_cur()
    cur.execute(sql)
    res = cur.fetchall()
    conditions = list(map(lambda r: Sponsor(**dict(r)), res))
    cur.close()

    return conditions


# --------------------------------------------------
@app.get('/phases', response_model=List[Phase])
def phases() -> List[Phase]:
    """ Phases """

    sql = """
        select   p.phase_id, p.phase_name, count(s.study_id) as num_studies
        from     phase p, study s
        where    p.phase_id=s.phase_id
        group by 1, 2
        order by 2
    """

    cur = get_cur()
    cur.execute(sql)
    res = cur.fetchall()
    phases = list(map(lambda r: Phase(**dict(r)), res))
    cur.close()

    return phases
