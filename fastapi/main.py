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
from configparser import ConfigParser
from fastapi import FastAPI, HTTPException
from fastapi.responses import StreamingResponse
from functools import lru_cache
from itertools import chain
from pydantic import BaseModel
from pymongo import MongoClient
from starlette.middleware.cors import CORSMiddleware
from typing import List, Optional

#
# Read configuration for global settings
#
config_file = './config.ini'
assert os.path.isfile(config_file)
config = ConfigParser(interpolation=None)
config.read(config_file)

app = FastAPI(root_path=config['DEFAULT']['api_prefix'])

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


class Dataload(BaseModel):
    num_studies: int
    updated_on: str


class Phase(BaseModel):
    phase_id: int
    phase_name: str


class SavedSearch(BaseModel):
    saved_search_id: int
    search_name: str
    full_text: str
    full_text_bool: int
    sponsors: str
    sponsors_bool: int
    conditions: str
    conditions_bool: int
    interventions: str
    interventions_bool: int
    phase_ids: str
    study_type_ids: str
    enrollment: int
    email_to: str


class SaveSearchResponse(BaseModel):
    num_saved_searches: int


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


class Sponsor(BaseModel):
    sponsor_id: int
    sponsor_name: str
    num_studies: int


class Summary(BaseModel):
    num_studies: int


class SearchResults(BaseModel):
    count: int
    records: List[StudySearchResult]


dsn_tmpl = 'dbname={} user={} password={} host={}'
dsn = dsn_tmpl.format(config['DEFAULT']['dbname'], config['DEFAULT']['dbuser'],
                      config['DEFAULT']['dbpass'], config['DEFAULT']['dbhost'])
dbh = psycopg2.connect(dsn)


# --------------------------------------------------
def get_cur():
    """ Get db cursor """

    return dbh.cursor(cursor_factory=psycopg2.extras.DictCursor)


# --------------------------------------------------
@app.get('/view_cart', response_model=List[StudyCart])
def view_cart(study_ids: str) -> List[StudyCart]:
    """ View studies in cart """

    ids = list(filter(str.isdigit, re.split(r'\s*,\s*', study_ids)))
    sql = """
        select s.study_id, s.nct_id, s.brief_title
        from   study s
        where  s.study_id in ({})
    """.format(', '.join(ids))

    # print(sql)

    def f(rec):
        return StudyCart(study_id=rec['study_id'],
                         nct_id=rec['nct_id'],
                         title=rec['brief_title'])

    res = []
    try:
        cur = get_cur()
        cur.execute(sql)
        res = cur.fetchall()
    except:
        dbh.rollback()
    finally:
        cur.close()

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
        'completion_date', 'last_known_status', 'overall_status', 'conditions',
        'interventions', 'outcomes', 'sponsors', 'study_docs'
    ]

    sql = """
        select s.study_id, s.nct_id, s.official_title,
               s.brief_title, s.brief_summary,
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
    finally:
        cur.close()

    def clean(s):
        if isinstance(s, str):
            return re.sub('\s+', ' ', s)

    stream = io.StringIO()
    if res:
        flds = fields.split(',') if fields else default_fields
        writer = csv.DictWriter(stream, fieldnames=flds, delimiter=',')
        writer.writeheader()
        for row in map(dict, res):
            if 'conditions' in flds:
                row['conditions'] = ';'.join(
                    get_study_conditions(row['study_id']))

            if 'interventions' in flds:
                row['interventions'] = ';'.join(
                    get_study_interventions(row['study_id']))

            if 'sponsors' in flds:
                row['sponsors'] = ';'.join(get_study_sponsors(row['study_id']))

            if 'outcomes' in flds:
                row['outcomes'] = ';'.join(get_study_outcomes(row['study_id']))

            if 'study_docs' in flds:
                row['study_docs'] = ';'.join(get_study_docs(row['study_id']))

            writer.writerow({f: clean(row[f]) for f in flds})

    response = StreamingResponse(iter([stream.getvalue()]),
                                 media_type="text/csv")
    response.headers[
        "Content-Disposition"] = "attachment; filename=download.csv"
    return response


# --------------------------------------------------
def get_study_conditions(study_id: int) -> List[str]:
    """ Get conditions for study """

    sql = """
        select c.condition_name
        from   condition c, study_to_condition s2c
        where  s2c.study_id=%s
        and    s2c.condition_id=c.condition_id
    """

    res = []
    try:
        cur = get_cur()
        cur.execute(sql, (study_id, ))
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()
    finally:
        cur.close()

    return list(map(lambda r: r['condition_name'], res))


# --------------------------------------------------
def get_study_interventions(study_id: int) -> List[str]:
    """ Get interventions for study """

    sql = """
        select i.intervention_name
        from   intervention i, study_to_intervention s2i
        where  s2i.study_id=%s
        and    s2i.intervention_id=i.intervention_id
    """

    res = []
    try:
        cur = get_cur()
        cur.execute(sql, (study_id, ))
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()
    finally:
        cur.close()

    return list(map(lambda r: r['intervention_name'], res))


# --------------------------------------------------
def get_study_outcomes(study_id: int) -> List[str]:
    """ Get outcomes for study """

    sql = """
        select o.outcome_type, o.measure, o.time_frame, o.description
        from   study_outcome o
        where  o.study_id=%s
    """

    res = []
    try:
        cur = get_cur()
        cur.execute(sql, (study_id, ))
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()
    finally:
        cur.close()

    def f(rec):
        return '::'.join([
            rec['outcome_type'] or '',
            rec['measure'] or '',
            rec['time_frame'] or '',
            rec['description'] or '',
        ])

    return list(map(f, res))


# --------------------------------------------------
def get_study_docs(study_id: int) -> List[str]:
    """ Get study_docs for study """

    sql = """
        select d.doc_id, d.doc_type, d.doc_url, d.doc_comment
        from   study_doc d
        where  d.study_id=%s
    """

    res = []
    try:
        cur = get_cur()
        cur.execute(sql, (study_id, ))
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()
    finally:
        cur.close()

    def f(rec):
        return '::'.join([
            rec['doc_id'] or '',
            rec['doc_type'] or '',
            rec['doc_url'] or '',
            rec['doc_comment'] or '',
        ])

    return list(map(f, res))


# --------------------------------------------------
def get_study_sponsors(study_id: int) -> List[str]:
    """ Get sponsors for study """

    sql = """
        select p.sponsor_name
        from   sponsor p, study_to_sponsor s2p
        where  s2p.study_id=%s
        and    s2p.sponsor_id=p.sponsor_id
    """

    res = []
    try:
        cur = get_cur()
        cur.execute(sql, (study_id, ))
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()
    finally:
        cur.close()

    return list(map(lambda r: r['sponsor_name'], res))


# --------------------------------------------------
@app.get('/search', response_model=SearchResults)
def search(text: Optional[str] = '',
           text_bool: Optional[int] = 0,
           condition_names: Optional[str] = '',
           conditions_bool: Optional[int] = 0,
           sponsor_names: Optional[str] = '',
           sponsors_bool: Optional[int] = 0,
           intervention_names: Optional[str] = '',
           interventions_bool: Optional[int] = 0,
           enrollment: Optional[str] = '',
           overall_status_id: Optional[int] = 0,
           last_known_status_id: Optional[int] = 0,
           condition_ids: Optional[str] = '',
           sponsor_ids: Optional[str] = '',
           study_type_ids: Optional[str] = '',
           phase_ids: Optional[str] = '',
           limit: Optional[int] = 0) -> List[StudySearchResult]:
    """ Search """

    flds = ['study_id', 'nct_id', 'official_title']
    where = []

    if text:
        where.append({
            'tables': [],
            'where': ['s.fulltext @@ {}'.format(tsquery(text, text_bool))]
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
        names = 'c.condition_name @@ {}'.format(
            tsquery(condition_names, conditions_bool))

        where.append({
            'tables': ['study_to_condition s2c', 'condition c'],
            'where': [
                's.study_id=s2c.study_id', 's2c.condition_id=c.condition_id',
                names
            ]
        })

    if sponsor_names:
        names = 'sp.sponsor_name @@ {}'.format(
            tsquery(sponsor_names, sponsors_bool))
        where.append({
            'tables': ['study_to_sponsor s2p', 'sponsor sp'],
            'where':
            ['s.study_id=s2p.study_id', 's2p.sponsor_id=sp.sponsor_id', names]
        })

    if intervention_names:
        names = 'i.intervention_name @@ {}'.format(
            tsquery(intervention_names, interventions_bool))
        where.append({
            'tables': ['study_to_intervention s2i', 'intervention i'],
            'where': [
                's.study_id=s2i.study_id',
                's2i.intervention_id=i.intervention_id', names
            ]
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

    count_sql = """
        select count(s.study_id)
        from   {}
        where  {}
    """.format(', '.join(table_names), where)

    select_sql = """
        select s.study_id, s.nct_id, s.official_title
        from   {}
        where  {}
        limit {}
    """.format(', '.join(table_names), where, limit or 'ALL')

    res = []
    count = 0
    try:
        cur = get_cur()
        cur.execute(count_sql)
        if counts := cur.fetchone():
            count = counts[0]

        cur.execute(select_sql)
        res = cur.fetchall()
        cur.close()
    except:
        dbh.rollback()

    def f(rec):
        return StudySearchResult(study_id=rec['study_id'],
                                 nct_id=rec['nct_id'],
                                 title=rec['official_title'] or 'NA')

    return SearchResults(count=count, records=list(map(f, res)))


# --------------------------------------------------
def tsquery(query: str, bool_search: int) -> str:
    """ Make into query """

    if bool_search:
        query = make_bool(query)

    return f"{'' if bool_search else 'plain'}to_tsquery('english', '{query}')"


# --------------------------------------------------
def make_bool(s: str) -> str:
    """ Turn and or to & | """

    s = re.sub('[*]', '', s)
    s = re.sub('\s+and\s+', ' & ', s, re.I)
    s = re.sub('\s+or\s+', ' | ', s, re.I)
    s = re.sub('\s+not\s+', ' ! ', s, re.I)
    return s


# --------------------------------------------------
@app.get('/summary', response_model=Summary)
# @lru_cache()
def summary():
    """ DB summary stats """

    cur = get_cur()
    res = []
    try:
        cur.execute('select count(study_id) as num_studies from study')
        res = cur.fetchone()
    except:
        dbh.rollback()
    finally:
        cur.close()

    if res:
        return Summary(num_studies=res['num_studies'])
    else:
        return []


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
@lru_cache()
def study_types() -> List[StudyType]:
    """ Study Types """

    sql = f"""
        select   t.study_type_id, t.study_type_name
        from     study_type t
        order by 2
    """

    cur = get_cur()
    res = []
    try:
        cur.execute(sql)
        res = cur.fetchall()
    except:
        dbh.rollback()
    finally:
        cur.close()

    return list(map(lambda r: StudyType(**dict(r)), res))


# --------------------------------------------------
@app.get('/conditions', response_model=List[ConditionDropDown])
def conditions(name: str,
               bool_search: Optional[int] = 0) -> List[ConditionDropDown]:
    """ Conditions/Num Studies """

    clause = 'and c.condition_name @@ {}'.format(tsquery(name, bool_search))

    sql = f"""
        select   c.condition_id, c.condition_name,
                 count(s.study_id) as num_studies
        from     condition c, study_to_condition s2c, study s
        where    c.condition_id=s2c.condition_id
        and      s2c.study_id=s.study_id
        {clause}
        group by 1, 2
        order by 3 desc, 2
    """

    # print(sql)

    res = []
    try:
        cur = get_cur()
        cur.execute(sql)
        res = cur.fetchall()
    except Exception as e:
        dbh.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        cur.close()

    return list(map(lambda r: ConditionDropDown(**dict(r)), res))


# --------------------------------------------------
@app.get('/sponsors', response_model=List[Sponsor])
def sponsors(name: str, bool_search: Optional[int] = 0) -> List[Sponsor]:
    """ Sponsors/Num Studies """

    clause = 'and p.sponsor_name @@ {}'.format(tsquery(name, bool_search))
    sql = f"""
        select   p.sponsor_id, p.sponsor_name, count(s.study_id) as num_studies
        from     sponsor p, study_to_sponsor s2p, study s
        where    p.sponsor_id=s2p.sponsor_id
        and      s2p.study_id=s.study_id
        {clause}
        group by 1, 2
        order by 3 desc, 2
    """

    cur = get_cur()
    res = []
    try:
        cur.execute(sql)
        res = cur.fetchall()
    except Exception as e:
        dbh.rollback()
        raise HTTPException(status_code=500, detail=str(e))
    finally:
        cur.close()

    return list(map(lambda r: Sponsor(**dict(r)), res))


# --------------------------------------------------
@app.get('/phases', response_model=List[Phase])
def phases() -> List[Phase]:
    """ Phases """

    sql = """
        select   p.phase_id, p.phase_name
        from     phase p
        order by 2
    """

    cur = get_cur()
    res = []
    try:
        cur.execute(sql)
        res = cur.fetchall()
    except:
        dbh.rollback()
    finally:
        cur.close()

    return list(map(lambda r: Phase(**dict(r)), res))


# --------------------------------------------------
@app.get('/save_search', response_model=SaveSearchResponse)
def save_search(search_name: str,
                email_id: str,
                full_text: Optional[str] = '',
                full_text_bool: Optional[int] = 0,
                conditions: Optional[str] = '',
                conditions_bool: Optional[int] = 0,
                sponsors: Optional[str] = '',
                sponsors_bool: Optional[int] = 0,
                interventions: Optional[str] = '',
                interventions_bool: Optional[int] = 0,
                phase_ids: Optional[str] = '',
                study_type_ids: Optional[str] = '',
                enrollment: Optional[int] = 0,
                email_to: Optional[str] = '') -> int:
    """ Save search """

    user, _ = ct.WebUser.get_or_create(email=email_id)

    saved_search, _ = ct.SavedSearch.get_or_create(
        web_user_id=user.web_user_id,
        search_name=search_name,
        full_text=full_text,
        full_text_bool=full_text_bool,
        conditions=conditions,
        conditions_bool=conditions_bool,
        sponsors=sponsors,
        sponsors_bool=sponsors_bool,
        interventions=interventions,
        interventions_bool=interventions_bool,
        phase_ids=phase_ids,
        study_type_ids=study_type_ids,
        enrollment=enrollment,
        email_to=email_to)

    return SaveSearchResponse(num_saved_searches=1)


# --------------------------------------------------
@app.get('/saved_searches', response_model=List[SavedSearch])
def saved_searches(email: str) -> List[SavedSearch]:
    """ Get saved searches """

    user, _ = ct.WebUser.get_or_create(email=email)

    sql = f"""
        select   s.saved_search_id, s.search_name,
                 s.full_text, s.full_text_bool,
                 s.conditions, s.conditions_bool,
                 s.sponsors, s.sponsors_bool,
                 s.interventions, s.interventions_bool,
                 s.phase_ids, s.study_type_ids,
                 s.enrollment, s.email_to
        from     saved_search s
        where    s.web_user_id={user.web_user_id}
        order by 2
    """

    cur = get_cur()
    res = []
    try:
        cur.execute(sql)
        res = cur.fetchall()
    except:
        dbh.rollback()
    finally:
        cur.close()

    return list(map(lambda r: SavedSearch(**dict(r)), res))


# --------------------------------------------------
@app.get('/dataload', response_model=Dataload)
def phases() -> Dataload:
    """ Dataload """

    sql = """
        select   updated_on
        from     dataload
        order by 1 desc
        limit 1
    """

    cur = get_cur()
    num_studies = 0
    updated_on = 'NA'

    try:
        cur.execute(sql)
        res = cur.fetchone()
        if res:
            updated_on = str(res['updated_on'])

        cur.execute('select count(study_id) as num_studies from study')
        res = cur.fetchone()
        if res:
            num_studies = res['num_studies']

    except:
        dbh.rollback()
    finally:
        cur.close()

    return Dataload(num_studies=num_studies, updated_on=updated_on)
