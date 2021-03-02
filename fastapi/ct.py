from peewee import *

# database = PostgresqlDatabase('ct', user='postgres', host='127.0.0.1')
database = PostgresqlDatabase('ct')

class UnknownField(object):
    def __init__(self, *_, **__): pass

class BaseModel(Model):
    class Meta:
        database = database

class Condition(BaseModel):
    condition_id = AutoField()
    condition_name = CharField(unique=True)

    class Meta:
        table_name = 'condition'

class Intervention(BaseModel):
    intervention_id = AutoField()
    intervention_name = CharField(unique=True)

    class Meta:
        table_name = 'intervention'

class Phase(BaseModel):
    phase_id = AutoField()
    phase_name = CharField(unique=True)

    class Meta:
        table_name = 'phase'

class Sponsor(BaseModel):
    sponsor_id = AutoField()
    sponsor_name = CharField(unique=True)

    class Meta:
        table_name = 'sponsor'

class Status(BaseModel):
    status_id = AutoField()
    status_name = CharField(unique=True)

    class Meta:
        table_name = 'status'

class StudyType(BaseModel):
    study_type_id = AutoField()
    study_type_name = CharField(unique=True)

    class Meta:
        table_name = 'study_type'

class Study(BaseModel):
    acronym = TextField(null=True)
    all_text = TextField(null=True)
    biospec_description = TextField(null=True)
    biospec_retention = TextField(null=True)
    brief_summary = TextField(null=True)
    brief_title = TextField(null=True)
    completion_date = DateField(null=True)
    detailed_description = TextField(null=True)
    enrollment = IntegerField(null=True)
    has_expanded_access = TextField(null=True)
    keywords = TextField(null=True)
    last_known_status = ForeignKeyField(column_name='last_known_status_id', field='status_id', model=Status)
    nct_id = CharField(unique=True)
    official_title = TextField(null=True)
    org_study_id = TextField(null=True)
    overall_status = ForeignKeyField(backref='status_overall_status_set', column_name='overall_status_id', field='status_id', model=Status)
    phase = ForeignKeyField(column_name='phase_id', field='phase_id', model=Phase)
    rank = TextField(null=True)
    record_last_updated = DateTimeField(constraints=[SQL("DEFAULT CURRENT_TIMESTAMP")], null=True)
    source = TextField(null=True)
    start_date = DateField(null=True)
    study_id = AutoField()
    study_type = ForeignKeyField(column_name='study_type_id', field='study_type_id', model=StudyType)
    target_duration = TextField(null=True)
    why_stopped = TextField(null=True)

    class Meta:
        table_name = 'study'
        indexes = (
            ((), False),
        )

class StudyDoc(BaseModel):
    doc_comment = TextField(null=True)
    doc_id = CharField(null=True)
    doc_type = CharField(null=True)
    doc_url = TextField(null=True)
    study_doc_id = AutoField()
    study = ForeignKeyField(column_name='study_id', field='study_id', model=Study)

    class Meta:
        table_name = 'study_doc'

class StudyOutcome(BaseModel):
    description = TextField(null=True)
    measure = TextField()
    outcome_type = CharField()
    study = ForeignKeyField(column_name='study_id', field='study_id', model=Study)
    study_outcome_id = AutoField()
    time_frame = TextField(null=True)

    class Meta:
        table_name = 'study_outcome'

class StudyToCondition(BaseModel):
    condition = ForeignKeyField(column_name='condition_id', field='condition_id', model=Condition)
    study = ForeignKeyField(column_name='study_id', field='study_id', model=Study)
    study_to_condition_id = AutoField()

    class Meta:
        table_name = 'study_to_condition'

class StudyToIntervention(BaseModel):
    intervention = ForeignKeyField(column_name='intervention_id', field='intervention_id', model=Intervention)
    study = ForeignKeyField(column_name='study_id', field='study_id', model=Study)
    study_to_intervention_id = AutoField()

    class Meta:
        table_name = 'study_to_intervention'

class StudyToSponsor(BaseModel):
    sponsor = ForeignKeyField(column_name='sponsor_id', field='sponsor_id', model=Sponsor)
    study = ForeignKeyField(column_name='study_id', field='study_id', model=Study)
    study_to_sponsor_id = AutoField()

    class Meta:
        table_name = 'study_to_sponsor'

