####### python scripts for PICU_CAL
# from sqlalchemy import (UniqueConstraint,MetaData,Table,Column,Integer,Numeric,Float,Boolean,String,DateTime,ForeignKey,create_engine,select,update,delete,insert,and_)
# from sqlalchemy_utils.functions import create_database
# from sqlalchemy import inspect
# from sqlalchemy import func
import sqlalchemy as sa
import os
from datetime import datetime, timedelta
import sys
import glob
import pandas as pd
import pytz
from pytz import timezone
from pathlib import Path

def prepE4Data(in_file,cols,Hz):#,to_datetime,read_csv,date_range,pytz):
    if 'IBI' in cols:
        with open(in_file,'r') as InFile:
            start = pd.to_datetime(InFile.readline().split(',')[0],unit='s').tz_localize(pytz.timezone('UTC')).tz_convert(pytz.timezone('US/Eastern'))
            E4_dump = pd.read_csv(InFile,header=None,skiprows=1,encoding='',sep=',',index_col=None)
            E4_dump.columns = ['offset','IBI']
            #from datetime import timedelta
            E4_dump['TimeStamp'] = E4_dump.offset.apply(lambda x: start + timedelta(seconds=x))
            E4_dump.drop(columns='offset',inplace=True)
    else:
        with open(in_file,'r') as InFile:
            start = InFile.readline()
            E4_dump = pd.read_csv(InFile,header=None,skiprows=1,encoding='',sep=',',index_col=None)
            start = pd.to_datetime(int(start.split('.')[0])*1000000000).tz_localize(pytz.timezone('UTC')).tz_convert(pytz.timezone('US/Eastern'))
            E4_dump['TimeStamp'] = pd.date_range(start=start,periods=len(E4_dump),freq=Hz)
            newCols = cols + ['TimeStamp']
            E4_dump.columns = newCols
    return E4_dump


def raw_E4_to_db(db_name, db_loc, load_EDA, load_HR, load_ACC, load_Temp, load_IBI, load_BVP, download_path,shift_tracking_file):
    # This takes all data downloaded from Empatica site, and adds it to the project sql db
    # connect to db..
    engine = create_engine(str('sqlite:///')+db_loc+db_name)
    metadata = MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    # which measures to import
    load_measures = {
        'EDA':load_EDA,
        'HR':load_HR,
        'ACC':load_ACC,
        'Temp':load_Temp,
        'IBI':load_IBI,
        'BVP':load_BVP
    }

    # set up data structures
    measHz = {
        'EDA':'250L', # = 4 Hz
        'HR':'1s',#1Hz
        'ACC':'31250U', # this is 32 Hz; U is microseconds
        'Temp':'250L', #4Hz
        'IBI':None, # IBI is not regularly sampled
        'BVP':'15625U'# 64 Hz
    }

    colTypes = {
        'EDA': {'TimeStamp':DateTime(),'MicroS':Float()},
        'HR': {'TimeStamp':DateTime(),'AvgHR':Float()},
        'ACC': {'TimeStamp':DateTime(),'x':Integer(),'y':Integer(),'z':Integer()},
        'Temp': {'TimeStamp':DateTime(),'DegreesC':Float()},
        'IBI':{'TimeStamp':DateTime(),'IBI':Float()},
        'BVP':{'TimeStamp':DateTime(),'BVP':Float()}
    }

    cols = {
        'EDA': ['MicroS'],
        'HR': ['AvgHR'],
        'ACC': ['x','y','z'],
        'Temp': ['DegreesC'],
        'IBI':['IBI'],
        'BVP':['BVP']
    }

    ### Import tracking sheet with three colums: Shift name, collected (0/1... meaning is it ready to upload or not),
    ### and uploaded (0/1... meaning has it been uploaded into the db yet)
    shift_tracking = pd.read_csv(shift_tracking_file)
    #print(len(shift_tracking))
    shifts = shift_tracking[shift_tracking.Collected == 1] # Keep only shifts with collected data ready for upload
    shifts = shift_tracking[shift_tracking.Uploaded == 0] # Keep only those which haven't been uploaded
    #print(len(shift_tracking))
    #shift_tracking.head(10)

    # loads data into db
    #for shift in [ folder for folder in os.listdir(download_path) if os.path.isdir(os.path.join(download_path, folder)) ]:#os.listdir(download_path):
    #    if not shift.startswith(".") and shift not in shifts_imported['shift'].unique():
    shifts = shifts.Shift.unique()
    for shift in shifts:
        shift_path = os.path.join(download_path,shift,'E4_data')
        if not os.path.isdir(shift_path):
            print('Problem!... no folder for '+shift)
        else:

            print(shift)
            print(pd.to_datetime('now'))
            shift_tracking.loc[shift_tracking.index[shift_tracking['Shift'] == shift].tolist()[0],'Uploaded'] = 1

            for device_data_pull in os.listdir(shift_path):
                if not device_data_pull.startswith(".") and ('_' in device_data_pull):
                    device_data_pull_path = os.path.join(shift_path,device_data_pull)
                    device = device_data_pull.split('_')[1]
                    print(device)
                    for measure, value in load_measures.items():
                        if value == True:
                            print(measure)
                            infile = os.path.join(device_data_pull_path,str(measure+'.csv'))
                            if os.path.exists(infile):
                                Table_name = str('Table_'+device+'_'+measure)
                                data = prepE4Data(infile,cols[measure],measHz[measure],pd.to_datetime,pd.read_csv,pd.date_range,pytz)
                                data.to_sql(Table_name,con=connection,if_exists='append',dtype=colTypes[measure],chunksize=1000,index=False)
                            else: print('No file to import!!!')
            shift_tracking.to_csv('PICU_Shift_Upload_Tracking.csv',index=False)
