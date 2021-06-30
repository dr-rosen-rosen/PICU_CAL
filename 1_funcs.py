####### python scripts for PICU_CAL

import sqlalchemy as sa
import os
from datetime import datetime, timedelta
import sys
import glob
import pandas as pd
import pytz
from pytz import timezone
from pathlib import Path
import numpy as np
from multiprocessing import Pool, cpu_count
from contextlib import closing

####################################################################################
####################################################################################
################ E4 Scrpits
####################################################################################
####################################################################################

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


def raw_E4_to_db(db_name, db_loc, load_EDA, load_HR, load_ACC, load_Temp, load_IBI, load_BVP, download_path,tracking_file_loc,tracking_file):
    # This takes all data downloaded from Empatica site, and adds it to the project sql db
    # connect to db..
    engine = sa.create_engine(str('sqlite:///')+db_loc+db_name)
    metadata = sa.MetaData(bind=engine)
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
        'EDA': {'TimeStamp':sa.DateTime(),'MicroS':sa.Float()},
        'HR': {'TimeStamp':sa.DateTime(),'AvgHR':sa.Float()},
        'ACC': {'TimeStamp':sa.DateTime(),'x':sa.Integer(),'y':sa.Integer(),'z':sa.Integer()},
        'Temp': {'TimeStamp':sa.DateTime(),'DegreesC':sa.Float()},
        'IBI':{'TimeStamp':sa.DateTime(),'IBI':sa.Float()},
        'BVP':{'TimeStamp':sa.DateTime(),'BVP':sa.Float()}
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
    shift_tracking = pd.read_excel(os.path.join(tracking_file_loc,tracking_file))
    #print(len(shift_tracking))
    #shifts = shift_tracking[shift_tracking.Collected == 1] # Keep only shifts with collected data ready for upload
    shifts = shift_tracking[shift_tracking.e4_in_db != 1] # Keep only those which haven't been uploaded
    #print(len(shift_tracking))
    #shift_tracking.head(10)

    # loads data into db
    #for shift in [ folder for folder in os.listdir(download_path) if os.path.isdir(os.path.join(download_path, folder)) ]:#os.listdir(download_path):
    #    if not shift.startswith(".") and shift not in shifts_imported['shift'].unique():
    shifts = shifts.shift_day.unique()
    for shift in shifts:
        shift_path = os.path.join(download_path,shift,'E4_data')
        if not os.path.isdir(shift_path):
            print('Problem!... no folder for '+shift)
        else:

            print(shift)
            print(pd.to_datetime('now'))
            #shift_tracking.loc[shift_tracking.index[shift_tracking['shift_day'] == shift].tolist()[0],'e4_in_db'] = 1

            for device_data_pull in os.listdir(shift_path):
                if not device_data_pull.startswith(".") and ('_' in device_data_pull):
                    device_data_pull_path = os.path.join(shift_path,device_data_pull)
                    device = device_data_pull.split('_')[1]
                    print(device)
                    try: shift_tracking.loc[((shift_tracking.shift_day == shift) & (shift_tracking.e4_ID == device)),'e4_in_db'] = 1
                    except: print('Problem tracking '+str(device)+' for '+str(shift_day))
                    for measure, value in load_measures.items():
                        if value == True:
                            print(measure)
                            infile = os.path.join(device_data_pull_path,str(measure+'.csv'))
                            if os.path.exists(infile):
                                Table_name = str('Table_'+device+'_'+measure)
                                data = prepE4Data(infile,cols[measure],measHz[measure])
                                data.to_sql(Table_name,con=connection,if_exists='append',dtype=colTypes[measure],chunksize=1000,index=False)
                            else: print('No file to import!!!')
    shift_tracking.to_excel('PICU_Shift_Upload_Tracking_updated.xlsx',index=False)


def get_e4_SH(db_name, db_loc,tracking_file_loc,tracking_file,download_path,load_EDA, load_HR, load_ACC, load_Temp, load_IBI, load_BVP):

    load_measures = {
        'EDA':load_EDA,
        'HR':load_HR,
        'ACC':load_ACC,
        'Temp':load_Temp,
        'IBI':load_IBI,
        'BVP':load_BVP
    }
    # connect to db..
    engine = sa.create_engine(str('sqlite:///')+db_loc+db_name)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    # reads in tracking sheet, and finds shifts with uploaded data, but not a SH file generated
    shift_tracking = pd.read_excel(os.path.join(tracking_file_loc,tracking_file))

    shift_tracking = shift_tracking[shift_tracking.e4_in_db == 1] # Keep only those which have been uploaded
    shift_tracking = shift_tracking[shift_tracking.e4_exported != 1] # Keep only those which have been uploaded

    # Reads in device assignment per shift
    #E4_ids = pd.read_excel(os.path.join(tracking_file_loc,tracking_file),index_col=None)
    #for j, rho in shift_tracking.iterrows():
    #print(rho.Shift)
    for i,row in shift_tracking.iterrows():
        print(row['e4_id'])
        for measure, value in load_measures.items():
            if value == True:
                print(measure)
                Table_name = str('Table_'+row['e4_id']+'_'+measure)
                if sa.inspect(engine).has_table(Table_name): #engine.dialect.has_table(engine,Table_name):
                    currentTable = metadata.tables[Table_name]

                    if row.am_or_pm == 'am':
                        start = row['date'].replace(hour=7,minute=0)
                    elif row.am_or_pm == 'pm':
                        start = row['date'].replace(hour=19,minute=0)
                    else:
                        print('Uh oh... AM or PM Shift?')
                    stop = start + pd.Timedelta(value = 12, unit = 'hours')

                    print(row['date'])
                    print(start)
                    print(stop)

                    s = sa.select([currentTable]).where(currentTable.c.TimeStamp.between(start,stop))
                    rp = connection.execute(s)
                    df = pd.DataFrame(rp.fetchall())
                    if df.empty:
                        print("uh oh.. no data in table!!!")
                    else:
                        df.columns = rp.keys()
                        p = Path(download_path,row.shift_day,'E4_data_processed',row['e4_id'])
                        p.mkdir(parents=True, exist_ok=True)
                        df.to_csv(Path(p,str(measure+'.csv')), index=False)
                else:
                    print('Uh oh... missing table: '+Table_name)
            else:
                print('skipping measure: '+measure)

####################################################################################
####################################################################################
################ RTLS Scrpits
####################################################################################
####################################################################################
def locRecode_izer(df_chunk_pckged):
    '''Takes dataframe chunk and recodes it'''
    df_chunk = df_chunk_pckged[0]
    receiver_dict = df_chunk_pckged[1]
    receiverName_dict = df_chunk_pckged[2]
    df_chunk['Receiver_recode'] = df_chunk['Receiver'].replace(to_replace=receiver_dict, inplace=False)
    df_chunk['Receiver_name'] = df_chunk['Receiver'].replace(to_replace=receiverName_dict,inplace=False)
    df_chunk['Duration'] = (df_chunk.Time_Out - df_chunk.Time_In).astype('timedelta64[s]')/60
    return df_chunk

def locRecode_parallel_izer(func,badge_data,receiver_dict, receiverName_dict, num_processes):
    '''Takes a function and dataframe, chunks up by badge'''
    if num_processes==None:
        num_processes = cpu_count()#min(df.shape[1], cpu_count())
    with closing(Pool(num_processes)) as pool:
        # creates list of data frames for each badge
        df_chunks = [badge_data[badge_data.RTLS_ID == ID].copy() for ID in badge_data.RTLS_ID.unique()]
        df_chunks_pckged = [[df_chunk,receiver_dict,receiverName_dict] for df_chunk in df_chunks] # packages dicts with each data chunk
        results_list = pool.map(func, df_chunks_pckged)
        pool.terminate()
        return pd.concat(results_list)#, axis=1)

def loc_code_badge_data(badge_data, db_name, db_loc):
    #### This recodes a set of worn badges using reciever recodes stored in reciever tables
    ####
    # connect to db
    engine = sa.create_engine('sqlite:///'+db_loc+db_name)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect()
    RTLS_Receivers = metadata.tables['RTLS_Receivers']
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    # get all recievers w/o a location code
    rp = connection.execute(sa.select([RTLS_Receivers]))#.where(RTLS_Receivers.c.Receiver.in_(list(badge_data.Receiver.unique()))))
    receivers = pd.DataFrame(rp.fetchall())
    if receivers.empty:
        print('Problem mapping recievers to badge data.')
        return None
    else:
        receivers.columns = rp.keys()
        receiver_dict = receivers.set_index('Receiver')['LocationCode'].to_dict() #IM_loc_map uses no 'fill' from old coding, but does have NA's
        receiverName_dict = receivers.set_index('Receiver')['ReceiverName'].to_dict()
        num_processes = 4
        df_loc_coded = locRecode_parallel_izer(
            func = locRecode_izer,
            badge_data = badge_data,
            receiver_dict = receiver_dict,
            receiverName_dict = receiverName_dict,
            num_processes = 4
            )
        return df_loc_coded

def get_RTLS(db_loc, db_name, tracking_file_loc,tracking_file):
    # connect to db..
    engine = sa.create_engine(str('sqlite:///')+db_loc+db_name)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    '''
    Read in tracking sheet to get list of shifts ready to pull data'''
    shift_tracking = pd.read_excel(os.path.join(tracking_file_loc,tracking_file))
    shift_tracking = shift_tracking[shift_tracking.rtls_in_db == 1] # keep only shifts with rtls data loaded into the db
    shift_tracking = shift_tracking[shift_tracking.rtls_final_export == 0] # keep only shifts wihout final data pulled

    keeps = ['shift_day','date','rtls_id','am_or_pm']
    badges = shift_tracking[keeps].copy()
    badges['date'] = pd.to_datetime(badges['date'])
    badges.date = badges.date.dt.normalize()

    badges['start'] = np.where(
        badges.am_or_pm == 'am',
        badges.date + pd.Timedelta(7, unit = 'hours'),
        badges.date + pd.Timedelta(19, unit = 'hours'))
    badges['end'] = badges.start + pd.Timedelta(12, unit = 'hours')
    badges['shift_num'] = badges.shift_day.str.rsplit('_',expand=True)[1]
    #badges[badges.am_or_pm == 'pm'].head()

    for shift in badges.shift_day.unique():
        print(len(badges[badges.shift_day == shift]))
        df_list = []
        for i, badge in badges[badges.shift_day == shift].iterrows():
            Table_name = 'Table_'+str(badge.rtls_id)
            if sa.inspect(engine).has_table(Table_name): #engine.dialect.has_table(engine, Table_name):
                RTLS_data = metadata.tables[Table_name]
                s = sa.select([RTLS_data]).where(sa.and_(RTLS_data.c.Time_In >= badge.start,RTLS_data.c.Time_In <= badge.end))
                rp = connection.execute(s)
                badge_df = pd.DataFrame(rp.fetchall())
                if not badge_df.empty:
                    badge_df.columns = rp.keys()
                    badge_df['RTLS_ID'] =  badge.rtls_id
                    df_list.append(badge_df)
                else: print('Missing DATA for badge... '+str(badge))
            else: print('Missing TABLE for badge... '+str(badge))
        if len(df_list) > 0:
            df = pd.concat(df_list)
            df = loc_code_badge_data(badge_data = df,db_name = db_name,db_loc = db_loc)
            df.to_csv(str(shift)+'_RTLS.csv',index=False) #update this to save in appropriate directory
        else: print('No data!!!')
        print(len(df))
