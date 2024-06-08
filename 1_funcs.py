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
from statsmodels.tsa.ar_model import AR, AutoReg
from statsmodels.tsa.stattools import acf

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
    shifts = shift_tracking[shift_tracking.e4_in_db != 1] # Keep only those which haven't been uploaded
    shifts = shifts.shift_day.unique()
    print(shifts)
    # loads data into db
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
                    except: print('Problem tracking '+str(device)+' for '+str(shift))
                    for measure, value in load_measures.items():
                        if value == True:
                            print(measure)
                            infile = os.path.join(device_data_pull_path,str(measure+'.csv'))
                            if os.path.exists(infile):
                                Table_name = str('Table_'+device+'_'+measure)
                                data = prepE4Data(infile,cols[measure],measHz[measure])
                                data.to_sql(Table_name,con=connection,if_exists='append',dtype=colTypes[measure],chunksize=1000,index=False)
                            else: print('No file to import!!!')

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
################ RTLS Scrpits for PG db
####################################################################################
####################################################################################
# def locRecode_izer(df_chunk_pckged):
#     '''Takes dataframe chunk and recodes it'''
#     df_chunk = df_chunk_pckged[0]
#     receiver_dict = df_chunk_pckged[1]
#     receiverName_dict = df_chunk_pckged[2]
#     df_chunk['Receiver_recode'] = df_chunk['Receiver'].replace(to_replace=receiver_dict, inplace=False)
#     df_chunk['Receiver_name'] = df_chunk['Receiver'].replace(to_replace=receiverName_dict,inplace=False)
#     df_chunk['Duration'] = (df_chunk.Time_Out - df_chunk.Time_In).astype('timedelta64[s]')/60
#     return df_chunk
#
# def locRecode_parallel_izer(func,badge_data,receiver_dict, receiverName_dict, num_processes):
#     '''Takes a function and dataframe, chunks up by badge'''
#     if num_processes==None:
#         num_processes = cpu_count()#min(df.shape[1], cpu_count())
#     with closing(Pool(num_processes)) as pool:
#         # creates list of data frames for each badge
#         df_chunks = [badge_data[badge_data.RTLS_ID == ID].copy() for ID in badge_data.RTLS_ID.unique()]
#         df_chunks_pckged = [[df_chunk,receiver_dict,receiverName_dict] for df_chunk in df_chunks] # packages dicts with each data chunk
#         results_list = pool.map(func, df_chunks_pckged)
#         pool.terminate()
#         return pd.concat(results_list)#, axis=1)

def loc_code_badge_data_pg(badge_data, db_u, db_pw):
    #### This recodes a set of worn badges using reciever recodes stored in reciever tables
    ####
    # connect to db
    df_string = 'postgresql://'+db_u+':'+db_pw+'@localhost:5433/rtls_jhh' # Format for ps string: dialect+driver://username:password@host:port/database
    engine = sa.create_engine(df_string)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
    RTLS_Receivers = metadata.tables['rtls_receivers']

    # get all recievers w/o a location code
    rp = connection.execute(sa.select([RTLS_Receivers]))#.where(RTLS_Receivers.c.Receiver.in_(list(badge_data.Receiver.unique()))))
    receivers = pd.DataFrame(rp.fetchall())
    if receivers.empty:
        print('Problem mapping recievers to badge data.')
        return None
    else:
        receivers.columns = rp.keys()
        receiver_dict = receivers.set_index('receiver')['location_code'].to_dict() #IM_loc_map uses no 'fill' from old coding, but does have NA's
        receiverName_dict = receivers.set_index('receiver')['receiver_name'].to_dict()
        num_processes = 4
        df_loc_coded = locRecode_parallel_izer(
            func = locRecode_izer,
            badge_data = badge_data,
            receiver_dict = receiver_dict,
            receiverName_dict = receiverName_dict,
            num_processes = 4
            )
        return df_loc_coded

def get_RTLS_pg(db_u, db_pw, tracking_file_loc,tracking_file, save_shifts):
    # connect to db..
    df_string = 'postgresql://'+db_u+':'+db_pw+'@localhost:5433/rtls_jhh' # Format for ps string: dialect+driver://username:password@host:port/database
    engine = sa.create_engine(df_string)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
    #insp = inspect(engine)
    '''
    Read in tracking sheet to get list of shifts ready to pull data'''
    shift_tracking = pd.read_excel(os.path.join(tracking_file_loc,tracking_file))
    shift_tracking = shift_tracking[shift_tracking.rtls_in_db == 1] # keep only shifts with rtls data loaded into the db
    shift_tracking = shift_tracking[shift_tracking.rtls_final_export != 1] # keep only shifts wihout final data pulled

    keeps = ['shift_day','date','rtls_id','am_or_pm']
    badges = shift_tracking[keeps].copy()
    badges['date'] = pd.to_datetime(badges['date'])
    badges.date = badges.date.dt.normalize()

    badges['start'] = np.where(
        badges.am_or_pm == 'am',
        badges.date + pd.Timedelta(7, unit = 'hours'),
        badges.date + pd.Timedelta(19, unit = 'hours'))
    badges['end'] = badges.start + pd.Timedelta(12, unit = 'hours')
    #badges['shift_num'] = badges.shift_day.str.rsplit('_',expand=True)[1] # I think this should be dropped; not used anywhere
    #badges[badges.am_or_pm == 'pm'].head()
    shift_list = []
    for shift in badges.shift_day.unique():
        print(len(badges[badges.shift_day == shift]))
        df_list = []
        for i, badge in badges[badges.shift_day == shift].iterrows():
            Table_name = 'table_'+str(badge.rtls_id)
            if sa.inspect(engine).has_table(Table_name): #engine.dialect.has_table(engine, Table_name):
                RTLS_data = metadata.tables[Table_name]
                s = sa.select([RTLS_data]).where(sa.and_(RTLS_data.c.time_in >= badge.start,RTLS_data.c.time_in <= badge.end))
                rp = connection.execute(s)
                badge_df = pd.DataFrame(rp.fetchall())
                if not badge_df.empty:
                    badge_df.columns = rp.keys()
                    badge_df['rtls_id'] =  badge.rtls_id
                    df_list.append(badge_df)
                else: print('Missing DATA for badge... '+str(badge))
            else: print('Missing TABLE for badge... '+str(badge))
        if len(df_list) > 0:
            df = pd.concat(df_list)
            df = loc_code_badge_data_pg(badge_data = df,db_u = db_u,db_pw = db_pw)
            if save_shifts:
                df.to_csv(os.path.join(tracking_file_loc,shift,'RTLS_data',str(shift)+'_RTLS.csv'),index=False) #update this to save in appropriate directory
            df['shift'] = shift
            shift_list.append(df)
        else: print('No data!!!')
        #print(len(df))
    return(pd.concat(shift_list))

####################################################################################
####################################################################################
################ OLD RTLS Scrpits for sqlite db
####################################################################################
####################################################################################
def locRecode_izer(df_chunk_pckged):
    '''Takes dataframe chunk and recodes it'''
    df_chunk = df_chunk_pckged[0]
    receiver_dict = df_chunk_pckged[1]
    receiverName_dict = df_chunk_pckged[2]
    df_chunk['Receiver_recode'] = df_chunk['receiver'].replace(to_replace=receiver_dict, inplace=False)
    df_chunk['Receiver_name'] = df_chunk['receiver'].replace(to_replace=receiverName_dict,inplace=False)
    df_chunk['Duration'] = (df_chunk.time_out - df_chunk.time_in).astype('timedelta64[s]')/60
    return df_chunk

def locRecode_parallel_izer(func,badge_data,receiver_dict, receiverName_dict, num_processes):
    '''Takes a function and dataframe, chunks up by badge'''
    if num_processes==None:
        num_processes = cpu_count()#min(df.shape[1], cpu_count())
    with closing(Pool(num_processes)) as pool:
        # creates list of data frames for each badge
        df_chunks = [badge_data[badge_data.rtls_id == ID].copy() for ID in badge_data.rtls_id.unique()]
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
    RTLS_Receivers = metadata.tables['rtls_receivers']
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    # get all recievers w/o a location code
    rp = connection.execute(sa.select([rtls_receivers]))#.where(RTLS_Receivers.c.Receiver.in_(list(badge_data.Receiver.unique()))))
    receivers = pd.DataFrame(rp.fetchall())
    if receivers.empty:
        print('Problem mapping recievers to badge data.')
        return None
    else:
        receivers.columns = rp.keys()
        receiver_dict = receivers.set_index('receiver')['LocationCode'].to_dict() #IM_loc_map uses no 'fill' from old coding, but does have NA's
        receiverName_dict = receivers.set_index('receiver')['ReceiverName'].to_dict()
        num_processes = 4
        df_loc_coded = locRecode_parallel_izer(
            func = locRecode_izer,
            badge_data = badge_data,
            receiver_dict = receiver_dict,
            receiverName_dict = receiverName_dict,
            num_processes = 4
            )
        return df_loc_coded

def get_RTLS(db_loc, db_name, tracking_file_loc,tracking_file, save_shifts):
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
    shift_tracking = shift_tracking[shift_tracking.rtls_final_export != 1] # keep only shifts wihout final data pulled

    keeps = ['shift_day','date','rtls_id','am_or_pm']
    badges = shift_tracking[keeps].copy()
    badges['date'] = pd.to_datetime(badges['date'])
    badges.date = badges.date.dt.normalize()

    badges['start'] = np.where(
        badges.am_or_pm == 'am',
        badges.date + pd.Timedelta(7, unit = 'hours'),
        badges.date + pd.Timedelta(19, unit = 'hours'))
    badges['end'] = badges.start + pd.Timedelta(12, unit = 'hours')
    #badges['shift_num'] = badges.shift_day.str.rsplit('_',expand=True)[1] # I think this should be dropped; not used anywhere
    #badges[badges.am_or_pm == 'pm'].head()
    shift_list = []
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
                    badge_df['rtls_id'] =  badge.rtls_id
                    df_list.append(badge_df)
                else: print('Missing DATA for badge... '+str(badge))
            else: print('Missing TABLE for badge... '+str(badge))
        if len(df_list) > 0:
            df = pd.concat(df_list)
            df = loc_code_badge_data(badge_data = df,db_name = db_name,db_loc = db_loc)
            if save_shifts:
                df.to_csv(os.path.join(tracking_file_loc,shift,'RTLS_data',str(shift)+'_RTLS.csv'),index=False) #update this to save in appropriate directory
            df['Shift'] = shift
            shift_list.append(df)
        else: print('No data!!!')
        print(len(df))
    return(pd.concat(shift_list))

####################################################################################################
############################## Helper functions for network data manipulation
####################################################################################################

def relabel_nodes(df, nodes):
    node_dict = dict(zip(nodes.rec_num,nodes.id))
    df['receiver'] = df['receiver'].map(node_dict)
    return(df)

####################################################################################
####################################################################################
################ E4 Synchrony Scrpits
####################################################################################
####################################################################################

### Creates energy metric for ACC data
def create_ACC_energy_metric(E4_data):
    # convert x, y , z to energy metric
    # consider 10.1371/journal.pone.0160644
    dimensions = ['x','y','z']
    for dimension in dimensions:
        E4_data[dimension] = E4_data[dimension].apply(lambda x: np.square(x))
    E4_data['energy'] = E4_data[dimensions].sum(axis=1)
    E4_data['energy'] = E4_data['energy']**(1/2)
    E4_data.drop(columns=dimensions,inplace=True)
    return(E4_data)

def get_e4_data_for_sync(e4_ids, shift_start, shift_end, measure, sampling_freq):

    #connect to db... This is slow (reconnects for each person / shift; but this is all just workaround)
    db_loc = "/Users/mrosen44/Documents/Data_Analysis_Local/PICU_CAL/data/"
    db_name = 'PICU_E4_data.db'
    engine = sa.create_engine(str('sqlite:///')+db_loc+db_name)
    metadata = sa.MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    E4_tab = pd.DataFrame(data = None) # One dataframe for all of the E4 data for specific task
    df_list = []
    all_E4s_inData = True # flag used to stop synch calculations if there is missing E4 data for a task

    for e4 in e4_ids:
        t_name = 'Table_'+e4+'_'+measure
        print(t_name)
        try:
            t = metadata.tables[t_name]
            s = sa.select([t]).where(sa.and_(t.c.TimeStamp >= shift_start,t.c.TimeStamp <= shift_end))
            rp = connection.execute(s)
            data = pd.DataFrame(rp.fetchall())
        except:
            print('Problem with DB for... '+t_name)
            all_E4s_inData = False # will stop further synch calculations below
        if data.empty:
            print('No data in DB...'+t_name)
            all_E4s_inData = False
        else:
            data.columns = rp.keys()
            data.TimeStamp = pd.to_datetime(data.TimeStamp)
            # data.TimeStamp = data.TimeStamp.dt.tz_localize(pytz.timezone('US/Eastern')) # E4 timestamps are stored in utc
            data.set_index('TimeStamp', inplace = True)

            ### Creates energy metric for ACC data
            if measure == 'ACC':
                data = create_ACC_energy_metric(data)
            else: pass

            ### resamples to the set sampling frequency (in config file)
            data = data.resample(sampling_freq).mean()

            ### renames data column to device id
            data.columns = [e4]
            ### adds E4_data for one role to the overall E4_tab dataframe
            if E4_tab.empty:
                E4_tab = data.copy()
            else:
                E4_tab = E4_tab.merge(data, on = 'TimeStamp', how = 'outer')
    pre_len = len(E4_tab)
    E4_tab = E4_tab.dropna(axis = 'index', how = 'any')
    if E4_tab.empty:
        all_E4s_inData = False
        prop_missing = 1
    else:
        prop_missing = 1 - (len(E4_tab)/pre_len)
    print("Proportion missing... "+str(prop_missing))
    return(E4_tab, all_E4s_inData,prop_missing)

def time_blockify(time_period, abs_shift_start):
    time_block_offsets = {
        'all_shift':{'start':timedelta(hours = 0), 'end':timedelta(hours = 12)},
        'block_1':{'start':timedelta(hours = 0), 'end':timedelta(hours = 4)},
        'block_2':{'start':timedelta(hours = 4), 'end':timedelta(hours = 8)},
        'block_3':{'start':timedelta(hours = 8), 'end':timedelta(hours = 12)}
    }
    shift_start = abs_shift_start + time_block_offsets[time_period]['start']
    shift_end = abs_shift_start + time_block_offsets[time_period]['end']
    return(shift_start, shift_end)

def get_sync_coef_py(e4_data, offset, use_residuals, corr_method, sampling_freq):
    working_roles = list(e4_data.columns.values)

    Sync_Coefs = pd.DataFrame(index=working_roles,columns=working_roles,data=None)
    ### Creates Table 1 in Guastello and Perisini
    for from_role in working_roles: # from roles are rows of sync_coef matrix from guastello
        ### calculate and store autocorrelation
        # print(from_role)
        # print(len(e4_data[from_role]))
        Sync_Coefs.loc[from_role,from_role] = acf(e4_data[from_role],fft = False, nlags=offset)[offset]
        to_roles = [role for role in working_roles if role != from_role] # gets all other roles
        for to_role in to_roles:
            E4_temp = e4_data.copy()
            E4_temp.dropna(axis=0,inplace=True,how='any') # Need to be more careful about missing data
            E4_temp = E4_temp.asfreq(freq=sampling_freq)
            # gets residuals from acf (controls for autocorrelation)... maybe better way to do this???
            if use_residuals:
                print('using residuals!')
                to_residuals = AutoReg(E4_temp[to_role], lags = [offset]).fit().resid
                to_residuals = pd.DataFrame({'TimeStamp':to_residuals.index, to_role:to_residuals.values})
                to_residuals.set_index('TimeStamp', inplace = True)
                E4_temp.drop(columns=to_role, inplace = True)
                E4_temp = E4_temp.merge(to_residuals, on = 'TimeStamp', how = 'outer')
            else: pass
            E4_temp[to_role] = E4_temp[to_role].shift(periods=(offset*-1),freq=sampling_freq)
            E4_temp.dropna(axis=0,inplace=True,how='any')
            '''
            HEY! below where it says .corr(method=corr_method) is where you can plug in any python function that takes two arrays
            and returns a float. Right now it's set above and does a pearson corrleation. There may be better ways to do this all around,
            but if you believe the docs the .corr method will take any function you define.
            '''
            coef_matrix = E4_temp[[from_role,to_role]].corr(method=corr_method) # RIGHT HERE!!!
            Sync_Coefs.loc[from_role,to_role] = coef_matrix.loc[from_role,to_role]
    return(Sync_Coefs, working_roles)

def update_sync_metrics(Sync_df, Sync_Coefs, working_roles, measure, shift_num, time_period):
    Sync_Coefs_sq = np.square(Sync_Coefs)
    highest_empath = ()
    for role in working_roles:
        e_score = Sync_Coefs_sq[role].sum() #empath scores
        d_score = Sync_Coefs_sq.loc[role,working_roles].sum(axis=0) #driver scores
        Sync_df = Sync_df.append({
                        'shift': shift_num,
                        'part_id': role,
                        'time_period': time_period,
                        'measure': measure,
                        'metric': 'AR',
                        'value': Sync_Coefs_sq.loc[role,role]
                        }, ignore_index = True)
        Sync_df = Sync_df.append({
                            'shift': shift_num,
                            'part_id': role,
                            'time_period': time_period,
                            'measure': measure,
                            'metric': 'Empath',
                            'value': e_score
                            }, ignore_index = True)
        Sync_df = Sync_df.append({
                            'shift': shift_num,
                            'part_id': role,
                            'time_period': time_period,
                            'measure': measure,
                            'metric': 'Driver',
                            'value': d_score
                            }, ignore_index = True)
        if not highest_empath:
            highest_empath = (role,e_score)
        elif highest_empath[1] < e_score:
            highest_empath = (role,e_score)
        else: pass

    # calcluates Se score
    if highest_empath:
        empath = highest_empath[0]
        V_prime = Sync_Coefs_sq[empath].copy()
        V_prime.drop(index=empath,inplace=True)
        M = Sync_Coefs_sq.drop(columns=empath)
        M.drop(index=empath,inplace=True)
        if not M.isnull().values.any(): #skips if there is missing info.. need to figure out why it would get here and be empty
            M1 = M.astype('float',copy=True)
            M_inv = pd.DataFrame(data = np.linalg.pinv(M1.values),columns=M1.columns,index=M1.index)
            Q = M_inv.dot(V_prime)
            Se = V_prime.dot(Q)
            print(V_prime.dot(Q))
        else:
            Se = 99
            print('No getting to Se calc..')
            print(M)
    else: print('no highest empath?')
    return(Sync_df, Se)

def update_shift_metrics(Shift_metric_df, Se, prop_missing, measure, shift_num, time_period):
    Shift_metric_df = Shift_metric_df.append({
        'shift': shift_num,
        'time_period': time_period,
        'measure': measure,
        'prop_missing': prop_missing,
        'Se': Se
    }, ignore_index=True)
    return(Shift_metric_df)

def get_synchronies_py(shift_df,shift_num, measure, sync_metrics, sync_sampling_freq, sync_offset, sync_use_residuals, sync_corr_method):
    shift_df['study_member_id'] = shift_df['study_member_id'].astype('int')
    e4_ids = shift_df.e4_id.unique()
    shift_num = shift_num.shift_day.unique()[0]
    abs_shift_start = pd.Timestamp(shift_df.date.unique()[0]) #.tz_localize(pytz.timezone('US/Eastern')) # 'absolute' shift time since it is modified in the loop; this stays constant
    am_or_pm = shift_df.am_or_pm.unique()[0]
    if am_or_pm == 'am':
        abs_shift_start = abs_shift_start.replace(hour=7)
    elif am_or_pm == 'pm':
        abs_shift_start = abs_shift_start.replace(hour=19)
    print(abs_shift_start)
    cols = ['shift','part_id','time_period','measure','metric','value']
    Ind_Sync_df = pd.DataFrame(columns=cols,data=None,index=None)
    cols = ['shift','time_period','measure','prop_missing','Se']
    Shift_metric_df = pd.DataFrame(columns=cols,data=None,index=None)
    for time_period in ['all_shift','block_1', 'block_2', 'block_3']:
        shift_start, shift_end = time_blockify(time_period = time_period, abs_shift_start = abs_shift_start)
        e4_data, all_E4s_inData, prop_missing = get_e4_data_for_sync(e4_ids = e4_ids, shift_start = shift_start, shift_end = shift_end, measure = measure, sampling_freq = sync_sampling_freq)
        # e4id_to_pid = dict(zip(shift_df.e4_id,shift_df.study_member_id))
        # e4_data.rename(columns = e4id_to_pid, inplace = True)
        print(shift_start)
        print(shift_end)
        if all_E4s_inData & (prop_missing < .7):
            Sync_Coefs, working_roles = get_sync_coef_py(e4_data = e4_data, offset = sync_offset,
                                        use_residuals = sync_use_residuals, corr_method = sync_corr_method,
                                        sampling_freq = sync_sampling_freq)
            Ind_Sync_df, Se = update_sync_metrics(Sync_df = Ind_Sync_df, Sync_Coefs = Sync_Coefs, working_roles = working_roles,
                                        measure = measure, shift_num = shift_num, time_period = time_period)
            Shift_metric_df = update_shift_metrics(Shift_metric_df = Shift_metric_df, Se = Se, prop_missing = prop_missing, measure = measure, shift_num = shift_num, time_period = time_period)
        else: pass
    return({'ind_data':Ind_Sync_df,'shift_data':Shift_metric_df})

### This was written because R does not deal well with filtering timestamp in sqlite dbs.
### We can likely get rid of this (and just use dbplyr in parent R script) once we come up with
### a better db solution for all of this (postgress housed somwehere accessible to everyone)
# def pull_e4_data_py(t_name,shift_start,shift_stop):
#     # format timestamps
#     # they were not being passed from R cleanly. They were showing up as ndarray with one item.
#     # hence the [0] indexing to get the actual timestamp. The timezone info was not coming through either.
#     # hence setting them to utc and converting to eastern.
#     #print(shift_start)
#     shift_start = pd.Timestamp(shift_start[0]).tz_localize(pytz.timezone('UTC')).tz_convert(pytz.timezone('US/Eastern')) # for some reason, the times are comign in as nd.arrays
#     #print(shift_start)
#     #print(shift_stop)
#     shift_stop = pd.Timestamp(shift_stop[0]).tz_localize(pytz.timezone('UTC')).tz_convert(pytz.timezone('US/Eastern')) # for some reason, the times are comign in as nd.arrays
#     #print(shift_stop)
#     # connect to db... This is slow (reconnects for each person / shift; but this is all just workaround)
#     db_loc = "/Users/mrosen44/Documents/Data_Analysis_Local/PICU_CAL/data/"
#     db_name = 'PICU_E4_data.db'
#     engine = sa.create_engine(str('sqlite:///')+db_loc+db_name)
#     metadata = sa.MetaData(bind=engine)
#     metadata.reflect()
#     connection = engine.connect()
#     connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
#
#     t = metadata.tables[t_name]
#     s = sa.select([t]).where(sa.and_(t.c.TimeStamp >= shift_start,t.c.TimeStamp <= shift_stop))
#     rp = connection.execute(s)
#     data = pd.DataFrame(rp.fetchall())
#     if not data.empty:
#         data.columns = rp.keys()
#     return(data)
