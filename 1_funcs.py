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
    badges['shift_num'] = badges.shift_day.str.rsplit('_',expand=True)[1] # I think this should be dropped; not used anywhere
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
                    badge_df['RTLS_ID'] =  badge.rtls_id
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
    df['Receiver'] = df['Receiver'].map(node_dict)
    return(df)

####################################################################################
####################################################################################
################ E4 Synchrony Scrpits
####################################################################################
####################################################################################

# def get_synchronies(tracking_df):
#     '''1. Set up data structures and control variables'''
#
#     # define roles for the mission / shift
#     '''CURRENTLY ASSUMING THERE IS ONLY ONE SHIFT'''
    # tracking_df.study_member_id = tracking_df.study_member_id.astype('int').astype('str') # making sure they are str type and not floats
    # roles = tracking_df.study_member_id.unique() # pulls all participaint id's for shift
    #
    # '''ALL OF THESE SHOULD BE SET ELSEWHERE... CONFIG FILE?'''
    # measures = ['EDA','HR'] # ,'ACC'for IBI the loop below needs modification... likely upsample to a minute
    # # set lag length
    # offset = 50 # in seconds
    # # set frequency for resampling and to pass to AR
    # sampling_freq = 'S' # need to turn this into a dict to match resampling to specific measures
    # corr_method = 'pearson'
    # use_residuals = True # does cross-corrleation on residuals of autocorrelation...
    # # set missing vals
    # # missing_E4_flag = 'M'
    # # NA_E4_flag = 'NA'
    #
    # '''MOVE TO ONEDRIVE... use single DB. Onedrive limit is now 100GB; we should be ok'''
    # #db_path = os.path.join(os.getcwd(),'HERA_DBs')
    #
    # # Set up dataframe to store results; creates one dataframe for all loops
    # if len(tracking_df.shift_day.unique()) == 1:
    #     cols = ['Shift', 'Participant_ID', 'Time_period']
    #     for measure in measures:
    #         cols.append('Se_'+measure)
    #         for role in roles:
    #             cols.append(role+'_Driver_'+measure)
    #             cols.append(role+'_Empath_'+measure)
    #             cols.append(role+'_AR_'+measure)
    #     Sync_df = pd.DataFrame(columns=cols,data=None,index=range(0,3))
    # else: print('Uh oh... tracking df has more than one shift in it.')
    # Role_E4_dict = tracking_df.set_index('study_member_id').to_dict()['e4_id']
    # print(Role_E4_dict)
    # '''2. Iterate through each task in the task dataframe, create Table 1 measures from Guastello and Peressini:'''
    # for measure in measures:
    #     Se = 'Se_'+measure
    #     if am_shift:
    #         start =
    #
    # return Sync_df

# for measure in measures:
#     '''2. Iterate through each task in the task dataframe, create Table 1 measures from Guastello and Peressini:'''
#     Se = 'Se_'+measure
#     for i, row in tasks_df.iterrows():
#         E4_ids =  [x for x in row[roles].values if str(x) != 'nan'] # Drops any roles that have no E4 ID
#         E4_ids = [x for x in E4_ids if str(x) != NA_E4_flag]
#         Role_E4_dict = row[roles].to_dict() # Creates dict for relabeling roles and device IDs
#
#         # runs for all tasks with a dyad or greater with no missing E4 data
#         if (len(E4_ids) <= 1):
#             print('Task List: Too few E4s..')
#             pass
#         elif (any(missing_E4_flag in x for x in E4_ids)):
#             print('Task List: Missing E4 data...')
#             pass
#         elif (any(';' in x for x in E4_ids)) or (any('(' in x for x in E4_ids)) or (any(':' in x for x in E4_ids)): #need to expand this to make it measure specific for NEDA or NBVP
#             print('Task List: Incomplete E4 data...')
#             pass
#         else:
#             E4_tab = pd.DataFrame(data = None)
#             all_E4s_inData = True
#             for E4 in E4_ids:
#                 # this for loop builds dataframe with timestamp as index, device id's as column names, and measure as values
#                 # Pulls data from DB
#                 try:
#                     connection, metadata = db_connection(db_path,row.Team,row.Mission_day)
#                     t_name = str('Table_'+E4+'_'+measure)
#                     t = metadata.tables[t_name]
#                     s = select([t]).where((t.c.TimeStamp >= row['Start Time']) & (t.c.TimeStamp <= row['Stop Time']))
#                     rp = connection.execute(s)
#                     E4_data = pd.DataFrame(rp.fetchall())
#                 except:
#                     E4_data.drop(E4_data.index, inplace=True) # gets rid of data from previous cycle through loop if pulling data fails
#                     print('Problem with DB for '+t_name+' in '+str(row.Team)+' on '+str(row.Mission_day))
#
#                 if E4_data.empty:
#                     print('No data in DB...')
#                     all_E4s_inData = False
#                 else:
#                     E4_data.columns = rp.keys()
#
#                     # Set TZ for timestamps
#                     E4_data.TimeStamp = pd.to_datetime(E4_data.TimeStamp)
#                     E4_data.TimeStamp = E4_data.TimeStamp.dt.tz_localize(pytz.timezone('UTC')) # E4 timestamps are stored in utc
#                     E4_data.set_index('TimeStamp', inplace = True)
#
#                     if measure == 'ACC':
#                         # convert x, y , z to energy metric
#                         # consider 10.1371/journal.pone.0160644
#                         dimensions = ['x','y','z']
#                         for dimension in dimensions:
#                             E4_data[dimension] = E4_data[dimension].apply(lambda x: np.square(x))
#                         E4_data['energy'] = E4_data[dimensions].sum(axis=1)
#                         E4_data['energy'] = E4_data['energy']**(1/2)
#                         E4_data.drop(columns=dimensions,inplace=True)
#                     else: pass
#
#                     E4_data = E4_data.resample(sampling_freq).mean()
#
#                     # renames data column to device id
#                     E4_data.columns = [E4]
#
#                     if E4_tab.empty:
#                         E4_tab = E4_data.copy()
#                     else:
#                         E4_tab = E4_tab.merge(E4_data, on = 'TimeStamp', how = 'outer')
#
#             if all_E4s_inData:
#                 # rename columns as roles instead of device IDs
#                 E4_tab.columns.name = None
#                 Role_to_cols = {v: k for k, v in Role_E4_dict.items()}
#                 E4_tab.rename(columns=Role_to_cols,inplace=True)
#
#
#                 working_roles = list(E4_tab.columns.values) # for only processing roles present in the data
#                 Sync_Coefs = pd.DataFrame(index=working_roles,columns=working_roles,data=None)
#
#                 #Creates Table 1 in Guastello and Perisini
#                 for from_role in working_roles: # from roles are rows of sync_coef matrix from guastello
#
#                     # calculate and store autocorrelation
#                     Sync_Coefs.loc[from_role,from_role] = acf(E4_tab[from_role],unbiased=True, nlags=offset)[offset]
#
#                     to_roles = [role for role in working_roles if role != from_role] # gets all other roles
#                     for to_role in to_roles:
#                         E4_temp = E4_tab.copy()
#
#                         # gets residuals from acf (controls for autocorrelation)... maybe better way to do this???
#                         E4_temp.dropna(axis=0,inplace=True,how='any')
#                         E4_temp = E4_temp.asfreq(freq=sampling_freq)
#                         if use_residuals:
#                             to_residuals =  AutoReg(E4_temp[to_role], lags = [offset]).fit().resid
#                             to_residuals = pd.DataFrame({'TimeStamp':to_residuals.index, to_role:to_residuals.values})
#                             to_residuals.set_index('TimeStamp', inplace = True)
#                             E4_temp.drop(columns=to_role, inplace = True)
#                             E4_temp = E4_temp.merge(to_residuals, on = 'TimeStamp', how = 'outer')
#                         else: pass
#
#                         E4_temp[to_role] = E4_temp[to_role].tshift(periods=(offset*-1),freq=sampling_freq)
#                         E4_temp.dropna(axis=0,inplace=True,how='any')
#
#                         '''
#                         HEY! below where it says .corr(method=corr_method) is where you can plug in any python function that takes two arrays
#                         and returns a float. Right now it's set above and does a pearson corrleation. There may be better ways to do this all around,
#                         but if you believe the docs the .corr method will take any function you define.
#                         '''
#
#                         coef_matrix = E4_temp[[from_role,to_role]].corr(method=corr_method) # RIGHT HERE!!!
#                         Sync_Coefs.loc[from_role,to_role] = coef_matrix.loc[from_role,to_role]
#
#
#                 # saves AR, driver and empath scores
#                 Sync_df.loc[i,'Task_num'] = row['Task_num']
#                 print(row['Task_num'])
#                 highest_empath = ()
#                 Sync_Coefs_sq = np.square(Sync_Coefs) # added to
#                 for role in working_roles:
#
#                     Sync_df.loc[i,str(role+'_AR_'+measure)] = Sync_Coefs.loc[role,role]
#                     #Sync_Coefs = np.square(Sync_Coefs)
#                     e_score = Sync_Coefs_sq[role].sum() #empath scores
#                     Sync_df.loc[i,str(role+'_Empath_'+measure)] = e_score
#                     if not highest_empath:
#                         highest_empath = (role,e_score)
#                     elif highest_empath[1] < e_score:
#                         highest_empath = (role,e_score)
#                     else: pass
#                     Sync_df.loc[i,str(role+'_Driver_'+measure)] = Sync_Coefs_sq.loc[role,working_roles].sum(axis=0) #driver scores
#
#                 # saves Se score
#                 if highest_empath:
#                     empath = highest_empath[0]
#                     V_prime = Sync_Coefs_sq[empath].copy()
#                     V_prime.drop(index=empath,inplace=True)
#                     M = Sync_Coefs_sq.drop(columns=empath)
#                     M.drop(index=empath,inplace=True)
#                     if not M.isnull().values.any(): #skips if there is missing info.. need to figure out why it would get here and be empty
#                         M1 = M.astype('float',copy=True)
#                         M_inv = pd.DataFrame(data = np.linalg.pinv(M1.values),columns=M1.columns,index=M1.index)
#                         Q = M_inv.dot(V_prime)
#                         Sync_df.loc[i,Se] = V_prime.dot(Q)
#                         print(V_prime.dot(Q))
#                     else:
#                         Sync_df.loc[i,Se] = 99
#                         print('No getting to Se calc..')
#                         print(M)
#                 else: print('no highest empath?')
#             else: pass
# m = '_'.join(measures)
# out_file = 'Sync_df_{}-{}offset_{}residuals_{}corrMethod_runOn_{}.csv'.format(m,str(offset),str(use_residuals),corr_method,datetime.date(datetime.now()))
# Sync_df.to_csv(os.path.join(os.getcwd(),'Synch_outputs',out_file),index=False)
# %bell
