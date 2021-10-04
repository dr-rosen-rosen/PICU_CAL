####################################################################################################
####################################################################################################
################################ Functions adapted from initial jupyter notebook for pilot anlaysis
####################################################################################################
####################################################################################################
####################################################################################################

from sqlalchemy import (UniqueConstraint,MetaData,Table,Column,Integer,Numeric,Float,Boolean,String,DateTime,ForeignKey,create_engine,select,update,delete,insert,and_,inspect)
from sqlalchemy_utils.functions import create_database
from sqlalchemy import inspect
from sqlalchemy import func
import os
from datetime import datetime
import sys
import glob
import pandas as pd
from contextlib import closing
from multiprocessing import (Pool,cpu_count)
import numpy as np

####################################################################################################
############################## Legacy RTLS_scripts.py
####################################################################################################

#Pulls specific range of data from RTLS table, de-duplicates if needed
def pullRTLS(rng,RTLS_data,connection,select,datetime,timedelta,and_,pd):
    # set date range to pull from DB
    if rng == 'weekly_report':
        days_to_subtract = 7
        end = datetime.date(datetime.now()) - timedelta(days=1)
        start = end - timedelta(days=days_to_subtract)
    elif rng == 'pilot':
        end = pd.to_datetime('2019-6-30')
        start = pd.to_datetime('2018-7-1')
    elif rng == 'all_ama':
        end = pd.to_datetime(datetime.now())
        start = pd.to_datetime('2019-7-1')
    else: print('no time range set, no data pulled')
    # Pull RLTS hits in time range
    print(end)
    print(start)
    s = select([RTLS_data]).where(and_(RTLS_data.c.Time_In >= start,RTLS_data.c.Time_In <= end))
    #s = select([RTLS_data]).where(RTLS_data.c.Time_In >= start)
    rp = connection.execute(s)
    df = pd.DataFrame(rp.fetchall())
    print(len(df))
    df.columns = rp.keys()
    # Tests for any duplicates (there shouldn't be any); returns deduplicated if there are duplicates
    if len(df) == len(df.drop_duplicates(subset=None, keep='first', inplace=False)):
        print('no dupes in hits pulled!!!... '+str(len(df)))
        return df
    else:
        print('Got some dupes!!!... '+str(len(df) - len(df.drop_duplicates(subset=None, keep='first', inplace=False))))
        return df.drop_duplicates(subset=None, keep='first', inplace=False)

#Pulls all recievers from table
def pullReceivers(RTLS_Receivers,connection,select,pd):
    s = select([RTLS_Receivers])
    rp = connection.execute(s)
    receivers = pd.DataFrame(rp.fetchall())
    receivers.columns = rp.keys()
    print('this many receivers... '+str(len(receivers)))
    return receivers

# Inserts new recievers into the receivers table
def updateRTLSReceivers(receivers,RTLS_Receivers,connection,insert,select):
    for i,row in receivers.iterrows():
        exists = connection.execute(select([RTLS_Receivers.c.Receiver]).where(RTLS_Receivers.c.Receiver == row.Receiver)).scalar()
        if not exists:
            ins_vals = {
                'Receiver':row.Receiver,
                'ReceiverName':row.ReceiverName
            }
            ins = insert(RTLS_Receivers).values(ins_vals)
            connection.execute(ins)
    return

# puts new hits, checking for duplicates, in db
def storeRTLS(hits,RTLS_data,connection,insert,select,and_):
    for i,row in hits.iterrows():
        cond = and_(
        RTLS_data.c.Receiver == row.Receiver,
        RTLS_data.c.Time_In == row.Time_In,
        RTLS_data.c.Time_Out == row.Time_Out)
        exists = connection.execute(select([RTLS_data]).where(cond)).scalar()
        if not exists:
            ins_vals = {
                'Receiver':row.Receiver,
                'Time_In':row.Time_In,
                'Time_Out':row.Time_Out
            }
            #print ins_vals
            ins = insert(RTLS_data).values(ins_vals)
            connection.execute(ins)
        else:
            print('Dupe!')
    return

# uses reciever description to categorize recievers we don't have a manual code for
def RTLS_loc_recode(x):
    x = str(x).lower()
    if (('pt rm' in x) and not ('hall by' in x)) or ('picu rm' in x) or ('ped critical care' in x):
        return 'pt_rm'
    elif ('staff sta' in x) or ('work area' in x) or ('office' in x) or ('work room' in x) or ('report rm' in x) or ('clerk' in x) or ('attending off' in x) or ('ccsr' in x):
        return 'staff_admin_area'
    elif ('cvil' in x):
        return 'cvil'
    elif ('hall' in x) or ('corridor' in x) or ('loop' in x) or ('connector' in x) or ('corr' in x) or ('concourse' in x) or ('bridge' in x):
        return 'hall_etc'
    elif ('elev' in x) or ('stair' in x):
        return 'evelvator_stair'
    elif ('conference room' in x) or ('conf rm' in x) or ('classroom' in x) or ('report room' in x) or ('staff lounge' in x):
        return 'conf_classroom_lounge'
    elif ('exam' in x):
        return 'exam'
    elif ('playrm' in x) or ('family' in x) or ('waiting' in x) or ('playroom' in x):
        return 'family_waiting_space'
    elif (' or ' in x):
        return 'OR'
    elif (' rad ' in x):
        return('rad')
    elif ('pyxis' in x) or ('pxyis' in x):
        return('med_room')
    elif ('observation' in x):
        return 'observation'
    elif 'decon' in x:
        return 'decon'
    elif ('evs' in x) or ('nourishment' in x) or ('staging' in x) or ('med room') or ('equip' in x) or ('supply' in x) or ('central stores' in x) or ('storage' in x) or ('util' in x):
        return('equip / med / nourish')
    elif ('badge cabinet' in x):
        return('badge_cab')
    elif ('tow line' in x):
        return('tow_line')
    elif ('irc' in x):
        return('irc')
    elif ('pacu' in x):
        return('pacu')
    elif ('nir' in x):
        return('nir')
    elif ('clinical eng' in x):
        return('clinical_eng')
    elif ('shell' in x):
        return('shell_space')
    else:
        print(x)
        return('other')

####################################################################################################
############################## Runs reports
####################################################################################################
'''THIS HAS NOT BEEN UPDATED TO RUN IN NEW STRUCTURE... '''
def badge_reports(df,save_raw_data,save_badge_timelines,create_rollups,create_receiver_review,badge_timeline_dir,datetime):
    if create_rollups:
    #roll up to reciever and reciever type
        print('Making rollup files... ')
        Receiver_rollup = df.groupby(['Receiver_name']).agg({'Duration': 'sum'})
        Receiver_rollup.sort_values(by='Duration',axis=0,ascending=False,inplace=True)
        Receiver_recode_rollup = df.groupby(['Receiver_recode']).agg({'Duration': 'sum'})
        Receiver_recode_rollup.sort_values(by='Duration',axis=0,ascending=False,inplace=True)

        # creates total duration and number of unique recievers for each badge in the data
        f = {'Duration':'sum','Receiver': lambda t: len(df.ix[t.index].Receiver.unique())}
        badge_rollup = df.groupby(['RTLS_ID']).agg(f)
        badge_rollup.sort_values(by='Duration',axis=0,ascending=False,inplace=True)
    else: print('Skipping badge and receiver rollup reports...')

    if create_receiver_review:
        #creates a list of all of the receivers, their name, categories and total amount of person time there
        print('Saving receiver review files...')
        df_receivers = df[['Receiver','Receiver_recode','Receiver_name','Duration']].copy()
        df_receivers = df_receivers.groupby(['Receiver','Receiver_recode','Receiver_name'])['Duration'].agg('sum').reset_index()
        df_receivers.sort_values('Duration',axis='rows',inplace=True,ascending=False)
        fname = 'IM_Resident_Receiver_data_runOn{}.csv'.format(datetime.date(datetime.now()))
        df_receivers.to_csv(fname,encoding='utf-8',index=False)
        df_receivers.head()
    else: print('Skipping receiver review file...')

    #saves raw data and data by badge and reciever type
    if save_raw_data and create_rollups:
        print('Saving rollup files...')
        Receiver_rollup.to_csv('Receiver_rollup_{}.csv'.format(datetime.date(datetime.now())))
        #df.to_csv('Raw_data_{}.csv'.format(datetime.date(datetime.now())),index=False)
        #badge_rollup.to_csv('Badge_rollup_{}.csv'.format(datetime.date(datetime.now())))
        #Receiver_recode_rollup.plot(kind='barh')
        Receiver_recode_rollup.to_csv('Receiver_recode_rollup_{}.csv'.format(datetime.date(datetime.now())))
    else: print('Skipping saving receiver rollup files... ')

    #saves list of recievers in each reciever category
    #x = df.groupby(['Receiver_recode','Receiver_name']).size().reset_index().rename(columns={0:'count'})
    #x.to_csv('ReceiverCategory_by_ReceiverName_'+date_lab+'.csv',index=False)

    #Receiver_recode_rollup.to_csv('Receiver_recode_rollup_'+date_lab+'.csv')

    if save_badge_timelines:
        for badge in df.RTLS_ID.unique():
            df_temp = df[df['RTLS_ID'] == badge].copy()
            df_temp.sort_values(by='Time_In',ascending=True,axis=0,inplace=True)
            df_temp.to_csv(os.path.join(badge_timeline_dir,'Badge_'+str(badge)+'_timeline.csv'),encoding='utf-8',index=False)
    else: print('Skipping badge timelines...')

    return

def get_active_badges(badge_file):
    b = pd.read_excel(badge_file,index_col=None,header=0)
    b = b[b.Active == 'Yes']
    return b.RTLS_ID.unique()

# new func for generating report
def get_weekly_report(anchor_date,look_back_days,db_name,db_loc,target_badges,weekly_report_dir):
    # set date variables
    rght_win = anchor_date
    lft_win = rght_win - pd.Timedelta(look_back_days, unit='D')

    print('This many active badges... '+str(len(target_badges)))

    # make connection and get data
    engine = create_engine('sqlite:///'+db_loc+db_name)#os.path.join('sqlite:///',db_loc,db_name))
    metadata = MetaData(bind=engine)
    metadata.reflect()
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
    insp = inspect(engine)

    #for each target badge, get data within ragne!
    df_list = []
    target_badges = [int(b) for b in target_badges]
    for t in target_badges:#tables:
        print(t)
        if insp.has_table(str('Table_'+str(t))):
            tbl = metadata.tables[str('Table_'+str(t))]
            s = select([tbl]).where(and_(tbl.c.Time_In >= lft_win,tbl.c.Time_In <= rght_win))
            #s = select([RTLS_data]).where(RTLS_data.c.Time_In >= start)
            rp = connection.execute(s)
            df = pd.DataFrame(rp.fetchall())
            if df.empty:
                print("Empty badge... "+str(t))
            else:
                print(len(df))
                df.columns = rp.keys()
                df['RTLS_ID'] = t
                df_list.append(df)
        else: print("no table")#pass
    df = pd.concat(df_list)

    df['Duration'] = (df.Time_Out - df.Time_In).astype('timedelta64[s]')/60
    df = df.groupby(['RTLS_ID']).agg({'Duration': 'sum'})
    df.sort_values('Duration',axis=0,inplace=True,ascending=True)
    fname = 'IM_Badge_data_from_{}_to_{}_runOn{}.csv'.format(lft_win,rght_win,datetime.date(datetime.now()))
    df.to_csv(os.path.join(weekly_report_dir,fname))
    print("Of the {} active badges, {} had data between {} and {}".format(len(target_badges),len(df),lft_win,rght_win))
    print('These active badges did not have data: {}'.format(set(target_badges).difference(set(df.index.tolist()))))

    return df

####################################################################################################
############################## Reads in files and stores them in database
####################################################################################################

def csv_to_db(db_name, db_loc, in_path):
    # Creates connection to db and loads appropriate scripts
    engine = create_engine('sqlite:///'+db_loc+db_name)
    #if not engine.dialect.has_table(engine, 'RTLS_Receivers'):
    if not inspect(engine).has_table('RTLS_Receivers'):
        #%run '/Users/mrosen44/OneDrive - Johns Hopkins University/RTLS_Data/scripts/RTLS_DB_defs.py'
        metadata = MetaData(bind=db) # Should this be engine instead of DB? If so, move out of loop and remove dupe below
        RTLS_Receivers = Table('RTLS_Receivers',metadata,
                Column('Receiver',Integer(),primary_key=True,unique=True),
                Column('ReceiverName',String(255)),
                Column('LocationCode',String(255))
        )
        metadata.create_all(engine)
    else:
        metadata = MetaData(bind=engine)
        metadata.reflect()
        RTLS_Receivers = metadata.tables['RTLS_Receivers']
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
    #%run '/Users/mrosen44/OneDrive - Johns Hopkins University/RTLS_Data/scripts/RTLS_scripts.py'

    # Reads in all the csvs
    RTLS_dump = pd.concat([pd.read_csv(f,header=0,skiprows=[0,2], index_col=None,sep=',',encoding='utf-16',parse_dates=[6,7],infer_datetime_format=True) for f in glob.glob(os.path.join(in_path, '*.csv'))])
    #RTLS_dump = pd.concat([pd.read_excel(f,header=0,skiprows=[1], index_col=None,parse_dates=[6,7]) for f in glob.glob(os.path.join(in_path, '*.xlsx'))])
    updateRTLSReceivers(RTLS_dump[['Receiver','ReceiverName']].drop_duplicates(inplace=False),RTLS_Receivers,connection,insert,select)

    RTLS_dump.dropna(axis='index',how='any',inplace=True)
    RTLS_dump['BadgeTimeIn'] = pd.to_datetime(RTLS_dump['BadgeTimeIn'])#,errors='coerce')
    RTLS_dump['BadgeTimeOut'] = pd.to_datetime(RTLS_dump['BadgeTimeOut'])
    hit_cols = ['Badge','Receiver','BadgeTimeIn','BadgeTimeOut']
    hit_col_remap = ['RTLS_ID','Receiver','Time_In','Time_Out']
    hits = RTLS_dump[hit_cols].copy()
    hits.columns = hit_col_remap
    hits['Receiver'] = hits['Receiver'].astype('int')
    hits.drop_duplicates(subset=None, keep='first', inplace=True)

    for badge in hits.RTLS_ID.unique():

        Table_name = 'Table_'+str(badge)

        # check if badge table exists, and create if it doesn't
        #if not engine.dialect.has_table(engine, Table_name):
        if not inspect(engine).has_table(Table_name):
            metadata = MetaData(engine)
            # Create a table with the appropriate Columns
            RTLS_Data = Table(Table_name, metadata,
                Column('Receiver',Integer, nullable=False), #ForeignKey('RTLS_Receivers.Receiver')), #FOREIGN KEY?
                Column('Time_In',DateTime()),
                Column('Time_Out',DateTime()))
                # Implement the creation
            metadata.create_all()
        else:
            metadata = MetaData(bind=engine)
            metadata.reflect()
            RTLS_Data = metadata.tables[Table_name]
        badge_hits = hits[hits.RTLS_ID ==  badge].copy().drop('RTLS_ID',axis=1) # Send just the data for that badge
        storeRTLS(badge_hits,RTLS_Data,connection,insert,select,and_)
####################################################################################################
############################## Looks at all recievers in db, and adds location code for those with none
####################################################################################################
def rcvr_dscrp_to_loc_code(db_name, db_loc,rcvr_recode_file,rcvr_recode_file_loc, print_new_recievers):
    #### This function pulls all RTLS recievers in the database which do not have a location code, and
    #### codes and updates the reciever table.

    # connect to db
    engine = create_engine('sqlite:///'+db_loc+db_name)#os.path.join('sqlite:///',db_loc,db_name))
    metadata = MetaData(bind=engine)
    metadata.reflect()
    RTLS_Receivers = metadata.tables['RTLS_Receivers']
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')
    # # get all recievers w/o a location code
    rp = connection.execute(select([RTLS_Receivers]).where(RTLS_Receivers.c.LocationCode.is_(None)))
    receivers = pd.DataFrame(rp.fetchall())
    if receivers.empty:
        print('All receivers have been coded already!!')
    else:
        receivers.columns = rp.keys()
        print('This many receivers being recoded... '+str(len(receivers)))

        # read in locations from Travis' file and create dict for recoding
        IM_locs = pd.read_excel(os.path.join(os.getcwd(),rcvr_recode_file_loc,rcvr_recode_file),header=0)
        IM_locs = IM_locs[['ReceiverName','IM_loc_map']]

        #ReceiverDescription is the old general mapping... this uses the generic mapping to fill in the NAs for the recoded locations
        receivers['ReceiverDescription'] = receivers['ReceiverName'].apply(lambda x: RTLS_loc_recode(x))
        receivers = receivers.merge(IM_locs,how='left',on='ReceiverName') #would probably speed up considerably if merging on int receiver ID...
        receivers['ReceiverDescription_IMRes'] = receivers['IM_loc_map'].fillna(receivers['ReceiverDescription'])

        loc_collapse_dict = {'family_waiting_space':'Family waiting space',
                             'staff_admin_area':'Supply and admin',#'staff/admin area',
                             'staff admin':'Supply and admin',#'staff/admin area',
                             'staff area':'Supply and admin',#'staff/admin area',
                             'pt_rm':'Patient room',
                             'pt room':'Patient room',
                             'exam':'OTHER/UNKNOWN',
                             'Patient Room':'Patient room',
                             'equipment':'Supply and admin',#'equip / med / nourish',
                             'equip / med / nourish':'Supply and admin',
                             'staff/admin area': 'Supply and admin',
                             'ward hallway':'Ward Hall',
                             'md workroom':'MD Workroom',
                             'education':'Education',
                             'family area':'Family waiting space',
                             'family space':'Family waiting space',
                             'cvil':'OTHER/UNKNOWN',#'Procedure space',
                             'rad':'OTHER/UNKNOWN',#'Procedure space',
                             'OR':'OTHER/UNKNOWN',#'Procedure space',
                             'med_room':'Supply and admin',#'equip / med / nourish',
                             'Elevators / stairs':'Transit',
                             'Hallway':'Transit',
                             'evelvator_stair':'Transit',
                             'Elevators':'Transit',
                             'elevway':'Transit',
                             'elevator':'Transit',
                             'eleavator':'Transit',
                             'elevators':'Transit',
                             'hall_etc':'Transit',
                             'hallway':'Transit',
                             'Exit Point':'Transit',
                             'conf_classroom_lounge':'Education' # ?????? not sure about this one
                            }
        receivers['ReceiverDescription_IMRes'].replace(loc_collapse_dict,inplace=True)
        print(len(receivers))
        if print_new_recievers: receivers.to_csv('new_recievers_runOn{}.csv'.format(datetime.date(datetime.now())))
        # Saves the locations back to DB
        for i, row in receivers.iterrows():
            stmt = update(RTLS_Receivers).where(RTLS_Receivers.c.Receiver == row.Receiver).values(LocationCode = row.ReceiverDescription_IMRes)
            connection.execute(stmt)

####################################################################################################
############################## Codes locations for given set of badge data
####################################################################################################
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
        df_chunks = [badge_data[badge_data.Badge == ID].copy() for ID in badge_data.Badge.unique()]
        df_chunks_pckged = [[df_chunk,receiver_dict,receiverName_dict] for df_chunk in df_chunks] # packages dicts with each data chunk
        results_list = pool.map(func, df_chunks_pckged)
        pool.terminate()
        return pd.concat(results_list)#, axis=1)

def loc_code_badge_data(badge_data, db_name, db_loc):
    #### This recodes a set of worn badges using reciever recodes stored in reciever tables
    ####
    # connect to db
    engine = create_engine('sqlite:///'+db_loc+db_name)
    metadata = MetaData(bind=engine)
    metadata.reflect()
    RTLS_Receivers = metadata.tables['RTLS_Receivers']
    connection = engine.connect()
    connection.text_factory = lambda x: unicode(x, 'utf-8', 'ignore')

    # get all recievers w/o a location code
    rp = connection.execute(select([RTLS_Receivers]))#.where(RTLS_Receivers.c.Receiver.in_(list(badge_data.Receiver.unique()))))
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
####################################################################################################
############################## Applies data cleaning rules to recategorized data
####################################################################################################

def apply_rules(df,rule_1_thresh,rule_2_thresh,rule_2_locs):
# reclassify any of the below sepcific cases as UNKOWN/OTHER
    #Any “elevator" needs to be recoded as OTHER/UNKNOWN, if sensor pings for more than 10 minutes
    #Any “connector” needs to be recoded as OTHER/UNKNOWN, if sensor pings for more than 10 minut
    #Any “loop” needs to be recoded as OTHER/UNKNOWN, if sensor pings for more than 10 minutes
    #NH 01 Hall by Visitor Elevators should be recoded as OTHER/UNKNOWN, if sensor pings for more than 10 minutes
    print('Halls, etc. > 10 minutes...')
    print(len(df.loc[((df.Duration > 10) & (df.Receiver_name.str.contains('elevator|connector|loop|NH 01 Hall by Visitor Elevators',case=False,regex=True))),'Receiver_recode']))
    df.loc[((df.Duration > 10) & (df.Receiver_name.str.contains('elevator|connector|loop|NH 01 Hall by Visitor Elevators',case=False,regex=True))),'Receiver_recode'] = 'OTHER/UNKNOWN'

    full_num_hits = len(df)
    print(full_num_hits)
    rule_1_flagged = df[df.Duration > rule_1_thresh].copy()
    print('Number of rule 1 flagged... '+str(len(rule_1_flagged))+', '+str((len(rule_1_flagged) / full_num_hits)*10)+'% of total data')
    df.loc[df.Duration > rule_1_thresh,'Receiver_recode'] = 'OTHER/UNKNOWN'

    rule_2_flagged = df[(df.Duration > rule_2_thresh) & (df.Duration < rule_1_thresh) & (~df.Receiver_recode.isin(rule_2_locs))].copy()
    print('Number of rule 2 flagged... '+str(len(rule_2_flagged))+', '+str((len(rule_2_flagged) / full_num_hits)*10)+'% of total data')
    df.loc[((df.Duration > rule_2_thresh) & (df.Duration < rule_1_thresh) & (~df.Receiver_recode.isin(rule_2_locs))),'Receiver_recode'] = 'OTHER/UNKNOWN'

    return df

####################################################################################################
############################## Creates summary measures for different time intervals
####################################################################################################

# create df structure... a big multi-index with RTLS_ID, Day of the year; Day_type (actually add this as field after); Interval (all, morning, etc.).... fields for overall time and time by location

def make_interval_metrics(df):

    Days = pd.date_range(start=min(df.Time_In.dt.date), end=max(df.Time_Out.dt.date), freq='D', tz=None, normalize=False, name='Date', closed=None)
    Intervals = ['all_24','night','morning','afternoon','evening','rounds']
    Badges = df.Badge.unique()
    cols = df.Receiver_recode.unique().tolist()#.append('Total')
    cols = cols + ['Total']
    idx = pd.MultiIndex.from_product([Days, Badges, Intervals],
                                      names=['Day', 'Badges','Interval'])
    df_sum = pd.DataFrame(index=idx, columns = cols, data = None)

    interval_offsets = {
        'all_24': {'start':0,'stop':23},
        'night': {'start':0,'stop':5},
        'morning': {'start':6,'stop':11},
        'afternoon': {'start':12,'stop':17},
        'evening': {'start':18,'stop':23},
        'rounds': {'start':8, 'stop':9} # this should capture 9 to 11 am; the algorythm adds one to the end
    }

    # loop... for each day, for each badge, for each interval... calculate sums
    for day in Days:
        for Badge in Badges:
            df_day = df[((df.Badge == Badge)) & ((df.Time_In.dt.date == day) | (df.Time_Out.dt.date == day))].copy() # select all data for badge and day
            # test if empty
            if not df_day.empty:
                for Interval in Intervals:
                    # sets start and stop of the intervals
                    start = day + pd.offsets.Hour(interval_offsets[Interval]['start'])
                    stop = day + pd.offsets.Hour(interval_offsets[Interval]['stop'] + 1)
                    front_time_slice = ((df_day.Time_In >= start) & (df_day.Time_In <= stop))
                    back_time_slice = ((df_day.Time_Out >= start) & (df_day.Time_Out <= stop))
                    df_temp = df_day[(front_time_slice | back_time_slice)].copy()
                    if not df_temp.empty:
                        # trim
                        if df_temp.Time_In.min() < start:
                            df_temp.at[df_temp[['Time_In']].idxmin(),'Time_In'] = start
                        if df_temp.Time_Out.max() > stop:
                            df_temp.at[df_temp[['Time_Out']].idxmax(),'Time_Out'] = stop
                        # Recalculate duration
                        df_temp['Duration'] = (df_temp.Time_Out - df_temp.Time_In).dt.seconds

                        # Calculate sum durations by location category
                        x = df_temp.groupby('Receiver_recode')['Duration'].sum()
                        # store it in summary df; x ends up being a series with location values as row labels
                        for loc in x.index.array:
                            df_sum.loc[(day,Badge,Interval),loc] = x.at[loc]
                            df_sum.loc[(day,Badge,Interval),'Total'] = x.sum()

                    else:
                        print('EMPTY Segment!')

                else:
                    print('EMPTY Day!')
    return df_sum.reset_index(drop = False)

####################################################################################################
############################## Creates timeseries dataframe for area and sequence plots
####################################################################################################
    # define function to create timeseries for each badge and location

# function for creating timeseries from epoch data... used in seqdef and ts functions
def ts_it(idx,Time_In,Time_Out):
    In = pd.Series(data=True,index=Time_In)
    Out = pd.Series(data=False,index=Time_Out)
    both = pd.concat([In,Out])
    both.sort_index(ascending=True,inplace=True)
    both = both.loc[~both.index.duplicated(keep=False)]
    both = both.reindex(idx,method='ffill',fill_value=False)
    return both.values

def make_seqdef_data(df,start,stop,f):
    locs = df.Receiver_recode.unique().tolist()
    idx = pd.date_range(
        start,
        stop,
        freq=f)
    data = False
    df_ts = pd.DataFrame(index=idx,columns=locs,data=data)
    df_ts.index.rename('TimeStamp')
    for loc in locs:
        mask = (df.Receiver_recode == loc)
        df_ts[loc] = ts_it(idx,df[mask]['Time_In'].values,df[mask]['Time_Out'].values)

    df_ts['Combo'] = 'NA'
    for loc in locs:
        df_ts.loc[df_ts[loc]==True,'Combo'] = loc
    idx = pd.date_range(
        start,
        stop,
        freq='D'
    )
    cols = np.arange(len(idx)*24*60)
    print(cols)
    df_seq = pd.DataFrame(index = idx, columns = cols, data = 'NA')
    for dy in idx:
        print(dy)
        dy_offset = dy + pd.Timedelta(1,unit='D')-pd.Timedelta(1,unit='min')
        print(dy_offset)
        df_seq.loc[dy,slice(None)] = pd.Series(df_ts.loc[dy:dy_offset,'Combo'].values)
    return df_seq

def make_timeseries_df(df,f):
    # set up overall dataframe for tracking time series

    badges = [b for b in df.Badge.unique().astype('int')]
    locs = df.Receiver_recode.unique().tolist()
    cols = pd.MultiIndex.from_product([badges, locs],
                                      names=['Badge', 'Location'])
    idx = pd.date_range(
        min(df.Time_In.dt.date),
        max(df.Time_Out.dt.date),
        freq=f) #'min', 'S'
    data = False
    df_ts = pd.DataFrame(index=idx,columns=cols,data=data)
    df_ts.index.rename('TimeStamp')

    # iterate through badges and locations and populate the data structure
    for badge in badges:
        for loc in locs:
            mask = ((df.Badge == badge) & (df.Receiver_recode == loc))
            df_ts[badge,loc] = ts_it(idx,df[mask]['Time_In'].values,df[mask]['Time_Out'].values)
    # this collapses across all badges for a 'total' with multiple badges
    # or just one if only data for one badge is passed to function
    df_ts = df_ts.groupby(level = 'Location', axis = 1).sum()
    df_ts = df_ts.groupby(df_ts.index.hour).sum()
    df_ts.index.rename('hour', inplace = True)
    df_ts.reset_index(inplace = True)
    df_ts = df_ts.melt(id_vars = ['hour'],value_name = 'Duration')
    return df_ts

def make_timeseries_df_for_dummies(df):
    df.rename(columns = {'Time_In':'hour','Receiver_recode':'Location'},inplace = True)
    df.set_index('hour', inplace = True)
    df = df.groupby([df.index.hour, 'Location'])['Duration'].sum()
    df = df.reset_index()
    return df

def ts_it_PAR(df_chunk):
    d = df_chunk[0]
    idx = df_chunk[1]
    locs = df_chunk[2]
    #f = df_chunk[3]
    badge = df_chunk[3].astype('int')#d.Badge.unique()[0]#d.drop_duplicates(subset='Badge', keep='first', inplace=False)#d.Badge.unique[0]
    #cols = pd.MultiIndex.from_product([[badge], locs],
    #                                  names=['Badge', 'Location'])
    data = False
    df_ts = pd.DataFrame(index=idx,columns=locs,data=data)
    df_ts.index.rename('TimeStamp')
    for loc in locs:
        mask = (d.Receiver_recode == loc)
        In = pd.Series(data=True,index=d[mask]['Time_In'].values)
        Out = pd.Series(data=False,index=d[mask]['Time_Out'].values)
        both = pd.concat([In,Out])
        both.sort_index(ascending=True,inplace=True)
        both = both.loc[~both.index.duplicated(keep=False)]
        both = both.reindex(idx,method='ffill',fill_value=False)
        #df_ts[badge,loc] = both.values
        df_ts[loc] = both.values
    return [badge,df_ts]

def make_timeseries_df_PAR(df,f,num_processes):
    '''Takes a function and dataframe, chunks up by badge'''
    idx = pd.date_range(
        min(df.Time_In.dt.date),
        max(df.Time_Out.dt.date),
        freq=f)
    locs = df.Receiver_recode.unique().tolist()
    if num_processes==None:
        num_processes = 6#cpu_count() - 1#min(df.shape[1], cpu_count())
    with closing(Pool(num_processes)) as pool:
        # creates list of data frames for each badge
        df_chunks = [[df[df.Badge == ID].copy(),idx,locs,ID] for ID in df.Badge.unique().astype('int')]
        #df_chunks = [[df_chunk,idx,locs] for df_chunk in df_chunks] # packages other info with each data chunk
        results_list = pool.map(ts_it_PAR, df_chunks)
        pool.terminate()
        for i in results_list: print(i[0])
        return 'ok'#pd.concat(results_list, axis=1)#,keys = df.Badge.unique())

####################################################################################################
############################## Helper functions for network data manipulation
####################################################################################################

def relabel_nodes(df, nodes):
    node_dict = dict(zip(nodes.rec_num,nodes.id))
    df['Receiver'] = df['Receiver'].map(node_dict)
    return(df)
