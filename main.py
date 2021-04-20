import os
import tweepy as tw
import pandas as pd
import time

# first, update all your tweets
# data paths
#   for the record: the draft dfs were scraped on january 26th 2021
player_twitters = 'data/player-twitters.csv'
def draft_path(year):
    return 'data/drafts/'+str(year)+'-draft.csv'
def get_draft_df(year_from,year_to):
    df = pd.read_csv(draft_path(year_from))
    df['Year'] = [year_from]*len(df)
    for yr in range(year_from+1,year_to+1):
        new_df = pd.read_csv(draft_path(yr))
        new_df['Year'] = [yr]*len(new_df)
        df = pd.concat([df,new_df])
    return df
def add_twitter_data(handles, draft_df):
    # build lookup table for twitter handles
    hndls = {}
    for i in range(len(handles['Player'])):
        name = handles['Player'][i].split('\\')[0]
        hndls[name] = handles['Twitter'][i]
    draft_twitters = []
    for player in draft_df['Player']:
        if player in hndls:
            draft_twitters.append(hndls[player])
        else:
            draft_twitters.append('')
    draft_df['twitter_handle'] = draft_twitters
# dataframes
handles = pd.read_csv(player_twitters)
draft_df = get_draft_df(2010,2020)
add_twitter_data(handles,draft_df)
# subset to the players we care about
draft_df_subset = draft_df[(draft_df['Year'] <= 2018) & (draft_df['twitter_handle'] != '')]
# twitter keys
API_key = 'oixmKPW8n8t2lc36fPdl4mSAF'
API_secret_key = '32UMo5DcM8dMrgBIzYn46TAHw4PWdg73FLx5sCLmBRl3H10V8K'
bearer_token = 'AAAAAAAAAAAAAAAAAAAAAJ5TMAEAAAAApFSd9mnuFxae5fcUH3D%2FJKDYNi0%3DBjmfMoXjVlBjPTasAomlUd4AqlctFmk7GLml80DObF1yl4Lgsf'
auth = tw.AppAuthHandler(API_key, API_secret_key)
api = tw.API(auth)
def get_player_tweets(hndl, num_tweets):
    '''
    hndl: string, representing twitter handle
    returns a pandas series with the following variables:
        - handle
        - cleaned date/time
        - cleaned tweet text
    '''
    player_tweets = pd.DataFrame(columns = ['handle', 'time', 'text', 'mentions'])
    try:
        for i, tweet in enumerate(tw.Cursor(api.user_timeline, id = hndl).items(num_tweets)):
            row = [hndl, tweet.created_at,tweet.text, ','.join([m['screen_name'] for m in tweet.entities['user_mentions']])]
            player_tweets.loc[i] = row
    except:
        # I need to throw an exception
        raise Exception('Skipping user: '+hndl)
    return player_tweets
def get_all_tweets(draft_df, num_tweets_per):
    tweets = pd.DataFrame(columns = ['handle', 'time', 'text', 'mentions'])
    added = set() 
    for i, handle in enumerate(draft_df['twitter_handle']):
        if handle not in added:
            if handle != '':
                try:
                    player_tweets = get_player_tweets(handle,num_tweets_per)
                    tweets = pd.concat([tweets,player_tweets])
                    added.add(handle)
                except:
                    print('Waiting on user '+str(i)+', handle '+str(handle)+'...')
                    time.sleep(180)
    return tweets

tweets = pd.read_csv('data/player_tweets.csv')
print('Current # tweets: '+str(len(tweets['handle'])))
print('Getting more tweets...')
new_tweets = get_all_tweets(draft_df_subset, 3200)
print('Got tweets, saving.')
tweets = pd.concat([tweets,new_tweets])
tweets.drop_duplicates(inplace=True)
tweets.to_csv('data/player_tweets.csv', index=False)
print('Saved.')
print('# Players Scraped (Total): '+str(len(set(tweets['handle']))))
print('# Tweets Scraped (Total): '+str(len(tweets['handle'])))