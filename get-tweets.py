import tweepy as tw
import pandas as pd
import time



### FIXME ###
# Skipped users from Enes Kanter to Jabari Parker
# Adrien Payne to Henry Ellenson
# Pat Mccaw to Kostas Antetoukounpo



# data paths
#   for the record: these were scraped on january 26th 2021
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
    draft_df['Twitter'] = draft_twitters

# dataframes
handles = pd.read_csv(player_twitters)
draft_df = get_draft_df(2010,2020)
add_twitter_data(handles,draft_df)

# subset the draft_df to ignore the 2019-2020 years
draft_df_subset = draft_df[(draft_df['Year'] <= 2018) & (draft_df['Twitter'] != '')]


# twitter keys
API_key = 'oixmKPW8n8t2lc36fPdl4mSAF'
API_secret_key = '32UMo5DcM8dMrgBIzYn46TAHw4PWdg73FLx5sCLmBRl3H10V8K'
bearer_token = 'AAAAAAAAAAAAAAAAAAAAAJ5TMAEAAAAApFSd9mnuFxae5fcUH3D%2FJKDYNi0%3DBjmfMoXjVlBjPTasAomlUd4AqlctFmk7GLml80DObF1yl4Lgsf'
auth = tw.AppAuthHandler(API_key, API_secret_key)
api = tw.API(auth)


# twitter scraping function
def get_player_tweets(hndl, num_tweets, tweets):
    player_tweets = pd.DataFrame(columns = ['handle', 'time', 'text', 'mentions'])
    try:
        for i, tweet in enumerate(tw.Cursor(api.user_timeline, id = hndl).items(num_tweets)):
            row = [hndl, tweet.created_at,tweet.text, ','.join([m['screen_name'] for m in tweet.entities['user_mentions']])]
            player_tweets.loc[i] = row
    except:
        print('Skipping user: '+hndl)
        time.sleep(111)
    return player_tweets

def get_all_tweets(draft_df, num_tweets_per, tweets):
    for handle in draft_df['Twitter']:
        if handle != '':
            player_tweets = get_player_tweets(handle, num_tweets_per, tweets)
            tweets = pd.concat([tweets,player_tweets])
            tweets.drop_duplicates(inplace=True)
            print('Saving new tweets from: '+str(handle))
            tweets.to_csv('data/player_tweets.csv', index=False)
    return tweets


#tweets = pd.read_csv('data/player_tweets.csv')
tweets = pd.DataFrame(columns = ['handle', 'time', 'text', 'mentions'])
print('Getting tweets...')
new_tweets = get_all_tweets(draft_df_subset, 3200, tweets)
tweets = pd.concat([tweets,new_tweets])
tweets.drop_duplicates(inplace=True)
print('Saving tweets.csv...')
tweets.to_csv('data/player_tweets.csv', index=False)
print('Saved.')
print('# Players Scraped (Total): '+str(len(set(tweets['handle']))))
print('# Tweets Scraped (Total): '+str(len(tweets['handle'])))
