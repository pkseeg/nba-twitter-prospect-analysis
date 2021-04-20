import pandas as pd

# Interacting with the API
import json
from ibm_watson import NaturalLanguageUnderstandingV1
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator
from ibm_watson.natural_language_understanding_v1 import Features, EmotionOptions, SentimentOptions, CategoriesOptions

creds = {
  "apikey": "DxXZG9G24IFMBznqOQcI3SYa7MW6kU-hj3Hb3cWa-CoI",
  "iam_apikey_description": "Auto-generated for key 4a896ef7-db2c-4b9a-ba6e-87b526989ec2",
  "iam_apikey_name": "Auto-generated service credentials",
  "iam_role_crn": "crn:v1:bluemix:public:iam::::serviceRole:Manager",
  "iam_serviceid_crn": "crn:v1:bluemix:public:iam-identity::a/9cff25d774234ca6926e478038b1a5f0::serviceid:ServiceId-66f813a7-b446-48b3-838f-48fd95e3f20c",
  "url": "https://api.us-south.natural-language-understanding.watson.cloud.ibm.com/instances/60e2504b-0a72-434f-b0a6-606bca97a695"
}


def get_nlu_score(handle,text,season):
    authenticator = IAMAuthenticator(creds['apikey'])
    nlu = NaturalLanguageUnderstandingV1(
        version='2020-08-01',
        authenticator=authenticator
    )
    nlu.set_service_url(creds['url'])
    
    nlu_scores = pd.DataFrame(columns=['twitter_handle','season','cleaned_text',
                                      'doc_labels','doc_labels_weights',
                                      'emot_anger','emot_disgust','emot_fear',
                                      'emot_joy','emot_sadness',
                                      'sent_class','sent_score'])
    
    response = nlu.analyze(text=text,
                           features=Features(emotion=EmotionOptions(document=True),
                           sentiment=SentimentOptions(document=True),
                           categories=CategoriesOptions(limit=20))).get_result()
    
    row = [handle,season,text,
          ' '.join([d['label'] for d in response['categories']]),
          ' '.join([str(d['score']) for d in response['categories']]),
          response['emotion']['document']['emotion']['anger'],
          response['emotion']['document']['emotion']['disgust'],
          response['emotion']['document']['emotion']['fear'],
          response['emotion']['document']['emotion']['joy'],
          response['emotion']['document']['emotion']['sadness'],
          response['sentiment']['document']['label'],
          response['sentiment']['document']['score']]
    
    nlu_scores.loc[0] = row
    return nlu_scores
