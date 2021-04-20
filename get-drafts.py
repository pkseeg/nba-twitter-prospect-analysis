# imports
from bs4 import BeautifulSoup
import requests
import pandas as pd
import os

# URLs
def draft_url(year):
	return 'https://www.basketball-reference.com/draft/NBA_'+str(year)+'.html'

def draft_df(year):
	url = draft_url(year)
	page = requests.get(url)
	soup = BeautifulSoup(page.content, 'html.parser')
	table = soup.find('tbody')
	head = soup.find_all('th')
	row = soup.find('data-row')
	#print(row)
	print(table.contents[2])
	col_names = [i.aria_label for i in head]
	#column_names_raw = [head.text for i in head][1]
	#print(column_names_raw)

draft_df(2013)
