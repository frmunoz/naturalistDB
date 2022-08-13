# Needed libraries must be installed
pip install pandas
# conda install pandas
pip install pyinaturalist

import pandas as pd
from pyinaturalist import *
import os

from pyinaturalist.rest_api import add_photo_to_observation

os.chdir('C:/Users/munozfra/Documents/Rprojects/naturalistDB')

# The ID file should contain the identification information
id=pd.read_csv (r'id.csv',sep=';')
# Or directly include below
#{'user':['XXX'],
#'pwd':['XXX'],
#'app.id':['XXX'],
#'app.pwd':['XXX']}
#id=pd.DataFrame(id)

df = pd.read_csv (r'20220812 Export iNaturalist.csv',sep=';',decimal=',',encoding='latin-1')
print (df)
# Get values
df.values[1][0]
df['Species']
df.get('Species)[1]

# Add file path information
df2 = pd.DataFrame().assign(Path='C:/PlantNet/'+df['File.name'])

token = get_access_token(username=id.get('user')[0],password=id.get('pwd')[0],
app_id=id.get('app.id')[0],app_secret=id.get('app.pwd')[0])

for index in range(21, 40): #int(df.shape[0])):
  
  precision=10000
  
  taxon = get_taxa(q=df.get('Taxon')[index],rank=['species'])
  
  from datetime import datetime
  
  response = create_observation(taxon_id=taxon['results'][0]['id'],
  observed_on_string=df.get('Time')[index],
  latitude=df.get('lat')[index],
  longitude=df.get('long')[index],
  positional_accuracy=precision,
  description=df2.get('Path')[index],
  access_token=token)
  
  new_observation_id = response['id']
  
  # Alternative options to send pictures
  # upload_photos(new_observation_id,df2.get('Path')[index], access_token=token)
  try:
    add_photo_to_observation ( observation_id=new_observation_id, access_token=token,
  file_object=df2.get('Path')[index], photos=df2.get('Path')[index]) 
  except:
    print(index)
    
# Next: try to add picture to an existing observation without photo
