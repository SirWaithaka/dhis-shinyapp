import json
import requests

BASE_URL = "https://play.dhis2.org/release1/api"

AUTH = ("admin", "district")
session = requests.Session()
session.auth = AUTH


def index(value=2):
  
  return value


"""
- Handle endpoint to analytics url
"""
def analytics_api(payload={}):
  """
  Common parameters to this endpoint
  :
  :param dimension
  ::: dx -- (program,indicator)
  ::: ou -- (organisation unit)
  ::: pe -- (period)
  :param filter
  ::: dx -- (program,indicator)
  ::: ou -- (organisation unit)
  :param columns
  :param rows
  :param skipMeta {bool}
  """
  # endpoint
  path = "/analytics"
  
  # check if there is a value on payload AND ou,dx,pe
  if not payload.get("dx") or not payload.get("ou"):
    return json.dumps({"404": "Cant have null parameters"})
  
  dx = payload["dx"]
  ou = payload["ou"]

  params = {
    "dimension": [ "ou:"+ou, "pe:LAST_12_MONTHS" ],
    "filter": ["dx:"+dx],
    "skipMeta": "true",
    "displayProperty": "NAME"
  }

  url = BASE_URL + path
  
  session.params = params
  response = session.get(url)
  
  rows = response.json()["rows"]
  
  return rows



def organisationUnits_api(level=2):
  """
  Common parameters to this endpoint
  :
  :param paging {bool}
  :param fields {list}
  :param level {int}
  :param query
  """
  
  path = "/organisationUnits"
  
  params = {
    "level": level,
    "paging": "false",
    "fields": ["id", "name", "code"]
  }
  
  url = BASE_URL + path
  
  session.params = params
  response = session.get(url)
  
  return response.json()[path[1:]]



def indicator_members(group_id):
   # returns list of dicts with ids only
   def get_ids(group_id):

      path = "/indicatorGroups/"+group_id
      url = BASE_URL + path

      res = requests.get(url, auth=session.auth)
      return res.json()["indicators"]
      

   all_members_id = get_ids(group_id)

   members_list = []
   params = {
      "fields": ["id", "name"]
   }
   session.params = params
   path = "/indicators/"

   # # loop through list of dicts
   for key, val in enumerate(all_members_id):

      url = BASE_URL + path + val["id"]
      res = session.get(url)

      members_list.append(res.json())

   return members_list


"""
- Handle endpoint to indicators url
"""
def indicators_api(payload={}):
   """
   Common paramters to this endpoint
   :
   :param paging {bool}
   :param fields {list}
   :param query
   
   Additional args
   :arg members <string> @Returns members given <id>
   :arg kind <string> @Returns groups or individual
   """
   # /indicatorGroups

   path = "/indicatorGroups" if "".join(payload.get("group")) == "true" else "/indicators"

   params = {
      "paging": "false",
      "fields": ["id", "name"]
   }

   id = payload.get("members")
   if id:
      return indicator_members("".join(id))

   url = BASE_URL + path

   session.params = params
   response = session.get(url)

   # slice the path var to return
   # matching response always 
   return response.json()[path[1:]]

