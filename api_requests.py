import pandas as pd
import requests
from datetime import datetime, date, timedelta


def matrix_authenticate(session, username, password):

    url = "https://app.matrixbooking.com/api/v1/user/login"
    session.post(url, json={"username": username, "password": password})
    return session


def make_booking_params(time_from, time_to, status=None, pageSize=None, pageNum=0):
    params = {
        "f": time_from,
        "t": time_to,
        "bc": "ROOM",
        "status": status,
        "include": ["audit", "locations"],
        "pageSize": pageSize,
        "pageNum": pageNum,
    }
    return params


def get_payload(session, url, parameters):
    resp = session.get(url=url, cookies=session.cookies, params=parameters)
    print(f"GET {resp.url}")
    print(f"response status code: {resp.status_code}")
    return resp.json()
    
def get_locations_from_api(username, password):
  ses = requests.session()
  matrix_authenticate(ses, username, password)
  
  url = "https://app.matrixbooking.com/api/v1/booking"
  
  params = make_booking_params(time_from = date.today(), time_to = "eod", status = ["CONFIRMED", "TENTATIVE", "CANCELLED"])
  
  data = get_payload(ses, url, params)
  return(pd.io.json.json_normalize(data['locations']))
