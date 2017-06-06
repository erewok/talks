from urllib import parse

import requests

def post_counterpost():
    """
    POST "counter-post"


    Returns: 
        JSON response from the endpoint
    """
    url = "http://localhost:8000/counter-post"

    resp = requests.post(url)
    resp.raise_for_status()
    return resp.json()


def post_countermultiplier_by_mult(mult):
    """
    POST "counter-multiplier/{mult}"
    Args: 
        mult

    Returns: 
        JSON response from the endpoint
    """
    url = "http://localhost:8000/counter-multiplier/{mult}".format(
        mult=parse.quote(str(mult)))

    resp = requests.post(url)
    resp.raise_for_status()
    return resp.json()


def post_counterresetpost(data):
    """
    POST "counter-reset-post"


    Returns: 
        JSON response from the endpoint
    """
    url = "http://localhost:8000/counter-reset-post"

    resp = requests.post(url,
                         json=data)

    resp.raise_for_status()
    return resp.json()


def get_counterqueryparam(sortby, headerSomeHeader):
    """
    GET "counter-queryparam"


    Returns: 
        JSON response from the endpoint
    """
    url = "http://localhost:8000/counter-queryparam"

    headers = {"Some-Header": headerSomeHeader}
    params = {"sortby": sortby}
    resp = requests.get(url,
                        headers=headers,
                        params=params)

    resp.raise_for_status()
    return resp.json()

