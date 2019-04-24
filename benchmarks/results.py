import re
import collections
import json

def parse_results(output):
    return json.loads(output)

def show_results(results):
    return json.dumps(results)