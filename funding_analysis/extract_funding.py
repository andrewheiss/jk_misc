#!/usr/bin/env python3
import json
import csv
from collections import OrderedDict
from bs4 import BeautifulSoup
from pprint import pprint

# http://stackoverflow.com/a/480227/120898
def uniqify(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]

with open('raw_data/state_2004.json', 'r') as data_file:
    # Import all dicts as ordered!
    parsed = json.load(data_file, object_pairs_hook=OrderedDict)

with open('raw_data/' + parsed['text'], 'r') as raw:
    lines = raw.readlines()

current_region = ''
all_entries = []
entry = {}

if parsed['year'] == 2002:
    current_country = ''
    for line in lines:
        # Categorize each line
        # Get the region
        if line.startswith(tuple(parsed['regions'])):
            current_region = line.strip()
        # Parse individual fields
        elif line.startswith(tuple(parsed['fields'].keys())):
            for key, value in parsed['fields'].items():
                if line.startswith(key):
                    entry[value] = line.replace(key, '').strip()
        # If the line isn't the region or indiviudal fields, it's the country
        elif line.strip() != '':
            current_country = line.strip()

        # If the line is blank and the entry has fields, insert the entry into
        # the main list
        if line.strip() == '':
            if entry:
                entry['region'] = current_region

                if current_country == '':
                    entry['country'] = "NA"
                else:
                    entry['country'] = current_country.title()

                all_entries.append(entry)
                entry = {}

elif parsed['year'] == 2003:
    for line in lines:
        if line.startswith(tuple(parsed['regions'])):
            current_region = line.strip().title()
        # Parse individual fields
        elif line.startswith(tuple(parsed['fields'].keys())):
            for key, value in parsed['fields'].items():
                if line.startswith(key):
                    entry[value] = line.replace(key, '').strip()

        # If the line is blank and the entry has fields, insert the entry into the main list
        if line.strip() == '':
            if entry:
                entry['region'] = current_region
                all_entries.append(entry)
                entry = {}

elif parsed['year'] == 2004:
    # Parse the text with BeautifulSoup and get all the <tr> tags
    soup = BeautifulSoup(''.join(lines))
    html_rows = soup.select('tr')

    # Extract plain text from HTML and add to list
    raw_text = []
    for row in html_rows:
        p_tags = row.findAll('p')
        for actual_text in p_tags:
            raw_text.append(actual_text.text)

    # Combine the list of raw text into one big string and split into a list of
    # entries based on three newlines
    raw_entries = '\n'.join(raw_text).split('\n\n\n')

    print(raw_entries[1])

columns = uniqify(['region', 'country'] + list(parsed['fields'].values()))

# Write to CSV
with open('clean_data/state_{0}.csv'.format(parsed['year']), 'w') as file:
    writer = csv.DictWriter(file, fieldnames=columns)
    writer.writerow(dict((col, col) for col in columns))
    for row in all_entries:
        writer.writerow(row)
