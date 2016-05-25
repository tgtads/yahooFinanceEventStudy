#!/usr/bin/python3

# DESCRIPTION
# An efficient python script that reads a line separated list of stock symbols
# with optional start and end dates for range and saves the relevant daily
# volume and adjusted closing prices from Yahoo Finance.
# a logfile is also created to summarise the outcome in terms of available data

# please note that yahoo will return a different payload than expected if
# the start or end dates requested do not match global calendar dates,
# such as 2015-06-31
# I leave it to the user to check for this.

# for usage, use -h


import argparse
import urllib.request
import re
import csv
import os
from collections import defaultdict


# read a csv file and return dictionary objects
def get_csv_dict(path):
    dictObjs = []
    with open(path) as fileObj:
        # assuming the first line is a header
        header = csv.reader(fileObj, delimiter=",", quotechar='"').__next__()
        for line in csv.DictReader(fileObj, fieldnames=header):
            dictObjs.append(line)
    return dictObjs
    # why not convert to a list of objects than create and append?


# if it does not exist, it must be created
def init_folder(path):
    if os.path.exists(path):
        if os.path.isdir(path):
            return True
        else:
            print("File [%s] will not be overwritten" % path)
            return False
    else:
        try:
            os.makedirs(path)
            return True
        except FileExistsError as e:
            print("File Error [%s] with [%s]" % (e, path))
            return False


# forming urls specifically for the yahoo service
def form_url(symbol, start="", end="", frequency="d"):
    # check format, adjust month number, format to string
    # or leave blank if does not conform
    if re.search("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", start):
        dateInts = [int(d) for d in re.split("-", start)]
        dateInts[1] -= 1
        startDForm = "&c=%d&a=%d&b=%d" % tuple(dateInts)
    else:
        startDForm = ""
    if re.search("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", end):
        dateInts = [int(d) for d in re.split("-", end)]
        dateInts[1] -= 1
        endDForm = "&f=%d&d=%d&e=%d" % tuple(dateInts)
    else:
        endDForm = ""
    url = ("http://real-chart.finance.yahoo.com/table.csv" +
           "?s=" + symbol + endDForm + "&g=" + frequency + startDForm +
           "&ignore=.csv")
    return url


# cleanly return the results of a web request
def req(url):
    try:
        return urllib.request.urlopen(url)
    except urllib.request.URLError as e:
        print("HTTP Error [%s] with [%s]" % (e.code, url))


# return the http object contents in usable format
def read_decode(httpObj):
    body = httpObj.read()
    httpObj.close()
    try:
        return body.decode('utf-8')  # required, but a bottleneck
    except UnicodeDecodeError as e:
        print("Decode Error [%s]" % e)


# reform provided payload items for legibility and remove irrelevant variables
def reform_payload(items):
    # reversing the headed list into continual order
    items.append(items[0])
    items.reverse()
    items.pop()
    # rename the header fields
    items[0] = re.sub("Date", "date", items[0])
    items[0] = re.sub("Volume", "v", items[0])
    items[0] = re.sub("Adj Close", "p", items[0])
    # determine if the date format requires reformatting
    reformDate = True if (re.search("^[0-9]{2}/[0-9]{2}/[0-9]{4},.*",
                                    items[1])) else False
    # for each line, split by comma, extract only the desired elements
    for i in range(len(items)):
        items[i] = re.sub(",[^,]*,[^,]*,[^,]*,[^,]*", "", items[i])
        if reformDate:
            items[i] = ("%s-%s-%s%s" % (items[i][6:10],
                                        items[i][3:5],
                                        items[i][0:2],
                                        items[i][10:]))
    return items


# write list items en masse to a file
def write_items(path, items, mode="at"):
    with open(path, mode) as fileObj:
        try:
            for i in items:
                fileObj.write("%s\n" % i)
        finally:
            fileObj.close()


# write a line of text to a file
def writeln(path, text, mode="at"):
    with open(path, mode) as fileObj:
        try:
            fileObj.write("%s\n" % text)
            return True
        except:
            print("File error: could not write [%s] to [%s]" % (text, path))
            return False
        finally:
            fileObj.close()


# find unique items and preserve order. By Dave Kirby
def unique(seq):
    seen = set()
    return [x for x in seq if x not in seen and not seen.add(x)]


# transform a nested dictionary into (unique key), (value) pairs
def reform_items(items):
    s = []
    for i in items:
        s.append((i['symbol'], i['tradingDate']))
    d = defaultdict(list)
    for k, v in s:
        d[k].append(v)
    return d



# main section =========================================================

# start with parsing the arguments

argparser = argparse.ArgumentParser()
argparser.add_argument("items", type=str,
                       help=("CSV format items of stock symbols " +
                             "and dates of interest, YYYY-MM-DD format"))
argparser.add_argument("folder", type=str,
                       help=("The path to a folder to which stock " +
                             "price files are saved."))
argparser.add_argument("log", type=str,
                       help=("The path to a machine-readable logfile."))
argparser.add_argument("-s", "--startDate", type=str,
                       help=("Initial date sought for the range of " +
                             "time series data, YYYY-MM-DD format"))
argparser.add_argument("-e", "--endDate", type=str,
                       help=("Final date sought for the range of time " +
                             "series data, YYYY-MM-DD format"))

args = argparser.parse_args()
items = get_csv_dict(args.items)
initFolderSuccess = init_folder(args.folder)
initLogSuccess = writeln(path=args.log, mode="wt",
                         text="symbol,tradingDate,position,datapoints")
startDate = str(args.startDate) if args.startDate else ""
endDate = str(args.endDate) if args.endDate else ""


if items and initFolderSuccess and initLogSuccess:

    uniqueSymbols = unique(list(i['symbol'] for i in items))

    rItems = reform_items(items)

    for symbol in uniqueSymbols:
        print("Accessing %s" % symbol)

        # get the raw payload
        httpObj = req(form_url(symbol=symbol,
                               start=startDate,
                               end=endDate))

        if httpObj:
            # transform it to list items and check the number of rows
            nData = 0
            payload = re.split("\n", read_decode(httpObj))
            if payload:
                if payload[-1] == "":
                    payload.pop()  # workaround for final \n on split
                nData = len(payload) - 1

            # write the reformed payload
            rPayload = reform_payload(payload)
            write_items(path=("%s/%s.csv" % (args.folder, symbol)),
                        items=rPayload,
                        mode="wt")
            # get position of each tradingDate and write it to logfile
            for tradingDate in rItems[symbol]:
                position = ""
                if rPayload:
                    pattern = re.compile(tradingDate)
                    for pos in range(len(rPayload)):
                        if not position:
                            if pattern.match(rPayload[pos]):
                                position = str(pos)
                # perhaps it might be quicker to make a list of the results
                # and use write_items instead?
                writeln(path=args.log, mode="at",
                        text=("%s,%s,%s,%s" % (symbol,
                                               tradingDate,
                                               position,
                                               str(nData))))
