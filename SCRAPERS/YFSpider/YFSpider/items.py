# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy

from scrapy.item import Item, Field


class EventItem(Item):
    date = Field()
    symbol = Field()
    name = Field()
    surprise = Field()
    eps = Field()
    eeps = Field()


class EvtProfileItem(Item):
    symbol = Field()
    name = Field()
    bourse = Field()
    country = Field()
    sector = Field()
    industry = Field()


class EvtSymbolItem(Item):
    symbol = Field()
    newSymbol = Field()


class DirProfileItem(Item):
    symbol = Field()
    name = Field()
    bourse = Field()
    country = Field()
    sector = Field()
    industry = Field()


class DirSymbolItem(Item):
    symbol = Field()
    newSymbol = Field()
