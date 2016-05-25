# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html

import csv
import items


class MainPipeline(object):

    def init_evt_writers(self):

        self.evtCsv = csv.writer(open('../../DATA/MAIN/' +
                                      'events.csv', 'wb'))
        self.evtCsv.writerow(['date',
                              'symbol',
                              'name',
                              'surprise',
                              'eps',
                              'eeps'])

        self.evtProfileCsv = csv.writer(open('../../DATA/MAIN' +
                                             '/profiles.csv', 'wb'))
        self.evtProfileCsv.writerow(['symbol',
                                     'name',
                                     'bourse',
                                     'country',
                                     'sector',
                                     'industry'])

        self.evtSymbolCsv = csv.writer(open('../../DATA/MAIN' +
                                            '/symbol-changes.csv', 'wb'))
        self.evtSymbolCsv.writerow(['oldSymbol',
                                    'newSymbol'])

    def init_dir_writers(self):

        self.dirProfileCsv = csv.writer(open('../../DATA/SIM' +
                                             '/profiles.csv', 'wb'))
        self.dirProfileCsv.writerow(['symbol',
                                     'name',
                                     'bourse',
                                     'country',
                                     'sector',
                                     'industry'])

        self.dirSymbolCsv = csv.writer(open('../../DATA/SIM' +
                                            '/symbol-changes.csv', 'wb'))
        self.dirSymbolCsv.writerow(['oldSymbol',
                                    'newSymbol'])

    def open_spider(self, spider):
        if spider.name == 'directory-profiles':
            self.init_dir_writers()
        elif spider.name == 'surprise-profiles':
            self.init_evt_writers()

    def process_item(self, item, spider):

        if isinstance(item, items.EventItem):
            self.evtCsv.writerow([item['date'],
                                  item['symbol'],
                                  item['name'],
                                  item['surprise'],
                                  item['eps'],
                                  item['eeps']])
            return item

        if isinstance(item, items.EvtProfileItem):
            self.evtProfileCsv.writerow([item['symbol'],
                                         item['name'],
                                         item['bourse'],
                                         item['country'],
                                         item['sector'],
                                         item['industry']])
            return item

        if isinstance(item, items.EvtSymbolItem):
            self.evtSymbolCsv.writerow([item['symbol'],
                                        item['newSymbol']])
            return item

        if isinstance(item, items.DirProfileItem):
            self.dirProfileCsv.writerow([item['symbol'],
                                         item['name'],
                                         item['bourse'],
                                         item['country'],
                                         item['sector'],
                                         item['industry']])
            return item

        if isinstance(item, items.DirSymbolItem):
            self.dirSymbolCsv.writerow([item['symbol'],
                                        item['newSymbol']])
            return item
