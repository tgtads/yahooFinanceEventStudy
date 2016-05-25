import scrapy

import re
from YFSpider.items import EventItem, EvtProfileItem, EvtSymbolItem


class SurpriseSpider(scrapy.Spider):
    name = "surprise-profiles"
    allowed_domains = ["biz.yahoo.com", "finance.yahoo.com"]
    start_urls = ["http://biz.yahoo.com/z/20110103.html"]

    def parse(self, response):

        # extract and follow weekday links, unless links lead beyond range.
        for href in response.xpath("//center[position()=4]/a/@href"):
                relurl = href.extract()
                year = int(relurl[0:4])
                # controls which range of dates the spider looks at
                if (2010 < year < 2016):
                    fullurl = response.urljoin(relurl)
                    yield scrapy.Request(url=fullurl, callback=self.parse)

        table = response.xpath("//table[position()=5]")

        # extracting non-header rows with 7 columns with non-zero surprises
        rows = []
        for node in table.xpath("tr"):
            if node.xpath("@bgcolor").extract_first() != "dcdcdc":
                if (len(node.xpath("td")) == 7 and
                    node.xpath("td[position()=3]//text()").extract_first() != "0.00"):
                    rows.append(node)

        date = re.search("[0-9]{8}", response.url).group(0)

        for row in rows:
            # dig out the link to the profile
            # ie, http://finance.yahoo.com/q/pr?s=chmp
            url = row.xpath(".//a[contains(., 'Pro" +
                            "file')]/@href").extract_first()
            if url:
                yield scrapy.Request(url=url,
                                     meta={'dont_redirect': True},
                                     callback=self.parse_stock_profile)
                # if a redirect occurs, there is nothing to process

        for row in rows:
            event = EventItem()
            event['date'] = '{y}-{m}-{d}'.format(y=date[0:4],
                                                 m=date[4:6],
                                                 d=date[6:8])

            # in order to catch empty cells, we will need to iterate
            # each cell individually
            vals = []
            for cell in row.xpath("td"):
                val = cell.xpath(".//text()").extract_first()
                vals.append(val)

            (event['name'],
             event['symbol'],
             event['surprise'],
             event['eps'],
             event['eeps']) = vals[0:5]

            yield event

    def parse_stock_profile(self, response):

        profile = EvtProfileItem()

        symchg = EvtSymbolItem()

        # check for, record and follow
        # "Changed Ticker Symbol", ie
        # https://finance.yahoo.com/q/pr?s=bota

        titleText = response.xpath("//title/text()").extract_first()
        isInvalid = re.search("^Invalid", titleText)

        if isInvalid:
            # record the symbol change
            cell = response.xpath("//table[@id='yfncsumtab']" +
                                  "/tr[position()=2]/td")
            items = cell.xpath("./big//b//text()").extract()
            symchg['symbol'] = re.sub("\"", "", items[0])
            symchg['newSymbol'] = items[1]
            yield symchg

            # follow the link
            relurl = cell.xpath(".//a[(contains(@href, " +
                                "'/q/pr?s='))]/@href").extract_first()
            fullurl = response.urljoin(relurl)
            yield scrapy.Request(url=fullurl,
                                 meta={'dont_redirect': True},
                                 callback=self.parse_stock_profile)

        else:

            titleDiv = response.xpath("//div[@class='title']")
            titleStr = re.sub("^\s*|\s*$", "",
                              "".join(titleDiv.xpath(".//text()").extract()))

            # extracting the bourse name
            (nameSymbol, bourse) = re.split("\) -", titleStr)

            # making a single right-hand regex split
            # prepare the string by reversing it:
            nameSymbol = nameSymbol[::-1]
            p = re.compile(r"\( ")  # reversed pattern
            (symbol, name) = p.split(nameSymbol, maxsplit=1)
            # reverse the substrings
            name = name[::-1]
            symbol = symbol[::-1]

            profile['name'] = name.encode('utf-8')
            profile['symbol'] = symbol
            profile['bourse'] = bourse

            # dig out the link to the map, extract last query parameter
            maplink = response.xpath("//a[(contains(@href, " +
                                     "'/maps_result?'))]/@href").extract_first()
            if maplink:
                profile['country'] = re.sub("%20", " ",
                                            re.sub(".*&country=", "", maplink))
            else:
                profile['country'] = ""

            profile['sector'] = response.xpath(".//td[text()='Sector:']" +
                                               "/../td[position()=2]" +
                                               "//text()").extract_first()

            profile['industry'] = response.xpath(".//td[text()='Industry:']" +
                                                 "/../td[position()=2]" +
                                                 "//text()").extract_first()

            yield profile
