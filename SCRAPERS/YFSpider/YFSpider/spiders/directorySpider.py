import scrapy

import re
from YFSpider.items import DirProfileItem, DirSymbolItem


class DirectorySpider(scrapy.Spider):
    name = "directory-profiles"
    allowed_domains = ["us.rd.yahoo.com", "biz.yahoo.com", "finance.yahoo.com"]
    start_urls = ["http://biz.yahoo.com/p/industries.html"]

    def parse(self, response):

        # go to any company list or summaries
        for relurl in response.xpath("//a[(contains(@href, " +
                                     "'conameu.html'))]/@href").extract():
            fullurl = response.urljoin(relurl)
            yield scrapy.Request(url=fullurl, callback=self.parse)

        # follow all profile links containing
        # us.rd.yahoo.com/finance/industry/quote/colist/*http://biz.yahoo.com/
        for url in response.xpath("//a[(contains(@href, 'us.rd.yahoo.com" +
                                  "/finance/industry/quote/colist/*http:" +
                                  "//biz.yahoo.com/'))]/@href").extract():
            yield scrapy.Request(url=url,
                                 # meta={'dont_redirect': True},
                                 callback=self.parse_stock_profile)

    def parse_stock_profile(self, response):

        profile = DirProfileItem()

        symchg = DirSymbolItem()

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
