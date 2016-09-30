#---------------------------------------------------------------------------------
# Name: reviews_extractor.py
# Author: Laura Gabrysiak Gomez
# Date: 2016/04/10
# Description: Crawler using transaction file to etract the reviews of selected courses.
#---------------------------------------------------------------------------------
import urlparse
import scrapy
from CourseTalk.items import CoursetalkItem
#---------------------------------------------------------------------------------
class CoursetalkSpider(scrapy.Spider):
    name = 'coursetalk'
    allowed_domains = ['coursetalk.com']
    start_urls = ['https://www.coursetalk.com/search']

    def parse(self, response):
        for course in response.xpath('//a[@class=\'link-unstyled js-course-search-result\']'):
            print 'processing'
            course_url = ''.join(course.xpath('./@href').extract()).strip()
            if course_url:
                course_url = urlparse.urljoin(response.url, course_url)
            meta = {'course_url': course_url}
            yield scrapy.Request(
                course_url,
                callback=self.parse_coursecontent,
                dont_filter=True,
                meta=meta)
        # turn page:
        next_page = ''.join(response.xpath('//div[@class=\'js-course-pagination\']//li/a[@aria-label=\'Next\']//@href').extract()).strip()
        next_page_num = response.xpath('//div[@class=\'js-course-pagination\']//li/a[@aria-label=\'Next\']//@data-page-number').extract()
        page_limit = response.xpath('//div[@class=\'js-course-pagination\']//li//a/@data-page-number').extract()[-2]
        if next_page_num <= page_limit:
            next_page = urlparse.urljoin(response.url, next_page)
            yield scrapy.Request(
                next_page,
                callback=self.parse,
                dont_filter=True,
            )
        else:
            pass
    #Using Xpath to extract features
    def parse_coursecontent(self, response):
        course_url = response.meta['course_url']
        course_name = ''.join(response.xpath('//h1[@class=\'course-header__name__title\']//text()').extract().encode('utf-8')).strip()
        course_price = ''.join(response.xpath('//div[@class=\'course-enrollment-details__detail--narrow course-enrollment-details__detail--cost\']//text()').extract().encode('utf-8')).strip()
        course_desc = ''.join(response.xpath('//div[@class=\'course-info__academic__item--extra-whitespace\']//text()').extract().encode('utf-8')).strip()
        course_university = ''.join(response.xpath('//i[@class=\'course-info__academic__school-icon\']/../text()').extract().encode('utf-8')).strip()
        course_instructor = ''.join(response.xpath('//i[@class=\'course-info__academic__instuctor-icon\']/../text()').extract().encode('utf-8')).strip()
        course_provider = ''.join(response.xpath('//div[@class=\'course-enrollment-details course-enrollment-details--dashed\']/div[@itemprop=\'seller\']//a/img/@alt').extract().encode('utf-8')).strip()
        course_review_num = ''.join(response.xpath('//div[@itemprop=\'aggregateRating\']/div[@class=\'course-rating__count\']/span[@class=\'course-additional-info__reviews-count\']//text()').extract().encode('utf-8')).strip()
        course_rating = ''.join(response.xpath('//div[@itemprop=\'aggregateRating\']/div[@class=\'course-rating__stars\']//meta[@itemprop=\'ratingValue\']/@content').extract().encode('utf-8')).strip()
        course_date = ''.join(response.xpath('//div[@class=\'course-listing-details__startdate\']//text()').extract().encode('utf-8')).strip()
	#Define features
        item = CoursetalkItem()
        item['course_url'] = course_url
        item['course_name'] = course_name
        item['course_price'] = course_price
        item['course_date'] = course_date 
        item['course_desc'] = course_desc
        item['course_university'] = course_university
        item['course_instructor'] = course_instructor
        item['course_provider'] = course_provider
        item['course_review_num'] = course_review_num
        item['course_rating'] = course_rating #overall_rating

        yield item
