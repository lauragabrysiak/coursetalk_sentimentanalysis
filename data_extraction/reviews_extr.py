#---------------------------------------------------------------------------------
# Name: reviews_extractor.py
# Author: Laura Gabrysiak Gomez
# Date: 2016/04/10
# Description: Crawler using transaction file to etract the reviews of selected courses.
#---------------------------------------------------------------------------------
import sys
import re
import csv
import requests as req
from bs4 import BeautifulSoup as bsoup
#---------------------------------------------------------------------------------
# Predefined url list dnde by selection process in R
with open("urls.csv") as course_urls:
    for line in map(str.strip, course_urls):        
        url = 'https://www.coursetalk.com/providers/{}'.format(line)
        r = req.get(url)
        soup = bsoup(r.content)

        page_count_links = soup.find_all("a", href=re.compile(r".*page=.*"))

        try: # If page=1 then [n=1] else [3>n>2]
            num_pages = int(page_count_links[0].get_text())
        except IndexError:
            num_pages = 1

        # Add 1 because of Python range.
        url_list = ["{}?page={}#reviews".format(url, str(page)) for page in range(1, num_pages + 1)]

        with open("results2.txt","wb") as reviews:
            review_list = []
            for url_ in url_list: #For last url of the list (url_)
                r_new = req.get(url_)
                soup_new = bsoup(r_new.text)

                for item in soup_new.head:
                    course_title = soup_new.head.title.text.encode('utf-8').strip()
                    course_title = course_title.replace(' - online course reviews and ratings | CourseTalk','')
                    course_provider_lookup = re.search('by.*', course_title)

                    if course_provider_lookup:
                        course_provider = course_provider_lookup.group().encode('utf8')
                        course_provider = course_provider.replace('by','')
                        course_provider = course_provider.replace('\n', '')
                    course_title = re.sub('by .*','',course_title)

                for item in soup_new.body:
                    course_fee = soup_new.find_all("div", {"class" : "course-enrollment-details__detail--narrow course-enrollment-details__detail--cost"})[0].text.encode('utf-8').strip()
                    course_fee = course_fee.replace('\n', '')
                    course_instructor = soup_new.find_all("div", {"class" : "course-info__academic__item"})[0].text.encode('utf-8')
                    course_instructor = course_instructor.replace('\n\nInstructors:\xc2\xa0\n                            ','').strip()
                    course_instructor = course_instructor.replace('\n', '')
                    course_stage = soup_new.find_all("span", {"class" : re.compile("^review-body-info__course-stage")})[0].text.encode('utf-8').strip()
                    course_stage = course_stage.replace('\n', '')
                    course_description = soup.find_all("div", {"class" : "course-info__academic__item--extra-whitespace"})[0].text.encode('utf-8').strip()
                    course_description = course_description.replace('\n', '')

                for item in soup_new.find_all("div", {"class": "review js-review"}):
                    try:
                        username = item.contents[1].find_all("p", {"class" : "userinfo__username"})[0].text.encode('utf-8').strip()
                        date = item.contents[1].find_all("time", {"class":"review-body-info__pubdate"})[0].text.encode('utf-8').strip()
                        course_instructor = course_instructor.replace('\xc2\xa0', ' ').strip()
                        rating = item.contents[1].find_all("span", {"class":"sr-only"})[0].text.encode('utf-8').strip().strip()
                        rating = rating.replace('Student rates this course', '')
                        status = item.contents[1].find_all("span", {"class":re.compile("^review-body-info__course-stage")})[0].text.encode('utf-8').strip()

                        review = item.contents[1].find_all("div", {"class":"review-body__content"})[0].text.encode('utf-8').strip()
                    except:
                        pass
                    review_each = [course_provider, course_title, course_instructor, course_stage, course_description, date, username, status, rating, review]
                    review_list.append(review_each)
                    print(review_each)

                    file = open('log.txt','w')
                    value = str(review_list) + '\n'
                    file.write(value)
                    file.close()
