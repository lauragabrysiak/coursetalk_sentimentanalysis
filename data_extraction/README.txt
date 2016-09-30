* 1- What the project does:
This project was developed in the context of my Master Thesis "MOOC Interrupted: Determination of (Dis-)Engagement Factors using Sentiment Analysis." at the Humboldt University of Berlin under the supervision of Prof.Dr Stephan Lessmann. This project is comformed by two parts: 
	1- the first crawler extracts main features (including the URL) of a list of courses, using the surface of the platform Coursera.
	2- the second crawler uses this (selected-) URL list and for each URL link opens the page and extracts (over the embedded pages) all posted reviews.
	
* Example usage:
	1- URL extractor (url_extr.py)
	the important information to add is the URL domain ['coursetalk.com']
	and start URL which is in this case: ['https://www.coursetalk.com/search']

	2- REVIEWS extractor (reviews_extr.py)
	a trasaction file (*.csv) with the selected URLs is inserted.

* How to set up the dev environment
	Pycharm Installation guide: (https://www.jetbrains.com/pycharm-edu/quickstart/installation.html)
	> [...]/ tar xfz pycharm-.tar.gz.
	> [...]/ ./pycharm.sh

	Scrappy Installation guide: (http://doc.scrapy.org/en/latest/intro/install.html)
	You can either install Scrappy with:
	> pip install Scrapy
	> conda install -c scrapinghub scrapy

	BeautifulSoup Installation guide: (https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
	> $ apt-get install python-bs4
	> $ pip install beautifulsoup4
	> $ easy_install beautifulsoup4
	
* 2-License and author info

MIT License

Copyright (c) [2016] [Laura Gabrysiak Gomez]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
