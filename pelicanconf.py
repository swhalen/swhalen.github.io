#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Simon Whalen'
SITENAME = u'Simon Whalen'
SITEURL = ''

PATH = 'content'
THEME = 'theme'

TIMEZONE = 'Pacific/Auckland'

DEFAULT_LANG = u'en'

DEFAULT_PAGINATION = False
AUTHOR_SAVE_AS = ''
CATEGORY_SAVE_AS = ''

MENUITEMS = (("Home", "/"), ("Blog", "/blog.html"))
DIRECT_TEMPLATES = ('index', 'blog', 'archives')
PAGINATED_DIRECT_TEMPLATES = ('blog',)

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None
