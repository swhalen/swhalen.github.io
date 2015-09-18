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

# Disable Author pages, Categories, Tags
AUTHOR_SAVE_AS = ''
CATEGORY_SAVE_AS = ''
TAGS_SAVE_AS = ''
TAG_SAVE_AS = ''

# Paths
STATIC_PATHS = ['static']
ARTICLE_PATHS = ['posts']

# URLs
ARTICLE_URL = "blog/{slug}/"
ARTICLE_SAVE_AS = "blog/{slug}/index.html"
PAGE_URL = "{slug}/"
PAGE_SAVE_AS = "{slug}/index.html"

DIRECT_TEMPLATES = ('index',)

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

# Don't need the cache
CACHE_CONTENT = False
