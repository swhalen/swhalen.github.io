#!/usr/bin/env python

AUTHOR = u'Simon Whalen'
SITENAME = u'Simon Whalen'
SITEURL = ''

PATH = 'content'
THEME = 'theme'

TIMEZONE = 'Europe/London'

DEFAULT_LANG = u'en'

DEFAULT_PAGINATION = False

TYPOGRIFY = True

# Disable Author pages, Categories, Tags
AUTHOR_SAVE_AS = ''
CATEGORY_SAVE_AS = ''
TAGS_SAVE_AS = ''
TAG_SAVE_AS = ''

# Plugins
PLUGIN_PATHS = ['../pelican-plugins']
PLUGINS = ['pandoc_reader', 'dynamic']

# Pandoc
PANDOC_ARGS = [
    '--mathjax',
    '--smart',
    '--parse-raw',
    '-Fpandoc-citeproc',
    '--csl=american-physics-society.csl',
]

PANDOC_EXTENSIONS = [
    '-markdown_in_html_blocks',
]

# Paths
STATIC_PATHS = ['static', 'blog']
ARTICLE_PATHS = ['blog']

# URLs
USE_FOLDER_AS_CATEGORY = True
ARTICLE_URL = "blog/{category}/"
ARTICLE_SAVE_AS = "blog/{category}/index.html"
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
