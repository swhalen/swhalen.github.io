from pelican import signals


def format_resource(gen, metastring, formatter):
    """
    Create a list of URL-formatted script/style tags

    Parameters
    ----------
    gen: generator
        Pelican Generator
    metastring: string
        metadata['scripts'] or metadata['styles']
    formatter: string
        String format for output.

    Output
    ------
    List of formatted strings
    """
    metalist = metastring.replace(" ", "").split(',')
    site_url = gen.settings['SITEURL']
    return [formatter.format(site_url, x) for x in metalist]


def add_tags(gen, metadata):
    """
        The registered handler for the dynamic resources plugin. It will
        add the scripts and/or styles to the article
    """
    if 'scripts' in metadata.keys():
        script = '<script src="{1}"></script>'
        metadata['scripts'] = format_resource(gen, metadata['scripts'], script)

    if 'styles' in metadata.keys():
        style = '<link rel="stylesheet" href="{1}" type="text/css" />'
        metadata['styles'] = format_resource(gen, metadata['styles'], style)

    if 'd3' in metadata.keys():
        d3_script = '<script src="//d3js.org/d3.v3.min.js"></script>'
        metadata['scripts'].insert(0, d3_script)


def register():
    """
        Plugin registration
    """
    signals.article_generator_context.connect(add_tags)
    signals.page_generator_context.connect(add_tags)
