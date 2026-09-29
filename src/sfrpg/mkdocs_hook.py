"""MkDocs hook (see `hooks:` in mkdocs.yml): expand data macros in each page."""

from sfrpg.book import expand


def on_page_markdown(markdown, page, config, files):
    return expand(markdown)
