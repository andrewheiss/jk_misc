#!/usr/bin/env python3
import re
import os
from bs4 import BeautifulSoup
from collections import namedtuple
from glob import glob
from PIL import Image  # Actually Pillow...

final_name = "Appendix 2"

tables = glob('table_a*.html')
fig_images = glob('../final_figures/figureA_*.png')
fig_captions = glob('../final_figures/figureA_*.txt')

figure = namedtuple('Figure', ['chapter', 'image', 'caption'])
figures = [figure(re.search(r"figureA_(\d)_", x[0]).group(1), *x)
           for x in zip(fig_images, fig_captions)]

# TODO: Use proper stylesheet?

# stargazer outputs unescaped less-than signs, which BeautifulSoup chokes on
bad_stars = '<sup>*</sup>p**p***p'
good_stars = '<sup>*</sup>p &lt; 0.1; <sup>**</sup>p &lt; 0.05; <sup>***</sup>p &lt; 0.01'

document_template = """<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>{0}</title>
</head>
<body>
<h1>{0}</h1>
{1}
</body>
</html>
"""

all_tables = []
last_chapter = ''
for i, table in enumerate(tables):
    # Get the chapter number for the current table
    current_chapter = re.search(r"_a(\d)_", table).group(1)

    # Determine if this is the last table in the chapter
    if i != len(tables) - 1:
        next_chapter = re.search(r"_a(\d)_", tables[i + 1]).group(1)
        last_table_in_chapter = current_chapter != next_chapter
    else:
        last_table_in_chapter = True

    with open(table, 'r') as f:
        raw_html = f.read()

    # Parse the HTML
    soup = BeautifulSoup(raw_html, 'html.parser')

    if current_chapter != last_chapter:
        h2 = soup.new_tag('h1')
        h2.append('Chapter {0}'.format(current_chapter))
        soup.body.insert(0, h2)

        last_chapter = current_chapter

    table = soup.find('table')
    title = table.caption.get_text()
    table.caption.extract()

    h3 = soup.new_tag('h2')
    h3.append(title)
    soup.body.insert(1, h3)

    if last_table_in_chapter:
        chapter_figures = [fig for fig in figures if fig.chapter == current_chapter]
        if chapter_figures:
            for fig in chapter_figures:
                # Get figure caption
                with open(fig.caption, 'r') as f:
                    fig_caption = f.read().replace('\n', '')
                img_title = soup.new_tag('h2')
                img_title.append(fig_caption)

                # Get dimensions and rescale them for HTML tag
                width, height = Image.open(fig.image).size
                ratio = width / (4.5 * 72)  # Desired inches × Word DPI
                img = soup.new_tag('img', src=fig.image,
                                   width=width / ratio, height=height / ratio)

                # Add image to HTML
                soup.body.append(img_title)
                soup.body.append(img)

    html_final = ''.join([str(chunk) for chunk in soup.body.contents]).replace(bad_stars, good_stars)

    all_tables.append(html_final)

with open('{0}.html'.format(final_name), 'w') as f:
    f.write(document_template.format('Appendix 2: Statistical results',
                                     '\n'.join(all_tables)))

scpt = """set base_folder to "{0}/"
set file_in to base_folder & "{1}.html"
set file_out to base_folder & "{1}.docx"

tell application "Microsoft Word"
    activate
    open file_in

    set all_images to inline pictures of active document
    repeat with img in all_images
        break link link format of img
    end repeat

    set view type of view of active window to print view

    save as active document file name file_out file format format document
    close active document
end tell
""".format(os.getcwd(), final_name)

os.system("osascript -e '{0}'".format(scpt))
