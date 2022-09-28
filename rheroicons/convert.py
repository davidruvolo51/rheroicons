#'////////////////////////////////////////////////////////////////////////////
#' FILE: convert.py
#' AUTHOR: David Ruvolo
#' CREATED: 2022-09-28
#' MODIFIED: 2022-09-28
#' PURPOSE: convert heroicons into an R object
#' STATUS: stable
#' PACKAGES: os, datetime, tqdm, json, re, sys
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

from os import getcwd, path, listdir
from datetime import datetime
from tqdm import tqdm
import json
import re
import sys
sys.path.append(getcwd())

def parseSvgContent(content, type, name):
  """Format SVG markup
  Add height and width attributes and CSS classnames
  
  @param content output of readlines
  @param size heroicons icon set size (20, 24)
  @param style heroicons icon style (outline, solid)
  
  @return string containing svg markup
  """
  markup = ''.join([line.strip() for line in content])
  viewbox = re.search('viewBox\=.([\\w\\s]+).', markup)
  dims = re.sub(r'([a-zA-Z="])', '', viewbox.group()).split(' ')
  css = f"rheroicons rheroicons-{type} rheroicons-{name}"
  attribs = f'class="{css}" width="{dims[2]}" height="{dims[3]}"'
  return re.sub(r'^(<svg )', f"<svg {attribs} ", markup)


# ~ 0 ~
# Preparation: Download the latest release
# Instead of waiting for the latest Heroicons release on NPM, download the
# release from the Heroicons repository. Run the following code to view the
# available releases. Select the release that you would like to work with
# (by default the "latest" release is selected) and download to the "downloads"
# folder. The SVG files will be located in the following location:
# downloads/tailwindlabs-heroicons-<release>/optimized/*

# from rheroicons.api.github import github
# gh = github(owner='tailwindlabs', repo='heroicons')
# gh.listReleases()
# gh.downloadRelease(outDir='downloads')


# ~ 1 ~
# Compile file metadata for all svg files
# SVG files are stored in the optimized folder by icon size (20 or 24). The
# 20 pixel icons only come in solid, while the 24 pixel icons have two styles
# outline and solid.

basedir = f"downloads/{listdir('downloads/')[0]}/optimized"
if not path.isdir(basedir):
  SystemError("Entry point does not exist. Check the release structure in downloads/")
  

# list svg files by icon size and style
entrypoints = [{'path': f"{basedir}/{dir}", 'size': dir} for dir in listdir(basedir)]
svgfiles = []

for entrypoint in tqdm(entrypoints, desc='Building svg files'):
  subdirs = listdir(entrypoint['path'])
  for dir in subdirs:
    path = f"{entrypoint['path']}/{dir}"
    files = listdir(f"{entrypoint['path']}/{dir}")
    for file in files:
      svgfiles.append({
        'path': f"{path}/{file}",
        'type': dir,
        'size': entrypoint['size'],
        'name': file.replace('.svg', '')
      })
  
# ~ 2 ~
# Convert Icons
# For each svg file, read the svg markup and clean for use in R.
# Make sure all line breaks are removed and strings are trimmed.
# Add the following attributes to the begining of the svg
# string: width, height, and class. After each file is processed,
# add it to the main rheroicons list by icon name. Nest each icon
# style by icon name using the following format.
# <rheroicons>
#   <icon>
#     <icon_style>
#       <icon_style_svg>
#   ....
#
# Save prepared data to json format and integrate into the R
# package in the dev/dev.R script.

rheroicons = {}
for svg in tqdm(svgfiles):
  with open(svg['path'], 'r') as file:
    content = file.readlines()
  file.close()
  svgmarkup = parseSvgContent(
    content = content,
    type = 'mini' if svg['size'] == '20' else svg['type'],
    name = svg['name']
  )
  if svg['name'] not in rheroicons:
    rheroicons[svg['name']] = {}
    
  if svg['size'] == '20':
    rheroicons[svg['name']]['mini'] = svgmarkup
  else:
    rheroicons[svg['name']][svg['type']] = svgmarkup

# check markup to validate structure
# rheroicons['academic-cap']

# save file
date = datetime.now().strftime('%y-%m-%d')
outputfile = f"dev/prepared-data/rheroicons.{date}.json"
with open(outputfile, 'w') as f:
  json.dump(rheroicons, f,ensure_ascii=False, indent=2)
f.close()
