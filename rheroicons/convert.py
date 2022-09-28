#'////////////////////////////////////////////////////////////////////////////
#' FILE: convert.py
#' AUTHOR: David Ruvolo
#' CREATED: 2022-09-28
#' MODIFIED: 2022-09-28
#' PURPOSE: convert heroicons into an R object
#' STATUS: in.progress
#' PACKAGES: NA
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

from os import path, listdir
from tqdm import tqdm
import re

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
  
# convert icons
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

