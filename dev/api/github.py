#'////////////////////////////////////////////////////////////////////////////
#' FILE: ghReleaseDownloader.py
#' AUTHOR: David Ruvolo
#' CREATED: 2021-09-10
#' MODIFIED: 2022-05-04
#' PURPOSE: Find and download release from a Github repo
#' STATUS: working
#' PACKAGES: os, requests, datatable, datetime, tarfile
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

import os
import requests
import datatable as dt
from datatable import f
from datetime import datetime
import tarfile


class github:
  """GitHub API Client
  View and download files from a repo
  """

  def __init__(self, owner: str = None, repo: str = None):
    """GitHub API Client
    Download the latest release from a GitHub repository
    
    @param owner username of the repository
    @param repo name of the repository that contains the release
    """
    self.session=requests.Session()
    self.gh_owner=owner
    self.gh_repo=repo
    self.gh_host='https://api.github.com'
    self.gh_endpoint_base=f'{self.gh_host}/repos/{owner}/{repo}'
    self.gh_endpoint_release=f'{self.gh_endpoint_base}/releases'
    self.gh_endpoint_contents=f'{self.gh_endpoint_base}/contents'
    self.gh_default_header={'Accept': 'application/vnd.github.v3+json'}
    self.releases=[]


  def _printReleases(self):
    """Print Releases"""
    print(self.releases[:, ['id', 'name', 'tag_name', 'published_at']])      

  def _formatDate(self, date):
    if not date:
      return None
    return datetime.strptime(str(date), '%Y-%m-%dT%H:%M:%SZ').date()
      

  def GET(self, url, **kwargs):
    """GET
    @param url string containing endpoint to send the request
    @param **kwards optional arguments to add to the request
    """
    response=self.session.get(url, **kwargs)
    response.raise_for_status()
    return response.json()
      
  def listContents(self, path):
    """List Contents
    List available files and directories in a GitHub repository
    
    @param path location to list files
    
    @return list of all files
    """
    url=f'{self.gh_endpoint_contents}/{path}'
    data=self.GET(url=url,headers=self.gh_default_header)
    return data

  def listReleases(self, per_page: int = 30, page: int = 1):
    """List Releases
    Print a list of all releases available for the selected repository
    
    @param per_page number of results per page (default 30)
    @param page the page number to fetch (default: 1)
    @reference https://docs.github.com/en/rest/reference/repos#releases
    @return json
    """
    headers=self.gh_default_header
    if per_page: headers['per_page'] = str(per_page)
    if page: headers['page'] = str(page)

    data = self.GET(url=self.gh_endpoint_release, headers=headers)
    releases = []
    for row in data:
      releases.append({
        'id': row.get('id', None),
        'name': row.get('name', None),
        'tag_name': row.get('tag_name', None),
        'created_at': self._formatDate(row.get('created_at', None)),
        'published_at': self._formatDate(row.get('published_at', None)),
        'tarball_url': row.get('tarball_url', None),
        'zipball_url': row.get('zipball_url', None)
      })
    print('Found {} releases'.format(len([d['id'] for d in data])))
    
    self.releases = dt.Frame(releases)
    self._printReleases()


  def downloadRelease(self, outDir: str = '.', tag_name: str = "latest"):
    """Download Release
    Download a release by tag_name
    @param tag_name release tag name (use listReleases)
    """
    dir = os.path.abspath(outDir)
    release = tag_name
    if release == "latest":
      release = self.releases[0, ['tag_name']].to_dict()['tag_name'][0]
    
    url = self.releases[f.tag_name == release, :].to_dict()['tarball_url'][0]
    # path = dir + '/' + os.path.basename(url) + '.tar.gz'
    
    print('Downloading Release: {}\nTrying: {}'.format(release, url))
    resp = requests.get(url, headers = self.gh_default_header, stream = True)
    resp.raise_for_status()
    file = tarfile.open(fileobj = resp.raw, mode = 'r|gz')
    file.extractall(path = dir)
    print('Extracted at: {}'.format(dir))