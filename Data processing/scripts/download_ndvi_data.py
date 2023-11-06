# %%
from xml.dom import minidom
from urllib.request import urlopen
from urllib.request import urlretrieve
# %%
# Divide the url you get from the data portal into two parts
# Everything before "catalog/"
year = '2023'
server_url = 'https://www.ncei.noaa.gov/thredds/'
# Everything after "catalog/"
request_url = f'cdr/ndvi/{year}/'
   
def get_elements(url, tag_name, attribute_name):
  """Get elements from an XML file"""
  # usock = urllib2.urlopen(url)
  usock = urlopen(url)
  xmldoc = minidom.parse(usock)
  usock.close()
  tags = xmldoc.getElementsByTagName(tag_name)
  attributes=[]
  for tag in tags:
    attribute = tag.getAttribute(attribute_name)
    attributes.append(attribute)
  return attributes

# %%
url = server_url + request_url + 'catalog.xml'
print(url)
catalog = get_elements(url,'dataset','urlPath')
files=[]
for citem in catalog:
    if (citem[-3:]=='.nc'):
        files.append(citem)
count = 0
for f in files:
    count +=1
    file_url = server_url + 'fileServer/' + f
    file_prefix = file_url.split('/')[-1][:-3]
    file_name = file_prefix + '_' + str(count) + '.nc'
    print('Downloaing file %d of %d' % (count,len(files)))
    print(file_name)
    a = urlretrieve(file_url,file_name)
    print(a)
# %%
