import requests
from bs4 import BeautifulSoup
from time import sleep
import json
import re

letters = ['א', 'ב', 'ג', 'ד', 'ה', 'ו', 'ז', 'ח', 'ט', 'י', 'כ',
           'ל', 'מ', 'נ', 'ס', 'ע', 'פ', 'צ', 'ק', 'ר', 'ש', 'ת', ]

url = 'http://isl.org.il/wp-admin/admin-ajax.php'

# Get all words by searching for all the letters of the alphabet
words = dict()
for letter in letters:
    if letter == 'ו':
        continue  # There are too many words with ו so the server returns an error
    print(letter)
    params = {'action': 'search_video', 'term': letter}
    r = requests.get(url, params=params)
    for pair in r.json():
        words[pair['id']] = pair['value']

# Group all versions of the same word
word_groups = dict()
for (id, word) in words.items():
    base_word = re.search(r'(.*?) *(\(\d+\))?$', word).group(1)
    word_groups[base_word] = word_groups.get(base_word, []) + [str(id)]

sources = dict()

for (id, word) in words.items():
    print(word)
    data = {
        'MIME Type': 'application/x-www-form-urlencoded; charset=UTF-8',
        'action': 'get_myvideo',
        'post_title': word,
        'page_id': id,
    }
    r = requests.post(url, data=data)
    html = BeautifulSoup(r.json()['html'])

    sources[id] = {source.attrs['type']: source.attrs['src']
                   for source in html.find_all('source')}
    sleep(1.5)


with open('sources.json', 'w') as f:
    json.dump(sources, f, indent=2)

with open('words.json', 'w') as f:
    content = json.dumps(words, f, ensure_ascii=False, indent=None)
    f.write(content.encode('utf8'))


words_and_sources = dict()
for (id, word) in words.items():
    words_and_sources[id] = dict(text=word)

for (id, srcs) in sources.items():
    words_and_sources[id]['sources'] = srcs

combined = {
    'words': words_and_sources,
    'groups': word_groups
}

with open('combined.json', 'w') as f:
    content = json.dumps(combined, f, ensure_ascii=False, indent=2)
    f.write(content.encode('utf8'))
