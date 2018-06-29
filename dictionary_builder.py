import requests
from bs4 import BeautifulSoup
from time import sleep
import json
import re
import subprocess
import os

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

# Get the video urls for each word
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

# Delete words which doesn't actually have a video
bad_ids = [2813, 5557, 5561, 5565, 5569, 5573, 5577,
           5581, 5585, 5589, 5593, 5597, 7449, 10413, 12813]
for id in bad_ids:
    del words[str(id)]
    del sources[str(id)]

# Group all versions of the same word
word_groups = dict()
for (id, word) in words.items():
    base_word = re.search(r'(.*?) *(\(\d+\))?$', word).group(1)
    word_groups[base_word] = word_groups.get(base_word, []) + [str(id)]

# Save word sources
with open('sources.json', 'w') as f:
    json.dump(sources, f, indent=2)

# Save word ids
with open('words.json', 'w') as f:
    content = json.dumps(words, f, ensure_ascii=False, indent=None)
    f.write(content.encode('utf8'))

# Save combined data file
combined = {
    'words': words,
    'groups': word_groups
}
with open('combined.json', 'w') as f:
    content = json.dumps(combined, f, ensure_ascii=False, indent=2)
    f.write(content.encode('utf8'))

# Download and shrink all word video files beacuse they're *huge*
visited = {filename.replace('.mp4', '')
           for filename in os.listdir('videos')}
errored = set()
for (id, srcs) in sources.items():
    if id in visited:
        continue
    url = srcs['video/mp4'].replace(' ', '%20')
    # https://unix.stackexchange.com/questions/28803/how-can-i-reduce-a-videos-size-with-ffmpeg
    # https://superuser.com/questions/268985/remove-audio-from-video-file-with-ffmpeg
    # https://gist.github.com/dvlden/b9d923cb31775f92fa54eb8c39ccd5a9
    # https://stackoverflow.com/questions/20847674/ffmpeg-libx264-height-not-divisible-by-2
    command = ' '.join(["ffmpeg", "-i '{}'", "-an", "-b:v 500000",
                        "-vf scale=-2:480", "videos/{}.mp4"
                        ]).format(url, id)
    try:
        subprocess.check_call(command, shell=True)
    except subprocess.CalledProcessError as e:
        errored.add((id, url))
    except KeyboardInterrupt:
        break
    else:
        visited.add(id)
