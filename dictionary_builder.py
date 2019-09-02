# encoding: utf8
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

sanitize = {
    'לחם (מוצר מזון) 1': 'לחם (מוצר מזון) (1)',
    'לחם (מוצר מזון) 2': 'לחם (מוצר מזון) (2)',
    'אימא': 'אמא',
}

# Get all words by searching for all the letters of the alphabet
words = dict()
for letter in letters:
    if letter == 'ו':
        continue  # There are too many words with ו so the server returns an error
    print(letter)
    params = {'action': 'search_video', 'term': letter}
    r = requests.get(url, params=params)
    for pair in r.json():
        words[pair['id']] = sanitize.get(pair['value'], pair['value'])

# Group all versions of the same word
word_groups = dict()
for (id, word) in words.items():
    base_word = re.search(r'(.*?) *(\(\d+\))?$', word).group(1)
    word_groups[base_word] = word_groups.get(base_word, []) + [str(id)]

dictionary = {
    'groups': word_groups,
    'words': {id: group for (group, ids) in word_groups.items() for id in ids}
}

with open('dictionary.json', 'w') as f:
    json.dump(dictionary, f, ensure_ascii=False, indent=2)


# # Get the video urls for each word
# sources = dict()
# for (id, word) in words.items():
#     print(word)
#     data = {
#         'MIME Type': 'application/x-www-form-urlencoded; charset=UTF-8',
#         'action': 'get_myvideo',
#         'post_title': word,
#         'page_id': id,
#     }
#     r = requests.post(url, data=data)
#     html = BeautifulSoup(r.json()['html'])

#     sources[id] = {source.attrs['type']: source.attrs['src']
#                    for source in html.find_all('source')}
#     sleep(1.5)

# # Save word sources
# with open('sources.json', 'w') as f:
#     json.dump(sources, f, indent=2)


# def shrink_video(url, id, directory):
#     # https://unix.stackexchange.com/questions/28803/how-can-i-reduce-a-videos-size-with-ffmpeg
#     # https://superuser.com/questions/268985/remove-audio-from-video-file-with-ffmpeg
#     # https://gist.github.com/dvlden/b9d923cb31775f92fa54eb8c39ccd5a9
#     # https://stackoverflow.com/questions/20847674/ffmpeg-libx264-height-not-divisible-by-2
#     command = ' '.join(["ffmpeg", "-i '{}'", "-an", "-b:v 500000",
#                         "-vf scale=-2:480", "{}/{}.mp4"
#                         ]).format(url.replace(' ', '%20'), directory, id)
#     return subprocess.check_call(command, shell=True)


# def shrink_sources(srcs, id, directory):
#     try:
#         shrink_video(srcs['video/mp4'], id, directory)
#     except subprocess.CalledProcessError:
#         try:
#             shrink_video(srcs['video/webm'], id, directory)
#         except subprocess.CalledProcessError:
#             shrink_video(srcs['video/ogg'], id, directory)


# # Download and shrink all word video files beacuse they're *huge*
# visited = {filename.replace('.mp4', '')
#            for filename in os.listdir('videos')}
# errored = set()
# # for id in sources.keys():
# for id in words.keys():
#     srcs = sources[id]
#     if id in visited:
#         continue
#     try:
#         shrink_sources(srcs, id, 'videos')
#     except subprocess.CalledProcessError:
#         errored.add(id)
#     except KeyboardInterrupt:
#         break
#     else:
#         visited.add(id)
