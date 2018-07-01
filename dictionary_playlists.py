from bs4 import BeautifulSoup
import requests
import re
import json


def playlist_attrs(playlist):
    a = playlist.find('a', recursive=False)
    return a.attrs['href'], a.text.strip()


def playlist_id_from_url(url):
    idx = url.find('&list=')
    return url[idx+6:]


def clean(text):
    return (text
            .replace(u'בשפת הסימנים הישראלית - המכון לקידום החרש', u'')
            .replace(u'מילון שפת הסימנים - ', u'')
            .replace(u'"', u'')
            .replace(u"'", u'')
            .strip())


with open('isl-playlists.html') as f:
    soup = BeautifulSoup(f.read())

all_playlists = map(playlist_attrs, soup.find_all(
    'ytd-grid-playlist-renderer'))
playlists = [(playlist_id_from_url(url), clean(title)) for (url, title) in all_playlists if title.startswith(
    u'מילון שפת הסימנים')]

categories = dict()
for (playlist_id, playlist_title) in playlists:
    print playlist_title
    r = requests.get(
        'https://www.youtube.com/playlist?list={}'.format(playlist_id))
    soup = BeautifulSoup(r.text)
    rows = soup.find_all('tr')
    videos = [clean(row.find('td', {'class': 'pl-video-title'}).find('a').text)
              for row in rows]
    categories[playlist_title] = videos

cat_text = u'\n\n\n'.join([ttl + u'\n\t\t' + u'\n\t\t'.join([vid for vid in vids])
                           for (ttl, vids) in categories.items() if len(vids) < 100])

with open('videos.txt', 'w') as f:
    f.write(cat_text.encode('utf-8'))


too_big = [ttl for (ttl, vids) in categories.items() if len(vids) == 100]
for cat in too_big:
    print cat


def remove_punctuation(word):
    return re.sub(u'[^א-תa-zA-Z]', u'', word)


with open('combined.json') as f:
    grps = json.load(f)['groups']

groups = {remove_punctuation(base): ids for (base, ids) in grps.items()}
all_vids = {vid
            for vids in categories.values() for vid in vids}

vid_ids = dict()
for vid in all_vids:
    key = remove_punctuation(vid)
    if key in groups:
        vid_ids[vid] = groups[key]
    else:
        print u"u'{}': [],".format(vid)

missing_words = {
    u'מזל טוב': [],
    u'אכזבה / להתאכזב': [u'4653', u'13296'],
    u'האם': [u'7493'],
    u'בת זוג (שותפה לתחביב או פעולה)': [],
    u'יום הכיפורים': [u'3301'],
    u'תל אביב יפו': [],
    u'אוניברסיטה / מכללה': [u'6397', u'13342'],
    u'בעל': [],
    u'כוננית': [],
    u'חנוכה': [u'3385'],
    u'דרישת שלום': [],
    u'מלבן': [],
    u'שף': [],
    u'שידוכים בשפת הסימנים הישראלית- המכון לקידום החרש': [u'12785'],
    u'בן/ בת זוג (שותף לתחביב או פעולה)': [],
    u'בר מצווה': [],
    u'ברית מילה': [],
    u'דיסק און קי (החסן נייד)': [u'5057'],
    u'אטי': [],
    u'האִם (2)': [u'7493'],
    u'מיוחד': [],
    u'נקניק': [],
    u'דגל': [],
    u'[Deleted video]': [],
}

vid_ids.update(missing_words)

category_ids = dict()
for (cat, vids) in categories.items():
    category_ids[cat] = list({id for vid in categories[cat]
                              for id in vid_ids[vid]})

with open('categories.json', 'w') as f:
    content = json.dumps(category_ids, ensure_ascii=False, indent=None)
    f.write(content.encode('utf8'))
