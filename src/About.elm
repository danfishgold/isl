module About exposing (view)

import Element exposing (centerX, fill, maximum, paragraph, text, width)
import Util exposing (link)


view : List (Element.Element msg)
view =
    List.map (paragraph [ width (fill |> maximum 700), centerX ]) content


content : List (List (Element.Element msg))
content =
    [ [ text "אני "
      , link { url = "https://danfishgold.com", label = "דן פישגולד" }
      , text " ואני יצרתי את האתר הזה בשנת 2018 בזמן שלמדתי בקורס למתחילים בשפת הסימנים במכון לקידום החרש (שהיום הוא חלק מעמותת מעגלי שמע)."
      ]
    , [ text "בזמנו למכון היה מילון שס״י באינטרנט עם וידאוים שמדגימים סימונים של מילים. במילון הרשמי היו חסרות כמה אפשרויות שהקשו על השימוש שלי בו, ובאישור המכון השתמשתי בוידאוים מהאתר הרשמי כדי ליצור את האתר הזה. המילון הרשמי כבר לא קיים יותר, אבל הוידאוים שהיו בו עדיין נמצאים באתר הזה (וב"
      , link { url = "https://www.youtube.com/user/dpiisela/playlists", label = "חשבון היוטיוב של מעגלי שמע" }
      , text ")."
      ]
    , [ text "תוכלו למצוא את "
      , link { url = "https://github.com/danfishgold/isl", label = "קוד המקור של האתר ב GitHub" }
      , text " ואם יש לכם שאלה או הצעה אתם מוזמנים לפנות אלי בכל רשת חברתית. אני כמעט בודאות אהיה הדן פישגולד היחיד."
      ]
    ]
