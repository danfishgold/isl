module Localization exposing
    ( Locale(..)
    , Localized
    , container
    , dir
    , get
    , isLtr
    , localeFromString
    , localeToString
    , set
    , string
    )

import Element exposing (Element)
import Util


type alias Localized a =
    { hebrew : a
    , english : a
    }


type Locale
    = Hebrew
    | English


localeToString : Locale -> String
localeToString locale =
    case locale of
        Hebrew ->
            "he"

        English ->
            "en"


localeFromString : String -> Maybe Locale
localeFromString str =
    case str of
        "he" ->
            Just Hebrew

        "en" ->
            Just English

        _ ->
            Nothing


get : Locale -> Localized a -> a
get locale localized =
    case locale of
        Hebrew ->
            localized.hebrew

        English ->
            localized.english


set : Locale -> a -> Localized a -> Localized a
set locale value localized =
    case locale of
        Hebrew ->
            { localized | hebrew = value }

        English ->
            { localized | english = value }


container : Locale -> (List (Element.Attribute msg) -> List (Element msg) -> Element msg) -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
container locale group attrs children =
    if isLtr locale then
        group attrs children

    else
        group (dir English :: attrs) (List.reverse children)


isLtr : Locale -> Bool
isLtr locale =
    case locale of
        Hebrew ->
            False

        English ->
            True


dir : Locale -> Element.Attribute msg
dir locale =
    if isLtr locale then
        Util.dir "ltr"

    else
        Util.dir "rtl"


type alias Strings =
    { title : String
    , loading : String
    , errorMessage : String
    , search :
        { prompt : String
        , placeholder : Int -> Maybe String
        , noResults : String
        }
    , examples :
        { title : String
        , list : List ( String, String )
        }
    , unknownWord : String
    , playbackRate : String
    }


string : Locale -> (Strings -> a) -> a
string locale stringFn =
    get locale strings |> stringFn


strings : Localized Strings
strings =
    { hebrew =
        { title = "מילון שפת הסימנים"
        , loading = "המילון נטען. אנא המתינו..."
        , errorMessage = "אוי לא, היתה שגיאה! תנסו לטעון את העמוד מחדש"
        , search =
            { prompt = "מה בא לך להגיד בשפת הסימנים?"
            , placeholder = hebrewPlaceholder
            , noResults = "אין מילים שמתאימות לחיפוש שלך"
            }
        , examples =
            { title = "דוגמאות ממש טובות"
            , list =
                [ ( "שלום! ברוכים הבאים למילון שפת הסימנים", "#he-LTKFKykj8Rc" )
                , ( "איך קוראים לך?", "#he-JSgVC0Ed" )
                , ( "איך אני מגיע לספריה?", "#he-0Qq9EKEvZR0" )
                ]
            }
        , unknownWord = "מילה לא ידועה"
        , playbackRate = "מהירות"
        }
    , english =
        { title = "ISL Dictionary"
        , loading = "Loading, please wait..."
        , errorMessage = "Oh no, something went wrong! Please reload the page"
        , search =
            { prompt = "What do you want to say in ISL?"
            , placeholder = englishPlaceholder
            , noResults = "There are no words that match your search"
            }
        , examples =
            { title = "Some really good examples"
            , list =
                [ ( "Hello! Welcome to the ISL dictionary", "#en-LTKFKykj8Rc" )
                , ( "What's your name?", "#en-JSgVC0Ed" )
                , ( "How do I get to the library?", "#en-0Qq9EKEvZR0" )
                ]
            }
        , unknownWord = "unknown word"
        , playbackRate = "Playback rate"
        }
    }


hebrewPlaceholder : Int -> Maybe String
hebrewPlaceholder blockCount =
    case blockCount of
        0 ->
            Just "חיפוש"

        1 ->
            Just "הוסיפו עוד מילים כדי להרכיב משפט"

        2 ->
            Just "הוסיפו עוד מילים כדי להרכיב משפט"

        _ ->
            Nothing


englishPlaceholder : Int -> Maybe String
englishPlaceholder blockCount =
    case blockCount of
        0 ->
            Just "Search"

        1 ->
            Just "Add more words to make a sentence"

        2 ->
            Just "Add more words to make a sentence"

        _ ->
            Nothing
