module Emojis exposing (Emoji, EmojiData, emojiDataToEmoji)


type alias Emoji =
    { code : String
    , version : String
    , skinTone : List Int
    }


type alias EmojiData =
    { code : String
    , keywords : List String
    , version : String
    , skinTone : List Int
    }


emojiDataToEmoji : EmojiData -> Emoji
emojiDataToEmoji emojiData =
    Emoji emojiData.code emojiData.version emojiData.skinTone
