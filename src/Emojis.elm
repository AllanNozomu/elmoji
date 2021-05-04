module Emojis exposing (Emoji, EmojiData, SkinTone(..), emojiDataToEmoji)


type SkinTone
    = Default


type alias Emoji =
    { code : String
    , version : String
    , skinTone : String
    }


type alias EmojiData =
    { code : String
    , keywords : List String
    , version : String
    , skinTone : String
    }


emojiDataToEmoji : EmojiData -> Emoji
emojiDataToEmoji emojiData =
    Emoji emojiData.code emojiData.version emojiData.skinTone
