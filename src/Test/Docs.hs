
module Test.Docs(docs) where

import Hoogle.Type.TagStr
import Hoogle.Type.Docs
import Test.General


docs :: IO ()
docs = do
    let a =#= b = renderDocs (readDocsHTML a) === b
    "foo" =#= Str "foo"
    "foo <i>bar</i> baz" =#= Tags [Str "foo ", TagEmph (Str "bar"), Str " baz"]
