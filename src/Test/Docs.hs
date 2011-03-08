
module Test.Docs(docs) where

import Test.General
import Hoogle.Type.TagStr
import Hoogle.Type.Docs


docs = do
    let a === b = if renderDocs (readDocsHTML a) == b then pass else error $ "differences in docs " ++ show (renderDocs (readDocsHTML a))
    "foo" === Str "foo"
    "foo <i>bar</i> baz" === Tags [Str "foo ", TagEmph (Str "bar"), Str " baz"]
