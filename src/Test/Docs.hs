
module Test.Docs(docs) where

import Control.Monad
import Hoogle.Type.TagStr
import Hoogle.Type.Docs


docs :: IO ()
docs = do
    let a === b = when (renderDocs (readDocsHTML a) /= b) $ error $ "differences in docs " ++ show (renderDocs (readDocsHTML a))
    "foo" === Str "foo"
    "foo <i>bar</i> baz" === Tags [Str "foo ", TagEmph (Str "bar"), Str " baz"]
