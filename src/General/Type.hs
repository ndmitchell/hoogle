
module General.Type where


data TagStr = Str String
            | Tags [TagStr]
            | TagBold TagStr
            | TagUnderline TagStr
            | TagHyperlink String TagStr
            | TagColor Int TagStr


data Response = Warn String
              | Error String
