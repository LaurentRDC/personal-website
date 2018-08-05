module Monokai (monokai) where

import Skylighting.Types
import Skylighting.Styles

import qualified Data.Map as Map

color :: Int -> Maybe Color
color = toColor

red     = color 0xf92672
green   = color 0xa6e22e
blue    = color 0x66d9ef
grey    = color 0x75715e
white   = color 0xf8f8f2
yellow  = color 0xe6db74
purple  = color 0xae81ff
dark    = color 0x272822

-- This color is just a marker to idenfity what I don't know
orange  = color 0xf98c26

monokai :: Style
monokai = Style{
    tokenStyles = Map.fromList
    [ ( KeywordTok,             defStyle { tokenColor = red })
    , ( DataTypeTok,            defStyle { tokenColor = blue })
    -- Literal numbers
    , ( DecValTok,              defStyle { tokenColor = purple })
    , ( BaseNTok,               defStyle { tokenColor = purple })
    , ( FloatTok,               defStyle { tokenColor = purple })
    , ( ConstantTok,            defStyle { tokenColor = purple })
    -- Strings
    , ( CharTok,                defStyle { tokenColor = yellow })
    , ( SpecialCharTok,         defStyle { tokenColor = purple })
    , ( StringTok,              defStyle { tokenColor = yellow })
    , ( VerbatimStringTok,      defStyle { tokenColor = yellow })
    , ( SpecialStringTok,       defStyle { tokenColor = purple })

    , ( ImportTok,              defStyle { tokenColor = red })
    , ( CommentTok,             defStyle { tokenColor = grey })
    , ( DocumentationTok,       defStyle { tokenColor = yellow })
    , ( AnnotationTok,          defStyle { tokenColor = yellow })
    , ( CommentVarTok,          defStyle { tokenColor = yellow })
    , ( OtherTok,               defStyle { tokenColor = yellow })
    , ( FunctionTok,            defStyle { tokenColor = green })
    , ( VariableTok,            defStyle { tokenColor = purple })   -- Built-in variables (e.g. None, NotImplemented)
    , ( ControlFlowTok,         defStyle { tokenColor = red })
    , ( OperatorTok,            defStyle { tokenColor = red })
    , ( BuiltInTok,             defStyle { tokenColor = blue })
    , ( ExtensionTok,           defStyle { tokenColor = green })
    , ( PreprocessorTok,        defStyle { tokenColor = blue })     -- warnings
    , ( AttributeTok,           defStyle { tokenColor = green })    -- decorators
    , ( RegionMarkerTok,        defStyle { tokenColor = orange })
    , ( InformationTok,         defStyle { tokenColor = yellow })
    , ( WarningTok,             defStyle { tokenColor = orange })
    , ( AlertTok,               defStyle { tokenColor = orange })
    , ( ErrorTok,               defStyle { tokenColor = blue })
    , ( NormalTok,              defStyle { tokenColor = orange })
    ]
, defaultColor = white
, backgroundColor = dark
, lineNumberColor = grey
, lineNumberBackgroundColor = dark
}