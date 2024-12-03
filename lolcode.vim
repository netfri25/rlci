"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""" LANGUAGE:		LOLCODE
""""" AUTHOR:		ThÃ©ophile BASTIAN, Christine Dodrill
""""" LAST CHANGE:	2013 Nov 14
""""" LICENSE:		GNU GPLv3
""""" SYNTAX HIGHLIGHTING FOR VIM!!1
"""""
""""" NOTE: this is a modified version of the syntax

if exists("b:current_syntax")
	finish
endif

syn region LolMultiComment start=/OBTW/ end=/TLDR/
syn match LolLineComment 'BTW.*$'

" syn match LolKeyword "I IZ"
syn keyword LolKeyword IZ
syn match LolKeyword "ITZ"
syn match LolKeyword "ITZ A"
syn match LolKeyword "ITZ LIEK A"
syn match LolKeyword "IS A"
syn match LolKeyword "IM LIEK"
syn keyword LolKeyword R
syn keyword LolKeyword NOW
syn keyword LolKeyword A
syn keyword LolKeyword AN
syn keyword LolKeyword MAEK
syn keyword LolKeyword YR
syn keyword LolKeyword PLZ
syn keyword LolKeyword MKAY

syn match LolVarDec "HAS A"

syn match LolSpecChar ":)"
syn match LolSpecChar ":>"
syn match LolSpecChar ":\""
syn match LolSpecChar ":o"
syn match LolSpecChar "::"

syn region LolString start=+"+ end=+"+ contains=LolSpecChar

syn match LolBlock "WTF?"
syn match LolBlock "O RLY?"
syn match LolBlock "YA RLY"
syn match LolBlock "NO WAI"
syn match LolBlock "FOUND YR"
syn match LolBlock "FOUND UR"
" syn match LolBlock "HOW DUZ I"
syn match LolBlock "HOW IZ"
syn match LolBlock "IF U SAY SO"
syn match LolBlock "AWSUM THX"
syn match LolBlock "O NOES"
syn match LolBlock "O HAI IM"
syn match LolBlock "KTHX"
syn match LolBlock "HAI"
syn match LolBlock "KTHXBYE"
syn match LolBlock "SKIP DIS"
syn keyword LolBlock MEBBE
syn keyword LolBlock OIC
syn keyword LolBlock OMG
syn keyword LolBlock OMGWTF
syn keyword LolBlock GTFO

syn keyword LolType
			\ TROOF YARN NUMBR NUMBAR NOOB BUKKIT FUNKSHUN

syn keyword LolBool WIN FAIL

syn match LolNum '\<[0-9]\d*'
" syn match LolNum '[^A-Za-z_][+-]\?\d\+\.\d*'

syn match LolInclude "CAN HAS [A-Za-z]\+?"

syn keyword LolRepeat TIL
syn keyword LolRepeat WILE
syn match LolRepeat "IM IN YR "
syn match LolRepeat "IM OUTTA YR "

syn keyword LolBaseFunc
			\ VISIBLE INVISIBLE GIMMEH

syn keyword LolSpecial
            \ I IT ME parent

syn keyword LolFunc
			\ SUM DIFF PRODUKT QUOSHUNT MOD UPPIN NERFIN LOLG POWR SMOOSH BIGGR SMALLR BOTH EITHER WON NOT ALL ANY SAEM DIFFRINT

syn keyword LolComparator
			\ OF SRS

syn match LolOperator "'Z"

""" Defining highlighting ---
command -nargs=+ HiLink hi link <args>

HiLink LolMultiComment Comment
HiLink LolLineComment Comment
HiLink LolOperator Title
HiLink LolKeyword Statement
HiLink LolEscapedChar SpecialChar
HiLink LolBool Boolean
HiLink LolNum Number
HiLink LolString String
HiLink LolInclude Include
HiLink LolRepeat Delimiter
HiLink LolBaseFunc Function
HiLink LolSpecChar Special
HiLink LolVarDec StorageClass
HiLink LolBlock Delimiter
HiLink LolSpecial Special
HiLink LolFunc PreProc
HiLink LolComparator Title
HiLink LolType Type

delcommand HiLink

let b:current_syntax="lolcode"
