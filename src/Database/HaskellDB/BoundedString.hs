-----------------------------------------------------------
-- |
-- Module      :  BoundedString
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the types for strings with maximum length
-----------------------------------------------------------

module Database.HaskellDB.BoundedString where

import Database.HaskellDB.BoundedList

type BoundedString n = BoundedList Char n

instance (Size n) => Show (BoundedString n) where
    show s = take (listBound s) (fromBounded s)

type BStr0 = BoundedList Char (N N0)
type BStr1 = BoundedList Char (N N1)
type BStr2 = BoundedList Char (N N2)
type BStr3 = BoundedList Char (N N3)
type BStr4 = BoundedList Char (N N4)
type BStr5 = BoundedList Char (N N5)
type BStr6 = BoundedList Char (N N6)
type BStr7 = BoundedList Char (N N7)
type BStr8 = BoundedList Char (N N8)
type BStr9 = BoundedList Char (N N9)
type BStr10 = BoundedList Char (N N10)
type BStr11 = BoundedList Char (N N11)
type BStr12 = BoundedList Char (N N12)
type BStr13 = BoundedList Char (N N13)
type BStr14 = BoundedList Char (N N14)
type BStr15 = BoundedList Char (N N15)
type BStr16 = BoundedList Char (N N16)
type BStr17 = BoundedList Char (N N17)
type BStr18 = BoundedList Char (N N18)
type BStr19 = BoundedList Char (N N19)
type BStr20 = BoundedList Char (N N20)
type BStr21 = BoundedList Char (N N21)
type BStr22 = BoundedList Char (N N22)
type BStr23 = BoundedList Char (N N23)
type BStr24 = BoundedList Char (N N24)
type BStr25 = BoundedList Char (N N25)
type BStr26 = BoundedList Char (N N26)
type BStr27 = BoundedList Char (N N27)
type BStr28 = BoundedList Char (N N28)
type BStr29 = BoundedList Char (N N29)
type BStr30 = BoundedList Char (N N30)
type BStr31 = BoundedList Char (N N31)
type BStr32 = BoundedList Char (N N32)
type BStr33 = BoundedList Char (N N33)
type BStr34 = BoundedList Char (N N34)
type BStr35 = BoundedList Char (N N35)
type BStr36 = BoundedList Char (N N36)
type BStr37 = BoundedList Char (N N37)
type BStr38 = BoundedList Char (N N38)
type BStr39 = BoundedList Char (N N39)
type BStr40 = BoundedList Char (N N40)
type BStr41 = BoundedList Char (N N41)
type BStr42 = BoundedList Char (N N42)
type BStr43 = BoundedList Char (N N43)
type BStr44 = BoundedList Char (N N44)
type BStr45 = BoundedList Char (N N45)
type BStr46 = BoundedList Char (N N46)
type BStr47 = BoundedList Char (N N47)
type BStr48 = BoundedList Char (N N48)
type BStr49 = BoundedList Char (N N49)
type BStr50 = BoundedList Char (N N50)
type BStr51 = BoundedList Char (N N51)
type BStr52 = BoundedList Char (N N52)
type BStr53 = BoundedList Char (N N53)
type BStr54 = BoundedList Char (N N54)
type BStr55 = BoundedList Char (N N55)
type BStr56 = BoundedList Char (N N56)
type BStr57 = BoundedList Char (N N57)
type BStr58 = BoundedList Char (N N58)
type BStr59 = BoundedList Char (N N59)
type BStr60 = BoundedList Char (N N60)
type BStr61 = BoundedList Char (N N61)
type BStr62 = BoundedList Char (N N62)
type BStr63 = BoundedList Char (N N63)
type BStr64 = BoundedList Char (N N64)
type BStr65 = BoundedList Char (N N65)
type BStr66 = BoundedList Char (N N66)
type BStr67 = BoundedList Char (N N67)
type BStr68 = BoundedList Char (N N68)
type BStr69 = BoundedList Char (N N69)
type BStr70 = BoundedList Char (N N70)
type BStr71 = BoundedList Char (N N71)
type BStr72 = BoundedList Char (N N72)
type BStr73 = BoundedList Char (N N73)
type BStr74 = BoundedList Char (N N74)
type BStr75 = BoundedList Char (N N75)
type BStr76 = BoundedList Char (N N76)
type BStr77 = BoundedList Char (N N77)
type BStr78 = BoundedList Char (N N78)
type BStr79 = BoundedList Char (N N79)
type BStr80 = BoundedList Char (N N80)
type BStr81 = BoundedList Char (N N81)
type BStr82 = BoundedList Char (N N82)
type BStr83 = BoundedList Char (N N83)
type BStr84 = BoundedList Char (N N84)
type BStr85 = BoundedList Char (N N85)
type BStr86 = BoundedList Char (N N86)
type BStr87 = BoundedList Char (N N87)
type BStr88 = BoundedList Char (N N88)
type BStr89 = BoundedList Char (N N89)
type BStr90 = BoundedList Char (N N90)
type BStr91 = BoundedList Char (N N91)
type BStr92 = BoundedList Char (N N92)
type BStr93 = BoundedList Char (N N93)
type BStr94 = BoundedList Char (N N94)
type BStr95 = BoundedList Char (N N95)
type BStr96 = BoundedList Char (N N96)
type BStr97 = BoundedList Char (N N97)
type BStr98 = BoundedList Char (N N98)
type BStr99 = BoundedList Char (N N99)
type BStr100 = BoundedList Char (N N100)
type BStr101 = BoundedList Char (N N101)
type BStr102 = BoundedList Char (N N102)
type BStr103 = BoundedList Char (N N103)
type BStr104 = BoundedList Char (N N104)
type BStr105 = BoundedList Char (N N105)
type BStr106 = BoundedList Char (N N106)
type BStr107 = BoundedList Char (N N107)
type BStr108 = BoundedList Char (N N108)
type BStr109 = BoundedList Char (N N109)
type BStr110 = BoundedList Char (N N110)
type BStr111 = BoundedList Char (N N111)
type BStr112 = BoundedList Char (N N112)
type BStr113 = BoundedList Char (N N113)
type BStr114 = BoundedList Char (N N114)
type BStr115 = BoundedList Char (N N115)
type BStr116 = BoundedList Char (N N116)
type BStr117 = BoundedList Char (N N117)
type BStr118 = BoundedList Char (N N118)
type BStr119 = BoundedList Char (N N119)
type BStr120 = BoundedList Char (N N120)
type BStr121 = BoundedList Char (N N121)
type BStr122 = BoundedList Char (N N122)
type BStr123 = BoundedList Char (N N123)
type BStr124 = BoundedList Char (N N124)
type BStr125 = BoundedList Char (N N125)
type BStr126 = BoundedList Char (N N126)
type BStr127 = BoundedList Char (N N127)
type BStr128 = BoundedList Char (N N128)
type BStr129 = BoundedList Char (N N129)
type BStr130 = BoundedList Char (N N130)
type BStr131 = BoundedList Char (N N131)
type BStr132 = BoundedList Char (N N132)
type BStr133 = BoundedList Char (N N133)
type BStr134 = BoundedList Char (N N134)
type BStr135 = BoundedList Char (N N135)
type BStr136 = BoundedList Char (N N136)
type BStr137 = BoundedList Char (N N137)
type BStr138 = BoundedList Char (N N138)
type BStr139 = BoundedList Char (N N139)
type BStr140 = BoundedList Char (N N140)
type BStr141 = BoundedList Char (N N141)
type BStr142 = BoundedList Char (N N142)
type BStr143 = BoundedList Char (N N143)
type BStr144 = BoundedList Char (N N144)
type BStr145 = BoundedList Char (N N145)
type BStr146 = BoundedList Char (N N146)
type BStr147 = BoundedList Char (N N147)
type BStr148 = BoundedList Char (N N148)
type BStr149 = BoundedList Char (N N149)
type BStr150 = BoundedList Char (N N150)
type BStr151 = BoundedList Char (N N151)
type BStr152 = BoundedList Char (N N152)
type BStr153 = BoundedList Char (N N153)
type BStr154 = BoundedList Char (N N154)
type BStr155 = BoundedList Char (N N155)
type BStr156 = BoundedList Char (N N156)
type BStr157 = BoundedList Char (N N157)
type BStr158 = BoundedList Char (N N158)
type BStr159 = BoundedList Char (N N159)
type BStr160 = BoundedList Char (N N160)
type BStr161 = BoundedList Char (N N161)
type BStr162 = BoundedList Char (N N162)
type BStr163 = BoundedList Char (N N163)
type BStr164 = BoundedList Char (N N164)
type BStr165 = BoundedList Char (N N165)
type BStr166 = BoundedList Char (N N166)
type BStr167 = BoundedList Char (N N167)
type BStr168 = BoundedList Char (N N168)
type BStr169 = BoundedList Char (N N169)
type BStr170 = BoundedList Char (N N170)
type BStr171 = BoundedList Char (N N171)
type BStr172 = BoundedList Char (N N172)
type BStr173 = BoundedList Char (N N173)
type BStr174 = BoundedList Char (N N174)
type BStr175 = BoundedList Char (N N175)
type BStr176 = BoundedList Char (N N176)
type BStr177 = BoundedList Char (N N177)
type BStr178 = BoundedList Char (N N178)
type BStr179 = BoundedList Char (N N179)
type BStr180 = BoundedList Char (N N180)
type BStr181 = BoundedList Char (N N181)
type BStr182 = BoundedList Char (N N182)
type BStr183 = BoundedList Char (N N183)
type BStr184 = BoundedList Char (N N184)
type BStr185 = BoundedList Char (N N185)
type BStr186 = BoundedList Char (N N186)
type BStr187 = BoundedList Char (N N187)
type BStr188 = BoundedList Char (N N188)
type BStr189 = BoundedList Char (N N189)
type BStr190 = BoundedList Char (N N190)
type BStr191 = BoundedList Char (N N191)
type BStr192 = BoundedList Char (N N192)
type BStr193 = BoundedList Char (N N193)
type BStr194 = BoundedList Char (N N194)
type BStr195 = BoundedList Char (N N195)
type BStr196 = BoundedList Char (N N196)
type BStr197 = BoundedList Char (N N197)
type BStr198 = BoundedList Char (N N198)
type BStr199 = BoundedList Char (N N199)
type BStr200 = BoundedList Char (N N200)
type BStr201 = BoundedList Char (N N201)
type BStr202 = BoundedList Char (N N202)
type BStr203 = BoundedList Char (N N203)
type BStr204 = BoundedList Char (N N204)
type BStr205 = BoundedList Char (N N205)
type BStr206 = BoundedList Char (N N206)
type BStr207 = BoundedList Char (N N207)
type BStr208 = BoundedList Char (N N208)
type BStr209 = BoundedList Char (N N209)
type BStr210 = BoundedList Char (N N210)
type BStr211 = BoundedList Char (N N211)
type BStr212 = BoundedList Char (N N212)
type BStr213 = BoundedList Char (N N213)
type BStr214 = BoundedList Char (N N214)
type BStr215 = BoundedList Char (N N215)
type BStr216 = BoundedList Char (N N216)
type BStr217 = BoundedList Char (N N217)
type BStr218 = BoundedList Char (N N218)
type BStr219 = BoundedList Char (N N219)
type BStr220 = BoundedList Char (N N220)
type BStr221 = BoundedList Char (N N221)
type BStr222 = BoundedList Char (N N222)
type BStr223 = BoundedList Char (N N223)
type BStr224 = BoundedList Char (N N224)
type BStr225 = BoundedList Char (N N225)
type BStr226 = BoundedList Char (N N226)
type BStr227 = BoundedList Char (N N227)
type BStr228 = BoundedList Char (N N228)
type BStr229 = BoundedList Char (N N229)
type BStr230 = BoundedList Char (N N230)
type BStr231 = BoundedList Char (N N231)
type BStr232 = BoundedList Char (N N232)
type BStr233 = BoundedList Char (N N233)
type BStr234 = BoundedList Char (N N234)
type BStr235 = BoundedList Char (N N235)
type BStr236 = BoundedList Char (N N236)
type BStr237 = BoundedList Char (N N237)
type BStr238 = BoundedList Char (N N238)
type BStr239 = BoundedList Char (N N239)
type BStr240 = BoundedList Char (N N240)
type BStr241 = BoundedList Char (N N241)
type BStr242 = BoundedList Char (N N242)
type BStr243 = BoundedList Char (N N243)
type BStr244 = BoundedList Char (N N244)
type BStr245 = BoundedList Char (N N245)
type BStr246 = BoundedList Char (N N246)
type BStr247 = BoundedList Char (N N247)
type BStr248 = BoundedList Char (N N248)
type BStr249 = BoundedList Char (N N249)
type BStr250 = BoundedList Char (N N250)
type BStr251 = BoundedList Char (N N251)
type BStr252 = BoundedList Char (N N252)
type BStr253 = BoundedList Char (N N253)
type BStr254 = BoundedList Char (N N254)
type BStr255 = BoundedList Char (N N255)
