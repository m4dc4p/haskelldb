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

type Str0 = BoundedList Char (N N0)
type Str1 = BoundedList Char (N N1)
type Str2 = BoundedList Char (N N2)
type Str3 = BoundedList Char (N N3)
type Str4 = BoundedList Char (N N4)
type Str5 = BoundedList Char (N N5)
type Str6 = BoundedList Char (N N6)
type Str7 = BoundedList Char (N N7)
type Str8 = BoundedList Char (N N8)
type Str9 = BoundedList Char (N N9)
type Str10 = BoundedList Char (N N10)
type Str11 = BoundedList Char (N N11)
type Str12 = BoundedList Char (N N12)
type Str13 = BoundedList Char (N N13)
type Str14 = BoundedList Char (N N14)
type Str15 = BoundedList Char (N N15)
type Str16 = BoundedList Char (N N16)
type Str17 = BoundedList Char (N N17)
type Str18 = BoundedList Char (N N18)
type Str19 = BoundedList Char (N N19)
type Str20 = BoundedList Char (N N20)
type Str21 = BoundedList Char (N N21)
type Str22 = BoundedList Char (N N22)
type Str23 = BoundedList Char (N N23)
type Str24 = BoundedList Char (N N24)
type Str25 = BoundedList Char (N N25)
type Str26 = BoundedList Char (N N26)
type Str27 = BoundedList Char (N N27)
type Str28 = BoundedList Char (N N28)
type Str29 = BoundedList Char (N N29)
type Str30 = BoundedList Char (N N30)
type Str31 = BoundedList Char (N N31)
type Str32 = BoundedList Char (N N32)
type Str33 = BoundedList Char (N N33)
type Str34 = BoundedList Char (N N34)
type Str35 = BoundedList Char (N N35)
type Str36 = BoundedList Char (N N36)
type Str37 = BoundedList Char (N N37)
type Str38 = BoundedList Char (N N38)
type Str39 = BoundedList Char (N N39)
type Str40 = BoundedList Char (N N40)
type Str41 = BoundedList Char (N N41)
type Str42 = BoundedList Char (N N42)
type Str43 = BoundedList Char (N N43)
type Str44 = BoundedList Char (N N44)
type Str45 = BoundedList Char (N N45)
type Str46 = BoundedList Char (N N46)
type Str47 = BoundedList Char (N N47)
type Str48 = BoundedList Char (N N48)
type Str49 = BoundedList Char (N N49)
type Str50 = BoundedList Char (N N50)
type Str51 = BoundedList Char (N N51)
type Str52 = BoundedList Char (N N52)
type Str53 = BoundedList Char (N N53)
type Str54 = BoundedList Char (N N54)
type Str55 = BoundedList Char (N N55)
type Str56 = BoundedList Char (N N56)
type Str57 = BoundedList Char (N N57)
type Str58 = BoundedList Char (N N58)
type Str59 = BoundedList Char (N N59)
type Str60 = BoundedList Char (N N60)
type Str61 = BoundedList Char (N N61)
type Str62 = BoundedList Char (N N62)
type Str63 = BoundedList Char (N N63)
type Str64 = BoundedList Char (N N64)
type Str65 = BoundedList Char (N N65)
type Str66 = BoundedList Char (N N66)
type Str67 = BoundedList Char (N N67)
type Str68 = BoundedList Char (N N68)
type Str69 = BoundedList Char (N N69)
type Str70 = BoundedList Char (N N70)
type Str71 = BoundedList Char (N N71)
type Str72 = BoundedList Char (N N72)
type Str73 = BoundedList Char (N N73)
type Str74 = BoundedList Char (N N74)
type Str75 = BoundedList Char (N N75)
type Str76 = BoundedList Char (N N76)
type Str77 = BoundedList Char (N N77)
type Str78 = BoundedList Char (N N78)
type Str79 = BoundedList Char (N N79)
type Str80 = BoundedList Char (N N80)
type Str81 = BoundedList Char (N N81)
type Str82 = BoundedList Char (N N82)
type Str83 = BoundedList Char (N N83)
type Str84 = BoundedList Char (N N84)
type Str85 = BoundedList Char (N N85)
type Str86 = BoundedList Char (N N86)
type Str87 = BoundedList Char (N N87)
type Str88 = BoundedList Char (N N88)
type Str89 = BoundedList Char (N N89)
type Str90 = BoundedList Char (N N90)
type Str91 = BoundedList Char (N N91)
type Str92 = BoundedList Char (N N92)
type Str93 = BoundedList Char (N N93)
type Str94 = BoundedList Char (N N94)
type Str95 = BoundedList Char (N N95)
type Str96 = BoundedList Char (N N96)
type Str97 = BoundedList Char (N N97)
type Str98 = BoundedList Char (N N98)
type Str99 = BoundedList Char (N N99)
type Str100 = BoundedList Char (N N100)
type Str101 = BoundedList Char (N N101)
type Str102 = BoundedList Char (N N102)
type Str103 = BoundedList Char (N N103)
type Str104 = BoundedList Char (N N104)
type Str105 = BoundedList Char (N N105)
type Str106 = BoundedList Char (N N106)
type Str107 = BoundedList Char (N N107)
type Str108 = BoundedList Char (N N108)
type Str109 = BoundedList Char (N N109)
type Str110 = BoundedList Char (N N110)
type Str111 = BoundedList Char (N N111)
type Str112 = BoundedList Char (N N112)
type Str113 = BoundedList Char (N N113)
type Str114 = BoundedList Char (N N114)
type Str115 = BoundedList Char (N N115)
type Str116 = BoundedList Char (N N116)
type Str117 = BoundedList Char (N N117)
type Str118 = BoundedList Char (N N118)
type Str119 = BoundedList Char (N N119)
type Str120 = BoundedList Char (N N120)
type Str121 = BoundedList Char (N N121)
type Str122 = BoundedList Char (N N122)
type Str123 = BoundedList Char (N N123)
type Str124 = BoundedList Char (N N124)
type Str125 = BoundedList Char (N N125)
type Str126 = BoundedList Char (N N126)
type Str127 = BoundedList Char (N N127)
type Str128 = BoundedList Char (N N128)
type Str129 = BoundedList Char (N N129)
type Str130 = BoundedList Char (N N130)
type Str131 = BoundedList Char (N N131)
type Str132 = BoundedList Char (N N132)
type Str133 = BoundedList Char (N N133)
type Str134 = BoundedList Char (N N134)
type Str135 = BoundedList Char (N N135)
type Str136 = BoundedList Char (N N136)
type Str137 = BoundedList Char (N N137)
type Str138 = BoundedList Char (N N138)
type Str139 = BoundedList Char (N N139)
type Str140 = BoundedList Char (N N140)
type Str141 = BoundedList Char (N N141)
type Str142 = BoundedList Char (N N142)
type Str143 = BoundedList Char (N N143)
type Str144 = BoundedList Char (N N144)
type Str145 = BoundedList Char (N N145)
type Str146 = BoundedList Char (N N146)
type Str147 = BoundedList Char (N N147)
type Str148 = BoundedList Char (N N148)
type Str149 = BoundedList Char (N N149)
type Str150 = BoundedList Char (N N150)
type Str151 = BoundedList Char (N N151)
type Str152 = BoundedList Char (N N152)
type Str153 = BoundedList Char (N N153)
type Str154 = BoundedList Char (N N154)
type Str155 = BoundedList Char (N N155)
type Str156 = BoundedList Char (N N156)
type Str157 = BoundedList Char (N N157)
type Str158 = BoundedList Char (N N158)
type Str159 = BoundedList Char (N N159)
type Str160 = BoundedList Char (N N160)
type Str161 = BoundedList Char (N N161)
type Str162 = BoundedList Char (N N162)
type Str163 = BoundedList Char (N N163)
type Str164 = BoundedList Char (N N164)
type Str165 = BoundedList Char (N N165)
type Str166 = BoundedList Char (N N166)
type Str167 = BoundedList Char (N N167)
type Str168 = BoundedList Char (N N168)
type Str169 = BoundedList Char (N N169)
type Str170 = BoundedList Char (N N170)
type Str171 = BoundedList Char (N N171)
type Str172 = BoundedList Char (N N172)
type Str173 = BoundedList Char (N N173)
type Str174 = BoundedList Char (N N174)
type Str175 = BoundedList Char (N N175)
type Str176 = BoundedList Char (N N176)
type Str177 = BoundedList Char (N N177)
type Str178 = BoundedList Char (N N178)
type Str179 = BoundedList Char (N N179)
type Str180 = BoundedList Char (N N180)
type Str181 = BoundedList Char (N N181)
type Str182 = BoundedList Char (N N182)
type Str183 = BoundedList Char (N N183)
type Str184 = BoundedList Char (N N184)
type Str185 = BoundedList Char (N N185)
type Str186 = BoundedList Char (N N186)
type Str187 = BoundedList Char (N N187)
type Str188 = BoundedList Char (N N188)
type Str189 = BoundedList Char (N N189)
type Str190 = BoundedList Char (N N190)
type Str191 = BoundedList Char (N N191)
type Str192 = BoundedList Char (N N192)
type Str193 = BoundedList Char (N N193)
type Str194 = BoundedList Char (N N194)
type Str195 = BoundedList Char (N N195)
type Str196 = BoundedList Char (N N196)
type Str197 = BoundedList Char (N N197)
type Str198 = BoundedList Char (N N198)
type Str199 = BoundedList Char (N N199)
type Str200 = BoundedList Char (N N200)
type Str201 = BoundedList Char (N N201)
type Str202 = BoundedList Char (N N202)
type Str203 = BoundedList Char (N N203)
type Str204 = BoundedList Char (N N204)
type Str205 = BoundedList Char (N N205)
type Str206 = BoundedList Char (N N206)
type Str207 = BoundedList Char (N N207)
type Str208 = BoundedList Char (N N208)
type Str209 = BoundedList Char (N N209)
type Str210 = BoundedList Char (N N210)
type Str211 = BoundedList Char (N N211)
type Str212 = BoundedList Char (N N212)
type Str213 = BoundedList Char (N N213)
type Str214 = BoundedList Char (N N214)
type Str215 = BoundedList Char (N N215)
type Str216 = BoundedList Char (N N216)
type Str217 = BoundedList Char (N N217)
type Str218 = BoundedList Char (N N218)
type Str219 = BoundedList Char (N N219)
type Str220 = BoundedList Char (N N220)
type Str221 = BoundedList Char (N N221)
type Str222 = BoundedList Char (N N222)
type Str223 = BoundedList Char (N N223)
type Str224 = BoundedList Char (N N224)
type Str225 = BoundedList Char (N N225)
type Str226 = BoundedList Char (N N226)
type Str227 = BoundedList Char (N N227)
type Str228 = BoundedList Char (N N228)
type Str229 = BoundedList Char (N N229)
type Str230 = BoundedList Char (N N230)
type Str231 = BoundedList Char (N N231)
type Str232 = BoundedList Char (N N232)
type Str233 = BoundedList Char (N N233)
type Str234 = BoundedList Char (N N234)
type Str235 = BoundedList Char (N N235)
type Str236 = BoundedList Char (N N236)
type Str237 = BoundedList Char (N N237)
type Str238 = BoundedList Char (N N238)
type Str239 = BoundedList Char (N N239)
type Str240 = BoundedList Char (N N240)
type Str241 = BoundedList Char (N N241)
type Str242 = BoundedList Char (N N242)
type Str243 = BoundedList Char (N N243)
type Str244 = BoundedList Char (N N244)
type Str245 = BoundedList Char (N N245)
type Str246 = BoundedList Char (N N246)
type Str247 = BoundedList Char (N N247)
type Str248 = BoundedList Char (N N248)
type Str249 = BoundedList Char (N N249)
type Str250 = BoundedList Char (N N250)
type Str251 = BoundedList Char (N N251)
type Str252 = BoundedList Char (N N252)
type Str253 = BoundedList Char (N N253)
type Str254 = BoundedList Char (N N254)
type Str255 = BoundedList Char (N N255)
