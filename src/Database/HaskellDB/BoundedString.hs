-----------------------------------------------------------
-- |
-- Module      :  BoundedString
-- Copyright   :  HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
--
-- BoundedString represents the sql types; CHARACTER and CHARACTER VARYING
-- both defined in SQL 1992.
-- BoundedString supports sizes in the range [0,255] and 65535.
-- Greater sizes and the sql type SQL_TEXT (SQL 1992) will might be 
-- supported in the future.
--
-- The use of BoundedString together with HaskellDB enables feedback when
-- the length of a string exceeds the bound of a certain database field.
-- BoundedString also provides a layer of type safety against loss of data
-- due to sql string truncation when extracting and re-insert data into fields
-- with smaller bound.
--
-- 
-----------------------------------------------------------

module Database.HaskellDB.BoundedString where

import Database.HaskellDB.BoundedList

type BoundedString n = BoundedList Char n

type BStr0 = BoundedString N0
type BStr1 = BoundedString N1
type BStr2 = BoundedString N2
type BStr3 = BoundedString N3
type BStr4 = BoundedString N4
type BStr5 = BoundedString N5
type BStr6 = BoundedString N6
type BStr7 = BoundedString N7
type BStr8 = BoundedString N8
type BStr9 = BoundedString N9
type BStr10 = BoundedString N10
type BStr11 = BoundedString N11
type BStr12 = BoundedString N12
type BStr13 = BoundedString N13
type BStr14 = BoundedString N14
type BStr15 = BoundedString N15
type BStr16 = BoundedString N16
type BStr17 = BoundedString N17
type BStr18 = BoundedString N18
type BStr19 = BoundedString N19
type BStr20 = BoundedString N20
type BStr21 = BoundedString N21
type BStr22 = BoundedString N22
type BStr23 = BoundedString N23
type BStr24 = BoundedString N24
type BStr25 = BoundedString N25
type BStr26 = BoundedString N26
type BStr27 = BoundedString N27
type BStr28 = BoundedString N28
type BStr29 = BoundedString N29
type BStr30 = BoundedString N30
type BStr31 = BoundedString N31
type BStr32 = BoundedString N32
type BStr33 = BoundedString N33
type BStr34 = BoundedString N34
type BStr35 = BoundedString N35
type BStr36 = BoundedString N36
type BStr37 = BoundedString N37
type BStr38 = BoundedString N38
type BStr39 = BoundedString N39
type BStr40 = BoundedString N40
type BStr41 = BoundedString N41
type BStr42 = BoundedString N42
type BStr43 = BoundedString N43
type BStr44 = BoundedString N44
type BStr45 = BoundedString N45
type BStr46 = BoundedString N46
type BStr47 = BoundedString N47
type BStr48 = BoundedString N48
type BStr49 = BoundedString N49
type BStr50 = BoundedString N50
type BStr51 = BoundedString N51
type BStr52 = BoundedString N52
type BStr53 = BoundedString N53
type BStr54 = BoundedString N54
type BStr55 = BoundedString N55
type BStr56 = BoundedString N56
type BStr57 = BoundedString N57
type BStr58 = BoundedString N58
type BStr59 = BoundedString N59
type BStr60 = BoundedString N60
type BStr61 = BoundedString N61
type BStr62 = BoundedString N62
type BStr63 = BoundedString N63
type BStr64 = BoundedString N64
type BStr65 = BoundedString N65
type BStr66 = BoundedString N66
type BStr67 = BoundedString N67
type BStr68 = BoundedString N68
type BStr69 = BoundedString N69
type BStr70 = BoundedString N70
type BStr71 = BoundedString N71
type BStr72 = BoundedString N72
type BStr73 = BoundedString N73
type BStr74 = BoundedString N74
type BStr75 = BoundedString N75
type BStr76 = BoundedString N76
type BStr77 = BoundedString N77
type BStr78 = BoundedString N78
type BStr79 = BoundedString N79
type BStr80 = BoundedString N80
type BStr81 = BoundedString N81
type BStr82 = BoundedString N82
type BStr83 = BoundedString N83
type BStr84 = BoundedString N84
type BStr85 = BoundedString N85
type BStr86 = BoundedString N86
type BStr87 = BoundedString N87
type BStr88 = BoundedString N88
type BStr89 = BoundedString N89
type BStr90 = BoundedString N90
type BStr91 = BoundedString N91
type BStr92 = BoundedString N92
type BStr93 = BoundedString N93
type BStr94 = BoundedString N94
type BStr95 = BoundedString N95
type BStr96 = BoundedString N96
type BStr97 = BoundedString N97
type BStr98 = BoundedString N98
type BStr99 = BoundedString N99
type BStr100 = BoundedString N100
type BStr101 = BoundedString N101
type BStr102 = BoundedString N102
type BStr103 = BoundedString N103
type BStr104 = BoundedString N104
type BStr105 = BoundedString N105
type BStr106 = BoundedString N106
type BStr107 = BoundedString N107
type BStr108 = BoundedString N108
type BStr109 = BoundedString N109
type BStr110 = BoundedString N110
type BStr111 = BoundedString N111
type BStr112 = BoundedString N112
type BStr113 = BoundedString N113
type BStr114 = BoundedString N114
type BStr115 = BoundedString N115
type BStr116 = BoundedString N116
type BStr117 = BoundedString N117
type BStr118 = BoundedString N118
type BStr119 = BoundedString N119
type BStr120 = BoundedString N120
type BStr121 = BoundedString N121
type BStr122 = BoundedString N122
type BStr123 = BoundedString N123
type BStr124 = BoundedString N124
type BStr125 = BoundedString N125
type BStr126 = BoundedString N126
type BStr127 = BoundedString N127
type BStr128 = BoundedString N128
type BStr129 = BoundedString N129
type BStr130 = BoundedString N130
type BStr131 = BoundedString N131
type BStr132 = BoundedString N132
type BStr133 = BoundedString N133
type BStr134 = BoundedString N134
type BStr135 = BoundedString N135
type BStr136 = BoundedString N136
type BStr137 = BoundedString N137
type BStr138 = BoundedString N138
type BStr139 = BoundedString N139
type BStr140 = BoundedString N140
type BStr141 = BoundedString N141
type BStr142 = BoundedString N142
type BStr143 = BoundedString N143
type BStr144 = BoundedString N144
type BStr145 = BoundedString N145
type BStr146 = BoundedString N146
type BStr147 = BoundedString N147
type BStr148 = BoundedString N148
type BStr149 = BoundedString N149
type BStr150 = BoundedString N150
type BStr151 = BoundedString N151
type BStr152 = BoundedString N152
type BStr153 = BoundedString N153
type BStr154 = BoundedString N154
type BStr155 = BoundedString N155
type BStr156 = BoundedString N156
type BStr157 = BoundedString N157
type BStr158 = BoundedString N158
type BStr159 = BoundedString N159
type BStr160 = BoundedString N160
type BStr161 = BoundedString N161
type BStr162 = BoundedString N162
type BStr163 = BoundedString N163
type BStr164 = BoundedString N164
type BStr165 = BoundedString N165
type BStr166 = BoundedString N166
type BStr167 = BoundedString N167
type BStr168 = BoundedString N168
type BStr169 = BoundedString N169
type BStr170 = BoundedString N170
type BStr171 = BoundedString N171
type BStr172 = BoundedString N172
type BStr173 = BoundedString N173
type BStr174 = BoundedString N174
type BStr175 = BoundedString N175
type BStr176 = BoundedString N176
type BStr177 = BoundedString N177
type BStr178 = BoundedString N178
type BStr179 = BoundedString N179
type BStr180 = BoundedString N180
type BStr181 = BoundedString N181
type BStr182 = BoundedString N182
type BStr183 = BoundedString N183
type BStr184 = BoundedString N184
type BStr185 = BoundedString N185
type BStr186 = BoundedString N186
type BStr187 = BoundedString N187
type BStr188 = BoundedString N188
type BStr189 = BoundedString N189
type BStr190 = BoundedString N190
type BStr191 = BoundedString N191
type BStr192 = BoundedString N192
type BStr193 = BoundedString N193
type BStr194 = BoundedString N194
type BStr195 = BoundedString N195
type BStr196 = BoundedString N196
type BStr197 = BoundedString N197
type BStr198 = BoundedString N198
type BStr199 = BoundedString N199
type BStr200 = BoundedString N200
type BStr201 = BoundedString N201
type BStr202 = BoundedString N202
type BStr203 = BoundedString N203
type BStr204 = BoundedString N204
type BStr205 = BoundedString N205
type BStr206 = BoundedString N206
type BStr207 = BoundedString N207
type BStr208 = BoundedString N208
type BStr209 = BoundedString N209
type BStr210 = BoundedString N210
type BStr211 = BoundedString N211
type BStr212 = BoundedString N212
type BStr213 = BoundedString N213
type BStr214 = BoundedString N214
type BStr215 = BoundedString N215
type BStr216 = BoundedString N216
type BStr217 = BoundedString N217
type BStr218 = BoundedString N218
type BStr219 = BoundedString N219
type BStr220 = BoundedString N220
type BStr221 = BoundedString N221
type BStr222 = BoundedString N222
type BStr223 = BoundedString N223
type BStr224 = BoundedString N224
type BStr225 = BoundedString N225
type BStr226 = BoundedString N226
type BStr227 = BoundedString N227
type BStr228 = BoundedString N228
type BStr229 = BoundedString N229
type BStr230 = BoundedString N230
type BStr231 = BoundedString N231
type BStr232 = BoundedString N232
type BStr233 = BoundedString N233
type BStr234 = BoundedString N234
type BStr235 = BoundedString N235
type BStr236 = BoundedString N236
type BStr237 = BoundedString N237
type BStr238 = BoundedString N238
type BStr239 = BoundedString N239
type BStr240 = BoundedString N240
type BStr241 = BoundedString N241
type BStr242 = BoundedString N242
type BStr243 = BoundedString N243
type BStr244 = BoundedString N244
type BStr245 = BoundedString N245
type BStr246 = BoundedString N246
type BStr247 = BoundedString N247
type BStr248 = BoundedString N248
type BStr249 = BoundedString N249
type BStr250 = BoundedString N250
type BStr251 = BoundedString N251
type BStr252 = BoundedString N252
type BStr253 = BoundedString N253
type BStr254 = BoundedString N254
type BStr255 = BoundedString N255
type BStr65535 = BoundedString N65535
