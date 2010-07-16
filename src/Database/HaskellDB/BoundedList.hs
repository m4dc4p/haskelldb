{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances
, FlexibleContexts, UndecidableInstances #-}

-----------------------------------------------------------
-- |
-- Module      :  BoundedList
-- Copyright   :  HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
--
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
--
-- The main idea of bounded lists is to create lists with predetermined
-- maximum size.
--
-- BoundedList is a simple, fast and type safe approach to implementing 
-- this idea.
-- The implementation is based on inductive instances, making it very easy to
-- expand with new bounds. A new bound only requires one instance of size and 
-- two instances of Less.
--
-- BoundedList works as follows.
-- Every bound is build up by declaring a data-type representing the new bound.
-- The instance of size only returns the size as an Int.
-- The first instance of Less is for telling the typechecker that this bound
-- is greater than the largest smaller bound.
-- The second instance of Less is used by the typechecker to construct a chain
-- of instances if there is no hardcoded instance available.
-- This way the type checker can determine if a bound is smaller\/greater
-- then any other bound.
--
-- This inductive approach gives the complexity O(n) on the number of instances
-- and very short type checking times compared to an O(n\^2) implementation.
--
-- BoundedList also comes with a few utility function for manipulation an
-- contructing bounded lists.
--
-- To be noted:
-- Since each bound is a unique type:
-- Explicit shrink and\/or grow is needed before using (==).
-- BoundedList does not have an instance of Ordering. (This might change)
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.BoundedList (shrink,
		    grow,
		    trunc,
		    listBound,
		    toBounded,
		    fromBounded,
		    Size,
		    BoundedList,
		    N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,
 		    N10, N11, N12, N13, N14, N15, N16, N17, N18, N19,
		    N20, N21, N22, N23, N24, N25, N26, N27, N28, N29,
		    N30, N31, N32, N33, N34, N35, N36, N37, N38, N39, 
		    N40, N41, N42, N43, N44, N45, N46, N47, N48, N49,
		    N50, N51, N52, N53, N54, N55, N56, N57, N58, N59,
		    N60, N61, N62, N63, N64, N65, N66, N67, N68, N69,
		    N70, N71, N72, N73, N74, N75, N76, N77, N78, N79,
		    N80, N81, N82, N83, N84, N85, N86, N87, N88, N89,
		    N90, N91, N92, N93, N94, N95, N96, N97, N98, N99,
		    N100, N101, N102, N103, N104, N105, N106, N107, N108, N109,
		    N110, N111, N112, N113, N114, N115, N116, N117, N118, N119,
		    N120, N121, N122, N123, N124, N125, N126, N127, N128, N129,
		    N130, N131, N132, N133, N134, N135, N136, N137, N138, N139,
		    N140, N141, N142, N143, N144, N145, N146, N147, N148, N149,
		    N150, N151, N152, N153, N154, N155, N156, N157, N158, N159,
		    N160, N161, N162, N163, N164, N165, N166, N167, N168, N169,
		    N170, N171, N172, N173, N174, N175, N176, N177, N178, N179,
		    N180, N181, N182, N183, N184, N185, N186, N187, N188, N189,
		    N190, N191, N192, N193, N194, N195, N196, N197, N198, N199,
		    N200, N201, N202, N203, N204, N205, N206, N207, N208, N209,
		    N210, N211, N212, N213, N214, N215, N216, N217, N218, N219,
		    N220, N221, N222, N223, N224, N225, N226, N227, N228, N229,
		    N230, N231, N232, N233, N234, N235, N236, N237, N238, N239,
		    N240, N241, N242, N243, N244, N245, N246, N247, N248, N249,
		    N250, N251, N252, N253, N254, N255, N65535)
where
		    
class Size n where
    size :: n -> Int

class (Size a, Size b) => Less a b

class (Size a, Size b) => LessEq a b
instance (Size a) => LessEq a a
instance (Size a, Size b, Less a b) => LessEq a b

data N0 = N0
instance Size N0 where size _ = 0

data N1 = N1
instance Size N1 where size _ = 1
instance Less N0 N1

data N2 = N2
instance Size N2 where size _ = 2
instance Less N1 N2
instance Less a N1 => Less a N2

data N3 = N3
instance Size N3 where size _ = 3
instance Less N2 N3
instance Less a N2 => Less a N3

data N4 = N4
instance Size N4 where size _ = 4
instance Less N3 N4
instance Less a N3 => Less a N4

data N5 = N5
instance Size N5 where size _ = 5
instance Less N4 N5
instance Less a N4 => Less a N5

data N6 = N6
instance Size N6 where size _ = 6
instance Less N5 N6
instance Less a N5 => Less a N6

data N7 = N7
instance Size N7 where size _ = 7
instance Less N6 N7
instance Less a N6 => Less a N7

data N8 = N8
instance Size N8 where size _ = 8
instance Less N7 N8
instance Less a N7 => Less a N8

data N9 = N9
instance Size N9 where size _ = 9
instance Less N8 N9
instance Less a N8 => Less a N9

data N10 = N10
instance Size N10 where size _ = 10
instance Less N9 N10
instance Less a N9 => Less a N10

data N11 = N11
instance Size N11 where size _ = 11
instance Less N10 N11
instance Less a N10 => Less a N11

data N12 = N12
instance Size N12 where size _ = 12
instance Less N11 N12
instance Less a N11 => Less a N12

data N13 = N13
instance Size N13 where size _ = 13
instance Less N12 N13
instance Less a N12 => Less a N13

data N14 = N14
instance Size N14 where size _ = 14
instance Less N13 N14
instance Less a N13 => Less a N14

data N15 = N15
instance Size N15 where size _ = 15
instance Less N14 N15
instance Less a N14 => Less a N15

data N16 = N16
instance Size N16 where size _ = 16
instance Less N15 N16
instance Less a N15 => Less a N16

data N17 = N17
instance Size N17 where size _ = 17
instance Less N16 N17
instance Less a N16 => Less a N17

data N18 = N18
instance Size N18 where size _ = 18
instance Less N17 N18
instance Less a N17 => Less a N18

data N19 = N19
instance Size N19 where size _ = 19
instance Less N18 N19
instance Less a N18 => Less a N19

data N20 = N20
instance Size N20 where size _ = 20
instance Less N19 N20
instance Less a N19 => Less a N20

data N21 = N21
instance Size N21 where size _ = 21
instance Less N20 N21
instance Less a N20 => Less a N21

data N22 = N22
instance Size N22 where size _ = 22
instance Less N21 N22
instance Less a N21 => Less a N22

data N23 = N23
instance Size N23 where size _ = 23
instance Less N22 N23
instance Less a N22 => Less a N23

data N24 = N24
instance Size N24 where size _ = 24
instance Less N23 N24
instance Less a N23 => Less a N24

data N25 = N25
instance Size N25 where size _ = 25
instance Less N24 N25
instance Less a N24 => Less a N25

data N26 = N26
instance Size N26 where size _ = 26
instance Less N25 N26
instance Less a N25 => Less a N26

data N27 = N27
instance Size N27 where size _ = 27
instance Less N26 N27
instance Less a N26 => Less a N27

data N28 = N28
instance Size N28 where size _ = 28
instance Less N27 N28
instance Less a N27 => Less a N28

data N29 = N29
instance Size N29 where size _ = 29
instance Less N28 N29
instance Less a N28 => Less a N29

data N30 = N30
instance Size N30 where size _ = 30
instance Less N29 N30
instance Less a N29 => Less a N30

data N31 = N31
instance Size N31 where size _ = 31
instance Less N30 N31
instance Less a N30 => Less a N31

data N32 = N32
instance Size N32 where size _ = 32
instance Less N31 N32
instance Less a N31 => Less a N32

data N33 = N33
instance Size N33 where size _ = 33
instance Less N32 N33
instance Less a N32 => Less a N33

data N34 = N34
instance Size N34 where size _ = 34
instance Less N33 N34
instance Less a N33 => Less a N34

data N35 = N35
instance Size N35 where size _ = 35
instance Less N34 N35
instance Less a N34 => Less a N35

data N36 = N36
instance Size N36 where size _ = 36
instance Less N35 N36
instance Less a N35 => Less a N36

data N37 = N37
instance Size N37 where size _ = 37
instance Less N36 N37
instance Less a N36 => Less a N37

data N38 = N38
instance Size N38 where size _ = 38
instance Less N37 N38
instance Less a N37 => Less a N38

data N39 = N39
instance Size N39 where size _ = 39
instance Less N38 N39
instance Less a N38 => Less a N39

data N40 = N40
instance Size N40 where size _ = 40
instance Less N39 N40
instance Less a N39 => Less a N40

data N41 = N41
instance Size N41 where size _ = 41
instance Less N40 N41
instance Less a N40 => Less a N41

data N42 = N42
instance Size N42 where size _ = 42
instance Less N41 N42
instance Less a N41 => Less a N42

data N43 = N43
instance Size N43 where size _ = 43
instance Less N42 N43
instance Less a N42 => Less a N43

data N44 = N44
instance Size N44 where size _ = 44
instance Less N43 N44
instance Less a N43 => Less a N44

data N45 = N45
instance Size N45 where size _ = 45
instance Less N44 N45
instance Less a N44 => Less a N45

data N46 = N46
instance Size N46 where size _ = 46
instance Less N45 N46
instance Less a N45 => Less a N46

data N47 = N47
instance Size N47 where size _ = 47
instance Less N46 N47
instance Less a N46 => Less a N47

data N48 = N48
instance Size N48 where size _ = 48
instance Less N47 N48
instance Less a N47 => Less a N48

data N49 = N49
instance Size N49 where size _ = 49
instance Less N48 N49
instance Less a N48 => Less a N49

data N50 = N50
instance Size N50 where size _ = 50
instance Less N49 N50
instance Less a N49 => Less a N50

data N51 = N51
instance Size N51 where size _ = 51
instance Less N50 N51
instance Less a N50 => Less a N51

data N52 = N52
instance Size N52 where size _ = 52
instance Less N51 N52
instance Less a N51 => Less a N52

data N53 = N53
instance Size N53 where size _ = 53
instance Less N52 N53
instance Less a N52 => Less a N53

data N54 = N54
instance Size N54 where size _ = 54
instance Less N53 N54
instance Less a N53 => Less a N54

data N55 = N55
instance Size N55 where size _ = 55
instance Less N54 N55
instance Less a N54 => Less a N55

data N56 = N56
instance Size N56 where size _ = 56
instance Less N55 N56
instance Less a N55 => Less a N56

data N57 = N57
instance Size N57 where size _ = 57
instance Less N56 N57
instance Less a N56 => Less a N57

data N58 = N58
instance Size N58 where size _ = 58
instance Less N57 N58
instance Less a N57 => Less a N58

data N59 = N59
instance Size N59 where size _ = 59
instance Less N58 N59
instance Less a N58 => Less a N59

data N60 = N60
instance Size N60 where size _ = 60
instance Less N59 N60
instance Less a N59 => Less a N60

data N61 = N61
instance Size N61 where size _ = 61
instance Less N60 N61
instance Less a N60 => Less a N61

data N62 = N62
instance Size N62 where size _ = 62
instance Less N61 N62
instance Less a N61 => Less a N62

data N63 = N63
instance Size N63 where size _ = 63
instance Less N62 N63
instance Less a N62 => Less a N63

data N64 = N64
instance Size N64 where size _ = 64
instance Less N63 N64
instance Less a N63 => Less a N64

data N65 = N65
instance Size N65 where size _ = 65
instance Less N64 N65
instance Less a N64 => Less a N65

data N66 = N66
instance Size N66 where size _ = 66
instance Less N65 N66
instance Less a N65 => Less a N66

data N67 = N67
instance Size N67 where size _ = 67
instance Less N66 N67
instance Less a N66 => Less a N67

data N68 = N68
instance Size N68 where size _ = 68
instance Less N67 N68
instance Less a N67 => Less a N68

data N69 = N69
instance Size N69 where size _ = 69
instance Less N68 N69
instance Less a N68 => Less a N69

data N70 = N70
instance Size N70 where size _ = 70
instance Less N69 N70
instance Less a N69 => Less a N70

data N71 = N71
instance Size N71 where size _ = 71
instance Less N70 N71
instance Less a N70 => Less a N71

data N72 = N72
instance Size N72 where size _ = 72
instance Less N71 N72
instance Less a N71 => Less a N72

data N73 = N73
instance Size N73 where size _ = 73
instance Less N72 N73
instance Less a N72 => Less a N73

data N74 = N74
instance Size N74 where size _ = 74
instance Less N73 N74
instance Less a N73 => Less a N74

data N75 = N75
instance Size N75 where size _ = 75
instance Less N74 N75
instance Less a N74 => Less a N75

data N76 = N76
instance Size N76 where size _ = 76
instance Less N75 N76
instance Less a N75 => Less a N76

data N77 = N77
instance Size N77 where size _ = 77
instance Less N76 N77
instance Less a N76 => Less a N77

data N78 = N78
instance Size N78 where size _ = 78
instance Less N77 N78
instance Less a N77 => Less a N78

data N79 = N79
instance Size N79 where size _ = 79
instance Less N78 N79
instance Less a N78 => Less a N79

data N80 = N80
instance Size N80 where size _ = 80
instance Less N79 N80
instance Less a N79 => Less a N80

data N81 = N81
instance Size N81 where size _ = 81
instance Less N80 N81
instance Less a N80 => Less a N81

data N82 = N82
instance Size N82 where size _ = 82
instance Less N81 N82
instance Less a N81 => Less a N82

data N83 = N83
instance Size N83 where size _ = 83
instance Less N82 N83
instance Less a N82 => Less a N83

data N84 = N84
instance Size N84 where size _ = 84
instance Less N83 N84
instance Less a N83 => Less a N84

data N85 = N85
instance Size N85 where size _ = 85
instance Less N84 N85
instance Less a N84 => Less a N85

data N86 = N86
instance Size N86 where size _ = 86
instance Less N85 N86
instance Less a N85 => Less a N86

data N87 = N87
instance Size N87 where size _ = 87
instance Less N86 N87
instance Less a N86 => Less a N87

data N88 = N88
instance Size N88 where size _ = 88
instance Less N87 N88
instance Less a N87 => Less a N88

data N89 = N89
instance Size N89 where size _ = 89
instance Less N88 N89
instance Less a N88 => Less a N89

data N90 = N90
instance Size N90 where size _ = 90
instance Less N89 N90
instance Less a N89 => Less a N90

data N91 = N91
instance Size N91 where size _ = 91
instance Less N90 N91
instance Less a N90 => Less a N91

data N92 = N92
instance Size N92 where size _ = 92
instance Less N91 N92
instance Less a N91 => Less a N92

data N93 = N93
instance Size N93 where size _ = 93
instance Less N92 N93
instance Less a N92 => Less a N93

data N94 = N94
instance Size N94 where size _ = 94
instance Less N93 N94
instance Less a N93 => Less a N94

data N95 = N95
instance Size N95 where size _ = 95
instance Less N94 N95
instance Less a N94 => Less a N95

data N96 = N96
instance Size N96 where size _ = 96
instance Less N95 N96
instance Less a N95 => Less a N96

data N97 = N97
instance Size N97 where size _ = 97
instance Less N96 N97
instance Less a N96 => Less a N97

data N98 = N98
instance Size N98 where size _ = 98
instance Less N97 N98
instance Less a N97 => Less a N98

data N99 = N99
instance Size N99 where size _ = 99
instance Less N98 N99
instance Less a N98 => Less a N99

data N100 = N100
instance Size N100 where size _ = 100
instance Less N99 N100
instance Less a N99 => Less a N100

data N101 = N101
instance Size N101 where size _ = 101
instance Less N100 N101
instance Less a N100 => Less a N101

data N102 = N102
instance Size N102 where size _ = 102
instance Less N101 N102
instance Less a N101 => Less a N102

data N103 = N103
instance Size N103 where size _ = 103
instance Less N102 N103
instance Less a N102 => Less a N103

data N104 = N104
instance Size N104 where size _ = 104
instance Less N103 N104
instance Less a N103 => Less a N104

data N105 = N105
instance Size N105 where size _ = 105
instance Less N104 N105
instance Less a N104 => Less a N105

data N106 = N106
instance Size N106 where size _ = 106
instance Less N105 N106
instance Less a N105 => Less a N106

data N107 = N107
instance Size N107 where size _ = 107
instance Less N106 N107
instance Less a N106 => Less a N107

data N108 = N108
instance Size N108 where size _ = 108
instance Less N107 N108
instance Less a N107 => Less a N108

data N109 = N109
instance Size N109 where size _ = 109
instance Less N108 N109
instance Less a N108 => Less a N109

data N110 = N110
instance Size N110 where size _ = 110
instance Less N109 N110
instance Less a N109 => Less a N110

data N111 = N111
instance Size N111 where size _ = 111
instance Less N110 N111
instance Less a N110 => Less a N111

data N112 = N112
instance Size N112 where size _ = 112
instance Less N111 N112
instance Less a N111 => Less a N112

data N113 = N113
instance Size N113 where size _ = 113
instance Less N112 N113
instance Less a N112 => Less a N113

data N114 = N114
instance Size N114 where size _ = 114
instance Less N113 N114
instance Less a N113 => Less a N114

data N115 = N115
instance Size N115 where size _ = 115
instance Less N114 N115
instance Less a N114 => Less a N115

data N116 = N116
instance Size N116 where size _ = 116
instance Less N115 N116
instance Less a N115 => Less a N116

data N117 = N117
instance Size N117 where size _ = 117
instance Less N116 N117
instance Less a N116 => Less a N117

data N118 = N118
instance Size N118 where size _ = 118
instance Less N117 N118
instance Less a N117 => Less a N118

data N119 = N119
instance Size N119 where size _ = 119
instance Less N118 N119
instance Less a N118 => Less a N119

data N120 = N120
instance Size N120 where size _ = 120
instance Less N119 N120
instance Less a N119 => Less a N120

data N121 = N121
instance Size N121 where size _ = 121
instance Less N120 N121
instance Less a N120 => Less a N121

data N122 = N122
instance Size N122 where size _ = 122
instance Less N121 N122
instance Less a N121 => Less a N122

data N123 = N123
instance Size N123 where size _ = 123
instance Less N122 N123
instance Less a N122 => Less a N123

data N124 = N124
instance Size N124 where size _ = 124
instance Less N123 N124
instance Less a N123 => Less a N124

data N125 = N125
instance Size N125 where size _ = 125
instance Less N124 N125
instance Less a N124 => Less a N125

data N126 = N126
instance Size N126 where size _ = 126
instance Less N125 N126
instance Less a N125 => Less a N126

data N127 = N127
instance Size N127 where size _ = 127
instance Less N126 N127
instance Less a N126 => Less a N127

data N128 = N128
instance Size N128 where size _ = 128
instance Less N127 N128
instance Less a N127 => Less a N128

data N129 = N129
instance Size N129 where size _ = 129
instance Less N128 N129
instance Less a N128 => Less a N129

data N130 = N130
instance Size N130 where size _ = 130
instance Less N129 N130
instance Less a N129 => Less a N130

data N131 = N131
instance Size N131 where size _ = 131
instance Less N130 N131
instance Less a N130 => Less a N131

data N132 = N132
instance Size N132 where size _ = 132
instance Less N131 N132
instance Less a N131 => Less a N132

data N133 = N133
instance Size N133 where size _ = 133
instance Less N132 N133
instance Less a N132 => Less a N133

data N134 = N134
instance Size N134 where size _ = 134
instance Less N133 N134
instance Less a N133 => Less a N134

data N135 = N135
instance Size N135 where size _ = 135
instance Less N134 N135
instance Less a N134 => Less a N135

data N136 = N136
instance Size N136 where size _ = 136
instance Less N135 N136
instance Less a N135 => Less a N136

data N137 = N137
instance Size N137 where size _ = 137
instance Less N136 N137
instance Less a N136 => Less a N137

data N138 = N138
instance Size N138 where size _ = 138
instance Less N137 N138
instance Less a N137 => Less a N138

data N139 = N139
instance Size N139 where size _ = 139
instance Less N138 N139
instance Less a N138 => Less a N139

data N140 = N140
instance Size N140 where size _ = 140
instance Less N139 N140
instance Less a N139 => Less a N140

data N141 = N141
instance Size N141 where size _ = 141
instance Less N140 N141
instance Less a N140 => Less a N141

data N142 = N142
instance Size N142 where size _ = 142
instance Less N141 N142
instance Less a N141 => Less a N142

data N143 = N143
instance Size N143 where size _ = 143
instance Less N142 N143
instance Less a N142 => Less a N143

data N144 = N144
instance Size N144 where size _ = 144
instance Less N143 N144
instance Less a N143 => Less a N144

data N145 = N145
instance Size N145 where size _ = 145
instance Less N144 N145
instance Less a N144 => Less a N145

data N146 = N146
instance Size N146 where size _ = 146
instance Less N145 N146
instance Less a N145 => Less a N146

data N147 = N147
instance Size N147 where size _ = 147
instance Less N146 N147
instance Less a N146 => Less a N147

data N148 = N148
instance Size N148 where size _ = 148
instance Less N147 N148
instance Less a N147 => Less a N148

data N149 = N149
instance Size N149 where size _ = 149
instance Less N148 N149
instance Less a N148 => Less a N149

data N150 = N150
instance Size N150 where size _ = 150
instance Less N149 N150
instance Less a N149 => Less a N150

data N151 = N151
instance Size N151 where size _ = 151
instance Less N150 N151
instance Less a N150 => Less a N151

data N152 = N152
instance Size N152 where size _ = 152
instance Less N151 N152
instance Less a N151 => Less a N152

data N153 = N153
instance Size N153 where size _ = 153
instance Less N152 N153
instance Less a N152 => Less a N153

data N154 = N154
instance Size N154 where size _ = 154
instance Less N153 N154
instance Less a N153 => Less a N154

data N155 = N155
instance Size N155 where size _ = 155
instance Less N154 N155
instance Less a N154 => Less a N155

data N156 = N156
instance Size N156 where size _ = 156
instance Less N155 N156
instance Less a N155 => Less a N156

data N157 = N157
instance Size N157 where size _ = 157
instance Less N156 N157
instance Less a N156 => Less a N157

data N158 = N158
instance Size N158 where size _ = 158
instance Less N157 N158
instance Less a N157 => Less a N158

data N159 = N159
instance Size N159 where size _ = 159
instance Less N158 N159
instance Less a N158 => Less a N159

data N160 = N160
instance Size N160 where size _ = 160
instance Less N159 N160
instance Less a N159 => Less a N160

data N161 = N161
instance Size N161 where size _ = 161
instance Less N160 N161
instance Less a N160 => Less a N161

data N162 = N162
instance Size N162 where size _ = 162
instance Less N161 N162
instance Less a N161 => Less a N162

data N163 = N163
instance Size N163 where size _ = 163
instance Less N162 N163
instance Less a N162 => Less a N163

data N164 = N164
instance Size N164 where size _ = 164
instance Less N163 N164
instance Less a N163 => Less a N164

data N165 = N165
instance Size N165 where size _ = 165
instance Less N164 N165
instance Less a N164 => Less a N165

data N166 = N166
instance Size N166 where size _ = 166
instance Less N165 N166
instance Less a N165 => Less a N166

data N167 = N167
instance Size N167 where size _ = 167
instance Less N166 N167
instance Less a N166 => Less a N167

data N168 = N168
instance Size N168 where size _ = 168
instance Less N167 N168
instance Less a N167 => Less a N168

data N169 = N169
instance Size N169 where size _ = 169
instance Less N168 N169
instance Less a N168 => Less a N169

data N170 = N170
instance Size N170 where size _ = 170
instance Less N169 N170
instance Less a N169 => Less a N170

data N171 = N171
instance Size N171 where size _ = 171
instance Less N170 N171
instance Less a N170 => Less a N171

data N172 = N172
instance Size N172 where size _ = 172
instance Less N171 N172
instance Less a N171 => Less a N172

data N173 = N173
instance Size N173 where size _ = 173
instance Less N172 N173
instance Less a N172 => Less a N173

data N174 = N174
instance Size N174 where size _ = 174
instance Less N173 N174
instance Less a N173 => Less a N174

data N175 = N175
instance Size N175 where size _ = 175
instance Less N174 N175
instance Less a N174 => Less a N175

data N176 = N176
instance Size N176 where size _ = 176
instance Less N175 N176
instance Less a N175 => Less a N176

data N177 = N177
instance Size N177 where size _ = 177
instance Less N176 N177
instance Less a N176 => Less a N177

data N178 = N178
instance Size N178 where size _ = 178
instance Less N177 N178
instance Less a N177 => Less a N178

data N179 = N179
instance Size N179 where size _ = 179
instance Less N178 N179
instance Less a N178 => Less a N179

data N180 = N180
instance Size N180 where size _ = 180
instance Less N179 N180
instance Less a N179 => Less a N180

data N181 = N181
instance Size N181 where size _ = 181
instance Less N180 N181
instance Less a N180 => Less a N181

data N182 = N182
instance Size N182 where size _ = 182
instance Less N181 N182
instance Less a N181 => Less a N182

data N183 = N183
instance Size N183 where size _ = 183
instance Less N182 N183
instance Less a N182 => Less a N183

data N184 = N184
instance Size N184 where size _ = 184
instance Less N183 N184
instance Less a N183 => Less a N184

data N185 = N185
instance Size N185 where size _ = 185
instance Less N184 N185
instance Less a N184 => Less a N185

data N186 = N186
instance Size N186 where size _ = 186
instance Less N185 N186
instance Less a N185 => Less a N186

data N187 = N187
instance Size N187 where size _ = 187
instance Less N186 N187
instance Less a N186 => Less a N187

data N188 = N188
instance Size N188 where size _ = 188
instance Less N187 N188
instance Less a N187 => Less a N188

data N189 = N189
instance Size N189 where size _ = 189
instance Less N188 N189
instance Less a N188 => Less a N189

data N190 = N190
instance Size N190 where size _ = 190
instance Less N189 N190
instance Less a N189 => Less a N190

data N191 = N191
instance Size N191 where size _ = 191
instance Less N190 N191
instance Less a N190 => Less a N191

data N192 = N192
instance Size N192 where size _ = 192
instance Less N191 N192
instance Less a N191 => Less a N192

data N193 = N193
instance Size N193 where size _ = 193
instance Less N192 N193
instance Less a N192 => Less a N193

data N194 = N194
instance Size N194 where size _ = 194
instance Less N193 N194
instance Less a N193 => Less a N194

data N195 = N195
instance Size N195 where size _ = 195
instance Less N194 N195
instance Less a N194 => Less a N195

data N196 = N196
instance Size N196 where size _ = 196
instance Less N195 N196
instance Less a N195 => Less a N196

data N197 = N197
instance Size N197 where size _ = 197
instance Less N196 N197
instance Less a N196 => Less a N197

data N198 = N198
instance Size N198 where size _ = 198
instance Less N197 N198
instance Less a N197 => Less a N198

data N199 = N199
instance Size N199 where size _ = 199
instance Less N198 N199
instance Less a N198 => Less a N199

data N200 = N200
instance Size N200 where size _ = 200
instance Less N199 N200
instance Less a N199 => Less a N200

data N201 = N201
instance Size N201 where size _ = 201
instance Less N200 N201
instance Less a N200 => Less a N201

data N202 = N202
instance Size N202 where size _ = 202
instance Less N201 N202
instance Less a N201 => Less a N202

data N203 = N203
instance Size N203 where size _ = 203
instance Less N202 N203
instance Less a N202 => Less a N203

data N204 = N204
instance Size N204 where size _ = 204
instance Less N203 N204
instance Less a N203 => Less a N204

data N205 = N205
instance Size N205 where size _ = 205
instance Less N204 N205
instance Less a N204 => Less a N205

data N206 = N206
instance Size N206 where size _ = 206
instance Less N205 N206
instance Less a N205 => Less a N206

data N207 = N207
instance Size N207 where size _ = 207
instance Less N206 N207
instance Less a N206 => Less a N207

data N208 = N208
instance Size N208 where size _ = 208
instance Less N207 N208
instance Less a N207 => Less a N208

data N209 = N209
instance Size N209 where size _ = 209
instance Less N208 N209
instance Less a N208 => Less a N209

data N210 = N210
instance Size N210 where size _ = 210
instance Less N209 N210
instance Less a N209 => Less a N210

data N211 = N211
instance Size N211 where size _ = 211
instance Less N210 N211
instance Less a N210 => Less a N211

data N212 = N212
instance Size N212 where size _ = 212
instance Less N211 N212
instance Less a N211 => Less a N212

data N213 = N213
instance Size N213 where size _ = 213
instance Less N212 N213
instance Less a N212 => Less a N213

data N214 = N214
instance Size N214 where size _ = 214
instance Less N213 N214
instance Less a N213 => Less a N214

data N215 = N215
instance Size N215 where size _ = 215
instance Less N214 N215
instance Less a N214 => Less a N215

data N216 = N216
instance Size N216 where size _ = 216
instance Less N215 N216
instance Less a N215 => Less a N216

data N217 = N217
instance Size N217 where size _ = 217
instance Less N216 N217
instance Less a N216 => Less a N217

data N218 = N218
instance Size N218 where size _ = 218
instance Less N217 N218
instance Less a N217 => Less a N218

data N219 = N219
instance Size N219 where size _ = 219
instance Less N218 N219
instance Less a N218 => Less a N219

data N220 = N220
instance Size N220 where size _ = 220
instance Less N219 N220
instance Less a N219 => Less a N220

data N221 = N221
instance Size N221 where size _ = 221
instance Less N220 N221
instance Less a N220 => Less a N221

data N222 = N222
instance Size N222 where size _ = 222
instance Less N221 N222
instance Less a N221 => Less a N222

data N223 = N223
instance Size N223 where size _ = 223
instance Less N222 N223
instance Less a N222 => Less a N223

data N224 = N224
instance Size N224 where size _ = 224
instance Less N223 N224
instance Less a N223 => Less a N224

data N225 = N225
instance Size N225 where size _ = 225
instance Less N224 N225
instance Less a N224 => Less a N225

data N226 = N226
instance Size N226 where size _ = 226
instance Less N225 N226
instance Less a N225 => Less a N226

data N227 = N227
instance Size N227 where size _ = 227
instance Less N226 N227
instance Less a N226 => Less a N227

data N228 = N228
instance Size N228 where size _ = 228
instance Less N227 N228
instance Less a N227 => Less a N228

data N229 = N229
instance Size N229 where size _ = 229
instance Less N228 N229
instance Less a N228 => Less a N229

data N230 = N230
instance Size N230 where size _ = 230
instance Less N229 N230
instance Less a N229 => Less a N230

data N231 = N231
instance Size N231 where size _ = 231
instance Less N230 N231
instance Less a N230 => Less a N231

data N232 = N232
instance Size N232 where size _ = 232
instance Less N231 N232
instance Less a N231 => Less a N232

data N233 = N233
instance Size N233 where size _ = 233
instance Less N232 N233
instance Less a N232 => Less a N233

data N234 = N234
instance Size N234 where size _ = 234
instance Less N233 N234
instance Less a N233 => Less a N234

data N235 = N235
instance Size N235 where size _ = 235
instance Less N234 N235
instance Less a N234 => Less a N235

data N236 = N236
instance Size N236 where size _ = 236
instance Less N235 N236
instance Less a N235 => Less a N236

data N237 = N237
instance Size N237 where size _ = 237
instance Less N236 N237
instance Less a N236 => Less a N237

data N238 = N238
instance Size N238 where size _ = 238
instance Less N237 N238
instance Less a N237 => Less a N238

data N239 = N239
instance Size N239 where size _ = 239
instance Less N238 N239
instance Less a N238 => Less a N239

data N240 = N240
instance Size N240 where size _ = 240
instance Less N239 N240
instance Less a N239 => Less a N240

data N241 = N241
instance Size N241 where size _ = 241
instance Less N240 N241
instance Less a N240 => Less a N241

data N242 = N242
instance Size N242 where size _ = 242
instance Less N241 N242
instance Less a N241 => Less a N242

data N243 = N243
instance Size N243 where size _ = 243
instance Less N242 N243
instance Less a N242 => Less a N243

data N244 = N244
instance Size N244 where size _ = 244
instance Less N243 N244
instance Less a N243 => Less a N244

data N245 = N245
instance Size N245 where size _ = 245
instance Less N244 N245
instance Less a N244 => Less a N245

data N246 = N246
instance Size N246 where size _ = 246
instance Less N245 N246
instance Less a N245 => Less a N246

data N247 = N247
instance Size N247 where size _ = 247
instance Less N246 N247
instance Less a N246 => Less a N247

data N248 = N248
instance Size N248 where size _ = 248
instance Less N247 N248
instance Less a N247 => Less a N248

data N249 = N249
instance Size N249 where size _ = 249
instance Less N248 N249
instance Less a N248 => Less a N249

data N250 = N250
instance Size N250 where size _ = 250
instance Less N249 N250
instance Less a N249 => Less a N250

data N251 = N251
instance Size N251 where size _ = 251
instance Less N250 N251
instance Less a N250 => Less a N251

data N252 = N252
instance Size N252 where size _ = 252
instance Less N251 N252
instance Less a N251 => Less a N252

data N253 = N253
instance Size N253 where size _ = 253
instance Less N252 N253
instance Less a N252 => Less a N253

data N254 = N254
instance Size N254 where size _ = 254
instance Less N253 N254
instance Less a N253 => Less a N254

data N255 = N255
instance Size N255 where size _ = 255
instance Less N254 N255
instance Less a N254 => Less a N255

data N65535 = N65535
instance Size N65535 where size _ = 65535
instance Less N255 N65535
instance Less a N255 => Less a N65535

newtype BoundedList a n = L [a]

instance (Show a, Size n) => Show (BoundedList a n) where
    show l@(L xs) = show xs

instance (Size n, Eq a) => Eq (BoundedList a n) where
    L c == L d = c == d

-- | Shrinks the 'BoundedList' supplied if
-- it can do so without truncating the list. Returns Nothing
-- if the list inside was to long.
shrink :: (Size n, Size m) => BoundedList a n -> Maybe (BoundedList a m)
shrink =  toBounded . fromBounded

-- | Takes a 'BoundedList' add grows it size.
grow :: LessEq n m => BoundedList a n -> BoundedList a m
grow (L xs) = (L xs)

-- | Takes a 'BoundedList' and return the list inside.
fromBounded :: Size n => BoundedList a n -> [a]
fromBounded (L xs) = xs


listLength :: BoundedList a n -> Int
listLength (L l) = length l

-- | Returns the length of a 'BoundedList'.
listBound :: Size n => BoundedList a n -> Int
listBound = size . listBoundType

listBoundType :: BoundedList a n -> n
listBoundType _ = undefined

-- | Takes a list and transforms it to a 'BoundedList'.
-- If the list doesn\'t fit, Nothing is returned.
toBounded :: Size n => [a] -> Maybe (BoundedList a n)
toBounded a = toBound_ (L a)
    where
    toBound_ :: Size n => BoundedList a n -> Maybe (BoundedList a n)
    toBound_ l
	| listLength l <= listBound l = Just l
	| otherwise = Nothing

-- | Takes a list and transforms it to a 'BoundedList'.
-- If the list doesn\'n fit, the list is truncated
-- to make it fit into the bounded list.
trunc :: Size n => [a] -> BoundedList a n
trunc xs = trunc_ (L xs)
    where
    trunc_ :: Size n => BoundedList a n -> BoundedList a n
    trunc_ l@(L xs) = (L $ take (listBound l) xs)
