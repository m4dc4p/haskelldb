-----------------------------------------------------------
-- |
-- Module      :  BoundedList
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines a polymorphic list with given length
-----------------------------------------------------------
module Database.HaskellDB.BoundedList (shrink, 
		    grow,
		    trunc,
		    listBound,
		    toBounded,
		    fromBounded,
		    Size,
		    BoundedList,
		    N, N0, N1, N2, N3, N4, N5, N6, N7, N8, N9,
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
		    N250, N251, N252, N253, N254, N255)

where
		    
class Size n where
    size :: n -> Int

class Less a b

class LessEq a b
instance LessEq (N a) (N a)
instance Less (N a) (N b) => LessEq (N a) (N b)

newtype N n = N n

data N0 = N0
instance Size (N N0) where size _ = 0

data N1 = N1
instance Size (N N1) where size _ = 1
instance Less (N N0) (N N1)
instance Less a (N N0) => Less a (N N1)

data N2 = N2
instance Size (N N2) where size _ = 2
instance Less (N N1) (N N2)
instance Less a (N N1) => Less a (N N2)

data N3 = N3
instance Size (N N3) where size _ = 3
instance Less (N N2) (N N3)
instance Less a (N N2) => Less a (N N3)

data N4 = N4
instance Size (N N4) where size _ = 4
instance Less (N N3) (N N4)
instance Less a (N N3) => Less a (N N4)

data N5 = N5
instance Size (N N5) where size _ = 5
instance Less (N N4) (N N5)
instance Less a (N N4) => Less a (N N5)

data N6 = N6
instance Size (N N6) where size _ = 6
instance Less (N N5) (N N6)
instance Less a (N N5) => Less a (N N6)

data N7 = N7
instance Size (N N7) where size _ = 7
instance Less (N N6) (N N7)
instance Less a (N N6) => Less a (N N7)

data N8 = N8
instance Size (N N8) where size _ = 8
instance Less (N N7) (N N8)
instance Less a (N N7) => Less a (N N8)

data N9 = N9
instance Size (N N9) where size _ = 9
instance Less (N N8) (N N9)
instance Less a (N N8) => Less a (N N9)

data N10 = N10
instance Size (N N10) where size _ = 10
instance Less (N N9) (N N10)
instance Less a (N N9) => Less a (N N10)

data N11 = N11
instance Size (N N11) where size _ = 11
instance Less (N N10) (N N11)
instance Less a (N N10) => Less a (N N11)

data N12 = N12
instance Size (N N12) where size _ = 12
instance Less (N N11) (N N12)
instance Less a (N N11) => Less a (N N12)

data N13 = N13
instance Size (N N13) where size _ = 13
instance Less (N N12) (N N13)
instance Less a (N N12) => Less a (N N13)

data N14 = N14
instance Size (N N14) where size _ = 14
instance Less (N N13) (N N14)
instance Less a (N N13) => Less a (N N14)

data N15 = N15
instance Size (N N15) where size _ = 15
instance Less (N N14) (N N15)
instance Less a (N N14) => Less a (N N15)

data N16 = N16
instance Size (N N16) where size _ = 16
instance Less (N N15) (N N16)
instance Less a (N N15) => Less a (N N16)

data N17 = N17
instance Size (N N17) where size _ = 17
instance Less (N N16) (N N17)
instance Less a (N N16) => Less a (N N17)

data N18 = N18
instance Size (N N18) where size _ = 18
instance Less (N N17) (N N18)
instance Less a (N N17) => Less a (N N18)

data N19 = N19
instance Size (N N19) where size _ = 19
instance Less (N N18) (N N19)
instance Less a (N N18) => Less a (N N19)

data N20 = N20
instance Size (N N20) where size _ = 20
instance Less (N N19) (N N20)
instance Less a (N N19) => Less a (N N20)

data N21 = N21
instance Size (N N21) where size _ = 21
instance Less (N N20) (N N21)
instance Less a (N N20) => Less a (N N21)

data N22 = N22
instance Size (N N22) where size _ = 22
instance Less (N N21) (N N22)
instance Less a (N N21) => Less a (N N22)

data N23 = N23
instance Size (N N23) where size _ = 23
instance Less (N N22) (N N23)
instance Less a (N N22) => Less a (N N23)

data N24 = N24
instance Size (N N24) where size _ = 24
instance Less (N N23) (N N24)
instance Less a (N N23) => Less a (N N24)

data N25 = N25
instance Size (N N25) where size _ = 25
instance Less (N N24) (N N25)
instance Less a (N N24) => Less a (N N25)

data N26 = N26
instance Size (N N26) where size _ = 26
instance Less (N N25) (N N26)
instance Less a (N N25) => Less a (N N26)

data N27 = N27
instance Size (N N27) where size _ = 27
instance Less (N N26) (N N27)
instance Less a (N N26) => Less a (N N27)

data N28 = N28
instance Size (N N28) where size _ = 28
instance Less (N N27) (N N28)
instance Less a (N N27) => Less a (N N28)

data N29 = N29
instance Size (N N29) where size _ = 29
instance Less (N N28) (N N29)
instance Less a (N N28) => Less a (N N29)

data N30 = N30
instance Size (N N30) where size _ = 30
instance Less (N N29) (N N30)
instance Less a (N N29) => Less a (N N30)

data N31 = N31
instance Size (N N31) where size _ = 31
instance Less (N N30) (N N31)
instance Less a (N N30) => Less a (N N31)

data N32 = N32
instance Size (N N32) where size _ = 32
instance Less (N N31) (N N32)
instance Less a (N N31) => Less a (N N32)

data N33 = N33
instance Size (N N33) where size _ = 33
instance Less (N N32) (N N33)
instance Less a (N N32) => Less a (N N33)

data N34 = N34
instance Size (N N34) where size _ = 34
instance Less (N N33) (N N34)
instance Less a (N N33) => Less a (N N34)

data N35 = N35
instance Size (N N35) where size _ = 35
instance Less (N N34) (N N35)
instance Less a (N N34) => Less a (N N35)

data N36 = N36
instance Size (N N36) where size _ = 36
instance Less (N N35) (N N36)
instance Less a (N N35) => Less a (N N36)

data N37 = N37
instance Size (N N37) where size _ = 37
instance Less (N N36) (N N37)
instance Less a (N N36) => Less a (N N37)

data N38 = N38
instance Size (N N38) where size _ = 38
instance Less (N N37) (N N38)
instance Less a (N N37) => Less a (N N38)

data N39 = N39
instance Size (N N39) where size _ = 39
instance Less (N N38) (N N39)
instance Less a (N N38) => Less a (N N39)

data N40 = N40
instance Size (N N40) where size _ = 40
instance Less (N N39) (N N40)
instance Less a (N N39) => Less a (N N40)

data N41 = N41
instance Size (N N41) where size _ = 41
instance Less (N N40) (N N41)
instance Less a (N N40) => Less a (N N41)

data N42 = N42
instance Size (N N42) where size _ = 42
instance Less (N N41) (N N42)
instance Less a (N N41) => Less a (N N42)

data N43 = N43
instance Size (N N43) where size _ = 43
instance Less (N N42) (N N43)
instance Less a (N N42) => Less a (N N43)

data N44 = N44
instance Size (N N44) where size _ = 44
instance Less (N N43) (N N44)
instance Less a (N N43) => Less a (N N44)

data N45 = N45
instance Size (N N45) where size _ = 45
instance Less (N N44) (N N45)
instance Less a (N N44) => Less a (N N45)

data N46 = N46
instance Size (N N46) where size _ = 46
instance Less (N N45) (N N46)
instance Less a (N N45) => Less a (N N46)

data N47 = N47
instance Size (N N47) where size _ = 47
instance Less (N N46) (N N47)
instance Less a (N N46) => Less a (N N47)

data N48 = N48
instance Size (N N48) where size _ = 48
instance Less (N N47) (N N48)
instance Less a (N N47) => Less a (N N48)

data N49 = N49
instance Size (N N49) where size _ = 49
instance Less (N N48) (N N49)
instance Less a (N N48) => Less a (N N49)

data N50 = N50
instance Size (N N50) where size _ = 50
instance Less (N N49) (N N50)
instance Less a (N N49) => Less a (N N50)

data N51 = N51
instance Size (N N51) where size _ = 51
instance Less (N N50) (N N51)
instance Less a (N N50) => Less a (N N51)

data N52 = N52
instance Size (N N52) where size _ = 52
instance Less (N N51) (N N52)
instance Less a (N N51) => Less a (N N52)

data N53 = N53
instance Size (N N53) where size _ = 53
instance Less (N N52) (N N53)
instance Less a (N N52) => Less a (N N53)

data N54 = N54
instance Size (N N54) where size _ = 54
instance Less (N N53) (N N54)
instance Less a (N N53) => Less a (N N54)

data N55 = N55
instance Size (N N55) where size _ = 55
instance Less (N N54) (N N55)
instance Less a (N N54) => Less a (N N55)

data N56 = N56
instance Size (N N56) where size _ = 56
instance Less (N N55) (N N56)
instance Less a (N N55) => Less a (N N56)

data N57 = N57
instance Size (N N57) where size _ = 57
instance Less (N N56) (N N57)
instance Less a (N N56) => Less a (N N57)

data N58 = N58
instance Size (N N58) where size _ = 58
instance Less (N N57) (N N58)
instance Less a (N N57) => Less a (N N58)

data N59 = N59
instance Size (N N59) where size _ = 59
instance Less (N N58) (N N59)
instance Less a (N N58) => Less a (N N59)

data N60 = N60
instance Size (N N60) where size _ = 60
instance Less (N N59) (N N60)
instance Less a (N N59) => Less a (N N60)

data N61 = N61
instance Size (N N61) where size _ = 61
instance Less (N N60) (N N61)
instance Less a (N N60) => Less a (N N61)

data N62 = N62
instance Size (N N62) where size _ = 62
instance Less (N N61) (N N62)
instance Less a (N N61) => Less a (N N62)

data N63 = N63
instance Size (N N63) where size _ = 63
instance Less (N N62) (N N63)
instance Less a (N N62) => Less a (N N63)

data N64 = N64
instance Size (N N64) where size _ = 64
instance Less (N N63) (N N64)
instance Less a (N N63) => Less a (N N64)

data N65 = N65
instance Size (N N65) where size _ = 65
instance Less (N N64) (N N65)
instance Less a (N N64) => Less a (N N65)

data N66 = N66
instance Size (N N66) where size _ = 66
instance Less (N N65) (N N66)
instance Less a (N N65) => Less a (N N66)

data N67 = N67
instance Size (N N67) where size _ = 67
instance Less (N N66) (N N67)
instance Less a (N N66) => Less a (N N67)

data N68 = N68
instance Size (N N68) where size _ = 68
instance Less (N N67) (N N68)
instance Less a (N N67) => Less a (N N68)

data N69 = N69
instance Size (N N69) where size _ = 69
instance Less (N N68) (N N69)
instance Less a (N N68) => Less a (N N69)

data N70 = N70
instance Size (N N70) where size _ = 70
instance Less (N N69) (N N70)
instance Less a (N N69) => Less a (N N70)

data N71 = N71
instance Size (N N71) where size _ = 71
instance Less (N N70) (N N71)
instance Less a (N N70) => Less a (N N71)

data N72 = N72
instance Size (N N72) where size _ = 72
instance Less (N N71) (N N72)
instance Less a (N N71) => Less a (N N72)

data N73 = N73
instance Size (N N73) where size _ = 73
instance Less (N N72) (N N73)
instance Less a (N N72) => Less a (N N73)

data N74 = N74
instance Size (N N74) where size _ = 74
instance Less (N N73) (N N74)
instance Less a (N N73) => Less a (N N74)

data N75 = N75
instance Size (N N75) where size _ = 75
instance Less (N N74) (N N75)
instance Less a (N N74) => Less a (N N75)

data N76 = N76
instance Size (N N76) where size _ = 76
instance Less (N N75) (N N76)
instance Less a (N N75) => Less a (N N76)

data N77 = N77
instance Size (N N77) where size _ = 77
instance Less (N N76) (N N77)
instance Less a (N N76) => Less a (N N77)

data N78 = N78
instance Size (N N78) where size _ = 78
instance Less (N N77) (N N78)
instance Less a (N N77) => Less a (N N78)

data N79 = N79
instance Size (N N79) where size _ = 79
instance Less (N N78) (N N79)
instance Less a (N N78) => Less a (N N79)

data N80 = N80
instance Size (N N80) where size _ = 80
instance Less (N N79) (N N80)
instance Less a (N N79) => Less a (N N80)

data N81 = N81
instance Size (N N81) where size _ = 81
instance Less (N N80) (N N81)
instance Less a (N N80) => Less a (N N81)

data N82 = N82
instance Size (N N82) where size _ = 82
instance Less (N N81) (N N82)
instance Less a (N N81) => Less a (N N82)

data N83 = N83
instance Size (N N83) where size _ = 83
instance Less (N N82) (N N83)
instance Less a (N N82) => Less a (N N83)

data N84 = N84
instance Size (N N84) where size _ = 84
instance Less (N N83) (N N84)
instance Less a (N N83) => Less a (N N84)

data N85 = N85
instance Size (N N85) where size _ = 85
instance Less (N N84) (N N85)
instance Less a (N N84) => Less a (N N85)

data N86 = N86
instance Size (N N86) where size _ = 86
instance Less (N N85) (N N86)
instance Less a (N N85) => Less a (N N86)

data N87 = N87
instance Size (N N87) where size _ = 87
instance Less (N N86) (N N87)
instance Less a (N N86) => Less a (N N87)

data N88 = N88
instance Size (N N88) where size _ = 88
instance Less (N N87) (N N88)
instance Less a (N N87) => Less a (N N88)

data N89 = N89
instance Size (N N89) where size _ = 89
instance Less (N N88) (N N89)
instance Less a (N N88) => Less a (N N89)

data N90 = N90
instance Size (N N90) where size _ = 90
instance Less (N N89) (N N90)
instance Less a (N N89) => Less a (N N90)

data N91 = N91
instance Size (N N91) where size _ = 91
instance Less (N N90) (N N91)
instance Less a (N N90) => Less a (N N91)

data N92 = N92
instance Size (N N92) where size _ = 92
instance Less (N N91) (N N92)
instance Less a (N N91) => Less a (N N92)

data N93 = N93
instance Size (N N93) where size _ = 93
instance Less (N N92) (N N93)
instance Less a (N N92) => Less a (N N93)

data N94 = N94
instance Size (N N94) where size _ = 94
instance Less (N N93) (N N94)
instance Less a (N N93) => Less a (N N94)

data N95 = N95
instance Size (N N95) where size _ = 95
instance Less (N N94) (N N95)
instance Less a (N N94) => Less a (N N95)

data N96 = N96
instance Size (N N96) where size _ = 96
instance Less (N N95) (N N96)
instance Less a (N N95) => Less a (N N96)

data N97 = N97
instance Size (N N97) where size _ = 97
instance Less (N N96) (N N97)
instance Less a (N N96) => Less a (N N97)

data N98 = N98
instance Size (N N98) where size _ = 98
instance Less (N N97) (N N98)
instance Less a (N N97) => Less a (N N98)

data N99 = N99
instance Size (N N99) where size _ = 99
instance Less (N N98) (N N99)
instance Less a (N N98) => Less a (N N99)

data N100 = N100
instance Size (N N100) where size _ = 100
instance Less (N N99) (N N100)
instance Less a (N N99) => Less a (N N100)

data N101 = N101
instance Size (N N101) where size _ = 101
instance Less (N N100) (N N101)
instance Less a (N N100) => Less a (N N101)

data N102 = N102
instance Size (N N102) where size _ = 102
instance Less (N N101) (N N102)
instance Less a (N N101) => Less a (N N102)

data N103 = N103
instance Size (N N103) where size _ = 103
instance Less (N N102) (N N103)
instance Less a (N N102) => Less a (N N103)

data N104 = N104
instance Size (N N104) where size _ = 104
instance Less (N N103) (N N104)
instance Less a (N N103) => Less a (N N104)

data N105 = N105
instance Size (N N105) where size _ = 105
instance Less (N N104) (N N105)
instance Less a (N N104) => Less a (N N105)

data N106 = N106
instance Size (N N106) where size _ = 106
instance Less (N N105) (N N106)
instance Less a (N N105) => Less a (N N106)

data N107 = N107
instance Size (N N107) where size _ = 107
instance Less (N N106) (N N107)
instance Less a (N N106) => Less a (N N107)

data N108 = N108
instance Size (N N108) where size _ = 108
instance Less (N N107) (N N108)
instance Less a (N N107) => Less a (N N108)

data N109 = N109
instance Size (N N109) where size _ = 109
instance Less (N N108) (N N109)
instance Less a (N N108) => Less a (N N109)

data N110 = N110
instance Size (N N110) where size _ = 110
instance Less (N N109) (N N110)
instance Less a (N N109) => Less a (N N110)

data N111 = N111
instance Size (N N111) where size _ = 111
instance Less (N N110) (N N111)
instance Less a (N N110) => Less a (N N111)

data N112 = N112
instance Size (N N112) where size _ = 112
instance Less (N N111) (N N112)
instance Less a (N N111) => Less a (N N112)

data N113 = N113
instance Size (N N113) where size _ = 113
instance Less (N N112) (N N113)
instance Less a (N N112) => Less a (N N113)

data N114 = N114
instance Size (N N114) where size _ = 114
instance Less (N N113) (N N114)
instance Less a (N N113) => Less a (N N114)

data N115 = N115
instance Size (N N115) where size _ = 115
instance Less (N N114) (N N115)
instance Less a (N N114) => Less a (N N115)

data N116 = N116
instance Size (N N116) where size _ = 116
instance Less (N N115) (N N116)
instance Less a (N N115) => Less a (N N116)

data N117 = N117
instance Size (N N117) where size _ = 117
instance Less (N N116) (N N117)
instance Less a (N N116) => Less a (N N117)

data N118 = N118
instance Size (N N118) where size _ = 118
instance Less (N N117) (N N118)
instance Less a (N N117) => Less a (N N118)

data N119 = N119
instance Size (N N119) where size _ = 119
instance Less (N N118) (N N119)
instance Less a (N N118) => Less a (N N119)

data N120 = N120
instance Size (N N120) where size _ = 120
instance Less (N N119) (N N120)
instance Less a (N N119) => Less a (N N120)

data N121 = N121
instance Size (N N121) where size _ = 121
instance Less (N N120) (N N121)
instance Less a (N N120) => Less a (N N121)

data N122 = N122
instance Size (N N122) where size _ = 122
instance Less (N N121) (N N122)
instance Less a (N N121) => Less a (N N122)

data N123 = N123
instance Size (N N123) where size _ = 123
instance Less (N N122) (N N123)
instance Less a (N N122) => Less a (N N123)

data N124 = N124
instance Size (N N124) where size _ = 124
instance Less (N N123) (N N124)
instance Less a (N N123) => Less a (N N124)

data N125 = N125
instance Size (N N125) where size _ = 125
instance Less (N N124) (N N125)
instance Less a (N N124) => Less a (N N125)

data N126 = N126
instance Size (N N126) where size _ = 126
instance Less (N N125) (N N126)
instance Less a (N N125) => Less a (N N126)

data N127 = N127
instance Size (N N127) where size _ = 127
instance Less (N N126) (N N127)
instance Less a (N N126) => Less a (N N127)

data N128 = N128
instance Size (N N128) where size _ = 128
instance Less (N N127) (N N128)
instance Less a (N N127) => Less a (N N128)

data N129 = N129
instance Size (N N129) where size _ = 129
instance Less (N N128) (N N129)
instance Less a (N N128) => Less a (N N129)

data N130 = N130
instance Size (N N130) where size _ = 130
instance Less (N N129) (N N130)
instance Less a (N N129) => Less a (N N130)

data N131 = N131
instance Size (N N131) where size _ = 131
instance Less (N N130) (N N131)
instance Less a (N N130) => Less a (N N131)

data N132 = N132
instance Size (N N132) where size _ = 132
instance Less (N N131) (N N132)
instance Less a (N N131) => Less a (N N132)

data N133 = N133
instance Size (N N133) where size _ = 133
instance Less (N N132) (N N133)
instance Less a (N N132) => Less a (N N133)

data N134 = N134
instance Size (N N134) where size _ = 134
instance Less (N N133) (N N134)
instance Less a (N N133) => Less a (N N134)

data N135 = N135
instance Size (N N135) where size _ = 135
instance Less (N N134) (N N135)
instance Less a (N N134) => Less a (N N135)

data N136 = N136
instance Size (N N136) where size _ = 136
instance Less (N N135) (N N136)
instance Less a (N N135) => Less a (N N136)

data N137 = N137
instance Size (N N137) where size _ = 137
instance Less (N N136) (N N137)
instance Less a (N N136) => Less a (N N137)

data N138 = N138
instance Size (N N138) where size _ = 138
instance Less (N N137) (N N138)
instance Less a (N N137) => Less a (N N138)

data N139 = N139
instance Size (N N139) where size _ = 139
instance Less (N N138) (N N139)
instance Less a (N N138) => Less a (N N139)

data N140 = N140
instance Size (N N140) where size _ = 140
instance Less (N N139) (N N140)
instance Less a (N N139) => Less a (N N140)

data N141 = N141
instance Size (N N141) where size _ = 141
instance Less (N N140) (N N141)
instance Less a (N N140) => Less a (N N141)

data N142 = N142
instance Size (N N142) where size _ = 142
instance Less (N N141) (N N142)
instance Less a (N N141) => Less a (N N142)

data N143 = N143
instance Size (N N143) where size _ = 143
instance Less (N N142) (N N143)
instance Less a (N N142) => Less a (N N143)

data N144 = N144
instance Size (N N144) where size _ = 144
instance Less (N N143) (N N144)
instance Less a (N N143) => Less a (N N144)

data N145 = N145
instance Size (N N145) where size _ = 145
instance Less (N N144) (N N145)
instance Less a (N N144) => Less a (N N145)

data N146 = N146
instance Size (N N146) where size _ = 146
instance Less (N N145) (N N146)
instance Less a (N N145) => Less a (N N146)

data N147 = N147
instance Size (N N147) where size _ = 147
instance Less (N N146) (N N147)
instance Less a (N N146) => Less a (N N147)

data N148 = N148
instance Size (N N148) where size _ = 148
instance Less (N N147) (N N148)
instance Less a (N N147) => Less a (N N148)

data N149 = N149
instance Size (N N149) where size _ = 149
instance Less (N N148) (N N149)
instance Less a (N N148) => Less a (N N149)

data N150 = N150
instance Size (N N150) where size _ = 150
instance Less (N N149) (N N150)
instance Less a (N N149) => Less a (N N150)

data N151 = N151
instance Size (N N151) where size _ = 151
instance Less (N N150) (N N151)
instance Less a (N N150) => Less a (N N151)

data N152 = N152
instance Size (N N152) where size _ = 152
instance Less (N N151) (N N152)
instance Less a (N N151) => Less a (N N152)

data N153 = N153
instance Size (N N153) where size _ = 153
instance Less (N N152) (N N153)
instance Less a (N N152) => Less a (N N153)

data N154 = N154
instance Size (N N154) where size _ = 154
instance Less (N N153) (N N154)
instance Less a (N N153) => Less a (N N154)

data N155 = N155
instance Size (N N155) where size _ = 155
instance Less (N N154) (N N155)
instance Less a (N N154) => Less a (N N155)

data N156 = N156
instance Size (N N156) where size _ = 156
instance Less (N N155) (N N156)
instance Less a (N N155) => Less a (N N156)

data N157 = N157
instance Size (N N157) where size _ = 157
instance Less (N N156) (N N157)
instance Less a (N N156) => Less a (N N157)

data N158 = N158
instance Size (N N158) where size _ = 158
instance Less (N N157) (N N158)
instance Less a (N N157) => Less a (N N158)

data N159 = N159
instance Size (N N159) where size _ = 159
instance Less (N N158) (N N159)
instance Less a (N N158) => Less a (N N159)

data N160 = N160
instance Size (N N160) where size _ = 160
instance Less (N N159) (N N160)
instance Less a (N N159) => Less a (N N160)

data N161 = N161
instance Size (N N161) where size _ = 161
instance Less (N N160) (N N161)
instance Less a (N N160) => Less a (N N161)

data N162 = N162
instance Size (N N162) where size _ = 162
instance Less (N N161) (N N162)
instance Less a (N N161) => Less a (N N162)

data N163 = N163
instance Size (N N163) where size _ = 163
instance Less (N N162) (N N163)
instance Less a (N N162) => Less a (N N163)

data N164 = N164
instance Size (N N164) where size _ = 164
instance Less (N N163) (N N164)
instance Less a (N N163) => Less a (N N164)

data N165 = N165
instance Size (N N165) where size _ = 165
instance Less (N N164) (N N165)
instance Less a (N N164) => Less a (N N165)

data N166 = N166
instance Size (N N166) where size _ = 166
instance Less (N N165) (N N166)
instance Less a (N N165) => Less a (N N166)

data N167 = N167
instance Size (N N167) where size _ = 167
instance Less (N N166) (N N167)
instance Less a (N N166) => Less a (N N167)

data N168 = N168
instance Size (N N168) where size _ = 168
instance Less (N N167) (N N168)
instance Less a (N N167) => Less a (N N168)

data N169 = N169
instance Size (N N169) where size _ = 169
instance Less (N N168) (N N169)
instance Less a (N N168) => Less a (N N169)

data N170 = N170
instance Size (N N170) where size _ = 170
instance Less (N N169) (N N170)
instance Less a (N N169) => Less a (N N170)

data N171 = N171
instance Size (N N171) where size _ = 171
instance Less (N N170) (N N171)
instance Less a (N N170) => Less a (N N171)

data N172 = N172
instance Size (N N172) where size _ = 172
instance Less (N N171) (N N172)
instance Less a (N N171) => Less a (N N172)

data N173 = N173
instance Size (N N173) where size _ = 173
instance Less (N N172) (N N173)
instance Less a (N N172) => Less a (N N173)

data N174 = N174
instance Size (N N174) where size _ = 174
instance Less (N N173) (N N174)
instance Less a (N N173) => Less a (N N174)

data N175 = N175
instance Size (N N175) where size _ = 175
instance Less (N N174) (N N175)
instance Less a (N N174) => Less a (N N175)

data N176 = N176
instance Size (N N176) where size _ = 176
instance Less (N N175) (N N176)
instance Less a (N N175) => Less a (N N176)

data N177 = N177
instance Size (N N177) where size _ = 177
instance Less (N N176) (N N177)
instance Less a (N N176) => Less a (N N177)

data N178 = N178
instance Size (N N178) where size _ = 178
instance Less (N N177) (N N178)
instance Less a (N N177) => Less a (N N178)

data N179 = N179
instance Size (N N179) where size _ = 179
instance Less (N N178) (N N179)
instance Less a (N N178) => Less a (N N179)

data N180 = N180
instance Size (N N180) where size _ = 180
instance Less (N N179) (N N180)
instance Less a (N N179) => Less a (N N180)

data N181 = N181
instance Size (N N181) where size _ = 181
instance Less (N N180) (N N181)
instance Less a (N N180) => Less a (N N181)

data N182 = N182
instance Size (N N182) where size _ = 182
instance Less (N N181) (N N182)
instance Less a (N N181) => Less a (N N182)

data N183 = N183
instance Size (N N183) where size _ = 183
instance Less (N N182) (N N183)
instance Less a (N N182) => Less a (N N183)

data N184 = N184
instance Size (N N184) where size _ = 184
instance Less (N N183) (N N184)
instance Less a (N N183) => Less a (N N184)

data N185 = N185
instance Size (N N185) where size _ = 185
instance Less (N N184) (N N185)
instance Less a (N N184) => Less a (N N185)

data N186 = N186
instance Size (N N186) where size _ = 186
instance Less (N N185) (N N186)
instance Less a (N N185) => Less a (N N186)

data N187 = N187
instance Size (N N187) where size _ = 187
instance Less (N N186) (N N187)
instance Less a (N N186) => Less a (N N187)

data N188 = N188
instance Size (N N188) where size _ = 188
instance Less (N N187) (N N188)
instance Less a (N N187) => Less a (N N188)

data N189 = N189
instance Size (N N189) where size _ = 189
instance Less (N N188) (N N189)
instance Less a (N N188) => Less a (N N189)

data N190 = N190
instance Size (N N190) where size _ = 190
instance Less (N N189) (N N190)
instance Less a (N N189) => Less a (N N190)

data N191 = N191
instance Size (N N191) where size _ = 191
instance Less (N N190) (N N191)
instance Less a (N N190) => Less a (N N191)

data N192 = N192
instance Size (N N192) where size _ = 192
instance Less (N N191) (N N192)
instance Less a (N N191) => Less a (N N192)

data N193 = N193
instance Size (N N193) where size _ = 193
instance Less (N N192) (N N193)
instance Less a (N N192) => Less a (N N193)

data N194 = N194
instance Size (N N194) where size _ = 194
instance Less (N N193) (N N194)
instance Less a (N N193) => Less a (N N194)

data N195 = N195
instance Size (N N195) where size _ = 195
instance Less (N N194) (N N195)
instance Less a (N N194) => Less a (N N195)

data N196 = N196
instance Size (N N196) where size _ = 196
instance Less (N N195) (N N196)
instance Less a (N N195) => Less a (N N196)

data N197 = N197
instance Size (N N197) where size _ = 197
instance Less (N N196) (N N197)
instance Less a (N N196) => Less a (N N197)

data N198 = N198
instance Size (N N198) where size _ = 198
instance Less (N N197) (N N198)
instance Less a (N N197) => Less a (N N198)

data N199 = N199
instance Size (N N199) where size _ = 199
instance Less (N N198) (N N199)
instance Less a (N N198) => Less a (N N199)

data N200 = N200
instance Size (N N200) where size _ = 200
instance Less (N N199) (N N200)
instance Less a (N N199) => Less a (N N200)

data N201 = N201
instance Size (N N201) where size _ = 201
instance Less (N N200) (N N201)
instance Less a (N N200) => Less a (N N201)

data N202 = N202
instance Size (N N202) where size _ = 202
instance Less (N N201) (N N202)
instance Less a (N N201) => Less a (N N202)

data N203 = N203
instance Size (N N203) where size _ = 203
instance Less (N N202) (N N203)
instance Less a (N N202) => Less a (N N203)

data N204 = N204
instance Size (N N204) where size _ = 204
instance Less (N N203) (N N204)
instance Less a (N N203) => Less a (N N204)

data N205 = N205
instance Size (N N205) where size _ = 205
instance Less (N N204) (N N205)
instance Less a (N N204) => Less a (N N205)

data N206 = N206
instance Size (N N206) where size _ = 206
instance Less (N N205) (N N206)
instance Less a (N N205) => Less a (N N206)

data N207 = N207
instance Size (N N207) where size _ = 207
instance Less (N N206) (N N207)
instance Less a (N N206) => Less a (N N207)

data N208 = N208
instance Size (N N208) where size _ = 208
instance Less (N N207) (N N208)
instance Less a (N N207) => Less a (N N208)

data N209 = N209
instance Size (N N209) where size _ = 209
instance Less (N N208) (N N209)
instance Less a (N N208) => Less a (N N209)

data N210 = N210
instance Size (N N210) where size _ = 210
instance Less (N N209) (N N210)
instance Less a (N N209) => Less a (N N210)

data N211 = N211
instance Size (N N211) where size _ = 211
instance Less (N N210) (N N211)
instance Less a (N N210) => Less a (N N211)

data N212 = N212
instance Size (N N212) where size _ = 212
instance Less (N N211) (N N212)
instance Less a (N N211) => Less a (N N212)

data N213 = N213
instance Size (N N213) where size _ = 213
instance Less (N N212) (N N213)
instance Less a (N N212) => Less a (N N213)

data N214 = N214
instance Size (N N214) where size _ = 214
instance Less (N N213) (N N214)
instance Less a (N N213) => Less a (N N214)

data N215 = N215
instance Size (N N215) where size _ = 215
instance Less (N N214) (N N215)
instance Less a (N N214) => Less a (N N215)

data N216 = N216
instance Size (N N216) where size _ = 216
instance Less (N N215) (N N216)
instance Less a (N N215) => Less a (N N216)

data N217 = N217
instance Size (N N217) where size _ = 217
instance Less (N N216) (N N217)
instance Less a (N N216) => Less a (N N217)

data N218 = N218
instance Size (N N218) where size _ = 218
instance Less (N N217) (N N218)
instance Less a (N N217) => Less a (N N218)

data N219 = N219
instance Size (N N219) where size _ = 219
instance Less (N N218) (N N219)
instance Less a (N N218) => Less a (N N219)

data N220 = N220
instance Size (N N220) where size _ = 220
instance Less (N N219) (N N220)
instance Less a (N N219) => Less a (N N220)

data N221 = N221
instance Size (N N221) where size _ = 221
instance Less (N N220) (N N221)
instance Less a (N N220) => Less a (N N221)

data N222 = N222
instance Size (N N222) where size _ = 222
instance Less (N N221) (N N222)
instance Less a (N N221) => Less a (N N222)

data N223 = N223
instance Size (N N223) where size _ = 223
instance Less (N N222) (N N223)
instance Less a (N N222) => Less a (N N223)

data N224 = N224
instance Size (N N224) where size _ = 224
instance Less (N N223) (N N224)
instance Less a (N N223) => Less a (N N224)

data N225 = N225
instance Size (N N225) where size _ = 225
instance Less (N N224) (N N225)
instance Less a (N N224) => Less a (N N225)

data N226 = N226
instance Size (N N226) where size _ = 226
instance Less (N N225) (N N226)
instance Less a (N N225) => Less a (N N226)

data N227 = N227
instance Size (N N227) where size _ = 227
instance Less (N N226) (N N227)
instance Less a (N N226) => Less a (N N227)

data N228 = N228
instance Size (N N228) where size _ = 228
instance Less (N N227) (N N228)
instance Less a (N N227) => Less a (N N228)

data N229 = N229
instance Size (N N229) where size _ = 229
instance Less (N N228) (N N229)
instance Less a (N N228) => Less a (N N229)

data N230 = N230
instance Size (N N230) where size _ = 230
instance Less (N N229) (N N230)
instance Less a (N N229) => Less a (N N230)

data N231 = N231
instance Size (N N231) where size _ = 231
instance Less (N N230) (N N231)
instance Less a (N N230) => Less a (N N231)

data N232 = N232
instance Size (N N232) where size _ = 232
instance Less (N N231) (N N232)
instance Less a (N N231) => Less a (N N232)

data N233 = N233
instance Size (N N233) where size _ = 233
instance Less (N N232) (N N233)
instance Less a (N N232) => Less a (N N233)

data N234 = N234
instance Size (N N234) where size _ = 234
instance Less (N N233) (N N234)
instance Less a (N N233) => Less a (N N234)

data N235 = N235
instance Size (N N235) where size _ = 235
instance Less (N N234) (N N235)
instance Less a (N N234) => Less a (N N235)

data N236 = N236
instance Size (N N236) where size _ = 236
instance Less (N N235) (N N236)
instance Less a (N N235) => Less a (N N236)

data N237 = N237
instance Size (N N237) where size _ = 237
instance Less (N N236) (N N237)
instance Less a (N N236) => Less a (N N237)

data N238 = N238
instance Size (N N238) where size _ = 238
instance Less (N N237) (N N238)
instance Less a (N N237) => Less a (N N238)

data N239 = N239
instance Size (N N239) where size _ = 239
instance Less (N N238) (N N239)
instance Less a (N N238) => Less a (N N239)

data N240 = N240
instance Size (N N240) where size _ = 240
instance Less (N N239) (N N240)
instance Less a (N N239) => Less a (N N240)

data N241 = N241
instance Size (N N241) where size _ = 241
instance Less (N N240) (N N241)
instance Less a (N N240) => Less a (N N241)

data N242 = N242
instance Size (N N242) where size _ = 242
instance Less (N N241) (N N242)
instance Less a (N N241) => Less a (N N242)

data N243 = N243
instance Size (N N243) where size _ = 243
instance Less (N N242) (N N243)
instance Less a (N N242) => Less a (N N243)

data N244 = N244
instance Size (N N244) where size _ = 244
instance Less (N N243) (N N244)
instance Less a (N N243) => Less a (N N244)

data N245 = N245
instance Size (N N245) where size _ = 245
instance Less (N N244) (N N245)
instance Less a (N N244) => Less a (N N245)

data N246 = N246
instance Size (N N246) where size _ = 246
instance Less (N N245) (N N246)
instance Less a (N N245) => Less a (N N246)

data N247 = N247
instance Size (N N247) where size _ = 247
instance Less (N N246) (N N247)
instance Less a (N N246) => Less a (N N247)

data N248 = N248
instance Size (N N248) where size _ = 248
instance Less (N N247) (N N248)
instance Less a (N N247) => Less a (N N248)

data N249 = N249
instance Size (N N249) where size _ = 249
instance Less (N N248) (N N249)
instance Less a (N N248) => Less a (N N249)

data N250 = N250
instance Size (N N250) where size _ = 250
instance Less (N N249) (N N250)
instance Less a (N N249) => Less a (N N250)

data N251 = N251
instance Size (N N251) where size _ = 251
instance Less (N N250) (N N251)
instance Less a (N N250) => Less a (N N251)

data N252 = N252
instance Size (N N252) where size _ = 252
instance Less (N N251) (N N252)
instance Less a (N N251) => Less a (N N252)

data N253 = N253
instance Size (N N253) where size _ = 253
instance Less (N N252) (N N253)
instance Less a (N N252) => Less a (N N253)

data N254 = N254
instance Size (N N254) where size _ = 254
instance Less (N N253) (N N254)
instance Less a (N N253) => Less a (N N254)

data N255 = N255
instance Size (N N255) where size _ = 255
instance Less (N N254) (N N255)
instance Less a (N N254) => Less a (N N255)

newtype BoundedList a n = L [a]

instance (Show a, Size n) => Show (BoundedList a n) where
    show l@(L xs) = shows xs $ showChar ':' $ shows (listBound l) "" 

shrink :: Size m => BoundedList a n -> Maybe (BoundedList a m)
shrink =  toBounded . fromBounded

grow :: LessEq n m => BoundedList a n -> BoundedList a m
grow (L xs) = (L xs)

fromBounded :: BoundedList a n -> [a]
fromBounded (L xs) = xs

listLength :: BoundedList a n -> Int
listLength (L l) = length l

listBound :: Size n => BoundedList a n -> Int
listBound (_ :: BoundedList a n) = size (undefined :: n)

toBounded :: Size n => [a] -> Maybe (BoundedList a n)
toBounded a = toBound_ (L a)
    where
    toBound_ :: Size n => BoundedList a n -> Maybe (BoundedList a n)
    toBound_ l
	| listLength l <= listBound l = Just l
	| otherwise = Nothing

trunc :: Size n => [a] -> BoundedList a n
trunc xs = L xs
    where
    trunc_ :: Size n => BoundedList a n -> BoundedList a n
    trunc_ l@(L xs) = (L xs)