local a = "double \"quoted\" \\ \n \t \65 \x41 \u{48}"
local b = 'single \'quoted\''
local c = "line1\
line2"
local d = "skip\z
           blanks"
local e = [[long
string]]
local f = [==[ ]] and ]=] inside ]==]
local g = ""
print[[call with a long string]]
print"call with a string"
