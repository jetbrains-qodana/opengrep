local integers = { 0, 7, 007, 1234, 0xff, 0XFF, 0b101, 0B11 }
local floats = { 1.5, .5, 3., 1e3, 1E-3, 2.5e+10, 0x1p4, 0x.8, 0xA.8p1 }
-- LuaJIT
local jit = { 10LL, 10ULL, 0x7fffffffffffffffULL, 3i, 2.5i }
