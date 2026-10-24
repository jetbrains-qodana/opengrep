-- ruleid: lua-mig-d31-callback
d31_on("x", function() d31_h() end)
-- ok: lua-mig-d31-callback
d31_on("x", d31_named)
