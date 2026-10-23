-- ruleid: lua-mig-d07-string-value
d07_eq("hello")
-- ruleid: lua-mig-d07-string-value
d07_eq('hello')
-- ruleid: lua-mig-d07-string-value
d07_eq([[hello]])
-- ok: lua-mig-d07-string-value
d07_eq("hello2")
-- ok: lua-mig-d07-string-value
d07_eq("")
