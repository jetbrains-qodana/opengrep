-- ruleid: lua-mig-d32-named-field
d32_t({ name = "x" })
-- ruleid: lua-mig-d32-named-field
d32_t{ name = "y" }
-- ok: lua-mig-d32-named-field
d32_t({ other = "x" })
