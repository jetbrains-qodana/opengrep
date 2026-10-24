-- ruleid: lua-mig-d18-explicit-self
function d18.m(self, x) self.y = x end
-- ok: lua-mig-d18-explicit-self
function d18.n(this, x) end
