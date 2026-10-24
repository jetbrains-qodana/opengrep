-- ruleid: lua-mig-d16-method-is-self-param
function d16:m(x) end
-- ruleid: lua-mig-d16-method-is-self-param
function d16.n(self, x) end
-- ok: lua-mig-d16-method-is-self-param
function d16.o(x) end
