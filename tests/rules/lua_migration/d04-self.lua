function d04_C:init(v)
  -- ruleid: lua-mig-d04-self
  self.value = v
end
function d04_plain(other)
  -- ok: lua-mig-d04-self
  other.value = 1
end
