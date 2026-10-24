function d18_n(...args)
  -- ruleid: lua-mig-d18-named-vararg
  return d18_use(args)
end
function d18_m(a, ...rest)
  -- ruleid: lua-mig-d18-named-vararg
  d18_use(args)
end
