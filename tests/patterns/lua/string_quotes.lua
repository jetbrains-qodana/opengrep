-- ERROR: match
os.execute("rm")
-- ERROR: match
os.execute('rm')
-- ERROR: match
os.execute([[rm]])
-- ERROR: match
os.execute([==[rm]==])
os.execute("ls")
