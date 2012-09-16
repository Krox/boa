#!/usr/bin/python

import sys
import glob
import subprocess
import os
import re

logfile = open("compile.log", "w")

def testFile(filename):
	args = filter((lambda s: s!=''), open(filename).readline()[2:].split())
	if args[0] == 'yes':
		expect = int(args[1])
	elif args[0] == 'no':
		expect = 255
	else:
		return "malformed testcase"


	logfile.flush()
	logfile.write("===== Running: "+filename+" =====\n");
	logfile.flush()
	got = subprocess.call(["./../bin/boa", "-I/home/krox/devel/boa/import", filename], stdout=logfile, stderr=subprocess.STDOUT)
	if got == expect:
		return ""

	return "got "+str(got)+" but expected "+str(expect)


failCount = 0
for filename in sorted(os.listdir("./")):
	if re.match("^test[0-9]+\.boa$", filename):
		print filename + ":",
		try:
			ret = testFile(filename)
			if ret == "":
				print "ok"
			else:
				failCount += 1
				print "FAIL:",ret
		except Exception, e:
			failCount += 1
			print "EXCEPTION:", e


print "\n=====", failCount, "failed", "=====\n"
if failCount > 0:
	sys.exit(1)




