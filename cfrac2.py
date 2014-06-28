#!/usr/bin/env python
import math
import fractions
import sys

#Turn a float into a continued fraction
#This is potentially an infinite list!
def CFrac(n,bigTerm=1000,lenLimit=100):
	if n > 1.0:
		a = int(n)
		b = n - a
		yield a
		lenLimit -= 1
		if lenLimit == 0:
			return
		n = b
	while n > 0.0:
		n = 1.0/n
		a = int(n)
		if a > bigTerm:
			return
		b = n - a
		yield a
		n = b
		lenLimit -= 1
		if lenLimit == 0:
			return

def CFracApprox(val,cf):
	answer = []
	rcf = list(reversed(cf))
	while True:
		num = 0
		den = 1
		for t in rcf:
			a = num
			b = den
			c = t.numerator
			d = t.denominator
			(num,den) = (a*d+c*b, b*d)
			(num,den) = (den,num)
		if den <= 1024 and num <= 1024:
			f = fractions.Fraction(den,num)
			#error in cents 
			err = - int ( (math.log(float(f),2) - math.log(val,2)) * 1200 )
			err2 = err*err
			#show anything within 30 cents
			if err2 < 30*30:
				answer.append( "%s(%d)" % (str(f),err) )
		if len(rcf) < 2:
			break
		rcf = rcf[1:]
	return list(reversed(answer))

if len(sys.argv) == 1:
	print "usage: cfrac2.py [numberOfFretsPerOctave]"
	et = 12
else:
	et = int(sys.argv[1])

for fret in range(0,et+1):
	pitch = math.pow(2, (1.0*fret)/et)
	cf = list(CFrac(pitch))
	approximations = [str(t) for t in CFracApprox(pitch,cf)]
	print "fret:%d %s" % (fret, approximations)
