
spawn,'./gen_source start.00'
spawn,'./trace3 -t'
tmp=readsh('begin.dat')
tmp2=readsh('star.01')
ray=0


a0=READ_GFILE('start.00')
;ray = gensourcesynch(a0)
ray = gensourcegeom(a0)

print,'Source'
print,ray[0:12,0:3]
print,''
print,tmp.ray[0:12,0:3]


a1=READ_GFILE('start.01')
ray2=TRACEOE(a1,ray,1L)


print,'OE'
print,ray2[0:12,0:3]
print,''
print,tmp2.ray[0:12,0:3]

END
