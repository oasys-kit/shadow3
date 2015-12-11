;
; this is a simple test of the shadow3 idl interface main procedures. 
;
; it is not recommended to use these procedures for programming. For
; an example of the recommended object-oriented programming style see
; the shadowVui macros in shadow3_test.ws  (available from
; ShadowVUI->Load Example Workspace->From Example Dir.
;

;spawn,'./gen_source start.00'
;spawn,'./trace3 -t'
tmp=readsh('../begin.dat')
tmp2=readsh('../star.01')
ray=0


a0=READ_GFILE('../start.00')
IF ((a0.FDISTR EQ 4) OR (a0.FSOURCE_DEPTH EQ 4) OR (a0.F_WIGGLER GT 0)) THEN BEGIN
  ray = gensourcesync(a0)
ENDIF ELSE BEGIN
  ray = gensourcegeom(a0)
ENDELSE



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
