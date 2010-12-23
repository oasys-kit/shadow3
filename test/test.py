from Shadow import *

src=Source()
src.read("start.00")

oe1=OE()
oe1.read("start.01")

oe2=OE()
oe2.read("start.02")

oe3=OE()
oe3.read("start.03")

oe4=OE()
oe4.read("start.04")

ray=Ray()
ray.genSource(src)
ray.write("begin.dat")


ray.trace(oe1)
ray.write("star.01")

ray.trace(oe2)
ray.write("star.02")

ray.trace(oe3)
ray.write("star.03")

ray.trace(oe4)
ray.write("starPy.04")


