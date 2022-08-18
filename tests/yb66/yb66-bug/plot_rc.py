import Shadow

beam = Shadow.Beam()
root = ''
beam.load(root+"star.01")

x = beam.getshcol(6)
col23 = beam.getshcol(23)
col24 = beam.getshcol(24)
col25 = beam.getshcol(25)

print(x.shape,col24.shape)


from srxraylib.plot.gol import plot
plot(x,col23,
    x,col24,
    x, col25,
    linestyle=['','',''],marker=['.','.','.'], legend=['total %g' % col23.sum(),'sigma %g' % col24.sum(),'pi %g' % col25.sum()],
     title="intensities")
# Shadow.ShadowTools.plotxy(beam,6,23)