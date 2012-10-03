"""

ShadowPreprocessorsXraylib: Shadow preprocessors using python+xraylib

        functions: 
             prerefl():    preprocessor for mirrors
             pre_mlayer(): preprocessor for multilayers
             bragg():      preprocessor for crystals

"""

__author__ = "Manuel Sanchez del Rio"
__contact__ = "srio@esrf.eu"
__copyright = "ESRF, 2012"


import math
import xraylib 
# these ones needed in bragg
import cmath
import numpy
# this is for physical constants, may be eliminated (see comments)
import scipy.constants.codata


def prerefl():
    """
     Preprocessor for mirrors - python+xraylib version

     -""" 
  
    # retrieve physical constants needed
    codata = scipy.constants.codata.physical_constants
    codata_c, tmp1, tmp2 = codata["speed of light in vacuum"]
    codata_h, tmp1, tmp2 = codata["Planck constant"]
    codata_ec, tmp1, tmp2 = codata["elementary charge"]
    # or hard code them 
    # In [174]: print("codata_c = %20.11e \n" % codata_c )
    # codata_c =    2.99792458000e+08 
    # In [175]: print("codata_h = %20.11e \n" % codata_h )
    # codata_h =    6.62606930000e-34 
    # In [176]: print("codata_ec = %20.11e \n" % codata_ec )
    # codata_ec =    1.60217653000e-19

    tocm = codata_h*codata_c/codata_ec*1e2

    # input section
    print("prerefl: Preprocessor for mirrors - python+xraylib version")
    iMaterial = raw_input("Enter material expression (symbol,formula): ")
    density = raw_input("Density [ g/cm3 ] ?") 
    density = float(density)

    estart = raw_input("Enter starting photon energy: ") 
    estart = float(estart)

    efinal = raw_input("Enter end photon energy: ") 
    efinal = float(efinal)

    estep = raw_input("Enter step photon energy:") 
    estep = float(estep)

    out_file  = raw_input("Output file : ")

    twopi = math.pi*2
    npoint = int( (efinal-estart)/estep + 1 )
    depth0 = density/2.0
    qmin = estart/tocm*twopi
    qmax = efinal/tocm*twopi
    qstep = estep/tocm*twopi

    f = open(out_file, 'wb')
    f.write( ("%20.11e "*4+"\n") % tuple([qmin,qmax,qstep,depth0]) )
    f.write("%i \n" % int(npoint))
    for i in range(npoint):
       energy = (estart+estep*i)*1e-3
       tmp = 2e0*(1e0-xraylib.Refractive_Index_Re(iMaterial,energy,density))
       f.write("%e \n" % tmp)
    for i in range(npoint):
       energy = (estart+estep*i)*1e-3
       tmp2 = 2e0*(xraylib.Refractive_Index_Im(iMaterial,energy,density))
       f.write("%e \n" % tmp2)
    print("File written to disk: %s" % out_file)
    f.close()

    # test (not needed)
    itest = 0
    if itest:
       cdtest = xraylib.CompoundParser(iMaterial)
       print "    ",iMaterial," contains %i atoms and %i elements"% (cdtest['nAtomsAll'], cdtest['nElements'])
       for i in range(cdtest['nElements']):
          print "    Element %i: %lf %%" % (cdtest['Elements'][i],cdtest['massFractions'][i]*100.0)
       print ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
       print qmin,qmax,qstep,depth0
       print npoint
       for i in range(npoint):
          energy = (estart+estep*i)*1e-3
          qq = qmin+qstep*i
          print energy,qq, \
              2e0*(1e0-xraylib.Refractive_Index_Re(iMaterial,energy,density)),\
              2e0*(xraylib.Refractive_Index_Im(iMaterial,energy,density))
       print ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"

    return None


def pre_mlayer():
    """
     SHADOW preprocessor for multilayers - python+xraylib version

     -""" 
    # input section
    print("pre_mlayer: SHADOW preprocessor for multilayers - python+xraylib version")
    fileout = raw_input("Name of output file : ")
    estart = raw_input("Photon energy (eV) from : ")
    estart = float(estart)
    efinal = raw_input("                     to : ")
    efinal = float(efinal)

    elfactor = math.log10(1.0e4/30.0)/300.0
    istart = int(math.log10(estart/30.0e0)/elfactor + 1)
    ifinal = int(math.log10(efinal/30.0e0)/elfactor + 2)
    np = int(ifinal - istart) + 1

    f = open(fileout, 'wb')
    f.write("%i \n" % np)
    for i in range(np):
        energy = 30e0*math.pow(10,elfactor*(istart+i-1))
        f.write("%e " % energy)
    f.write( "\n")

    print("  ")
    print("The stack is as follows: ")
    print("      ")
    print("                 vacuum   ")
    print("      |------------------------------|  \   ")
    print("      |          odd (n)             |  |   ")
    print("      |------------------------------|  | BILAYER # n   ")
    print("      |          even (n)            |  |   ")
    print("      |------------------------------|  /   ")
    print("      |          .                   |   ")
    print("      |          .                   |   ")
    print("      |          .                   |   ")
    print("      |------------------------------|  \   ")
    print("      |          odd (1)             |  |   ")
    print("      |------------------------------|  | BILAYER # 1   ")
    print("      |          even (1)            |  |   ")
    print("      |------------------------------|  /   ")
    print("      |                              |   ")
    print("      |///////// substrate //////////|   ")
    print("      |                              |   ")
    print("      ")
    print(" ")

    # substrate
    matSubstrate = raw_input("Specify the substrate material : ")
    denSubstrate = raw_input("Specify the substrate density [g/cm^3] : ")
    denSubstrate = float(denSubstrate)
    for i in range(np):
        energy = 30e0*math.pow(10,elfactor*(istart+i-1)) *1e-3 # in keV!!
        delta = 1e0-xraylib.Refractive_Index_Re(matSubstrate,energy,denSubstrate)
        beta = xraylib.Refractive_Index_Im(matSubstrate,energy,denSubstrate)
        f.write( ("%26.17e "*2+"\n") % tuple([delta,beta]) )
    
    print("Right above the substrate is the even layer material")
    matEven = raw_input("Specify the even layer material : ")
    denEven = raw_input("Specify the even layer density [g/cm^3] : ")
    denEven = float(denEven)
    for i in range(np):
        energy = 30e0*math.pow(10,elfactor*(istart+i-1)) *1e-3 # in keV!!
        delta = 1e0-xraylib.Refractive_Index_Re(matEven,energy,denEven)
        beta = xraylib.Refractive_Index_Im(matEven,energy,denEven)
        f.write( ("%26.17e  "*2+"\n") % tuple([delta,beta]) )

    print("Odd layer material is on top of the even layer.")
    matOdd = raw_input("Specify the odd layer material : ")
    denOdd = raw_input("Specify the odd layer density [g/cm^3] : ")
    denOdd = float(denOdd)
    for i in range(np):
        energy = 30e0*math.pow(10,elfactor*(istart+i-1)) *1e-3 # in keV!!
        delta = 1e0-xraylib.Refractive_Index_Re(matOdd,energy,denOdd)
        beta = xraylib.Refractive_Index_Im(matOdd,energy,denOdd)
        f.write( ("%26.17e "*2+"\n") % tuple([delta,beta]) )


    npair = raw_input("No. of layer pairs : ")
    npair = int(npair)
    #! srio@esrf.eu 2012-06-07 Nevot-Croce ML roughness model implemented.
    #! By convention, starting from the version that includes ML roughness
    #! we set NPAR negative, in order to assure compatibility with old
    #! versions. If NPAR<0, roughness data are read, if NPAR>0 no roughness.
    f.write("%i \n" % -npair)

    print(" ")
    print("Starting from the substrate surface, specify the thickness t :")
    print("      t = t(odd) + t(even)        in Angstroms,")
    print("and the gamma ratio :")
    print("      t(even) / (t(odd) + t(even))")
    print("for EACH bilayer.")
    print(" ")
    print("Type two -1 whenever you want the remaining layers ")
    print("to assume the thickness, gamma ratio and roughnesses of the previous one.")
    print(" ")
 
    #define variables
    thick=[0e0]*npair
    gamma1=[0e0]*npair
    mlroughness1=[0e0]*npair
    mlroughness2=[0e0]*npair

    for i in range(npair): 
        tmps = ("thickness [A], gamma ratio, roughness even [A] and roughness odd [A] of bilayer %i: \n"% (i+1) )
        tmp = raw_input(tmps)
        tmp1 = tmp.split()
        if ((i != 0) and (int(float(tmp1[0])) == -1)):
            thick[i:(npair-1)] = [thick[i-1]] * (npair-i)
            gamma1[i:(npair-1)] = [gamma1[i-1]] * (npair-i)
            mlroughness1[i:(npair-1)] = [mlroughness1[i-1]] * (npair-i)
            mlroughness2[i:(npair-1)] = [mlroughness2[i-1]] * (npair-i)
            break
        else:
            thick[i] = float(tmp1[0])
            gamma1[i] = float(tmp1[1])
            mlroughness1[i] = float(tmp1[2])
            mlroughness2[i] = float(tmp1[3])

    for i in range(npair):
        f.write( ("%26.17e "*4+"\n") % tuple([thick[i],gamma1[i],mlroughness1[i],mlroughness2[i]]) )

    print("***************************************************")
    print("  Is the multilayer graded over the surface? ")
    print("      0: No ")
    print("      1: t and/or gamma graded over the surface ")
    print("         (input spline files with t and gamma gradient")
    print("      2: t graded over the surface ")
    print("         (input quadratic fit to t gradient)")
    print("      ")

    igrade = raw_input("Is t and/or gamma graded over the surface [0=No/1=Yes] ? ")
    igrade = int(igrade)
    f.write("%i \n" % igrade)
    if igrade == 1:
        print("Generation of the spline coefficients for the t and gamma factors")
        print("over the surface.")
        print("Then GRADE_MLAYER should be run to generate the spline ")
        print("coefficients for the t and gamma factors over the surface.")
        print("Here just type in the file name that WILL be used to store")
        print("the spline coefficients :")
        fgrade = raw_input("File name (output from grade_mlayer: ")
        f.write("%s \n" % fgrade)
    elif igrade == 2:  # igrade=2, coefficients
        print("A second degree polynomial fit of the thickness grading")
        print("must be available:")
        print("t(y) = BILATER_THICHNESS(y)/BILAYER_THICKNESS(y=0)")
        print("t(y) = a0 + a1*y + a2*(y^2)  ")
        print("a0 (constant term) ")
        print("a1 (slope term) ")
        print("a2 (quadratic term) ")
        tmp = raw_input("Enter a0, a1, a2: ")
        f.write("%s \n" % tmp)

    f.close()
    print("File written to disk: %s" % fileout)
    return None

def bragg():
    """
     SHADOW preprocessor for crystals - python+xraylib version

     -""" 
    # retrieve physical constants needed
    codata = scipy.constants.codata.physical_constants
    codata_e2_mc2, tmp1, tmp2 = codata["classical electron radius"]
    # or, hard-code them
    # In [179]: print("codata_e2_mc2 = %20.11e \n" % codata_e2_mc2 )
    # codata_e2_mc2 =    2.81794032500e-15

    print("bragg: SHADOW preprocessor for crystals - python+xraylib version")
    fileout = raw_input("Name of output file : ")
    f = open(fileout, 'wb')

    print(" bragg (python) only works now for ZincBlende Cubic structures. ")
    print(" Valid descriptor are: ")
    print("     Si (alternatiovely Si_NIST, Si2) ")
    print("     Ge")
    print("     Diamond")
    print("     GaAs, GaSb, GaP")
    print("     InAs, InP, InSb")
    print("     SiC")

    descriptor = raw_input("Name of crystal descriptor : ")
    cryst = xraylib.Crystal_GetCrystal(descriptor)

    #test crystal data - not needed
    itest = 1
    if itest: 
        if (cryst == None):
            sys.exit(1)
        print "  Unit cell dimensions are %f %f %f" % (cryst['a'],cryst['b'],cryst['c'])
        print "  Unit cell angles are %f %f %f" % (cryst['alpha'],cryst['beta'],cryst['gamma'])
        volume = cryst['volume']
        print "  Unit cell volume is %f" % volume
        volume = volume*1e-8*1e-8*1e-8 # in cm^3
        print "  Atoms at:"
        print "     Z  fraction    X        Y        Z"
        for i in range(cryst['n_atom']):
            atom =  cryst['atom'][i]
            print "    %3i %f %f %f %f" % (atom['Zatom'], atom['fraction'], atom['x'], atom['y'], atom['z'])
        print "  "

    print("Miller indices of crystal plane of reflection.")
    miller = raw_input("H K L: ")
    miller = miller.split()
    hh = int(miller[0])
    kk = int(miller[1])
    ll = int(miller[2])

    #flag ZincBlende
    f.write( "%i " % 0) 
    #1/V*electronRadius
    f.write( "%e " % ((1e0/volume)*(codata_e2_mc2*1e2)) ) 
    #dspacing
    dspacing = xraylib.Crystal_dSpacing(cryst, hh, kk, ll)
    f.write( "%e " % (dspacing*1e-8) ) 
    f.write( "\n")
    #Z's
    atom =  cryst['atom']
    f.write( "%i " % atom[0]["Zatom"] )
    f.write( "%i " % atom[7]["Zatom"] )
    temper = raw_input("Temperature (Debye-Waller) factor (set 1 for default): ")
    temper = float(temper)
    f.write( "%e " % temper ) # temperature parameter
    f.write( "\n")

    ga = (1e0+0j) + cmath.exp(1j*cmath.pi*(hh+kk))  \
                             + cmath.exp(1j*cmath.pi*(hh+ll))  \
                             + cmath.exp(1j*cmath.pi*(kk+ll))
    gb = ga * cmath.exp(1j*cmath.pi*0.5*(hh+kk+ll))
    ga_bar = ga.conjugate()
    gb_bar = gb.conjugate()


    f.write( "(%20.11e,%20.11e ) \n" % (ga.real, ga.imag) ) 
    f.write( "(%20.11e,%20.11e ) \n" % (ga_bar.real, ga_bar.imag) ) 
    f.write( "(%20.11e,%20.11e ) \n" % (gb.real, gb.imag) ) 
    f.write( "(%20.11e,%20.11e ) \n" % (gb_bar.real, gb_bar.imag) ) 

    zetas = numpy.array([atom[0]["Zatom"],atom[7]["Zatom"]])
    for zeta in zetas:
        xx01 = 1e0/2e0/dspacing
        xx00 = xx01-0.1
        xx02 = xx01+0.1
        yy00= xraylib.FF_Rayl(int(zeta),xx00)
        yy01= xraylib.FF_Rayl(int(zeta),xx01)
        yy02= xraylib.FF_Rayl(int(zeta),xx02)
        xx = numpy.array([xx00,xx01,xx02])
        yy = numpy.array([yy00,yy01,yy02])
        fit = numpy.polyfit(xx,yy,2)
        #print "zeta: ",zeta
        #print "z,xx,YY: ",zeta,xx,yy
        #print "fit: ",fit[::-1] # reversed coeffs
        #print "fit-tuple: ",(tuple(fit[::-1].tolist())) # reversed coeffs
        #print("fit-tuple: %e %e %e  \n" % (tuple(fit[::-1].tolist())) ) # reversed coeffs
        f.write("%e %e %e  \n" % (tuple(fit[::-1].tolist())) ) # reversed coeffs


    emin = raw_input("minimum photon energy (eV): ")
    emin = float(emin)
    emax = raw_input("maximum photon energy (eV): ")
    emax = float(emax)
    estep = raw_input("energy step (eV): ")
    estep = float(estep)

    npoint  = int( (emax - emin)/estep + 1 )
    f.write( ("%i \n") % npoint)
    for i in range(npoint): 
        energy = (emin+estep*i)
        f1a = xraylib.Fi(int(zetas[0]),energy*1e-3)
        f2a = xraylib.Fii(int(zetas[0]),energy*1e-3)
        f1b = xraylib.Fi(int(zetas[1]),energy*1e-3)
        f2b = xraylib.Fii(int(zetas[1]),energy*1e-3)
        out = numpy.array([energy,f1a,abs(f2a),f1b,abs(f2b)])
        f.write( ("%20.11e %20.11e %20.11e \n %20.11e %20.11e \n") % ( tuple(out.tolist()) ) )

    f.close()
    print("File written to disk: %s" % fileout)
    return None

if __name__ == '__main__':


    import sys
    preprocessor_option = -1
    if len(sys.argv) >= 2: 
        preprocessor_option = int(sys.argv[1])
    else:
        print "preprocessor_option:    "
        print "     0 prerefl"
        print "     1 pre_mlayer"
        print "     2 bragg"
        tmp = raw_input("?>")
        preprocessor_option = int(tmp)

    if preprocessor_option == 0:
        prerefl()
    elif preprocessor_option == 1:
        pre_mlayer()
    elif preprocessor_option == 2:
        bragg()

