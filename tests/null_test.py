# -*- coding: utf-8 -*-
"""Basic pytest to see if Shadow loads

:copyright: Copyright (c) 2015 RadiaSoft LLC.  All Rights Reserved.
:license: http://www.apache.org/licenses/LICENSE-2.0.html
"""
from __future__ import absolute_import, division, print_function

import pytest

def test_1(capsys):
    from sys import stderr
    import Shadow
    beam = Shadow.Beam()
    src = Shadow.Source()
    assert 2 == src.FDISTR, \
        'Basic value for Source'
    # Would like to capture stdout, but too complex with extensions.
    # Probably extension needs to call a python output method.
    beam.genSource(src)
    assert 5000 == beam.nrays(), \
        'Basic method call for Beam'
    oe = Shadow.OE()
    oe.D_SPACING = 3
    assert 3 == oe.D_SPACING, \
        'Basic assignment for OE'
    beam.traceOE(oe, 1)

