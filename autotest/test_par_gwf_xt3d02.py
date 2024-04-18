"""
This tests reuses the simulation data in test_gwf_ifmod_xt3d02.py
and runs it in parallel on three cpus with

cpu 0: 'refmodel'
cpu 1: 'leftmodel'
cpu 2: 'rightmodel'

so we can compare the parallel coupling of 'leftmodel' + 'rightmodel'
with a serial 'refmodel' in case of XT3D
"""

import pytest

from autotest.framework import TestFramework

cases = ["par_xt3d02"]


def build_models(idx, test):
    from autotest.test_gwf_ifmod_xt3d02 import build_models as build

    sim, dummy = build(idx, test)
    return sim, dummy


def check_output(idx, test):
    from autotest.test_gwf_ifmod_xt3d02 import check_output as check

    check(idx, test)


@pytest.mark.parallel
@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        compare=None,
        parallel=True,
        ncpus=3,
    )
    test.run()
