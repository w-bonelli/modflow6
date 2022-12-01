"""
MODFLOW 6 Autotest
Test zero-order decay by running a one-cell model with ten 1-day time steps
with a decay rate of 1.  And a starting concentration of 8.  Result should be
0 at the end and decay should shot off once concentration is zero.

"""
import os

import numpy as np
import pytest

try:
    import pymake
except:
    msg = "Error. Pymake package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install https://github.com/modflowpy/pymake/zipball/master"
    raise Exception(msg)

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = [
    "mst06_noadv",
]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"


def build_model(idx, dir):
    nlay, nrow, ncol = 1, 1, 1
    nper = 1
    perlen = [10.0]
    nstp = [10]
    tsmult = [1.0]
    delr = 7.0
    delc = 6.0
    top = 2.0
    botm = 0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
        model_nam_file=f"{gwtname}.nam",
    )
    gwt.name_file.save_flows = True

    # create iterative model solution and register the gwt model with it
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename=f"{gwtname}.ims",
    )
    sim.register_ims_package(imsgwt, [gwt.name])

    dis = flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=1,
        filename=f"{gwtname}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwtic(gwt, strt=8.0, filename=f"{gwtname}.ic")

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(
        gwt, porosity=0.1, zero_order_decay=True, decay=1.0
    )

    srcs = {0: [[(0, 0, 0), 0.00]]}
    src = flopy.mf6.ModflowGwtsrc(
        gwt,
        maxbound=len(srcs),
        stress_period_data=srcs,
        save_flows=False,
        pname="SRC-1",
    )

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.bud",
        concentration_filerecord=f"{gwtname}.ucn",
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )
    print(gwt.modelgrid.zcellcenters)
    return sim, None


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name

    fpth = os.path.join(sim.simpath, f"{gwtname}.ucn")
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        conc = cobj.get_ts((0, 0, 0))
    except:
        assert False, f'could not load data from "{fpth}"'

    # The answer
    # print(conc[:, 1])
    cres = np.array([7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0])
    msg = (
        "simulated concentrations do not match with known "
        "solution. {} {}".format(conc[:, 1], cres)
    )
    assert np.allclose(cres, conc[:, 1]), msg

    # Check budget file
    fpth = os.path.join(sim.simpath, f"{gwtname}.bud")
    try:
        bobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    except:
        assert False, f'could not load data from "{fpth}"'
    decay_list = bobj.get_data(text="DECAY-AQUEOUS")
    decay_rate = [dr[0] for dr in decay_list]
    decay_rate_answer = [
        -8.4,
        -8.4,
        -8.4,
        -8.4,
        -8.4,
        -8.4,
        -8.4,
        -8.4,
        0.0,
        0.0,
    ]
    msg = (
        "simulated decay rates do not match with known "
        "solution. {} {}".format(decay_rate, decay_rate_answer)
    )
    assert np.allclose(decay_rate, decay_rate_answer), msg

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir, exfunc=eval_transport, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_transport, idxsim=idx)
        test.run_mf6(sim)

    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
