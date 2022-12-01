import os
import sys

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
    "tvk05",
]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"

time_varying_k = [1.0, 10.0]


def build_model(idx, dir):
    nlay, nrow, ncol = 3, 3, 3
    perlen = [100.0, 100.0]
    nper = len(perlen)
    nstp = nper * [1]
    tsmult = nper * [1.0]
    delr = 1.0
    delc = 1.0
    delz = 1.0
    top = 1.0
    laytyp = 0
    botm = [0.0, -1.0, -2.0]
    strt = 1.0
    hk = 0.1

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

    # create gwf model
    gwfname = "gwf_" + name
    gwf = flopy.mf6.MFModel(
        sim,
        model_type="gwf6",
        modelname=gwfname,
        model_nam_file=f"{gwfname}.nam",
    )
    gwf.name_file.save_flows = True

    # create iterative model solution and register the gwf model with it
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        filename=f"{gwfname}.ims",
    )
    sim.register_ims_package(imsgwf, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=np.ones((nlay, nrow, ncol), dtype=int),
        filename=f"{gwfname}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt, filename=f"{gwfname}.ic")

    # node property flow
    tvk_filename = f"{gwfname}.npf.tvk"
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_specific_discharge=True,
        icelltype=laytyp,
        k=hk,
        k22=hk,
    )

    # tvk
    # k33 not originally specified in NPF, but specifying an
    # alternate value in the 2nd stress period to test MF6
    tvkspd = {}
    kper = 1
    hydraulic_conductivity = time_varying_k[kper]
    spd = []
    for k in range(nlay):
        for i in range(nrow):
            for j in range(ncol):
                spd.append([(k, i, j), "K33", hydraulic_conductivity])
    tvkspd[kper] = spd

    tvk = flopy.mf6.ModflowUtltvk(
        npf, print_input=True, perioddata=tvkspd, filename=tvk_filename
    )

    # chd files
    chdspd = []
    for i in range(nrow):
        chdspd.append([(0, i, 0), top + 1])

    chd = flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=chdspd,
        save_flows=False,
        print_flows=True,
        pname="CHD-1",
    )

    # ghb files
    ghbspd = []
    ghbcond = time_varying_k[1] * delz * delc / (0.5 * delr)
    for i in range(nrow):
        ghbspd.append([(nlay - 1, i, ncol - 1), top, ghbcond])

    ghb = flopy.mf6.ModflowGwfghb(
        gwf,
        stress_period_data=ghbspd,
        save_flows=False,
        print_flows=True,
        pname="GHB-1",
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    return sim, None


def eval_model(sim):
    print("evaluating model...")

    name = ex[sim.idxsim]

    # budget
    try:
        fname = f"gwf_{name}.lst"
        ws = sim.simpath
        fname = os.path.join(ws, fname)
        lst = flopy.utils.Mf6ListBudget(
            fname, budgetkey="VOLUME BUDGET FOR ENTIRE MODEL"
        )
        lstra = lst.get_incremental()
    except:
        assert False, f'could not load data from "{fname}"'

    # This is the answer to this problem.
    sp_x = []
    for kper, bud in enumerate(lstra):
        sp_x.append(bud)

    # comment when done testing
    print(f"Total outflow in stress period 1 is {str(sp_x[0][8])}")
    print(
        f"Total outflow in stress period 2 after increasing K33 is {str(sp_x[1][8])}"
    )
    errmsg = f"Expect higher flow rate in period 2 compared to period 1, but found equal or higher flow rate in period 1"
    assert 2.0 * sp_x[0][8] < sp_x[1][8], errmsg

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # initialize testing framework
    test = testing_framework()

    # build the model
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    test.run_mf6(Simulation(dir, exfunc=eval_model, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_model, idxsim=idx)
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()