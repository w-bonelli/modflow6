# test to evaluate Newton-Raphson solution for a single column steady-state
# dry multi-aquifer well problem. Developed to address issue #546

import os

import numpy as np
import pytest

try:
    import flopy
except:
    msg = "Error. FloPy package is not available.\n"
    msg += "Try installing using the following command:\n"
    msg += " pip install flopy"
    raise Exception(msg)

from framework import testing_framework
from simulation import Simulation

ex = ("maw_08a", "maw_08b")
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
dis_option = ("dis", "disv")

nlay = 3
nrow = 1
ncol = 1

vertices = [
    (0, 0, 0),
    (1, 0, 1),
    (2, 1, 1),
    (3, 1, 0),
]
cell2d = [
    (0, 0.5, 0.5, 5, 0, 1, 2, 3, 0),
]

delc = 1.0
delr = 1.0
gwfarea = delr * delc

top = 30.0
botm = [20, 10, 0]
maw_bot = 15.0

strt = 4.5
maw_strt = 16.0

Kh = 10
Kv = 1.0
radius = 0.05


def build_model(idx, dir):
    dvclose, rclose, relax = 1e-9, 1e-9, 1.0

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        memory_print_option="summary",
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units="DAYS")

    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="complex",
        outer_dvclose=dvclose,
        inner_dvclose=dvclose,
        rcloserecord=f"{rclose} strict",
        relaxation_factor=relax,
    )

    # create gwf model
    gwfname = "gwf_" + name

    newtonoptions = "NEWTON UNDER_RELAXATION"
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname=gwfname, newtonoptions=newtonoptions, print_input=True
    )

    if dis_option[idx] == "dis":
        dis = flopy.mf6.ModflowGwfdis(
            gwf,
            nlay=nlay,
            nrow=nrow,
            ncol=ncol,
            delr=delr,
            delc=delc,
            top=top,
            botm=botm,
        )
        cellids = [
            (0, 0, 0),
            (1, 0, 0),
        ]
    else:
        dis = flopy.mf6.ModflowGwfdisv(
            gwf,
            nlay=nlay,
            ncpl=nrow,
            nvert=4,
            vertices=vertices,
            cell2d=cell2d,
            top=top,
            botm=botm,
        )
        cellids = [
            (0, 0),
            (1, 0),
        ]

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=1,
        k=Kh,
        k33=Kv,
    )

    # <wellno> <radius> <bottom> <strt> <condeqn> <ngwfnodes>
    mawpackagedata = flopy.mf6.ModflowGwfmaw.packagedata.empty(gwf, maxbound=1)
    mawpackagedata["radius"] = radius
    mawpackagedata["bottom"] = maw_bot
    mawpackagedata["strt"] = maw_strt
    mawpackagedata["condeqn"] = "thiem"
    mawpackagedata["ngwfnodes"] = 2

    # <wellno> <icon> <cellid(ncelldim)> <scrn_top> <scrn_bot> <hk_skin> <radius_skin>
    mawconnectiondata = flopy.mf6.ModflowGwfmaw.connectiondata.empty(
        gwf, maxbound=2
    )
    mawconnectiondata["icon"] = [0, 1]
    mawconnectiondata["cellid"] = cellids
    mawconnectiondata["scrn_top"] = 100.0
    mawconnectiondata["scrn_bot"] = 0.0
    mawconnectiondata["hk_skin"] = -999
    mawconnectiondata["radius_skin"] = -999

    mbin = f"{gwfname}.maw.bin"
    mbud = f"{gwfname}.maw.bud"
    maw = flopy.mf6.ModflowGwfmaw(
        gwf,
        print_input=True,
        print_head=True,
        print_flows=True,
        save_flows=True,
        head_filerecord=mbin,
        budget_filerecord=mbud,
        packagedata=mawpackagedata,
        connectiondata=mawconnectiondata,
    )
    opth = f"{gwfname}.maw.obs"
    obsdata = {
        f"{gwfname}.maw.obs.csv": [
            ("whead", "head", (0,)),
        ]
    }
    maw.obs.initialize(
        filename=opth, digits=20, print_input=True, continuous=obsdata
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[
            (
                "HEAD",
                "ALL",
            ),
            (
                "BUDGET",
                "ALL",
            ),
        ],
        printrecord=[
            (
                "HEAD",
                "ALL",
            ),
            (
                "BUDGET",
                "ALL",
            ),
        ],
    )

    return sim, None


def eval_results(sim):
    print("evaluating results...")

    # calculate volume of water and make sure it is conserved
    name = ex[sim.idxsim]
    gwfname = "gwf_" + name
    fname = gwfname + ".maw.bin"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.HeadFile(fname, text="HEAD")

    well_head = bobj.get_data().flatten()
    assert np.allclose(
        well_head, 10.0
    ), f"simulated maw head ({well_head[0]}) does not equal 10."

    fname = gwfname + ".hds"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    hobj = flopy.utils.HeadFile(fname)
    head = hobj.get_alldata()[1:]

    # compare the maw-gwf flows with the gwf-maw flows
    fname = gwfname + ".maw.bud"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    mbud = flopy.utils.CellBudgetFile(fname, precision="double")
    maw_gwf = mbud.get_data(text="GWF")

    fname = gwfname + ".cbc"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    gbud = flopy.utils.CellBudgetFile(fname, precision="double")
    gwf_maw = gbud.get_data(text="MAW")

    assert len(maw_gwf) == len(gwf_maw), "number of budget records not equal"

    for istp, (ra_maw, ra_gwf) in enumerate(zip(maw_gwf, gwf_maw)):
        for i in range(ra_maw.shape[0]):
            qmaw = ra_maw[i]["q"]
            qgwf = ra_gwf[i]["q"]
            msg = f"step {istp} record {i} comparing qmaw with qgwf: {qmaw} {qgwf}"
            print(msg)
            assert np.allclose(qmaw, -qgwf), msg

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
    test.run_mf6(Simulation(dir, exfunc=eval_results, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_results, idxsim=idx)
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
