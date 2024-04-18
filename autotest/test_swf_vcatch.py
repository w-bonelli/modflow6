"""

This problem tests the SWF capability to simulate overland flow 
on a regular grid.  The problem is based on the tilted
v-catchment problem described by Panday and Huyakorn (2004):
Advances in Water Resources 27 (2004) 361-382.

"""

import os

import flopy
import numpy as np
import pytest

from autotest.framework import TestFramework

cases = [
    "swf-swr-vcatch",
]


def build_models(idx, test):
    Lx = 800.0 + 800.0 + 20.0
    Ly = 1000.0
    dx = 20.0
    nrow = int(Ly / dx)
    ncol = int(Lx / dx)
    nlay = 1

    slope_x = 0.05
    slope_y = 0.02

    x = np.linspace(-Lx / 2 + dx / 2, Lx / 2 - dx / 2, ncol)
    y = np.linspace(Ly - dx / 2, dx / 2, nrow)
    X, Y = np.meshgrid(x, y)
    land_surface = np.abs(X) * slope_x + Y * slope_y

    rough_overland = 0.015
    rough_channel = 0.15

    rainfall = 3.0e-6  # meters per second
    time_rainfall = 90  # minutes
    dt0 = 5  # seconds
    dtmin = 5
    dtmax = 100.0  # seconds
    dtadj = 2.0
    dtfailadj = 5.0

    nper = 2
    perlen = nper * [
        time_rainfall * 60.0
    ]  # convert time_rainfall from minutes to seconds
    nstp = nper * [1]
    tsmult = nper * [1]

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    name = "swf"

    # build MODFLOW 6 files
    ws = test.workspace
    sim = flopy.mf6.MFSimulation(
        sim_name=f"{name}_sim",
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )

    # create tdis package
    ats_filerecord = None
    tdis = flopy.mf6.ModflowTdis(
        sim,
        ats_filerecord=ats_filerecord,
        time_units="SECONDS",
        nper=nper,
        perioddata=tdis_rc,
    )

    # setup ats
    ats_filerecord = name + ".ats"
    atsperiod = [
        (0, dt0, dtmin, dtmax, dtadj, dtfailadj),
        (1, dt0, dtmin, dtmax, dtadj, dtfailadj),
    ]
    tdis.ats.initialize(
        maxats=len(atsperiod),
        perioddata=atsperiod,
        filename=ats_filerecord,
    )

    # surface water model
    swfname = f"{name}_model"
    swf = flopy.mf6.ModflowSwf(sim, modelname=swfname, save_flows=True)

    nouter, ninner = 15, 100
    hclose, rclose, relax = 1e-8, 1e-8, 1.0
    imsswf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        under_relaxation_theta=0.9,
        under_relaxation_kappa=0.0001,
        under_relaxation_gamma=0.0,
        inner_maximum=ninner,
        inner_dvclose=hclose,
        linear_acceleration="BICGSTAB",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
        # backtracking_number=5,
        # backtracking_tolerance=1.0,
        # backtracking_reduction_factor=0.3,
        # backtracking_residual_limit=100.0,
        filename=f"{swfname}.ims",
    )
    sim.register_ims_package(imsswf, [swf.name])

    botm = land_surface.reshape((nrow, ncol))
    dis = flopy.mf6.ModflowSwfdis2D(
        swf,
        nrow=nrow,
        ncol=ncol,
        delr=dx,
        delc=dx,
        botm=botm,
        xorigin=-810,
    )

    rough = rough_overland * np.ones((nlay, nrow, ncol), dtype=float)
    rough[0, :, int(ncol / 2)] = rough_channel

    dfw = flopy.mf6.ModflowSwfdfw(
        swf,
        print_flows=False,
        save_flows=True,
        manningsn=rough,
        idcxs=None,
    )

    sto = flopy.mf6.ModflowSwfsto(
        swf,
        save_flows=True,
        steady_state={0: False, 1: False},
        transient={0: True, 1: True},
    )

    ic = flopy.mf6.ModflowSwfic(
        swf,
        strt=botm,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{swfname}.bud",
        stage_filerecord=f"{swfname}.stage",
        saverecord=[
            ("STAGE", "ALL"),
            ("BUDGET", "ALL"),
        ],
        printrecord=[
            ("BUDGET", "ALL"),
        ],
    )

    # flw
    qinflow = rainfall * dx * dx
    spd = [(i, j, qinflow) for j in range(ncol) for i in range(nrow)]
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=len(spd),
        print_input=True,
        print_flows=True,
        stress_period_data={0: spd, 1: []},
    )

    # note: for specifying zero-based reach number, put reach number in tuple
    fname = f"{swfname}.zdg.obs.csv"
    zdg_obs = {
        fname: [
            ("OUTFLOW", "ZDG", (nrow - 1, int(ncol / 2))),
        ],
        "digits": 10,
    }

    idcxs = -1  # use cross section 0
    width = dx
    spd = [
        ((nrow - 1, int(ncol / 2)), idcxs, width, slope_y, rough_channel)
    ]
    zdg = flopy.mf6.ModflowSwfzdg(
        swf,
        observations=zdg_obs,
        print_input=True,
        maxbound=len(spd),
        stress_period_data=spd,
    )

    return sim, None


def make_plot(test, mfsim):
    print("making plots...")
    import matplotlib.pyplot as plt

    fpth = test.workspace / f"swf_model.zdg.obs.csv"
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    fig = plt.figure(figsize=(6, 4))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(
        obsvals["time"] / 60.0,
        -obsvals["OUTFLOW"],
        marker="o",
        mfc="none",
        mec="k",
        lw=0.0,
        label=f"MODFLOW 6 Simulated Outflow",
    )
    ax.plot([90, 90], [0, 5], "k--")
    ax.set_xlim(0, 180.0)
    ax.set_ylim(0, 5)
    plt.xlabel("time, in minutes")
    plt.ylabel("flow, in meters per second")
    plt.legend()
    fname = test.workspace / "swf_model.zdg.obs.png"
    plt.savefig(fname)

    return


def check_output(idx, test):
    print(f"evaluating model for case {idx}...")

    swfname = "swf_model"
    ws = test.workspace
    mfsim = flopy.mf6.MFSimulation.load(sim_ws=ws)

    makeplot = False
    if makeplot:
        make_plot(test, mfsim)

    # read the binary grid file
    fpth = test.workspace / f"{swfname}.dis2d.grb"
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.ncells + 1, "ia in grb file is not correct size"

    # read binary stage file
    fpth = test.workspace / f"{swfname}.stage"
    sobj = flopy.utils.HeadFile(fpth, precision="double", text="STAGE")
    stage_all = sobj.get_alldata()

    # read outflow observation
    fpth = test.workspace / f"swf_model.zdg.obs.csv"
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    outflow_answer = 4.859497719  # outflow value at end of first period
    idx = np.where(obsvals["time"] == 5400.0)
    outflow_sim = -obsvals["OUTFLOW"][idx]
    msg = (f"Simulated outflow at end of period should be {outflow_answer} "
           f"but found {outflow_sim}")
    assert np.allclose(outflow_answer, outflow_sim, atol=0.001), msg


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
    )
    test.run()
