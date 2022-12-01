# Same as test_gwf_lak01 except it uses ATS.  Test works by trying a
# large time step that does not converge.  ATS must then retry using
# a smaller time step.

import os
import sys

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

ex = ["gwf_ats_lak_01a"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# store global gwf for subsequent plotting
gwf = None


def get_idomain(nlay, nrow, ncol, lakend):
    idomain = np.ones((nlay, nrow, ncol), dtype=int)
    for k, j in enumerate(lakend):
        idomain[k, 0, 0:j] = 0
    return idomain


def build_model(idx, dir):
    lx = 300.0
    lz = 45.0
    nlay = 45
    nrow = 1
    ncol = 30
    nper = 1
    delc = 1.0
    delr = lx / ncol
    delz = lz / nlay
    top = 5.0
    botm = [top - (k + 1) * delz for k in range(nlay)]

    perlen = [200.0]
    nstp = [10]
    tsmult = [1.0]

    Kh = 1.0
    Kv = 1.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    nouter, ninner = 250, 300
    hclose, rclose, relax = 1e-8, 1e-6, 0.97

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )

    # set dt0, dtmin, dtmax, dtadj, dtfailadj
    dt0 = 200.0
    dtmin = 1.001e-5
    dtmax = 10.0
    dtadj = 2.0
    dtfailadj = 5.0

    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim,
        time_units="DAYS",
        nper=nper,
        perioddata=tdis_rc,
    )

    if True:
        ats_filerecord = name + ".ats"
        atsperiod = [
            (0, dt0, dtmin, dtmax, dtadj, dtfailadj),
            (7, dt0, dtmin, dtmax, dtadj, dtfailadj),
        ]
        tdis.ats.initialize(
            maxats=len(atsperiod),
            perioddata=atsperiod,
            filename=ats_filerecord,
        )

    # create gwf model
    gwfname = name
    global gwf
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, newtonoptions="NEWTON")

    imsgwf = flopy.mf6.ModflowIms(
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
        filename=f"{gwfname}.ims",
    )

    # number of columns to be a lake for layer 1, 2, , ... len(lakend)
    lakend = [10, 9, 8, 7, 6]
    idomain = get_idomain(nlay, nrow, ncol, lakend)
    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )

    # initial conditions
    strt = np.zeros((nlay, nrow, ncol), dtype=float)
    strt += top
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=1,
        k=Kh,
        k33=Kv,
    )

    sy = 0.3
    ss = np.zeros((nlay, nrow, ncol), dtype=float)
    # ss[0, :, :] = sy
    idx = np.where(idomain == 0)
    for k, i, j in zip(*idx):
        ss[k + 1, i, j] = 0.0  # sy
    sto = flopy.mf6.ModflowGwfsto(gwf, sy=sy, ss=ss, iconvert=1)

    irch = np.zeros((nrow, ncol), dtype=int)
    lake_vconnect = []
    idx = np.where(idomain == 0)
    for k, i, j in zip(*idx):
        if idomain[k + 1, i, j] == 1:
            lake_vconnect.append((k + 1, i, j))
            irch[i, j] = k + 1
    nlakeconn = len(lake_vconnect)

    # pak_data = [lakeno, strt, nlakeconn]
    initial_stage = 0.1
    pak_data = [(0, initial_stage, nlakeconn)]

    bedleak = 100.0  # "None"
    belev = 0.0
    con_data = [
        (0, i, idx, "VERTICAL", bedleak, belev, -99, -99, -99)
        for i, idx in enumerate(lake_vconnect)
    ]

    # period data
    p_data = [
        (0, "STATUS", "ACTIVE"),
    ]

    # note: for specifying lake number, use fortran indexing!
    fname = f"{gwfname}.lak.obs.csv"
    lak_obs = {
        fname: [
            ("lakestage", "stage", 1),
            ("lakevolume", "volume", 1),
            ("lak1", "lak", 1, 1),
        ],
        "digits": 10,
    }

    lak = flopy.mf6.modflow.ModflowGwflak(
        gwf,
        surfdep=0.0,
        save_flows=True,
        print_input=True,
        print_flows=True,
        print_stage=True,
        stage_filerecord=f"{gwfname}.lak.bin",
        budget_filerecord=f"{gwfname}.lak.bud",
        nlakes=len(pak_data),
        ntables=0,
        packagedata=pak_data,
        pname="LAK-1",
        connectiondata=con_data,
        perioddata=p_data,
        observations=lak_obs,
    )

    chdspd = [((0, 0, ncol - 1), 5.0)]
    chd = flopy.mf6.modflow.ModflowGwfchd(gwf, stress_period_data=chdspd)

    rech = 0.0001 * np.ones((nrow, ncol), dtype=float)
    # rech[:, 0:20] = 0.
    rch = flopy.mf6.modflow.ModflowGwfrcha(
        gwf, print_flows=True, save_flows=True, recharge=rech, irch=irch
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    return sim, None


def make_plot_xsect(sim, headall, stageall):
    print("making plots...")

    name = ex[sim.idxsim]
    ws = exdirs[sim.idxsim]

    import matplotlib.patches as patches
    import matplotlib.pyplot as plt
    from matplotlib.collections import PatchCollection

    # plot first and last
    itimes = [0, -1]
    nplots = len(itimes)
    fig = plt.figure(figsize=(8, 4))

    for ifig, itime in enumerate(itimes):
        print(f"processing {ifig + 1} of {nplots}")
        ax = fig.add_subplot(nplots, 1, ifig + 1, aspect="equal")
        stage = stageall[itime].flatten()
        xmin = 0
        xmax = 99.0
        ymin = 0.0
        ymax = stage
        rect = patches.Rectangle(
            (xmin, ymin),
            xmax - xmin,
            ymax - ymin,
            linewidth=1,
            edgecolor="r",
            facecolor="k",
        )
        coll = PatchCollection([rect], zorder=1)
        ax.add_collection(coll)

        xs = flopy.plot.PlotCrossSection(gwf, line={"row": 0}, ax=ax)
        head = headall[itime]
        xs.plot_array(head, head=head, cmap="jet", masked_values=[1e30])
        # ax.set_xlim(0, 100)
        # ax.set_ylim(-10, 5)

    fname = "fig-xsect.pdf"
    fname = os.path.join(ws, fname)
    plt.savefig(fname, bbox_inches="tight")

    return


def make_plot(sim, times, headall, stageall):
    print("making plots...")

    name = ex[sim.idxsim]
    ws = exdirs[sim.idxsim]

    import matplotlib.pyplot as plt

    fig = plt.figure(figsize=(6, 4))
    ax = fig.add_subplot(1, 1, 1)
    ax.plot(times, stageall.flatten(), "ro-", label="stage")
    h = np.ma.masked_where(headall == 1e30, headall)
    h = [hstep.flatten().max() for hstep in h]
    ax.plot(times, h, "bo-", label="max head")

    fname = "fig-timeseries.pdf"
    fname = os.path.join(ws, fname)
    plt.savefig(fname, bbox_inches="tight")

    return


def get_kij_from_node(node, nrow, ncol):
    "return zero based k, i, j from zero based node number"
    nrc = nrow * ncol
    k = int(node / nrc)
    ij = node - k * nrc
    i = int(ij / ncol)
    j = ij - i * ncol
    return k, i, j


def eval_results(sim):
    print("evaluating results...")

    # calculate volume of water and make sure it is conserved
    name = ex[sim.idxsim]
    gwfname = name
    fname = gwfname + ".lak.bin"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.HeadFile(fname, text="STAGE")
    times = bobj.get_times()
    stage = bobj.get_alldata()

    fname = gwfname + ".cbc"
    fname = os.path.join(sim.simpath, fname)
    bobj = flopy.utils.CellBudgetFile(fname, precision="double", verbose=False)
    times = bobj.get_times()
    idomain = gwf.dis.idomain.array
    botm = gwf.dis.botm.array

    all_passed = True
    for itime, t in enumerate(times):

        print(f"processing totim {t}")
        stage_current = stage[itime].flatten()
        print(f"lake stage = {stage_current}")

        qlakleak = np.zeros(idomain.shape, dtype=float).flatten()
        ilak = np.zeros(idomain.shape, dtype=int).flatten()
        records = bobj.get_data(text="lak", totim=t)[0]
        # print(records)
        for i, r in enumerate(records):
            node, node2, q = r
            qlakleak[node - 1] = q
            ilak[node - 1] = 1

        records = bobj.get_data(text="rch", totim=t)[0]
        # print(records)
        for i, r in enumerate(records):
            node, node2, q = r
            n0 = node - 1
            if ilak[n0] == 1:
                kk, ii, jj = get_kij_from_node(
                    n0, botm.shape[1], botm.shape[2]
                )
                tp = botm[kk - 1, ii, jj]
                if stage_current > tp and q != 0.0:
                    all_passed = False
                    msg = (
                        "recharge must be zero if overlying lake is "
                        "active. node {} qlak {} qrch {} time {}".format(
                            n0, qlakleak[n0], q, t
                        )
                    )
                    print(msg)
    assert all_passed, "found recharge applied to cell beneath active lake"

    fname = gwfname + ".hds"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    hobj = flopy.utils.HeadFile(fname)
    head = hobj.get_alldata()

    stage_answer = [
        0.18656752,
        0.26698475,
        0.41029603,
        0.5401282,
        0.65826109,
        0.76623049,
        0.86559848,
        0.9575622,
        1.03621155,
        1.10294338,
        1.16476142,
        1.22239145,
        1.27639471,
        1.32723404,
        1.37526871,
        1.42080773,
        1.4641106,
        1.50539963,
        1.54487098,
        1.58268724,
        1.61899406,
        1.65392046,
        1.68758034,
        1.72097227,
        1.75402292,
        1.78639623,
        1.81810513,
        1.84917469,
        1.87963534,
        1.9095193,
        1.93885876,
        1.96768493,
        1.99602767,
        2.02050604,
        2.04378479,
        2.06646125,
        2.08860587,
        2.11027176,
        2.13150123,
        2.15232931,
        2.17278583,
        2.19289665,
        2.21268448,
        2.23216952,
        2.25136988,
        2.27030183,
        2.28898012,
        2.30741817,
        2.325628,
        2.34362223,
        2.36140993,
    ]
    errmsg = "lake stage does not match known answer"
    assert np.allclose(stage_answer, stage.flatten()), errmsg

    if False:
        make_plot(sim, times, head, stage)
        make_plot_xsect(sim, head, stage)

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
