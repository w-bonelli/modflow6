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

paktest = "sfr"
ex = ["ts_sfr02"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))

# run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None


def get_model(ws, name, timeseries=False):
    # static model data
    # temporal discretization
    nper = 1
    tdis_rc = []
    for idx in range(nper):
        tdis_rc.append((1.0, 1, 1.0))
    ts_times = np.arange(0.0, 2.0, 1.0, dtype=float)

    auxnames = ["temp", "conc"]
    temp, conc = 32.5, 0.1

    # spatial discretization data
    nlay, nrow, ncol = 3, 10, 10
    delr, delc = 100.0, 100.0
    top = 0.0
    botm = [-10, -20, -30]
    strt = 0.0

    # calculate hk
    hk = 1.0e-4

    # solver options
    nouter, ninner = 600, 100
    hclose, rclose, relax = 1e-6, 0.1, 1.0
    newtonoptions = "NEWTON"
    imsla = "BICGSTAB"

    # build MODFLOW 6 files
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        memory_print_option="all",
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )
    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=ninner,
        inner_dvclose=hclose,
        rcloserecord=rclose,
        linear_acceleration=imsla,
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        newtonoptions=newtonoptions,
        save_flows=True,
        print_flows=True,
    )

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

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=hk)

    # chd files
    # chd data
    spd = [
        [(0, 0, 0), 1.0],
        [(0, nrow - 1, ncol - 1), 0.0],
    ]
    chd = flopy.mf6.modflow.ModflowGwfchd(
        gwf, stress_period_data=spd, pname="chd-1"
    )

    # drn file
    drn6 = [
        [(0, 1, 2), -1.0, 1.0],
        [(0, 2, 3), -1.0, 1.0],
    ]
    drn = flopy.mf6.modflow.ModflowGwfdrn(
        gwf, mover=True, stress_period_data=drn6, pname="drn-1"
    )

    # sfr file
    packagedata = [
        [
            0,
            (1 - 1, 4 - 1, 1 - 1),
            3.628e001,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-4,
            1.0e-1,
            2,
            0.0,
            1,
            temp,
            conc,
        ],
        [
            6,
            (1 - 1, 4 - 1, 2 - 1),
            1.061e002,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-4,
            1.0e-1,
            3,
            1.0,
            1,
            temp,
            conc,
        ],
        [
            2,
            (1 - 1, 4 - 1, 3 - 1),
            6.333e001,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-4,
            1.0e-1,
            4,
            1.0,
            2,
            temp,
            conc,
        ],
        [
            3,
            (1 - 1, 5 - 1, 3 - 1),
            4.279e001,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-4,
            1.0e-1,
            3,
            1.0,
            1,
            temp,
            conc,
        ],
        [
            4,
            (1 - 1, 5 - 1, 4 - 1),
            6.532e001,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            1.0e-4,
            1.0e-1,
            1,
            1.0,
            0,
            temp,
            conc,
        ],
        [
            5,
            (1 - 1, 4 - 1, 1 - 1),
            10.0,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            0.0,
            1.0e-1,
            1,
            0.0,
            0,
            temp,
            conc,
        ],
        [
            1,
            (1 - 1, 4 - 1, 2 - 1),
            10.0,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            0.0,
            1.0e-1,
            1,
            0.0,
            0,
            temp,
            conc,
        ],
        [
            7,
            (1 - 1, 4 - 1, 3 - 1),
            10.0,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            0.0,
            1.0e-1,
            1,
            0.0,
            0,
            temp,
            conc,
        ],
        [
            8,
            (1 - 1, 4 - 1, 3 - 1),
            10.0,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            0.0,
            1.0e-1,
            1,
            0.0,
            0,
            temp,
            conc,
        ],
        [
            9,
            (1 - 1, 5 - 1, 4 - 1),
            10.0,
            1.0,
            1.0e-003,
            0.0,
            1.0,
            0.0,
            1.0e-1,
            1,
            0.0,
            0,
            temp,
            conc,
        ],
    ]
    connectiondata = [
        [0, -6, -5],
        [6, 0, -2, -1],
        [2, -3, -7, -8, 6],
        [3, -4, -9, 2],
        [4, 3],
        [5, 0],
        [1, 6],
        [7, 2],
        [8, 2],
        [9, 3],
    ]
    cprior = "upto"
    divdata = [
        [0, 0, 5, cprior],
        [6, 0, 1, cprior],
        [2, 1, 7, cprior],
        [2, 0, 8, cprior],
        [3, 0, 9, cprior],
    ]
    inflow, divflow, divflow2, upstream_fraction = 1.0, 0.05, 0.04, 0.0
    ts_names = ["inflow", "divflow", "ustrf"] + auxnames
    perioddata = [
        [0, "status", "active"],
        [6, "status", "active"],
        [2, "status", "active"],
        [3, "status", "active"],
        [4, "status", "active"],
        [0, "diversion", 0, divflow],
        [6, "diversion", 0, divflow],
        [2, "diversion", 0, divflow2],
        [3, "diversion", 0, divflow],
    ]
    if timeseries:
        perioddata.append([0, "inflow", "inflow"])
        perioddata.append([2, "diversion", 1, "divflow"])
        perioddata.append([0, "AUXILIARY", "conc", "conc"])
        perioddata.append([2, "AUXILIARY", "temp", "temp"])
        perioddata.append([5, "upstream_fraction", "ustrf"])
        perioddata.append([7, "upstream_fraction", "ustrf"])
        perioddata.append([9, "upstream_fraction", "ustrf"])
        ts_methods = ["linearend"] * len(ts_names)
        ts_data = []
        for t in ts_times:
            ts_data.append((t, inflow, divflow, upstream_fraction, temp, conc))
    else:
        perioddata.append([0, "inflow", inflow])
        perioddata.append([2, "diversion", 1, divflow])

    budpth = f"{name}.{paktest}.cbc"
    cnvgpth = f"{name}.sfr.cnvg.csv"
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        maximum_picard_iterations=5,
        auxiliary=auxnames,
        print_input=True,
        budget_filerecord=budpth,
        mover=True,
        nreaches=len(packagedata),
        maximum_depth_change=1.0e-5,
        package_convergence_filerecord=cnvgpth,
        packagedata=packagedata,
        connectiondata=connectiondata,
        diversions=divdata,
        perioddata=perioddata,
        pname="sfr-1",
    )
    if timeseries:
        fname = f"{name}.sfr.ts"
        sfr.ts.initialize(
            filename=fname,
            timeseries=ts_data,
            time_series_namerecord=ts_names,
            interpolation_methodrecord=ts_methods,
        )

    packagedata = [
        [0, 1.0, -20.0, 0.0, "SPECIFIED", 2],
    ]
    nmawwells = len(packagedata)
    connectiondata = [
        [1 - 1, 1 - 1, (1 - 1, 5 - 1, 8 - 1), 0.0, -20, 1.0, 1.1],
        [1 - 1, 2 - 1, (2 - 1, 5 - 1, 8 - 1), 0.0, -20, 1.0, 1.1],
    ]
    perioddata = [[0, "FLOWING_WELL", 0.0, 0.0, 0.0], [0, "RATE", 1.0e-3]]
    maw = flopy.mf6.ModflowGwfmaw(
        gwf,
        mover=True,
        nmawwells=nmawwells,
        packagedata=packagedata,
        connectiondata=connectiondata,
        perioddata=perioddata,
        pname="maw-1",
    )

    packagedata = [(0, 1.0, 11), (1, 0.5, 11)]
    outlets = [(0, 0, 1, "manning", 0.001, 0.0, 0.1, 0.001)]
    nlakes = len(packagedata)
    noutlets = len(outlets)
    connectiondata = [
        (0, 0, (0, 0, 5), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 1, (0, 1, 4), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 2, (1, 1, 5), "vertical", 1.0e-05, -5.0, 0.0, 1.0, 0.0),
        (0, 3, (0, 2, 4), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 4, (0, 3, 5), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 5, (0, 2, 6), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 6, (1, 2, 5), "vertical", 1.0e-05, -5.0, 0.0, 1.0, 0.0),
        (0, 7, (0, 0, 6), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 8, (0, 2, 6), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 9, (0, 1, 7), "horizontal", 1.0e-05, -5.0, 0.0, 100.0, 100.0),
        (0, 10, (1, 1, 6), "vertical", 1.0e-05, -5.0, 0.0, 1.0, 0.0),
        (1, 0, (0, 0, 8), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 1, (0, 1, 7), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 2, (0, 1, 9), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 3, (1, 1, 8), "vertical", 1.0e-05, -1.0, 0.0, 0.0, 0.0),
        (1, 4, (0, 2, 7), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 5, (0, 2, 9), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 6, (1, 2, 8), "vertical", 1.0e-05, -1.0, 0.0, 0.0, 0.0),
        (1, 7, (0, 3, 7), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 8, (0, 4, 8), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 9, (0, 3, 9), "horizontal", 1.0e-05, -1.0, 0.0, 100.0, 100.0),
        (1, 10, (1, 3, 8), "vertical", 1.0e-05, -1.0, 0.0, 0.0, 0.0),
    ]
    perioddata = [
        (1, "status", "active"),
        (1, "rainfall", "0.0"),
        (1, "evaporation", "0.000000000000e+000"),
        (1, "runoff", "0.000000000000e+000"),
        (1, "withdrawal", "0.000000000000e+000"),
        (0, "rate", "1.000000000000e+000"),
        (0, "invert", "1.000000000000e-003"),
        (0, "width", "0.000000000000e+000"),
        (0, "slope", "1.000000000000e-003"),
        (0, "rough", "1.000000000000e-001"),
    ]
    cnvgpth = f"{name}.lak.cnvg.csv"
    lak = flopy.mf6.ModflowGwflak(
        gwf,
        mover=True,
        nlakes=nlakes,
        noutlets=noutlets,
        print_stage=True,
        print_flows=True,
        package_convergence_filerecord=cnvgpth,
        packagedata=packagedata,
        connectiondata=connectiondata,
        outlets=outlets,
        perioddata=perioddata,
        pname="lak-1",
    )

    packagedata = [
        (0, (0, 5, 1), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (1, (0, 5, 2), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (2, (0, 5, 3), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (3, (0, 6, 1), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (4, (0, 6, 2), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (5, (0, 6, 3), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (6, (0, 7, 1), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (7, (0, 7, 2), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
        (8, (0, 7, 3), 1, -1, 1.0, 1.0e-05, 0.2, 0.4, 0.3, 3.5),
    ]
    perioddata = [
        [0, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [1, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [2, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [3, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [4, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [5, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [6, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [7, 1.0e-8, 0, 0, 0, 0, 0, 0],
        [8, 1.0e-8, 0, 0, 0, 0, 0, 0],
    ]
    cnvgpth = f"{name}.uzf.cnvg.csv"
    uzf = flopy.mf6.ModflowGwfuzf(
        gwf,
        mover=True,
        package_convergence_filerecord=cnvgpth,
        nuzfcells=len(packagedata),
        ntrailwaves=7,
        nwavesets=40,
        packagedata=packagedata,
        perioddata=perioddata,
        pname="uzf-1",
    )

    packages = [("drn-1",), ("lak-1",), ("maw-1",), ("sfr-1",), ("uzf-1",)]
    perioddata = [
        ("drn-1", 0, "lak-1", 1, "excess", 1.0),
        ("drn-1", 0, "maw-1", 0, "threshold", 2.0),
        ("drn-1", 0, "sfr-1", 2, "upto", 3.0),
        ("drn-1", 1, "lak-1", 1, "excess", 1.0),
        ("drn-1", 1, "maw-1", 0, "threshold", 2.0),
        ("drn-1", 1, "sfr-1", 2, "upto", 3.0),
        ("lak-1", 0, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 0, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 1, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 2, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 3, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 4, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 5, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 6, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 7, "sfr-1", 0, "factor", 1.0),
        ("uzf-1", 8, "sfr-1", 0, "factor", 1.0),
        ("sfr-1", 2, "sfr-1", 3, "factor", 0.5),
        ("sfr-1", 1, "sfr-1", 4, "factor", 0.5),
        ("sfr-1", 8, "sfr-1", 4, "factor", 0.5),
    ]
    mvr = flopy.mf6.ModflowGwfmvr(
        gwf,
        maxmvr=len(perioddata),
        budget_filerecord=f"{name}.mvr.bud",
        maxpackages=len(packages),
        print_flows=True,
        packages=packages,
        perioddata=perioddata,
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("BUDGET", "LAST")],
    )

    return sim


def build_model(idx, dir):
    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = get_model(ws, name)

    # build MODFLOW 6 files with timeseries
    ws = os.path.join(dir, "mf6")
    mc = get_model(ws, name, timeseries=True)

    return sim, mc


def eval_model(sim):
    print("evaluating model budgets...")
    from budget_file_compare import eval_bud_diff

    # get ia/ja from binary grid file
    fname = f"{os.path.basename(sim.name)}.dis.grb"
    fpth = os.path.join(sim.simpath, fname)
    grbobj = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grbobj._datadict["IA"] - 1

    fname = f"{os.path.basename(sim.name)}.cbc"

    # open first gwf cbc file
    fpth = os.path.join(sim.simpath, fname)
    cobj0 = flopy.utils.CellBudgetFile(fpth, precision="double")

    # open second gwf cbc file
    fpth = os.path.join(sim.simpath, "mf6", fname)
    cobj1 = flopy.utils.CellBudgetFile(fpth, precision="double")

    # define file path and evaluate difference
    fname = f"{os.path.basename(sim.name)}.cbc.cmp.out"
    fpth = os.path.join(sim.simpath, fname)
    eval_bud_diff(fpth, cobj0, cobj1, ia)

    # evaluate the sfr package budget file
    fname = f"{os.path.basename(sim.name)}.{paktest}.cbc"
    # open first sfr cbc file
    fpth = os.path.join(sim.simpath, fname)
    cobj0 = flopy.utils.CellBudgetFile(fpth, precision="double")

    # open second sfr cbc file
    fpth = os.path.join(sim.simpath, "mf6", fname)
    cobj1 = flopy.utils.CellBudgetFile(fpth, precision="double")

    # define file path and evaluate difference
    fname = f"{os.path.basename(sim.name)}.{paktest}.cbc.cmp.out"
    fpth = os.path.join(sim.simpath, fname)
    eval_bud_diff(fpth, cobj0, cobj1)

    # do some spot checks on the first sfr cbc file
    nodes = [2, 5, 6, 8, 9, 10]
    # FLOW-JA-FACE
    v0 = cobj0.get_data(totim=1.0, text="FLOW-JA-FACE")[0]
    q = []
    for idx, node in enumerate(v0["node"]):
        if node in nodes:
            q.append(v0["q"][idx])
    v0 = np.array(q)
    check = np.ones(v0.shape, dtype=float) * 5e-2
    check[1] = 0.76743
    check[-2] = 4e-2
    assert np.allclose(v0, check), "FLOW-JA-FACE failed"

    v0 = cobj0.get_data(totim=1.0, text="EXT-OUTFLOW")[0]
    q = []
    for idx, node in enumerate(v0["node"]):
        if node in nodes:
            q.append(v0["q"][idx])
    v0 = np.array(q)
    check = np.array([-2.5e-2, -0.80871, -5e-2, -5e-2, -2.0e-2, -5e-2])
    assert np.allclose(v0, check), "EXT-OUTFLOW failed"

    v0 = cobj0.get_data(totim=1.0, text="FROM-MVR")[0]
    q = []
    for idx, node in enumerate(v0["node"]):
        if node in nodes:
            q.append(v0["q"][idx])
    v0 = np.array(q)
    check = np.array([0.0, 4.5e-2, 0.0, 0.0, 0.0, 0.0])
    assert np.allclose(v0, check), "FROM-MVR failed"

    v0 = cobj0.get_data(totim=1.0, text="TO-MVR")[0]
    q = []
    for idx, node in enumerate(v0["node"]):
        if node in nodes:
            q.append(v0["q"][idx])
    v0 = np.array(q)
    check = np.array([-2.5e-2, 0.0, 0.0, 0.0, -2.0e-2, 0.0])
    assert np.allclose(v0, check), "TO-MVR failed"

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

    # build the models
    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_model, idxsim=idx)
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
