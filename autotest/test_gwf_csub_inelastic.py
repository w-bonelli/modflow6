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

from framework import running_on_CI, testing_framework
from simulation import Simulation

paktest = "csub"
budtol = 1e-2

ex = ["csub_de01a"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"

updatemat = [None, True]
# run all examples on Travis
continuous_integration = [True for idx in range(len(exdirs))]

# set replace_exe to None to use default executable
replace_exe = None

# static model data
# spatial discretization
nlay, nrow, ncol = 2, 1, 3
shape3d = (nlay, nrow, ncol)
size3d = nlay * nrow * ncol
delr, delc = 1.0, 1.0
top = 100.0
botm = [2.0, 0.0]

# temporal discretization
nper = 1
tdis_rc = [(200.0, 5000, 1.0)]

strt6 = 99.4

hk = 1e6
laytyp = [1]
ss = 0.0
sy = 0.0

nouter, ninner = 1000, 300
hclose, rclose, relax = 1e-9, 1e-4, 0.97

ib = 1

chd_ts = [
    (0.0, 99.4),
    (55.9140, 80.0),
    (90.3227, 30.0),
    (145.1615, 98.0),
    (200.0, 20.0),
    (1000.0, 20.0),
]
c6 = [[(0, 0, j), "CHD"] for j in range(0, ncol, 2)]
cd6 = {0: c6}

# sub data
cc = 0.20
cr = 0.01
void = 0.5
theta = void / (1.0 + void)
kv = -999.0
sgm = 1
sgs = 1.0
ini_stress = 20.0
thick = [1.0]

sub6 = [
    [
        0,
        (1, 0, 1),
        "nodelay",
        ini_stress,
        thick[0],
        1.0,
        cc,
        cr,
        theta,
        kv,
        ini_stress,
    ]
]


def build_mf6(idx, ws, update=None):
    name = ex[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)

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
        linear_acceleration="CG",
        scaling_method="NONE",
        reordering_method="NONE",
        relaxation_factor=relax,
    )
    sim.register_ims_package(ims, [gwf.name])

    dis = flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        filename=f"{name}.dis",
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt6, filename=f"{name}.ic")

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=False, icelltype=laytyp, k=hk, k33=hk
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=0.0,
        sy=sy,
        transient={0: True},
    )

    # chd files
    chd = flopy.mf6.modflow.mfgwfchd.ModflowGwfchd(
        gwf, maxbound=len(c6), stress_period_data=cd6, save_flows=False
    )
    # initialize time series
    chnam = f"{name}.ch.ts"
    chd.ts.initialize(
        filename=chnam,
        timeseries=chd_ts,
        time_series_namerecord=["CHD"],
        interpolation_methodrecord=["linear"],
    )

    # csub files
    opth = f"{name}.csub.obs"
    csub = flopy.mf6.ModflowGwfcsub(
        gwf,
        print_input=True,
        update_material_properties=update,
        save_flows=True,
        ninterbeds=1,
        beta=0.0,
        cg_ske_cr=0.0,
        sgm=sgm,
        sgs=sgs,
        packagedata=sub6,
    )
    orecarray = {}
    orecarray["csub_obs.csv"] = [
        ("es", "estress-cell", (1, 0, 1)),
        ("theta", "theta", (0,)),
        ("comp", "interbed-compaction", (0,)),
        ("pcs", "preconstress-cell", (1, 0, 1)),
        ("sk", "sk", (0,)),
        ("ske", "ske", (0,)),
    ]
    csub_obs_package = csub.obs.initialize(
        filename=opth, digits=10, print_input=True, continuous=orecarray
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "LAST")],
        printrecord=[("BUDGET", "ALL")],
    )
    return sim


def build_model(idx, dir):
    name = ex[idx]
    ws = dir

    # build MODFLOW 6 files
    sim = build_mf6(idx, ws)

    ws = os.path.join(dir, "mf6")
    mc = build_mf6(idx, ws, update=True)

    return sim, mc


def calc_comp2void(comp):
    b0 = thick[0]
    e0 = void
    return e0 - comp * (1.0 + e0) / b0


def calc_void(theta):
    return theta / (1.0 - theta)


def eval_void(sim):
    print("evaluating void ratio...")

    fpth = os.path.join(sim.simpath, "csub_obs.csv")
    cd = np.genfromtxt(fpth, delimiter=",", names=True)

    fpth = os.path.join(sim.simpath, "mf6", "csub_obs.csv")
    cd2 = np.genfromtxt(fpth, delimiter=",", names=True)

    v = calc_comp2void(cd["COMP"])
    v2 = calc_void(cd2["THETA"])

    # calculate maximum absolute error
    diff = v - v2
    diffmax = np.abs(diff).max()
    dtol = 0.002
    msg = f"maximum absolute void ratio difference ({diffmax}) "

    # write summary
    fpth = os.path.join(
        sim.simpath, f"{os.path.basename(sim.name)}.comp.cmp.out"
    )
    f = open(fpth, "w")
    line = f"{'TOTIM':>15s}"
    line += f" {'VOID':>15s}"
    line += f" {'MF':>15s}"
    line += f" {'DIFF':>15s}"
    f.write(line + "\n")
    for i in range(diff.shape[0]):
        line = f"{cd['time'][i]:15g}"
        line += f" {v[i]:15g}"
        line += f" {v[i]:15g}"
        line += f" {diff[i]:15g}"
        f.write(line + "\n")
    f.close()

    if diffmax > dtol:
        sim.success = False
        msg += f"exceeds {dtol}"
        assert diffmax < dtol, msg
    else:
        sim.success = True
        print("    " + msg)

    return


# - No need to change any code below
@pytest.mark.parametrize(
    "idx, dir",
    list(enumerate(exdirs)),
)
def test_mf6model(idx, dir):
    # determine if running on CI infrastructure
    is_CI = running_on_CI()
    r_exe = None
    if not is_CI:
        if replace_exe is not None:
            r_exe = replace_exe

    # initialize testing framework
    test = testing_framework()

    # build the models
    test.build_mf6_models(build_model, idx, dir)

    # run the test model
    if is_CI and not continuous_integration[idx]:
        return

    test.run_mf6(Simulation(dir, exfunc=eval_void, idxsim=idx))


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_void, idxsim=idx)
        test.run_mf6(sim)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
