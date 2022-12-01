"""
Test adaptive time step module with a one-d vertical column in which cells
dry and then rewet based on a ghb in the bottom cell.

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

ex = ["gwf_ats02a"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
nlay, nrow, ncol = 5, 1, 1
botm = [80.0, 60.0, 40.0, 20.0, 0.0]

# set dt0, dtmin, dtmax, dtadj, dtfailadj
dt0 = 1.0
dtmin = 1.0e-5
dtmax = 2.0
dtadj = 2.0
dtfailadj = 5.0


def build_model(idx, dir):
    perlen = [10, 10]
    nper = len(perlen)
    nstp = [5, 5]
    tsmult = nper * [1.0]
    delr = 1.0
    delc = 1.0
    top = 100.0
    strt = 90.0
    hk = 2.0
    laytyp = 1
    ss = 0.0
    sy = 0.1

    tdis_rc = []
    for id in range(nper):
        tdis_rc.append((perlen[id], nstp[id], tsmult[id]))

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )

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
            (i, dt0, dtmin, dtmax, dtadj, dtfailadj) for i in range(nper)
        ]
        tdis.ats.initialize(
            maxats=len(atsperiod),
            perioddata=atsperiod,
            filename=ats_filerecord,
        )

    # create gwf model
    gwfname = name
    #        newtonoptions = "NEWTON UNDER_RELAXATION"
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        #            newtonoptions=newtonoptions,
    )

    # create iterative model solution and register the gwf model with it
    nouter, ninner = 20, 5
    hclose, rclose, relax = 1e-6, 1e-6, 0.97
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="DBD",
        under_relaxation_theta=0.7,
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
    )

    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)

    # node property flow
    rewet_record = [("WETFCT", 1.0, "IWETIT", 1, "IHDWET", 1)]
    wetdry = -0.001  # [0.001, 0.001, 0.001]
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        save_flows=False,
        cvoptions="variablecv dewatered",
        icelltype=laytyp,
        k=hk,
        rewet_record=rewet_record,
        wetdry=wetdry,
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp,
        ss=ss,
        sy=sy,
        steady_state={0: False},
        transient={0: True},
    )

    # ghb files
    cond = hk * 1.0 / 20.0
    ghbspdict = {
        0: [[(nlay - 1, 0, 0), 30.0, cond]],
        1: [[(nlay - 1, 0, 0), 100.0, cond]],
    }
    ghb = flopy.mf6.ModflowGwfghb(
        gwf,
        print_input=True,
        print_flows=True,
        stress_period_data=ghbspdict,
        save_flows=False,
    )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    obs_lst = []
    obs_lst.append(["obs1", "head", (0, 0, 0)])
    obs_lst.append(["obs2", "head", (4, 0, 0)])
    obs_dict = {f"{gwfname}.obs.csv": obs_lst}
    obs = flopy.mf6.ModflowUtlobs(
        gwf, pname="head_obs", digits=20, continuous=obs_dict
    )

    return sim, None


def make_plot(sim):
    print("making plots...")
    name = ex[sim.idxsim]
    ws = exdirs[sim.idxsim]
    gwfname = name

    fname = gwfname + ".hds"
    fname = os.path.join(ws, fname)
    hobj = flopy.utils.HeadFile(fname, precision="double")
    head = hobj.get_alldata()[:, :, 0, 0]
    times = hobj.times
    hobj.file.close()

    import matplotlib.pyplot as plt

    print(head.shape)
    plt.figure(figsize=(8, 5))
    times = np.array(times)
    for ilay in range(5):
        h = head[:, ilay]
        h = np.ma.masked_where(h < 0, h)
        (botline,) = plt.plot(
            [times.min(), times.max()], [botm[ilay], botm[ilay]]
        )
        plt.plot(
            times,
            h,
            marker="o",
            ls="-",
            color=botline.get_color(),
            label=f"Layer {ilay + 1}",
        )
    plt.legend()
    plt.show()


def eval_flow(sim):
    print("evaluating flow...")

    name = ex[sim.idxsim]
    gwfname = name

    if False:
        make_plot(sim)

    # This will fail if budget numbers cannot be read
    fpth = os.path.join(sim.simpath, f"{gwfname}.lst")
    mflist = flopy.utils.Mf6ListBudget(fpth)
    names = mflist.get_record_names()
    inc = mflist.get_incremental()
    msg = f"budget times not monotically increasing {inc['totim']}."
    assert np.all(np.diff(inc["totim"]) > dtmin), msg
    v = inc["totim"][-1]
    assert v == 20.0, f"Last time should be 20.  Found {v}"

    # ensure obs results changing monotonically
    fpth = os.path.join(sim.simpath, gwfname + ".obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'
    # ensure layer 1 is dry with the DRY value
    assert (
        np.max(tc["OBS1"][:201]) == -1.0e30
    ), "layer 1 should be dry for this period"


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
    test.run_mf6(Simulation(dir, exfunc=eval_flow, idxsim=idx))

    return


def main():
    # initialize testing framework
    test = testing_framework()

    # run the test model
    for idx, dir in enumerate(exdirs):
        test.build_mf6_models(build_model, idx, dir)
        sim = Simulation(dir, exfunc=eval_flow, idxsim=idx)
        test.run_mf6(sim)


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run main routine
    main()
