"""
Test the IST Package with a one cell model with water added and then
removed.

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

ex = ["ist01"]
laytyp = [1]
ss = [1.0e-10]
sy = [0.1]
thetaim = [0.05]
zetaim = [0.1]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))
ddir = "data"
nlay, nrow, ncol = 1, 1, 1


def build_model(idx, dir):

    perlen = [
        2.0,
    ]
    nper = len(perlen)
    nstp = [14]
    tsmult = [1.0]
    delr = 10.0
    delc = 10.0
    top = 10.0
    botm = [0.0]
    strt = 5
    hk = 1.0

    nouter, ninner = 100, 300
    hclose, rclose, relax = 1e-6, 1e-6, 0.97

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
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    # create gwf model
    gwfname = "gwf_" + name
    newtonoptions = "NEWTON"
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=gwfname,
        newtonoptions=newtonoptions,
    )

    # create iterative model solution and register the gwf model with it
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
        linear_acceleration="BICGSTAB",
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
    npf = flopy.mf6.ModflowGwfnpf(
        gwf, save_flows=False, icelltype=laytyp[idx], k=hk, k33=hk
    )
    # storage
    sto = flopy.mf6.ModflowGwfsto(
        gwf,
        save_flows=False,
        iconvert=laytyp[idx],
        ss=ss[idx],
        sy=sy[idx],
        steady_state={0: False},
        transient={0: True},
    )

    # wel files
    welspdict = {
        0: [[(0, 0, 0), 0.0, 0.0]],
    }
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        print_input=True,
        print_flows=True,
        stress_period_data=welspdict,
        save_flows=False,
        auxiliary="CONCENTRATION",
        pname="WEL-1",
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

    # create gwt model
    gwtname = "gwt_" + name
    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname, save_flows=True)

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
    ic = flopy.mf6.ModflowGwtic(gwt, strt=100.0)

    # advection
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")

    # mass storage and transfer
    mst = flopy.mf6.ModflowGwtmst(gwt, porosity=sy[idx])

    # immobile storage and transfer
    cim_filerecord = f"{gwtname}.ist.ucn"
    ist = flopy.mf6.ModflowGwtist(
        gwt,
        save_flows=True,
        cim_filerecord=cim_filerecord,
        cim=0.0,
        thetaim=thetaim[idx],
        zetaim=zetaim[idx],
    )

    # sources
    sourcerecarray = [("WEL-1", "AUX", "CONCENTRATION")]
    ssm = flopy.mf6.ModflowGwtssm(gwt, sources=sourcerecarray)

    # output control
    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        concentration_filerecord=f"{gwtname}.ucn",
        concentrationprintrecord=[
            ("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )

    # GWF GWT exchange
    gwfgwt = flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename=f"{name}.gwfgwt",
    )

    return sim, None


def eval_transport(sim):
    print("evaluating transport...")

    name = ex[sim.idxsim]
    gwtname = "gwt_" + name
    gwfname = "gwf_" + name

    # head
    fpth = os.path.join(sim.simpath, f"{gwfname}.hds")
    try:
        hobj = flopy.utils.HeadFile(fpth, precision="double")
        head = hobj.get_alldata().flatten()
    except:
        assert False, f'could not load data from "{fpth}"'

    # mobile concentration
    fpth = os.path.join(sim.simpath, f"{gwtname}.ucn")
    try:
        cobj = flopy.utils.HeadFile(
            fpth, precision="double", text="CONCENTRATION"
        )
        conc = cobj.get_alldata().flatten()
    except:
        assert False, f'could not load data from "{fpth}"'

    # immobile concentration
    fpth = os.path.join(sim.simpath, f"{gwtname}.ist.ucn")
    try:
        cobj = flopy.utils.HeadFile(fpth, precision="double", text="CIM")
        cim = cobj.get_alldata().flatten()
    except:
        assert False, f'could not load data from "{fpth}"'

    # budget
    fpth = os.path.join(sim.simpath, f"{gwtname}.cbc")
    try:
        bobj = flopy.utils.CellBudgetFile(fpth, precision="double")
        print(bobj.get_unique_record_names())
        immrate = bobj.get_data(text="IMMOBILE DOMAIN")
    except:
        assert False, f'could not load data from "{fpth}"'

    times = cobj.get_times()
    for i, t in enumerate(times):
        rate_sim = immrate[i]["q"][0]
        saturation = 0.5
        volume = 10.0 * 10.0 * 10.0
        rate_calc = (
            (cim[i] - conc[i]) * zetaim[sim.idxsim] * saturation * volume
        )
        print(t, conc[i], cim[i], rate_sim, rate_calc)
        msg = f"Rate: {rate_sim} /= {rate_calc} for time {t}"
        assert np.allclose(rate_sim, rate_calc), msg

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
