# multiple ssm sources and sinks using a flow model followed by a
# transport model.  Initial conditions and all inflows and outflows are
# assigned a concentration of 100.0 so the simulated concentration must also
# be 100.

import os
import shutil

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


import targets

exe_name_mf6 = targets.target_dict["mf6"]
exe_name_mf6 = os.path.abspath(exe_name_mf6)

testdir = "./temp"
testgroup = "ssm01"
d = os.path.join(testdir, testgroup)
if os.path.isdir(d):
    shutil.rmtree(d)

nlay = 1
nrow = 10
ncol = 10
delr = 10.0
delc = 10.0
top = 100.0
botm = 0.0


def run_flow_model():
    global idomain
    name = "flow"
    gwfname = name
    wsf = os.path.join(testdir, testgroup, name)
    sim = flopy.mf6.MFSimulation(
        sim_name=name, sim_ws=wsf, exe_name=exe_name_mf6
    )
    tdis_rc = [(100.0, 1, 1.0), (100.0, 1, 1.0)]
    nper = len(tdis_rc)
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)

    # ims
    hclose = 1.0e-6
    rclose = 1.0e-6
    nouter = 1000
    ninner = 100
    relax = 0.99
    imsgwf = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
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

    ic = flopy.mf6.ModflowGwfic(gwf, strt=100.0)

    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        save_saturation=True,
        icelltype=[1],
        k=10.0,
    )

    sto_on = False
    if sto_on:
        sto = flopy.mf6.ModflowGwfsto(
            gwf,
            save_flows=True,
            iconvert=[1],
            ss=1.0e-5,
            sy=0.3,
            steady_state={0: True},
            transient={0: False},
        )

    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.bud",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[
            ("COLUMNS", ncol, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    rch_on = False
    if rch_on:
        rch = flopy.mf6.ModflowGwfrcha(
            gwf, recharge={0: 4.79e-3}, pname="RCH-1"
        )

    # wel
    wellist = []
    for i in range(nrow):
        wellist.append(((0, i, 0), 100.0, 100.0))
    wel = flopy.mf6.ModflowGwfwel(
        gwf,
        stress_period_data=wellist,
        auxiliary=["concentration"],
        pname="WEL-1",
    )

    # ghb
    rows = [0, 1, 2, 3]
    for ipak, i in enumerate(rows):
        blist = []
        blist.append(((0, i, ncol - 1), 50.0, 1000.0, 100.0))
        fname = f"flow.{ipak + 1}.ghb"
        ghb = flopy.mf6.ModflowGwfghb(
            gwf,
            stress_period_data=blist,
            auxiliary=["concentration"],
            filename=fname,
            pname=f"GHB-{ipak + 1}",
        )

    # riv
    rows = [4, 5, 6]
    for ipak, i in enumerate(rows):
        blist = []
        blist.append(((0, i, ncol - 1), 50.0, 1000.0, 0.0, 100.0))
        fname = f"flow.{ipak + 1}.riv"
        riv = flopy.mf6.ModflowGwfriv(
            gwf,
            stress_period_data=blist,
            auxiliary=["concentration"],
            filename=fname,
            pname=f"RIV-{ipak + 1}",
        )

    # drn
    rows = [7, 8, 9]
    for ipak, i in enumerate(rows):
        blist = []
        blist.append(((0, i, ncol - 1), 50.0, 1000.0, 100.0))
        fname = f"flow.{ipak + 1}.drn"
        drn = flopy.mf6.ModflowGwfdrn(
            gwf,
            stress_period_data=blist,
            auxiliary=["concentration"],
            filename=fname,
            pname=f"DRN-{ipak + 1}",
        )

    sim.write_simulation()
    success, buff = sim.run_simulation(silent=False)
    errmsg = f"flow model did not terminate successfully\n{buff}"
    assert success, errmsg

    return


def run_transport_model():
    name = "transport"
    gwtname = name
    wst = os.path.join(testdir, testgroup, name)
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name=exe_name_mf6,
        sim_ws=wst,
        continue_=False,
    )

    tdis_rc = [(100.0, 10, 1.0), (100.0, 10, 1.0)]
    nper = len(tdis_rc)
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=nper, perioddata=tdis_rc
    )

    gwt = flopy.mf6.ModflowGwt(sim, modelname=gwtname)

    # ims
    hclose = 0.001
    rclose = 0.001
    nouter = 50
    ninner = 20
    relax = 0.97
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
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
    )
    ic = flopy.mf6.ModflowGwtic(gwt, strt=100.0)
    sto = flopy.mf6.ModflowGwtmst(gwt, porosity=0.3)
    adv = flopy.mf6.ModflowGwtadv(gwt, scheme="TVD")
    dsp = flopy.mf6.ModflowGwtdsp(gwt, alh=20.0, ath1=2, atv=0.2)

    # Create the ssm sources block information
    sourcerecarray = []
    # sourcerecarray += [("WEL-1", "AUX", "CONCENTRATION")]
    sourcerecarray += [
        (f"GHB-{i+1}", "AUX", "CONCENTRATION") for i in [0, 1, 2, 3]
    ]
    sourcerecarray += [
        (f"RIV-{i+1}", "AUX", "CONCENTRATION") for i in [0, 1, 2]
    ]
    sourcerecarray += [
        (f"DRN-{i+1}", "AUX", "CONCENTRATION") for i in [0, 1, 2]
    ]

    fileinput = [
        ("WEL-1", f"{gwtname}.wel1.spc"),
    ]
    ssm = flopy.mf6.ModflowGwtssm(
        gwt,
        print_flows=True,
        sources=sourcerecarray,
        fileinput=fileinput,
    )
    pd = [(i, "concentration", 100.0) for i in range(nrow)]
    spc = flopy.mf6.ModflowUtlspc(
        gwt, perioddata=pd, maxbound=len(pd), filename=f"{gwtname}.wel1.spc"
    )

    pd = [
        ("GWFHEAD", "../flow/flow.hds", None),
        ("GWFBUDGET", "../flow/flow.bud", None),
    ]
    fmi = flopy.mf6.ModflowGwtfmi(
        gwt, packagedata=pd, flow_imbalance_correction=True
    )

    oc = flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        budgetcsv_filerecord=f"{gwtname}.cbc.csv",
        concentration_filerecord=f"{gwtname}.ucn",
        concentrationprintrecord=[
            ("COLUMNS", ncol, "WIDTH", 15, "DIGITS", 6, "GENERAL")
        ],
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )

    sim.write_simulation()
    success, buff = sim.run_simulation(silent=False)
    errmsg = f"transport model did not terminate successfully\n{buff}"
    assert success, errmsg

    # ensure budget table can be parsed
    fname = gwtname + ".lst"
    fname = os.path.join(wst, fname)
    budl = flopy.utils.Mf6ListBudget(
        fname, budgetkey="MASS BUDGET FOR ENTIRE MODEL"
    )
    d0 = budl.get_budget()[0]

    # Load the csv representation of the budget
    fname = f"{gwtname}.cbc.csv"
    fname = os.path.join(wst, fname)
    d0 = np.genfromtxt(fname, names=True, delimiter=",", deletechars="")
    print(d0.dtype.names)

    for name, _, _ in sourcerecarray[1:]:
        name = f"{name[:3]}(SSM_{name})_OUT"
        a1 = d0["WEL(SSM_WEL-1)_IN"] / 10.0
        a2 = d0[name]
        print(f"Checking budet term {name} against WEL-1_IN / 10.")
        errmsg = f"{name} not equal WEL-1_IN / 10.\n{a1}\n{a2}"
        assert np.allclose(a1, a2), errmsg

    print("Checking that all simulated concentrations are 100.")
    simulated_concentration = gwt.output.concentration().get_alldata()
    errmsg = (
        "All simulated concentrations are not 100.  Simulated "
        "concentrations must all be 100. because the initial "
        "concentration and all inflows are 100."
    )
    assert np.all(simulated_concentration == 100.0), errmsg

    return


def test_ssm01fmi():
    run_flow_model()
    run_transport_model()
    d = os.path.join(testdir, testgroup)
    if os.path.isdir(d):
        shutil.rmtree(d)
    return


if __name__ == "__main__":
    # print message
    print(f"standalone run of {os.path.basename(__file__)}")

    # run tests
    test_ssm01fmi()
