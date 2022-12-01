# Simple one-layer model with a drn and sfr network on top.  Purpose is to
# test movement of solute between stress and advanced packages.  In this case
# water from a drain is moved into the first sfr reach.  The test confirms
# that the solute from the drain is moved into the sfr reach.
# There is no flow between the stream and the aquifer.

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

ex = ["mvt_02"]
exdirs = []
for s in ex:
    exdirs.append(os.path.join("temp", s))


def build_model(idx, dir):
    lx = 7.0
    lz = 1.0
    nlay = 1
    nrow = 1
    ncol = 7
    nper = 1
    delc = 1.0
    delr = lx / ncol
    delz = lz / nlay
    top = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    botm = list(top - np.arange(delz, nlay * delz + delz, delz))
    botm[2] = -1.0

    perlen = [10.0]
    nstp = [10]
    tsmult = [1.0]

    Kh = 20.0
    Kv = 20.0

    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))

    single_matrix = False
    nouter, ninner = 20, 10
    hclose, rclose, relax = 1e-8, 1e-6, 0.97

    name = ex[idx]

    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=ws,
        memory_print_option=["ALL"],
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

    imsgwf = flopy.mf6.ModflowIms(
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
        filename=f"{gwfname}.ims",
    )

    idomain = np.full((nlay, nrow, ncol), 1)
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
    ic = flopy.mf6.ModflowGwfic(gwf, strt=np.arange(ncol))

    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(
        gwf,
        xt3doptions=False,
        save_flows=True,
        save_specific_discharge=True,
        icelltype=0,
        k=Kh,
        k33=Kv,
    )

    # chd files
    drnlist1 = [
        [(0, 0, ncol - 1), 0.0, Kh],
    ]
    drn1 = flopy.mf6.ModflowGwfdrn(
        gwf,
        stress_period_data=drnlist1,
        print_input=True,
        print_flows=True,
        save_flows=False,
        mover=True,
        pname="DRN-1",
    )

    # wel files
    wellist1 = [
        [(0, 0, 0), 1.0, 100.0],
    ]
    wel1 = flopy.mf6.ModflowGwfwel(
        gwf,
        stress_period_data=wellist1,
        print_input=True,
        print_flows=True,
        save_flows=False,
        pname="WEL-1",
        auxiliary="CONCENTRATION",
    )

    # pak_data = [<rno> <cellid(ncelldim)> <rlen> <rwid> <rgrd> <rtp> <rbth> <rhk> <man> <ncon> <ustrf> <ndv> [<aux(naux)>] [<boundname>]]
    rlen = delr
    rwid = delc
    rgrd = 1.0
    rtp = 0.0
    rbth = 0.1
    rhk = 0.0  # 0.01
    rman = 1.0
    ncon = 2
    ustrf = 1.0
    ndv = 0
    pak_data = []
    for irno in range(ncol):
        ncon = 2
        if irno in [0, ncol - 1]:
            ncon = 1
        cellid = (0, 0, irno)
        t = (
            irno,
            cellid,
            rlen,
            rwid,
            rgrd,
            rtp,
            rbth,
            rhk,
            rman,
            ncon,
            ustrf,
            ndv,
        )
        pak_data.append(t)

    con_data = []
    for irno in range(ncol):
        if irno == 0:
            t = (irno, -(irno + 1))
        elif irno == ncol - 1:
            t = (irno, irno - 1)
        else:
            t = (irno, irno - 1, -(irno + 1))
        con_data.append(t)

    p_data = [
        (0, "INFLOW", 0.0),
    ]

    # note: for specifying sfr number, use fortran indexing!
    # sfr_obs = {('sfr_obs.csv'): [('lakestage', 'stage', 1),
    #                             ('lakevolume', 'volume', 1),
    #                             ('lak1', 'lak', 1, 1),
    #                             ('lak2', 'lak', 1, 2),
    #                             ('lak3', 'lak', 1, 3)]}

    sfr_on = True
    if sfr_on:
        sfr = flopy.mf6.modflow.ModflowGwfsfr(
            gwf,
            save_flows=True,
            print_input=True,
            print_flows=True,
            print_stage=True,
            mover=True,
            stage_filerecord=gwfname + ".sfr.stg",
            budget_filerecord=gwfname + ".sfr.bud",
            nreaches=ncol,
            packagedata=pak_data,
            pname="SFR-1",
            connectiondata=con_data,
            perioddata=p_data,
            # observations=lak_obs,
            # auxiliary=['CONCENTRATION',
            #           'DENSITY'],
        )

    # output control
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        headprintrecord=[("COLUMNS", 10, "WIDTH", 15, "DIGITS", 6, "GENERAL")],
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
        printrecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    packages = [
        ("drn-1",),
        ("sfr-1",),
    ]
    perioddata = [
        ("drn-1", 0, "sfr-1", 0, "factor", 1.0),
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

    transport = True
    if transport:

        # create gwt model
        gwtname = "gwt_" + name
        gwt = flopy.mf6.MFModel(
            sim,
            model_type="gwt6",
            modelname=gwtname,
            model_nam_file=f"{gwtname}.nam",
        )

        if not single_matrix:
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
            idomain=idomain,
        )

        # initial conditions
        ic = flopy.mf6.ModflowGwtic(gwt, strt=10.0, filename=f"{gwtname}.ic")

        # advection
        adv = flopy.mf6.ModflowGwtadv(
            gwt, scheme="UPSTREAM", filename=f"{gwtname}.adv"
        )

        # storage
        porosity = 1.0
        sto = flopy.mf6.ModflowGwtmst(
            gwt, porosity=porosity, filename=f"{gwtname}.sto"
        )
        # sources
        sourcerecarray = [
            ("WEL-1", "AUX", "CONCENTRATION"),
        ]
        ssm = flopy.mf6.ModflowGwtssm(
            gwt, sources=sourcerecarray, filename=f"{gwtname}.ssm"
        )

        # sft
        sftpackagedata = []
        for irno in range(ncol):
            t = (irno, 0.0, 99.0, 999.0, f"myreach{irno + 1}")
            sftpackagedata.append(t)

        sftperioddata = [
            (0, "STATUS", "ACTIVE"),
        ]

        sft_obs = {
            (gwtname + ".sft.obs.csv",): [
                (f"sft-{i + 1}-conc", "CONCENTRATION", i + 1) for i in range(7)
            ]
            + [
                ("sft-1-extinflow", "EXT-INFLOW", 1),
                ("sft-1-rain", "RAINFALL", 1),
                ("sft-1-roff", "RUNOFF", 1),
                ("sft-1-evap", "EVAPORATION", 1),
                ("sft-1-const", "CONSTANT", 1),
                ("sft-1-gwt1", "SFT", 1, 1),
                ("sft-1-gwt2", "SFT", 2, 1),
                ("sft-1-gwt3", "SFT", 3, 1),
                ("sft-1-myreach1", "SFT", "MYREACH1"),
            ],
        }
        # append additional obs attributes to obs dictionary
        sft_obs["digits"] = 12
        sft_obs["print_input"] = True
        sft_obs["filename"] = gwtname + ".sft.obs"

        sft = flopy.mf6.modflow.ModflowGwtsft(
            gwt,
            boundnames=True,
            save_flows=True,
            print_input=True,
            print_flows=True,
            print_concentration=True,
            concentration_filerecord=gwtname + ".sft.bin",
            budget_filerecord=gwtname + ".sft.bud",
            packagedata=sftpackagedata,
            reachperioddata=sftperioddata,
            observations=sft_obs,
            pname="SFR-1",
            auxiliary=["aux1", "aux2"],
        )

        # mover transport package
        fname = f"{gwtname}.mvt.bud"
        mvt = flopy.mf6.modflow.ModflowGwtmvt(
            gwt, print_flows=True, budget_filerecord=fname
        )

        # output control
        oc = flopy.mf6.ModflowGwtoc(
            gwt,
            budget_filerecord=f"{gwtname}.bud",
            budgetcsv_filerecord=f"{gwtname}.bud.csv",
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


def eval_results(sim):
    print("evaluating results...")
    name = ex[sim.idxsim]
    gwtname = "gwt_" + name

    # Load csv budget and make sure names are correct
    fname = f"{gwtname}.bud.csv"
    fname = os.path.join(sim.simpath, fname)
    budcsv = np.genfromtxt(fname, names=True, delimiter=",", deletechars="")
    answer = [
        "time",
        "STORAGE-AQUEOUS(MST)_IN",
        "WEL(SSM_WEL-1)_IN",
        "DRN(SSM_DRN-1)_IN",
        "DRN-TO-MVR(SSM_DRN-1)_IN",
        "SFT(SFR-1)_IN",
        "STORAGE-AQUEOUS(MST)_OUT",
        "WEL(SSM_WEL-1)_OUT",
        "DRN(SSM_DRN-1)_OUT",
        "DRN-TO-MVR(SSM_DRN-1)_OUT",
        "SFT(SFR-1)_OUT",
        "TOTAL_IN",
        "TOTAL_OUT",
        "PERCENT_DIFFERENCE",
    ]
    for i, name in enumerate(budcsv.dtype.names[: len(answer)]):
        assert answer[i] == name

    # ensure sfr concentrations were saved
    fname = gwtname + ".sft.bin"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    cobj = flopy.utils.HeadFile(fname, text="CONCENTRATION")
    csft = cobj.get_data().flatten()

    # load the aquifer concentrations
    fname = gwtname + ".ucn"
    fname = os.path.join(sim.simpath, fname)
    cobj = flopy.utils.HeadFile(fname, text="CONCENTRATION")
    caq = cobj.get_data().flatten()

    # sft observation results
    fpth = os.path.join(sim.simpath, gwtname + ".sft.obs.csv")
    try:
        tc = np.genfromtxt(fpth, names=True, delimiter=",")
    except:
        assert False, f'could not load data from "{fpth}"'

    # compare observation concs with binary file concs
    for i in range(7):
        oname = f"SFT{i + 1}CONC"
        assert np.allclose(
            tc[oname][-1], csft[i]
        ), f"{tc[oname][-1]} {csft[i]}"

    simres = tc["SFT1CONC"]
    answer = [
        5.35156250000,
        9.25781250000,
        13.6718750000,
        19.5703125000,
        27.1337890625,
        35.9912109375,
        45.4956054688,
        54.9609375000,
        63.8175964355,
        71.6825866699,
    ]

    assert np.allclose(simres, answer), f"{simres} {answer}"

    # load the mvt budget file
    fname = gwtname + ".mvt.bud"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    mobj = flopy.utils.CellBudgetFile(fname, precision="double", verbose=False)

    # load the sft budget file
    fname = gwtname + ".sft.bud"
    fname = os.path.join(sim.simpath, fname)
    assert os.path.isfile(fname)
    bobj = flopy.utils.CellBudgetFile(fname, precision="double", verbose=False)
    # check the flow-ja-face terms
    res = bobj.get_data(text="flow-ja-face")[-1]
    # print(res)

    # check the storage terms, which include the total mass in the reach as an aux variable
    res = bobj.get_data(text="storage")[-1]
    # print(res)

    # uncomment when testing so files aren't deleted
    # assert False

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
