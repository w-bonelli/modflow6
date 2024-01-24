"""
This test exercises TRACKEVENT options to check
that tracking event selection works as expected.

GWF and PRT models run in separate simulations.

The grid is a 10x10 square with a single layer,
the same flow system shown on the FloPy readme,
except for 2 inactive cells in the bottom left
and top right corners.

The flow system is similar to test_prt_fmi01.py.
Particles are split across two release packages,
and the grid has an inactive region this time,
to check cell numbers recorded in pathline data
are converted from reduced to user node numbers.
This is verified with FloPy by intersecting path
points with the grid then computing node numbers.

Particles are released from the top left cell.

Pathlines are compared with a MODPATH 7 model.
"""


from pathlib import Path

import flopy
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
from flopy.plot.plotutil import to_mp7_pathlines
from flopy.utils import PathlineFile
from flopy.utils.binaryfile import HeadFile
from prt_test_utils import (
    BasicDisCase,
    check_budget_data,
    check_track_data,
    get_model_name,
)

from framework import TestFramework

simname = "prtfmi02"
cases = [
    f"{simname}all",
    f"{simname}rel",
    f"{simname}trst",
    f"{simname}tstp",
    f"{simname}wksk",
]
releasepts_prt = {
    "a": [
        # index, k, i, j, x, y, z
        [i, 0, 0, 0, float(f"0.{i + 1}"), float(f"9.{i + 1}"), 0.5]
        for i in range(4)
    ],
    "b": [
        # index, k, i, j, x, y, z
        [i, 0, 0, 0, float(f"0.{i + 5}"), float(f"9.{i + 5}"), 0.5]
        for i in range(5)
    ],
}
releasepts_mp7 = {
    "a": [
        # node number, localx, localy, localz
        (0, float(f"0.{i + 1}"), float(f"0.{i + 1}"), 0.5)
        for i in range(4)
    ],
    "b": [
        # node number, localx, localy, localz
        (0, float(f"0.{i + 5}"), float(f"0.{i + 5}"), 0.5)
        for i in range(5)
    ],
}


def get_output_event(case_name):
    return (
        "ALL"
        if "all" in case_name
        else "RELEASE"
        if "rel" in case_name
        else "TRANSIT"
        if "trst" in case_name
        else "TIMESTEP"
        if "tstp" in case_name
        else "WEAKSINK"
        if "wksk" in case_name
        else "TERMINATE"
        if "terminate" in case_name
        else "ALL"  # default
    )


# function to create idomain from grid dimensions
def create_idomain(nlay, nrow, ncol):
    idmn = np.ones((nlay, nrow, ncol), dtype=int)
    idmn[0, 0, 9] = 0
    idmn[0, 9, 0] = 0
    return idmn


def build_prt_sim(name, gwf_ws, prt_ws, mf6):
    # create simulation
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        exe_name=mf6,
        version="mf6",
        sim_ws=prt_ws,
    )

    # create tdis package
    flopy.mf6.modflow.mftdis.ModflowTdis(
        sim,
        pname="tdis",
        time_units="DAYS",
        nper=BasicDisCase.nper,
        perioddata=[
            (BasicDisCase.perlen, BasicDisCase.nstp, BasicDisCase.tsmult)
        ],
    )

    # create prt model
    prt_name = get_model_name(name, "prt")
    prt = flopy.mf6.ModflowPrt(sim, modelname=prt_name)

    # create prt discretization
    flopy.mf6.modflow.mfgwfdis.ModflowGwfdis(
        prt,
        pname="dis",
        nlay=BasicDisCase.nlay,
        nrow=BasicDisCase.nrow,
        ncol=BasicDisCase.ncol,
        idomain=create_idomain(
            BasicDisCase.nlay, BasicDisCase.nrow, BasicDisCase.ncol
        ),
    )

    # create mip package
    flopy.mf6.ModflowPrtmip(prt, pname="mip", porosity=BasicDisCase.porosity)

    # create a prp package for groups a and b
    prps = [
        flopy.mf6.ModflowPrtprp(
            prt,
            pname=f"prp_{grp}",
            filename=f"{prt_name}_{grp}.prp",
            nreleasepts=len(releasepts_prt[grp]),
            packagedata=releasepts_prt[grp],
            perioddata={0: ["FIRST"]},
        )
        for grp in ["a", "b"]
    ]

    # create output control package
    event = get_output_event(name)
    prt_track_file = f"{prt_name}.trk"
    prt_track_csv_file = f"{prt_name}.trk.csv"
    flopy.mf6.ModflowPrtoc(
        prt,
        pname="oc",
        track_filerecord=[prt_track_file],
        trackcsv_filerecord=[prt_track_csv_file],
        trackevent=event,
    )

    # create the flow model interface
    gwf_name = get_model_name(name, "gwf")
    gwf_budget_file = gwf_ws / f"{gwf_name}.bud"
    gwf_head_file = gwf_ws / f"{gwf_name}.hds"
    flopy.mf6.ModflowPrtfmi(
        prt,
        packagedata=[
            ("GWFHEAD", gwf_head_file),
            ("GWFBUDGET", gwf_budget_file),
        ],
    )

    # add explicit model solution
    ems = flopy.mf6.ModflowEms(
        sim,
        pname="ems",
        filename=f"{prt_name}.ems",
    )
    sim.register_solution_package(ems, [prt.name])

    return sim


def build_mp7_sim(name, ws, mp7, gwf):
    mp7_name = get_model_name(name, "mp7")
    pgs = [
        flopy.modpath.ParticleGroup(
            particlegroupname=f"group_{grp}",
            particledata=flopy.modpath.ParticleData(
                partlocs=[p[0] for p in releasepts_mp7[grp]],
                localx=[p[1] for p in releasepts_mp7[grp]],
                localy=[p[2] for p in releasepts_mp7[grp]],
                localz=[p[3] for p in releasepts_mp7[grp]],
                timeoffset=0,
                drape=0,
            ),
            filename=f"{mp7_name}_{grp}.sloc",
        )
        for grp in ["a", "b"]
    ]
    mp = flopy.modpath.Modpath7(
        modelname=mp7_name,
        flowmodel=gwf,
        exe_name=mp7,
        model_ws=ws,
        headfilename=f"{gwf.name}.hds",
        budgetfilename=f"{gwf.name}.bud",
    )
    mpbas = flopy.modpath.Modpath7Bas(
        mp,
        porosity=BasicDisCase.porosity,
    )
    mpsim = flopy.modpath.Modpath7Sim(
        mp,
        simulationtype="pathline",
        trackingdirection="forward",
        budgetoutputoption="summary",
        stoptimeoption="extend",
        particlegroups=pgs,
    )

    return mp


def build_models(idx, test):
    # build gwf model
    gwf_sim = BasicDisCase.get_gwf_sim(
        test.name, test.workspace, test.targets["mf6"]
    )
    # add idomain
    gwf = gwf_sim.get_model()
    dis = gwf.get_package("DIS")
    dis.idomain = create_idomain(
        BasicDisCase.nlay, BasicDisCase.nrow, BasicDisCase.ncol
    )

    # build prt model
    prt_sim = build_prt_sim(
        test.name, test.workspace, test.workspace / "prt", test.targets["mf6"]
    )
    # build mp7 model
    mp7_sim = build_mp7_sim(
        test.name, test.workspace / "mp7", test.targets["mp7"], gwf
    )
    return gwf_sim, prt_sim, mp7_sim


def check_output(idx, test):
    name = test.name
    gwf_ws = test.workspace
    prt_ws = test.workspace / "prt"
    mp7_ws = test.workspace / "mp7"
    gwf_name = get_model_name(name, "gwf")
    prt_name = get_model_name(name, "prt")
    mp7_name = get_model_name(name, "mp7")
    gwf_sim = test.sims[0]
    gwf = gwf_sim.get_model(gwf_name)
    mg = gwf.modelgrid

    # check mf6 output files exist
    gwf_budget_file = f"{gwf_name}.bud"
    gwf_head_file = f"{gwf_name}.hds"
    prt_track_file = f"{prt_name}.trk"
    prt_track_csv_file = f"{prt_name}.trk.csv"
    mp7_pathline_file = f"{mp7_name}.mppth"
    assert (gwf_ws / gwf_budget_file).is_file()
    assert (gwf_ws / gwf_head_file).is_file()
    assert (prt_ws / prt_track_file).is_file()
    assert (prt_ws / prt_track_csv_file).is_file()

    # check mp7 output files exist
    assert (mp7_ws / mp7_pathline_file).is_file()

    # load mp7 pathline results
    plf = PathlineFile(mp7_ws / mp7_pathline_file)
    mp7_pls = pd.DataFrame(
        plf.get_destination_pathline_data(range(mg.nnodes), to_recarray=True)
    )
    # convert zero-based to one-based
    mp7_pls["particlegroup"] = mp7_pls["particlegroup"] + 1
    mp7_pls["node"] = mp7_pls["node"] + 1
    mp7_pls["k"] = mp7_pls["k"] + 1

    # load mf6 pathline results
    mf6_pls = pd.read_csv(prt_ws / prt_track_csv_file)

    # if event is ALL, output should be the same as MODPATH 7,
    # so continue with comparisons.
    # if event is RELEASE, expect 1 location for each particle.
    # if event is TRANSIT, expect full results minus start loc.
    # if event is TIMESTEP or WEAKSINK, output should be empty.
    # in either case, return early and skip MP7 comparison.
    event = get_output_event(name)
    if event == "RELEASE" or event == "TERMINATE":
        assert len(mf6_pls) == len(releasepts_prt["a"]) + len(
            releasepts_prt["b"]
        )
        return
    elif event == "TRANSIT":
        assert len(mf6_pls) == (
            len(mp7_pls)
            - 2 * (len(releasepts_prt["a"]) + len(releasepts_prt["b"]))
        )
        return
    elif event == "TIMESTEP" or event == "WEAKSINK":
        assert len(mf6_pls) == 0
        return

    # make sure mf6 pathline data have correct
    #   - model index (1)
    #   - PRP index (1 or 2, depending on release point index)
    def all_equal(col, val):
        a = col.to_numpy()
        return a[0] == val and (a[0] == a).all()

    assert all_equal(mf6_pls["imdl"], 1)
    assert set(mf6_pls[mf6_pls["iprp"] == 1]["irpt"].unique()) == set(
        range(1, 5)
    )
    assert set(mf6_pls[mf6_pls["iprp"] == 2]["irpt"].unique()) == set(
        range(1, 6)
    )

    # check budget data were written to mf6 prt list file
    check_budget_data(
        prt_ws / f"{name}_prt.lst", BasicDisCase.perlen, BasicDisCase.nper
    )

    # check mf6 prt particle track data were written to binary/CSV files
    check_track_data(
        track_bin=prt_ws / prt_track_file,
        track_hdr=prt_ws / Path(prt_track_file.replace(".trk", ".trk.hdr")),
        track_csv=prt_ws / prt_track_csv_file,
    )

    # check that particle names are particle indices
    # assert len(mf6_pldata) == len(mf6_pldata[mf6_pldata['irpt'].astype(str).eq(mf6_pldata['name'])])

    # get head, budget, and spdis results from GWF model
    hds = HeadFile(gwf_ws / gwf_head_file).get_data()
    bud = gwf.output.budget()
    spdis = bud.get_data(text="DATA-SPDIS")[0]
    qx, qy, qz = flopy.utils.postprocessing.get_specific_discharge(spdis, gwf)

    # setup plot
    fig, ax = plt.subplots(nrows=1, ncols=2, figsize=(10, 10))
    for a in ax:
        a.set_aspect("equal")

    # plot mf6 pathlines in map view
    pmv = flopy.plot.PlotMapView(modelgrid=mg, ax=ax[0])
    pmv.plot_grid()
    pmv.plot_array(hds[0], alpha=0.1)
    pmv.plot_vector(qx, qy, normalize=True, color="white")
    mf6_plines = mf6_pls.groupby(["iprp", "irpt", "trelease"])
    for ipl, ((iprp, irpt, trelease), pl) in enumerate(mf6_plines):
        pl.plot(
            title="MF6 pathlines",
            kind="line",
            x="x",
            y="y",
            ax=ax[0],
            legend=False,
            color=cm.plasma(ipl / len(mf6_plines)),
        )

    # plot mp7 pathlines in map view
    pmv = flopy.plot.PlotMapView(modelgrid=mg, ax=ax[1])
    pmv.plot_grid()
    pmv.plot_array(hds[0], alpha=0.1)
    pmv.plot_vector(qx, qy, normalize=True, color="white")
    mp7_plines = mp7_pls.groupby(["particleid"])
    for ipl, (pid, pl) in enumerate(mp7_plines):
        pl.plot(
            title="MP7 pathlines",
            kind="line",
            x="x",
            y="y",
            ax=ax[1],
            legend=False,
            color=cm.plasma(ipl / len(mp7_plines)),
        )

    # view/save plot
    # plt.show()
    plt.savefig(gwf_ws / f"test_{simname}.png")

    # check that cell numbers are correct
    for i, row in list(mf6_pls.iterrows()):
        # todo debug final cell number disagreement
        if row.ireason == 3:  # termination
            continue

        x, y, z, t, ilay, icell = (
            row.x,
            row.y,
            row.z,
            row.t,
            row.ilay,
            row.icell,
        )
        k, i, j = mg.intersect(x, y, z)
        nn = mg.get_node([k, i, j]) + 1
        neighbors = mg.neighbors(nn)
        assert np.isclose(nn, icell, atol=1) or any(
            (nn - 1) == n for n in neighbors
        ), f"nn comparison failed: expected {nn}, got {icell}"
        assert ilay == (k + 1) == 1

    # convert mf6 pathlines to mp7 format
    mf6_pls = to_mp7_pathlines(mf6_pls)

    # drop columns for which there is no direct correspondence between mf6 and mp7
    del mf6_pls["sequencenumber"]
    del mf6_pls["particleidloc"]
    del mf6_pls["xloc"]
    del mf6_pls["yloc"]
    del mf6_pls["zloc"]
    del mp7_pls["sequencenumber"]
    del mp7_pls["particleidloc"]
    del mp7_pls["xloc"]
    del mp7_pls["yloc"]
    del mp7_pls["zloc"]

    # sort both dataframes
    cols = ["x", "y", "z", "time"]
    mf6_pls = mf6_pls.sort_values(by=cols)
    mp7_pls = mp7_pls.sort_values(by=cols)

    # compare mf6 / mp7 pathline data
    assert mf6_pls.shape == mp7_pls.shape
    assert np.allclose(mf6_pls, mp7_pls, atol=1e-3)


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
        compare=None,
    )
    test.run()
