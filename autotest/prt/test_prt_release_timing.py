"""
Test cases exercising release timing, 1st via
package-level RELEASETIME option, & then with
period-block config STEPS 1 and FRACTION 0.5.
The model is setup to release halfway through
the first and only time step of the first and
only stress period, with duration 1 time unit,
so the same value of 0.5 can be used for both
RELEASETIME and FRACTION.

Period-block FRACTION should work with FIRST
and ALL, but flopy hangs with either option.
Todo: debug and enable corresponding cases.

The grid is a 10x10 square with a single layer,
the same flow system shown on the FloPy readme.

Particles are released from the top left cell.

Results are compared against a MODPATH 7 model.
Telease time 0.5 could be configured, but mp7
reports relative times, so there is no reason
& mp7 results are converted before comparison.
"""


from pathlib import Path
from pprint import pformat
from typing import Optional

import flopy
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
from flopy.plot.plotutil import to_mp7_pathlines
from flopy.utils import PathlineFile
from flopy.utils.binaryfile import HeadFile
from prt_test_utils import (BasicDisCase, all_equal, check_budget_data,
                            check_track_data, get_model_name, get_partdata)

from framework import TestFramework

simname = "prtfmi05"
cases = [
    # options block options
    f"{simname}relt",  # RELEASETIME 0.5
    # period block options
    # f"{simname}all",  # ALL FRACTION 0.5      # todo debug flopy hanging
    # f"{simname}frst", # FIRST FRACTION 0.5    # todo debug flopy hanging
    f"{simname}stps",  # STEPS 1 FRACTION 0.5
]


def get_perioddata(name, periods=1, fraction=None) -> Optional[dict]:
    if "relt" in name:
        return None
    opt = [
        "FIRST"
        if "frst" in name
        else "ALL"
        if "all" in name
        else ("STEPS", 1)
        if "stps" in name
        else None
    ]
    if opt[0] is None:
        raise ValueError(f"Invalid period option: {name}")
    if fraction is not None:
        opt.append(("FRACTION", fraction))
    return {i: opt for i in range(periods)}


def build_prt_sim(name, gwf_ws, prt_ws, mf6, fraction=None):
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
    )

    # create mip package
    flopy.mf6.ModflowPrtmip(prt, pname="mip", porosity=BasicDisCase.porosity)

    # convert mp7 particledata to prt release points
    partdata = get_partdata(prt.modelgrid, BasicDisCase.releasepts_mp7)
    releasepts = list(partdata.to_prp(prt.modelgrid))

    # check release points match expectation
    assert np.allclose(BasicDisCase.releasepts_prt, releasepts)

    # create prp package
    prp_track_file = f"{prt_name}.prp.trk"
    prp_track_csv_file = f"{prt_name}.prp.trk.csv"
    pdat = get_perioddata(prt_name, fraction=fraction)
    # fraction 0.5 equiv. to release time 0.5 since 1 period 1 step with length 1
    trelease = fraction if "relt" in prt_name else None
    flopy.mf6.ModflowPrtprp(
        prt,
        pname="prp1",
        filename=f"{prt_name}_1.prp",
        nreleasepts=len(releasepts),
        packagedata=releasepts,
        perioddata=pdat,
        track_filerecord=[prp_track_file],
        trackcsv_filerecord=[prp_track_csv_file],
        releasetime=trelease,
    )

    # create output control package
    prt_track_file = f"{prt_name}.trk"
    prt_track_csv_file = f"{prt_name}.trk.csv"
    flopy.mf6.ModflowPrtoc(
        prt,
        pname="oc",
        track_filerecord=[prt_track_file],
        trackcsv_filerecord=[prt_track_csv_file],
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
    partdata = get_partdata(gwf.modelgrid, BasicDisCase.releasepts_mp7)
    mp7_name = get_model_name(name, "mp7")
    pg = flopy.modpath.ParticleGroup(
        particlegroupname="G1",
        particledata=partdata,
        filename=f"{mp7_name}.sloc",
    )
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
        particlegroups=[pg],
    )

    return mp


def build_models(idx, test, fraction):
    gwf_sim = BasicDisCase.get_gwf_sim(
        test.name, test.workspace, test.targets.mf6
    )
    prt_sim = build_prt_sim(
        test.name,
        test.workspace,
        test.workspace / "prt",
        test.targets.mf6,
        fraction,
    )
    mp7_sim = build_mp7_sim(
        test.name,
        test.workspace / "mp7",
        test.targets.mp7,
        gwf_sim.get_model(),
    )
    return gwf_sim, prt_sim, mp7_sim


def check_output(idx, test, fraction):
    name = test.name
    ws = test.workspace
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
    prp_track_file = f"{prt_name}.prp.trk"
    prp_track_csv_file = f"{prt_name}.prp.trk.csv"
    assert (ws / gwf_budget_file).is_file()
    assert (ws / gwf_head_file).is_file()
    assert (prt_ws / prt_track_file).is_file()
    assert (prt_ws / prt_track_csv_file).is_file()
    assert (prt_ws / prp_track_file).is_file()
    assert (prt_ws / prp_track_csv_file).is_file()

    # check mp7 output files exist
    mp7_pathline_file = f"{mp7_name}.mppth"
    assert (mp7_ws / mp7_pathline_file).is_file()

    # load mp7 pathline results
    plf = PathlineFile(mp7_ws / mp7_pathline_file)
    mp7_pls = pd.DataFrame(
        plf.get_destination_pathline_data(range(mg.nnodes), to_recarray=True)
    )
    # convert zero-based to one-based indexing in mp7 results
    mp7_pls["particlegroup"] = mp7_pls["particlegroup"] + 1
    mp7_pls["node"] = mp7_pls["node"] + 1
    mp7_pls["k"] = mp7_pls["k"] + 1

    # apply reference time to mp7 results (mp7 reports relative times)
    mp7_pls["time"] = mp7_pls["time"] + fraction

    # load mf6 pathline results
    mf6_pls = pd.read_csv(prt_ws / prt_track_csv_file, na_filter=False)

    # make sure pathline df has "name" (boundname) column and empty values
    assert "name" in mf6_pls
    assert (mf6_pls["name"] == "").all()

    # make sure all mf6 pathline data have correct model and PRP index (1)
    assert all_equal(mf6_pls["imdl"], 1)
    assert all_equal(mf6_pls["iprp"], 1)

    # check budget data were written to mf6 prt list file
    check_budget_data(
        prt_ws / f"{name}_prt.lst", BasicDisCase.perlen, BasicDisCase.nper
    )

    # check mf6 prt particle track data were written to binary/CSV files
    # and that different formats are equal
    for track_csv in [
        prt_ws / prt_track_csv_file,
        prt_ws / prp_track_csv_file,
    ]:
        check_track_data(
            track_bin=prt_ws / prt_track_file,
            track_hdr=prt_ws
            / Path(prt_track_file.replace(".trk", ".trk.hdr")),
            track_csv=track_csv,
        )

    # extract head, budget, and specific discharge results from GWF model
    hds = HeadFile(ws / gwf_head_file).get_data()
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
    plt.savefig(ws / f"test_{simname}.png")

    # convert mf6 pathlines to mp7 format
    mf6_pls = to_mp7_pathlines(mf6_pls)

    # sort both dataframes by particleid and time
    mf6_pls.sort_values(by=["particleid", "time"], inplace=True)
    mp7_pls.sort_values(by=["particleid", "time"], inplace=True)

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

    # compare mf6 / mp7 pathline data
    assert mf6_pls.shape == mp7_pls.shape
    assert np.allclose(mf6_pls, mp7_pls, atol=1e-3)


@pytest.mark.parametrize("idx, name", enumerate(cases))
@pytest.mark.parametrize("fraction", [0.5])
def test_mf6model(idx, name, function_tmpdir, targets, fraction):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t, fraction),
        check=lambda t: check_output(idx, t, fraction),
        targets=targets,
        compare=None,
    )
    test.run()