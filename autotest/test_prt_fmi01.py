"""
Tests ability to run a GWF model then a PRT model
in separate simulations via flow model interface.

The grid is a 10x10 square with a single layer,
the same flow system shown on the FloPy readme.

Particles are released from the top left cell.

Results are compared against a MODPATH 7 model.

Two test cases are defined, one with the particle
release (PRP) package option STOP_AT_WEAK_SINK on
and one with the option off. No effect on results
are expected because the model has no weak sinks.
(Motivated by bug reports in which particles were
tracked improperly when this option was enabled,
even with no weak sink cells in the vicinity.)
"""


from pathlib import Path

import flopy
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest
from flopy.utils import PathlineFile
from flopy.utils.binaryfile import HeadFile
from prt_test_utils import check_budget_data, check_track_data, to_mp7_format


# simulation name
simname = "prtfmi01"

# test cases
ex = [simname, f"{simname}saws"]


# model names
def get_model_name(idx, mdl):
    return f"{ex[idx]}_{mdl}"


# model info
nlay = 1
nrow = 10
ncol = 10
top = 1.0
botm = [0.0]
nper = 1
perlen = 1.0
nstp = 1
tsmult = 1.0
porosity = 0.1

# release points in mp7 format (using local coordinates)
releasepts_mp7 = [
    # node number, localx, localy, localz
    # (0-based indexing converted to 1-based for mp7 by flopy)
    (0, float(f"0.{i + 1}"), float(f"0.{i + 1}"), 0.5)
    for i in range(9)
]

# expected release points in PRT format; below we will use flopy
# to convert from mp7 to prt format and make sure they are equal
releasepts_prt = [
    # particle index, k, i, j, x, y, z
    # (0-based indexing converted to 1-based for mf6 by flopy)
    (i, 0, 0, 0, float(f"0.{i + 1}"), float(f"9.{i + 1}"), 0.5)
    for i in range(9)
]


def get_partdata(grid):
    return flopy.modpath.ParticleData(
        partlocs=[grid.get_lrc(p[0])[0] for p in releasepts_mp7],
        structured=True,
        localx=[p[1] for p in releasepts_mp7],
        localy=[p[2] for p in releasepts_mp7],
        localz=[p[3] for p in releasepts_mp7],
        timeoffset=0,
        drape=0,
    )


def build_gwf_sim(name, ws, mf6):
    # create simulation
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        exe_name=mf6,
        version="mf6",
        sim_ws=ws,
    )

    # create tdis package
    flopy.mf6.modflow.mftdis.ModflowTdis(
        sim,
        pname="tdis",
        time_units="DAYS",
        nper=nper,
        perioddata=[(perlen, nstp, tsmult)],
    )

    # create gwf model
    gwfname = f"{name}_gwf"
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)

    # create gwf discretization
    flopy.mf6.modflow.mfgwfdis.ModflowGwfdis(
        gwf,
        pname="dis",
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
    )

    # create gwf initial conditions package
    flopy.mf6.modflow.mfgwfic.ModflowGwfic(gwf, pname="ic")

    # create gwf node property flow package
    flopy.mf6.modflow.mfgwfnpf.ModflowGwfnpf(
        gwf,
        pname="npf",
        save_saturation=True,
        save_specific_discharge=True,
    )

    # create gwf chd package
    spd = {
        0: [[(0, 0, 0), 1.0, 1.0], [(0, 9, 9), 0.0, 0.0]],
        1: [[(0, 0, 0), 0.0, 0.0], [(0, 9, 9), 1.0, 2.0]],
    }
    chd = flopy.mf6.ModflowGwfchd(
        gwf,
        pname="CHD-1",
        stress_period_data=spd,
        auxiliary=["concentration"],
    )

    # create gwf output control package
    # output file names
    gwf_budget_file = f"{gwfname}.bud"
    gwf_head_file = f"{gwfname}.hds"
    oc = flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=gwf_budget_file,
        head_filerecord=gwf_head_file,
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    # create iterative model solution for gwf model
    ims = flopy.mf6.ModflowIms(sim)

    return sim


def build_prt_sim(idx, ws, mf6):
    # create simulation
    sim = flopy.mf6.MFSimulation(
        sim_name=ex[idx],
        exe_name=mf6,
        version="mf6",
        sim_ws=ws,
    )

    # create tdis package
    flopy.mf6.modflow.mftdis.ModflowTdis(
        sim,
        pname="tdis",
        time_units="DAYS",
        nper=nper,
        perioddata=[(perlen, nstp, tsmult)],
    )

    # create prt model
    prtname = get_model_name(idx, "prt")
    prt = flopy.mf6.ModflowPrt(sim, modelname=prtname)

    # create prt discretization
    flopy.mf6.modflow.mfgwfdis.ModflowGwfdis(
        prt,
        pname="dis",
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
    )

    # create mip package
    flopy.mf6.ModflowPrtmip(prt, pname="mip", porosity=porosity)

    # convert mp7 particledata to prt release points
    partdata = get_partdata(prt.modelgrid)
    coords = partdata.to_coords(prt.modelgrid)
    releasepts = [(i, 0, 0, 0, c[0], c[1], c[2]) for i, c in enumerate(coords)]

    # check release points match expectation
    assert np.allclose(releasepts_prt, releasepts)

    # create prp package
    prp_track_file = f"{prtname}.prp.trk"
    prp_track_csv_file = f"{prtname}.prp.trk.csv"
    flopy.mf6.ModflowPrtprp(
        prt,
        pname="prp1",
        filename=f"{prtname}_1.prp",
        nreleasepts=len(releasepts),
        packagedata=releasepts,
        perioddata={0: ["FIRST"]},
        track_filerecord=[prp_track_file],
        trackcsv_filerecord=[prp_track_csv_file],
        stop_at_weak_sink="saws" in prtname,
    )

    # create output control package
    prt_track_file = f"{prtname}.trk"
    prt_track_csv_file = f"{prtname}.trk.csv"
    flopy.mf6.ModflowPrtoc(
        prt,
        pname="oc",
        track_filerecord=[prt_track_file],
        trackcsv_filerecord=[prt_track_csv_file],
    )

    # create the flow model interface
    gwfname = get_model_name(idx, "gwf")
    gwf_budget_file = f"{gwfname}.bud"
    gwf_head_file = f"{gwfname}.hds"
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
        filename=f"{prtname}.ems",
    )
    sim.register_solution_package(ems, [prt.name])

    return sim


def build_mp7_sim(idx, ws, mp7, gwf):
    # convert mp7 particledata to prt release points
    partdata = get_partdata(gwf.modelgrid)

    # create modpath 7 simulation
    mp7name = get_model_name(idx, "mp7")
    pg = flopy.modpath.ParticleGroup(
        particlegroupname="G1",
        particledata=partdata,
        filename=f"{mp7name}.sloc",
    )
    mp = flopy.modpath.Modpath7(
        modelname=mp7name,
        flowmodel=gwf,
        exe_name=mp7,
        model_ws=ws,
    )
    mpbas = flopy.modpath.Modpath7Bas(
        mp,
        porosity=porosity,
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


@pytest.mark.parametrize("idx, name", list(enumerate(ex)))
def test_prt_fmi01(idx, name, function_tmpdir, targets):
    # workspace
    ws = function_tmpdir

    # model names
    gwfname = get_model_name(idx, "gwf")
    prtname = get_model_name(idx, "prt")
    mp7name = get_model_name(idx, "mp7")

    # build mf6 models
    gwfsim = build_gwf_sim(ex[idx], ws, targets.mf6)
    prtsim = build_prt_sim(idx, ws, targets.mf6)

    # run mf6 models
    for sim in [gwfsim, prtsim]:
        sim.write_simulation()
        success, _ = sim.run_simulation()
        assert success

    # extract mf6 models
    gwf = gwfsim.get_model(gwfname)
    prt = prtsim.get_model(prtname)

    # extract model grid
    mg = gwf.modelgrid

    # build mp7 model
    mp7sim = build_mp7_sim(idx, ws, targets.mp7, gwf)

    # run mp7 model
    mp7sim.write_input()
    success, _ = mp7sim.run_model()
    assert success

    # check mf6 output files exist
    gwf_budget_file = f"{gwfname}.bud"
    gwf_head_file = f"{gwfname}.hds"
    prt_track_file = f"{prtname}.trk"
    prt_track_csv_file = f"{prtname}.trk.csv"
    prp_track_file = f"{prtname}.prp.trk"
    prp_track_csv_file = f"{prtname}.prp.trk.csv"
    assert (ws / gwf_budget_file).is_file()
    assert (ws / gwf_head_file).is_file()
    assert (ws / prt_track_file).is_file()
    assert (ws / prt_track_csv_file).is_file()
    assert (ws / prp_track_file).is_file()
    assert (ws / prp_track_csv_file).is_file()

    # check mp7 output files exist
    mp7_pathline_file = f"{mp7name}.mppth"
    assert (ws / mp7_pathline_file).is_file()

    # load mp7 pathline results
    plf = PathlineFile(ws / mp7_pathline_file)
    mp7_pldata = pd.DataFrame(
        plf.get_destination_pathline_data(range(mg.nnodes), to_recarray=True)
    )
    # convert zero-based to one-based indexing in mp7 results
    mp7_pldata["particleid"] = mp7_pldata["particleid"] + 1
    mp7_pldata["particlegroup"] = mp7_pldata["particlegroup"] + 1
    mp7_pldata["node"] = mp7_pldata["node"] + 1
    mp7_pldata["k"] = mp7_pldata["k"] + 1

    # load mf6 pathline results
    mf6_pldata = pd.read_csv(ws / prt_track_csv_file)

    # make sure all mf6 pathline data have correct model and PRP index (1)
    def all_equal(col, val):
        a = col.to_numpy()
        return a[0] == val and (a[0] == a).all()

    assert all_equal(mf6_pldata["imdl"], 1)
    assert all_equal(mf6_pldata["iprp"], 1)

    # check budget data were written to mf6 prt list file
    check_budget_data(ws / f"{name}_prt.lst", perlen, nper)

    # check mf6 prt particle track data were written to binary/CSV files
    # and that different formats are equal
    for track_csv in [ws / prt_track_csv_file, ws / prp_track_csv_file]:
        check_track_data(
            track_bin=ws / prt_track_file,
            track_hdr=ws / Path(prt_track_file.replace(".trk", ".trk.hdr")),
            track_csv=track_csv,
        )

    # extract head, budget, and specific discharge results from GWF model
    hds = HeadFile(ws / gwf_head_file).get_data()
    bud = gwf.output.budget()
    spdis = bud.get_data(text="DATA-SPDIS")[0]
    qx, qy, qz = flopy.utils.postprocessing.get_specific_discharge(spdis, gwf)

    # setup plot
    fig, ax = plt.subplots(nrows=1, ncols=2, figsize=(13, 13))
    for a in ax:
        a.set_aspect("equal")

    # plot mf6 pathlines in map view
    pmv = flopy.plot.PlotMapView(modelgrid=mg, ax=ax[0])
    pmv.plot_grid()
    pmv.plot_array(hds[0], alpha=0.1)
    pmv.plot_vector(qx, qy, normalize=True, color="white")
    mf6_plines = mf6_pldata.groupby(["iprp", "irpt", "trelease"])
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
    mp7_plines = mp7_pldata.groupby(["particleid"])
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
    mf6_pldata_mp7 = to_mp7_format(mf6_pldata)

    # sort both dataframes by particleid and time
    mf6_pldata_mp7.sort_values(by=["particleid", "time"], inplace=True)
    mp7_pldata.sort_values(by=["particleid", "time"], inplace=True)

    # drop duplicate locations
    # (mp7 includes a duplicate location at the end of each pathline??)
    cols = ["particleid", "x", "y", "z", "time"]
    mp7_pldata = mp7_pldata.drop_duplicates(subset=cols)
    mf6_pldata_mp7 = mf6_pldata_mp7.drop_duplicates(subset=cols)

    # drop columns for which there is no direct correspondence between mf6 and mp7
    del mf6_pldata_mp7["sequencenumber"]
    del mf6_pldata_mp7["particleidloc"]
    del mf6_pldata_mp7["xloc"]
    del mf6_pldata_mp7["yloc"]
    del mf6_pldata_mp7["zloc"]
    del mp7_pldata["sequencenumber"]
    del mp7_pldata["particleidloc"]
    del mp7_pldata["xloc"]
    del mp7_pldata["yloc"]
    del mp7_pldata["zloc"]

    # compare mf6 / mp7 pathline data
    assert mf6_pldata_mp7.shape == mp7_pldata.shape
    assert np.allclose(mf6_pldata_mp7, mp7_pldata, atol=1e-3)